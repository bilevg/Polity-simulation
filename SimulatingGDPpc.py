# simulate GDPpc for countries taking into account both serial auto-correlation and neighborhood correlation
# import matplotlib as mpl
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
# from statsmodels.tsa.stattools import acf
from functools import partial
import multiprocessing
from time import time
# from functools import lru_cache
# import line_profiler
# from datetime import datetime
# import trulia.stats
# import geocoder
from timeit import timeit
import os
# import requests
from statsmodels.graphics import tsaplots
# import ConfigParser as cp
np.set_printoptions(precision=3, suppress=True, threshold=10000)

# Read the combined Polity + GDP data
df_original = pd.read_csv(
    'Polity_Data.csv', na_values='-10000')
df_original.index = df_original.ccode
# drop 'ccode'
# df_original.drop('ccode', axis=1, inplace=True)
# melt GDPpc (turn to long format from wide)
gdpcols = df_original.columns.str.contains('GDP')
gdp = df_original.loc[:, gdpcols]
gdp = gdp.round()
gdp = pd.concat([df_original.ccode, gdp], axis=1)
gdp.columns = ['ccode'] + [x.split('_')[1] for x in gdp.columns[1:]]
# melt the df_original by year
gdp = gdp.melt(id_vars='ccode', var_name='Year', value_name='GDPpc')
gdp.set_index([gdp.Year.astype(int), gdp.ccode], inplace=True)
# melt Polity
polcols = df_original.columns.str.contains('polity')
pol = df_original.loc[:, polcols]
pol = pd.concat([df_original.ccode, pol], axis=1)
pol.columns = ['ccode'] + [x.split('_')[1] for x in pol.columns[1:]]
# melt the df_original index by year
pol = pol.melt(id_vars='ccode', var_name='Year', value_name='Polity')
pol.set_index([pol.Year.astype(int), pol.ccode], inplace=True)
pol.drop('Year', axis=1, inplace=True)
# combine
df_combined = pd.concat([gdp, pol[['Polity']]], axis=1)
df_combined.Year = df_combined.Year.astype(int)
# create a 1 year lagged GDPpc and Polity grouped by country
df_combined['GDPpc_lag1'] = df_combined.groupby(
    df_combined.ccode).GDPpc.shift(1)
df_combined['Polity_lag1'] = df_combined.groupby(
    df_combined.ccode).Polity.shift(1)

# multi-index worth it?
df_combined = df_combined.sort_index()

df_complete = df_combined.dropna(how='any')

# fit a simple model to predict internal score
mod = smf.ols(formula='Polity ~ np.log(GDPpc_lag1) + Polity_lag1',
              data=df_complete)
res = mod.fit(cov_type='cluster', cov_kwds={'groups': df_complete.ccode})
# print(res.summary())
parameter_means = res.params
vcov_matrix = res.cov_params()
# parameter_draws = np.random.multivariate_normal(
#     parameter_means, vcov_matrix, 1000).transpose()

# create a simple model dataframe with only the variables in the model and intercept (1)
df_sim = pd.DataFrame({'Intercept': 1,
                       'np.log(GDPpc)': df_complete['GDPpc'],
                       'Polity': df_complete['Polity']})
df_sim['np.log(GDPpc)'] = np.log(df_sim['np.log(GDPpc)'])
df_sim['GDPpc'] = df_complete['GDPpc']
df_sim['Polity_lag1'] = df_complete['Polity_lag1']


# Function to create a dictionary of year-dictionaries of neighbors
# takes 2 arguments - distance and contiuity=True
# read the data files for contiguity:
map_dict = {}
for year in range(1946, 2017):
    mapFileName = "World_Map_" + \
        str(year) + ".csv"
    map_dict[year] = pd.read_csv(mapFileName, delimiter=',', index_col=0)
    map_dict[year].index.name = 'ccode'
# read capital distances
distances = pd.read_csv(
    'Capital_Distances.csv',  index_col=0)
distances.index.name = 'ccode'

# distances-based neighbors
dist = 400
dist_neigh = {}
for index, series in distances.iterrows():
    dist_neigh[index] = tuple(
        series.index[(series != 0) & (series <= dist)].to_series().astype(int))
# contiguity based neighbors for every year combined with distance-based
neighbors = {}
for year, dat in map_dict.items():
    neighbors_year = {}
    for featureid, series in dat.iterrows():
        neighbors_year[featureid] = sorted(tuple(
            series.index[series == 1].to_series().astype(int)) + dist_neigh[featureid])
    # convert to ccodes
    neigh_feat = {}
    for key, value in neighbors_year.items():
        value = df_original.ccode[df_original['FEATUREID_' +
                                              str(year)].isin(value)].astype(int)
        neigh_feat[int(df_original.ccode[df_original['FEATUREID_' + str(year)] == key])
                   ] = value.sort_values()
    neighbors[year] = neigh_feat

# simulation loop, use a copy because Python only references
data_copy = df_sim.astype('float32')


df_sim['nmean'] = np.nan
# generate a new feature - mean polity score of the neighbors
for year, neighborhoods in neighbors.items():
    for country, neighs in neighborhoods.items():
        countries = df_sim.loc[year, :].index.get_level_values('ccode')
        if country not in countries:
            continue
        # use global average if no neighbors or no present neighbors
        elif len(neighs) == 0 or not neighs.isin(countries).all():
            neigh_score = df_sim.loc[year, 'Polity'].mean()
        else:
            neigh_score = df_sim.loc[(year, tuple(
                neighs)), 'Polity'].mean()
        df_sim.loc[(year, country), 'nmean'] = neigh_score
        # print(neigh_score)


# @profile  # for line_profile
# @lru_cache(maxsize=None)  # for functools, memoizing
def simulate(external_weight=.1,   df=data_copy, neighbors_dict=neighbors,  n_sims=1000):
    internal_weight = 1 - external_weight
    # initialize an empty df for the results and fill in first year
    polity_scores = pd.DataFrame(
        index=df.index, columns=range(n_sims), dtype='float32')
    countries_in_first_year = np.unique(
        df.loc[1947, :].index.get_level_values('ccode'))
    polity_scores.loc[1947, :] = np.array(
        [[df.loc[(1947, c), 'Polity']] * n_sims for c in countries_in_first_year])
    # loop over years but not over the last year
    for year in np.unique(df.index.get_level_values('Year'))[:-1]:
        # initialize based on polity scores for the year, update them to produce next year polity
        internal_scores = polity_scores.loc[year, :]
        # if any are missing (countries appeared), use observed Polity for year
        missing = internal_scores.iloc[:, 0].isnull()
        internal_scores.loc[missing, :] = np.array(
            [df.loc[year, 'Polity'][missing].values] * n_sims).T
        # after getting all scores, generate internal
        internal_scores = internal_scores.apply(lambda col: np.column_stack([df.loc[year, ('Intercept', 'np.log(GDPpc)')].values, np.array(col, dtype='float32')]).dot(
            np.random.multivariate_normal(parameter_means, vcov_matrix, 1).transpose()).ravel(), axis=0, raw=True).astype('float32')
        # external score using dict of neighbors for that year
        neigh_dict = neighbors[year]
        external_scores = pd.DataFrame(
            columns=range(n_sims), index=internal_scores.index, dtype='float32')
        # list of countries in a given year:
        countries = df.loc[year, :].index.get_level_values('ccode')
        # figure out countries with no present neighbors and assign internal to them
        countries_with_no_neighbors = [
            country for country in countries if set(neigh_dict[country]).isdisjoint(set(countries))]
        # no neighbors or no present neighbors, use the internal score since
        # the mean of a number with itself is the same number
        external_scores.loc[countries_with_no_neighbors,
                            :] = internal_scores.loc[countries_with_no_neighbors, :]
        for ccode in set(countries) - set(countries_with_no_neighbors):
            present = tuple(
                set(neigh_dict[ccode]).intersection(set(countries)))
            # w/ neighbors use the weighted average with the updated scores
            weights = df.loc[(year, present), 'GDPpc'].values
            external_scores.loc[ccode, :] = np.average(
                internal_scores.loc[present, :], weights=weights, axis=0)
        # mix internal and external score based on weights
        next_year_polity = internal_scores.mul(internal_weight).add(
            external_scores.mul(external_weight)).round()
        # update, stop at penultimate year
        present = pd.Index.intersection(
            df.loc[year + 1, :].index, next_year_polity.index)
        # replace polity scores for next year, faster than .loc[]=
        polity_scores.loc[year + 1,
                          :].update(next_year_polity.loc[present, :])
    return external_weight, polity_scores
#_______________________________________________________________________________


# multi-processing
pool = multiprocessing.Pool(os.cpu_count())
results = pd.DataFrame()
# for efficiency use a divisor of cpu_count
external_weight_range = np.linspace(0.0, 0.07, 8)
n_sims = 1000

t = time()
results = pool.map(partial(simulate, n_sims=n_sims), external_weight_range)
elapsed = time() - t
pool.close()

'Approximate simulations per minute: {}'.format(
    round(n_sims * len(external_weight_range) / (elapsed / 60)))

# plot US over all values
for i, e_w in enumerate(external_weight_range):
    results[i][1].loc[(slice(None), 2), :].apply('mean', axis=1).plot()
plt.show()


# To do for simulation loops:
# DONE get first available polity score if country has not existed in the year prior, otherwise calculated score from prior year
# DONE get a dataframe of neighbors in which a row will correspond to country in a year and neighbors listed as strings
# DONE calculate neighborhood/external
# DONE calculate Polity score based on internal + external with weights for gdp
# DONE Try rounding Polity scores - make it a little more rigid (alternatively floor or ceiling); use numpy's int8 for polity scores
# incorporate Population, which then allows to weigh neigh influence by GDP total
# change from ccode to featureid everywhere?

# # fit a simple model to predict internal score
# mod = smf.ols(formula='Polity ~ np.log(GDPpc) + Polity_lag1 + nmean',
#               data=df_sim)
# res = mod.fit(cov_type='cluster', cov_kwds={'groups': df_complete.ccode})
# res.summary()
# # include population in model? at least use pop to calculate GDP total for weighting neighbor effect
