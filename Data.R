## Data processing file for maps + Polity data + GDPpc and any controls
library(cshapes)
library(tidyr)
library(dplyr)
library(rworldmap)
library(spdep)
library(tripack)
library(maptools)
library(Amelia)
options(digits=8, max.print=1000, scipen=13)
##
Polity.data <- read.csv("~/Google Drive/GIT/democratizationsimulation/Data/Datasets/Polity/p4v2016.csv",
                        na.strings="", stringsAsFactors=FALSE)
## Polity IV 2016 changed Ivory Coast in only 2016 to Cote D'Ivoire, reverse:
Polity.data$country[Polity.data$country == "Cote D'Ivoire"] <- 'Ivory Coast'
## same for East Timor
Polity.data$country[Polity.data$country == "Timor Leste"] <- 'East Timor'
## reshape the data in the right format
Polity.df <- Polity.data %>%
    filter(year > 1945 ) %>%
    spread(key = year, value = polity2)
## rename years to year.polity
columns <- colnames(Polity.df)
columns[3:length(columns)] <- sapply(columns[3:length(columns)], function(x) {
    paste(string="polity_", x, sep="")
})
colnames(Polity.df) <- columns
##
## ## MAP STUFF ## cshapes package for all country borders between 1946 and 2015 ## It seems to be better to go with GWS years rather than COW years! No data for 2016, but I could just replicate 2015.
cs.all <- cshp(useGW=F) ## cs.thinned <- thinnedSpatialPoly(cs.all, tolerance=0.2, minarea=0.05, ## topologyPreserve = F) ## plot(cs.all);
cs.all$COUNTRY_NAME <- as.character(cs.all$CNTRY_NAME)
##
## In Polity: 2 identical COWCODEs for Serbia and Montenegro and Yugoslavia - 347; fix: create a unique code for Serbia and Montenegro: 3455, from 1992-2005; From 2006- Serbia code should be 342
cs.all@data$COWCODE[c(234, 237)] <- c(342, 342) ## Serbia Fix
## Serbia and Montenegro fix, 1992-2005:
Polity.df$ccode[c(53,54)] <- rep(3455, 2)
Polity.df$country[54] <- "Serbia and Montenegro"
Polity.df$'polity_1991'[54] <- NA
Polity.df$'polity_2006'[53] <- NA
## Serbia and Montenegro fix, 1992-2005
cs.all@data$COWCODE[230] <- 3455
cs.all@data$COUNTRY_NAME[230] <- "Serbia and Montenegro"
cs.all@data$COWCODE[231] <- 3455
cs.all@data$COWCODE[233] <- 3455
cs.all@data$COUNTRY_NAME[233] <- "Serbia and Montenegro"
## Montenegro fix
cs.all@data$COWCODE[235] <- 348
## Kosovo fix
cs.all@data$COWCODE[236] <- 341
## USSR fix
cs.all@data$COWCODE[c(192,232)] <- c(364, 364)
cs.all@data$COWEDAY[232] <- 31
## Sudans fix; Sudan (625) is 1956-2010, South Sudan (525) is 2011-, and Sudan-North (626) is 2011-now.
cs.all@data$COWCODE[246] <- 525
cs.all@data$COWCODE[245] <- 626
cs.all@data$COUNTRY_NAME[245] <- "Sudan-North"
Polity.df$'polity_2011'[127] <- NA
## Ethiopias (2 of them) fix; new one (529) start in 1993, old one is (530)
cs.all@data$COWCODE[57] <- 529; cs.all@data$GWCODE[57] <- 529;
cs.all@data$COWEDAY[195] <- 31; cs.all@data$COWEMONTH[195] <- 12;
cs.all@data$COWEYEAR[195] <- 1992
Polity.df$'polity_1993'[108] <- NA
## Pakistan fix:
Polity.df$ccode[160] <- 770
## Yemen fix, there are polity values for 3 different Yemen states in 1990
Polity.df$'polity_1990'[Polity.df$ccode == 678] <- NA ## North
Polity.df$'polity_1990'[Polity.df$ccode == 680] <- NA ## South
## Vietnams fix: Vietnam North (816) 1954-1975, Vietnam South (817) 1955-1964, 1973-1975 and Vietnam (818) 1976-
Polity.df$'polity_1976'[169] <- NA
cs.all@data$COUNTRY_NAME[205] <- "Vietnam South"
cs.all@data$COUNTRY_NAME[206] <- "Vietnam North"
cs.all@data$COWCODE[152] <- 818
## Another exception fix - Brazil's capital until 1960 is Rio de Janeiro - no point, simply setting it to Brasilia for the entire period, allows the use of a single matrix for capital distances
cs.all@data[241, c("CAPNAME", "CAPLONG", "CAPLAT")] <- cs.all@data[9, c("CAPNAME", "CAPLONG", "CAPLAT")]
## Germany - there should only be unified Germany in 1991
Polity.df$'polity_1990'[Polity.df$country == "Germany West"] <- NA
## ## to see the Yugoslavia mess
## cs.all@data[cs.all@data$COUNTRY_NAME ==  "Yugoslavia" |
##             cs.all@data$COUNTRY_NAME ==  "Serbia" |
##             cs.all@data$COUNTRY_NAME ==  "Serbia and Montenegro", ]
## Polity.df[Polity.df$country ==  "Yugoslavia" |
##           Polity.df$country ==  "Serbia" |
##           Polity.df$country ==  "Serbia and Montenegro", ]
## check to see if any are not matched
## Polity.df$country[!Polity.df$ccode %in% cs.all$COWCODE] # not in map
## next step - for each year pull the 31st of December map object matched on COW code, generate the map for every year 1950-2015;
## Should we have 1946 as the starting year (there are many missing observations though)
## Start with Polity.df, there should only be one country (ccode) value for Polity per year, if there are 2, eliminate; Pakistan and Serbia & M repeat so collapse them. Ccode is unique identifier.
##
Polity.df <- Polity.df %>%
    group_by(ccode) %>%
    ## select(ends_with("polity")) %>%
    summarise_all(funs(first(.[!is.na(.)]))) ## gets first available value, there is only one anyway
## end result map should be a 179 x 179  matrix of capital distances, value of zero if two states are contiguous
## matching by name:
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "Germany Federal Republic"] <- "Germany West"
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "Germany Democratic Republic"] <- "Germany East"
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "Slovakia"] <- "Slovak Republic"
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "Bosnia and Herzegovina"] <- "Bosnia"
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "The Gambia"] <- "Gambia"
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "Cote d'Ivoire"] <- "Ivory Coast"
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "Congo"] <- "Congo Brazzaville"
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "Congo, DRC"] <- "Congo Kinshasa"
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "Yemen Arab Republic"] <- "Yemen North"
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "Yemen People's Republic"] <- "Yemen South"
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "United Arab Emirates"] <- "UAE"
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "North Korea"] <- "Korea North"
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "South Korea"] <- "Korea South"
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "Solomon Is."] <- "Solomon Islands"
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "Myanmar"] <- "Myanmar (Burma)"
cs.all$COUNTRY_NAME[cs.all$COUNTRY_NAME == "Timor Leste"] <- "East Timor"
Polity.df$country[!Polity.df$country %in% cs.all$COUNTRY_NAME] # not in map
Polity.df$country[!Polity.df$ccode %in% cs.all$COWCODE] # not in map
## Manual fix for countries that do not exist on the map but are in Polity
## Syria
cs.all$COWEYEAR[96] <- 1961
################################################################################
################################################################################
## fix for 2016 (map ends in June, we want to use 31 Dec for all)
cs.all$COWEMONTH[cs.all$COWEYEAR == 2016] <- 12
cs.all$COWEDAY[cs.all$COWEYEAR == 2016] <- 31

## function to get a snapshot of time for each year of
## the country shapes; returns list with sp dataframe elements for each year
select.years <- function(Begin.year = 1946, End.year = 2016, data=cs.all){
    ## data <- data[data$COWCODE >= 0, ]
    if (is.na(Begin.year) | is.na(End.year)) {
        stop("Need a begin and end year")
    }
    if (Begin.year < 1946 | End.year > 2016) {
        stop("Specified years are out of range")
    }
    data$startdate <- as.Date(paste(data$COWSYEAR, data$COWSMONTH,
                               data$COWSDAY, sep = "-"))
    data$enddate <- as.Date(paste(data$COWEYEAR, data$COWEMONTH,
                                  data$COWEDAY, sep = "-"))
    out <- list()
    l <- 1
    ## include only countries for which we have polity data
    for(i in Begin.year:End.year){
        # vector for country present in that year in polity
        present <- !is.na(Polity.df[ , paste("polity_", i, sep="")])
        present.ccode <- Polity.df$ccode[present]
        ## try to match to countries in cshape based on ccode == COWCODE, country == COUNTRY_NAME and year
        clean_feature_id <- c() ## based on unique FEATUREID identifier in cs.all
        for(c in 1:length(present.ccode)){
            pol.code <- present.ccode[c]
            ## if only one match on ccode
            if (sum(pol.code == data$COWCODE) == 1){
                clean_feature_id[c] <- data$FEATUREID[data$COWCODE == pol.code]
            } else {
                if (sum(pol.code == data$COWCODE) == 0){
                    clean_feature_id[c] <- NA
                } else {
                    ## need to pick the right year of the map; cs.all
                    matches.df <- data@data[data$COWCODE == pol.code, ]
                    for (j in 1:dim(matches.df)[2]){
                        ## try by name
                        if (sum(Polity.df$country[Polity.df$ccode == pol.code] == matches.df$COUNTRY_NAME) == 1){
                            clean_feature_id[c] <- matches.df$FEATUREID[matches.df$COUNTRY_NAME == (Polity.df$country[Polity.df$ccode == pol.code])]
                        } else { ## try by year
                            current.year <- paste(i, 12, 31, sep="-") # end of the year
                            matches.part <- matches.df[matches.df$startdate <= current.year & matches.df$enddate >= current.year, ]
                            if (dim(matches.part)[1] == 1) {
                                clean_feature_id[c] <- matches.part$FEATUREID
                            } else{
                                ## try by alternative GW year
                                matches.df$GWsdate <- as.Date(paste(matches.df$GWSYEAR, matches.df$GWSMONTH,
                                                                      matches.df$GWSDAY, sep = "-"))
                                matches.df$GWedate <- as.Date(paste(matches.df$GWEYEAR, matches.df$GWEMONTH,
                                                                      matches.df$GWEDAY, sep = "-"))
                                matches.GW <- matches.df[matches.df$GWsdate <= current.year & matches.df$GWedate >= current.year, ]
                                if (dim(matches.GW)[1] == 1) {
                                    clean_feature_id[c] <- matches.GW$FEATUREID
                                }
                                if (dim(matches.GW)[1] > 1) {
                                    stop(matches.GW$COUNTRY_NAME[1], " is present in two different GW periods in ", i, ".")
                                }
                                if (dim(matches.GW)[1] == 0) {
                                    stop("Country ", matches.GW$COUNTRY_NAME[1] ," is not present in any time period.")
                                }
                            }
                        }
                    }
                }
            }
        }
        print(i)
        out.data <- data[data$FEATUREID %in% clean_feature_id, ]
        ## remove unnecessary variables
        out.data@data <- out.data@data %>%
            select(-c(COWSYEAR:ISO1AL3)) %>%
            select(-c(CNTRY_NAME, AREA))
        pol.data <- Polity.df[present, ]
        pol.data$country <- NULL
        pol.data$FEATUREID <- clean_feature_id
        out.data@data <- data.frame(merge(out.data, pol.data , by="FEATUREID"))
        ## polygon IDs are FEATUREID
        spChFIDs(out.data) <- out.data$FEATUREID
        out[[l]] <- out.data
        names(out)[l] <- i
        l <- l + 1
    }
    out
}
cs.list <- select.years(Begin.year = 1946, End.year = 2016)
##plot(cs.list[["2016"]])


## Now produce FEATUREID-year  so that we have one data file with polity to import
for(i in names(cs.list)){
    dat <- (cs.list[[i]])@data
    column <- rep(NA, length(Polity.df$ccode))
    for (c in seq_along(Polity.df$ccode)){
        country <- Polity.df$ccode[c]
        if(country %in% dat$ccode){
            column[c] <- dat$FEATUREID[dat$ccode == country]
        }
    }
    Polity.df <- data.frame(cbind(Polity.df, column))
    colnames(Polity.df)[dim(Polity.df)[2]] <- paste("FEATUREID_", i , sep="")
}
##

################################################################################
## Create a matrix of FEATUREID x FEATUREID for contiguity for every year and save it
for(i in seq_along(cs.list)){
    world <- cs.list[[i]]
    world.contiguous <- poly2nb(world, row.names = world@data$id, snap=.7, queen=TRUE)
    contiguous <- nb2listw(world.contiguous, style = "B", zero.policy=TRUE)
    lw.contiguous <- contiguous$neighbours
    neighbors <- list()
    for(j in 1:length(lw.contiguous)){
        vec <- lw.contiguous[[j]]
        if(length(vec) > 1 & vec[1] == 0) vec <- as.integer(vec[vec != 0])
        neighbors[[j]] <- vec
    }
    ## create a legitimate nb object for plotting and estimating Moran's I:
    attributes.copy <- attributes(world.contiguous)
    world.nb <- neighbors
    attributes(world.nb) <- attributes.copy
    nb.map <- data.frame(nb2mat(world.nb, style = "B", zero.policy = TRUE))
    colnames(nb.map) <- rownames(nb.map)
    nb.map <- cbind("FEATUREID" = rownames(nb.map), nb.map)
    ## print(dim(nb.map))
    ## save
    write.table(nb.map, paste("~/Google Drive/GIT/democratizationsimulation/Data/World_Map_",
                              names(cs.list)[i], ".csv", sep=""),
                quote=FALSE, sep=",", na="-10000", row.names=FALSE)
}



################################################################################
## Now to write out a matrix that has all of the capitals distances
dist.dat <- data.frame("FEATUREID"=NA, "CAPLONG"=NA, "CAPLAT"=NA)
for(j in seq_along(cs.all$FEATUREID)){
    id <- cs.all$FEATUREID[j]
    for(i in seq_along(cs.list)){
        world <- cs.list[[i]]
        ids <- world$FEATUREID
        if(id %in% ids){
            dist.dat[j, ] <- c(id, world$CAPLONG[ids == id], world$CAPLAT[ids == id])
        } else {next}
    }
}
dist.dat <- na.omit(dist.dat); rownames(dist.dat) <- NULL
mat <- as.matrix(dist.dat[, c(2:3)], ncol=2)
distances <- round(spDists(mat, longlat=TRUE), 0)
distances[distances == 0] <- NA
diag(distances) <- 0
distances <- cbind(dist.dat$FEATUREID, distances)
colnames(distances) <- c("FEATUREID", dist.dat$FEATUREID)
## ZEROES EXIST BECAUSE THE SAME GEOGRAPHIC STATE SHOWS UP MULTIPLE TIMES AS A DIFFERENT COUNTRY IN DIFFERENT POINTS IN TIME
## ## add a final column which sums up all the distances for each country
## distances <- cbind(distances, apply(distances, 1, sum, na.rm=T))
## write out
write.table(distances, "~/Google Drive/GIT/democratizationsimulation/Data/Capital_Distances.csv",
            quote=FALSE, sep=",", na="-10000", row.names=FALSE, col.names = TRUE)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


## GDPpc data from 2 sources, World Bank Dev Indicators and Maddison Project
## First and primary source is MPD (2011 $ chained), second is WB Dev Indicators (2010 $ chained), most of the recent values come from there
## use all available MP data points and then fill the remaining with WB after converting between the two using 1990 as the year of equivalency (to set up a conversion ratio)


## mpd <- read.csv("~/Google Drive/GIT/democratizationsimulation/Data/Datasets/MPD2013Modified.csv",
##                 na.strings="", check.names = FALSE)
## ## reshape the data in the right format
## mpd.df <- mpd %>%
##     gather(key = Year, value=GDPpc, na.rm=TRUE)
## colnames(mpd.df)[2] <- "Country_Name"
## mpd.df <- mpd.df %>%
##     mutate(y1 = 'GDPpc') %>% # neat trick to mass rename columns
##     unite(year, y1, Year) %>%
##     spread(key = year, value=GDPpc)

## import Maddison Project 2018
mpd <- read.csv("~/Google Drive/GIT/democratizationsimulation/Data/Datasets/mpd2018.csv",
                na.strings="", stringsAsFactors=FALSE)
## drop country code, it doesn't match the other ones
## remove years before 1946; rename GDPpc column
mpd <- mpd %>%
    select(country, year, cgdppc, pop) %>%
    filter(year > 1945 ) %>%
    rename(GDPpc = cgdppc)
## reshape the data in the right format for GDPpc
mpd.gdp <- mpd %>%
    select(-pop) %>%
    gather(key = key, value=value, na.rm=TRUE, GDPpc) %>%
    unite(year, key, year) %>%
    spread(key = year, value=value)
## same for population
mpd.pop <- mpd %>%
    select(-GDPpc) %>%
    gather(key = key, value=value, na.rm=TRUE, pop) %>%
    unite(year, key, year) %>%
    spread(key = year, value=value)
## join them
mpd <- join(mpd.gdp, mpd.pop)



Polity.df$country[!Polity.df$country %in% mpd$country]
## match by mpd$country and Polity.df$country
mpd$country[mpd$country == "Myanmar"] <- "Myanmar (Burma)"
mpd$country[mpd$country == "D.P.R. of Korea"] <- "Korea North"
mpd$country[mpd$country == "Republic of Korea"] <- "Korea South"
mpd$country[mpd$country == "Slovakia"] <- "Slovak Republic"
## Fix for the two Vietnams; Vietnam values should apply to both South and North
mpd <- data.frame(rbind(mpd, mpd[mpd$country == "Viet Nam", ]))
mpd$country[length(mpd$country)] <- "Vietnam North"
mpd <- data.frame(rbind(mpd, mpd[mpd$country == "Viet Nam", ]))
mpd$country[length(mpd$country)] <- "Vietnam South"
mpd$country[mpd$country == "Viet Nam"] <- "Vietnam"
## Fix for the Yemen; same for the two pre-1990 Yemens
mpd <- data.frame(rbind(mpd, mpd[mpd$country == "Yemen", ]))
mpd$country[length(mpd$country)] <- "Yemen North"
mpd <- data.frame(rbind(mpd, mpd[mpd$country == "Yemen", ]))
mpd$country[length(mpd$country)] <- "Yemen South"
## Fix for Germany, should only apply to Western Germany before 1990
mpd <- data.frame(rbind(mpd, mpd[mpd$country == "Germany", ]))
mpd$country[length(mpd$country)] <- "Germany West"
## Fix for Sudans, should match to former (till 2010) and the 2 new ones
mpd$country[mpd$country == "Sudan (Former)"] <- "Sudan"
mpd <- data.frame(rbind(mpd, mpd[mpd$country == "Sudan", ]))
mpd$country[length(mpd$country)] <- "South Sudan"
mpd <- data.frame(rbind(mpd, mpd[mpd$country == "Sudan", ]))
mpd$country[length(mpd$country)] <- "Sudan-North"
## Fix for Venezuela
mpd$country[mpd$country == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
## for Bolivia
mpd$country[mpd$country == "Bolivia (Plurinational State of)"] <- "Bolivia"
## for Macedonia
mpd$country[mpd$country == "TFYR of Macedonia"] <- "Macedonia"
## for Yugoslavia
mpd$country[mpd$country == "Former Yugoslavia"] <- "Yugoslavia"
## for Bosnia
mpd$country[mpd$country == "Bosnia and Herzegovina"] <- "Bosnia"
## for Moldova
mpd$country[mpd$country == "Republic of Moldova"] <- "Moldova"
## USSR
mpd$country[mpd$country == "Former USSR"] <- "USSR"
## Russia
mpd$country[mpd$country == "Russian Federation"] <- "Russia"
## Cape Verde
mpd$country[mpd$country == "Cabo Verde"] <- "Cape Verde"
## Ivory Coast
mpd$country[mpd$country == "Côte d'Ivoire"] <- "Ivory Coast"
## Congo DR
mpd$country[mpd$country == "D.R. of the Congo"] <- "Congo Kinshasa"
## Congo
mpd$country[mpd$country == "Congo"] <- "Congo Brazzaville"
## Tanzania
mpd$country[mpd$country == "U.R. of Tanzania: Mainland"] <- "Tanzania"
## Iran
mpd$country[mpd$country == "Iran (Islamic Republic of)"] <- "Iran"
## Syria
mpd$country[mpd$country == "Syrian Arab Republic"] <- "Syria"
## UAE
mpd$country[mpd$country == "United Arab Emirates"] <- "UAE"
## Taiwan
mpd$country[mpd$country == "Taiwan, Province of China"] <- "Taiwan"
## Laos
mpd$country[mpd$country == "Lao People's DR"] <- "Laos"
## Serbia fix; Serbia alone (2006-) and Serbia and Montenegro (1993-2005)
mpd <- data.frame(rbind(mpd, mpd[mpd$country == "Serbia", ]))
mpd$country[length(mpd$country)] <- "Serbia and Montenegro"
## ## check to see what is not matched
Polity.df$country[!Polity.df$country %in% mpd$country]
## this is as good as it gets for MPD, later we can fill as many of the blanks as we can with the WB Development Indicators database.                                        #--------------------------------------------------------------------------------
## combine Polity.df and mpd
combined.df <- join(Polity.df, mpd, by='country')
## ## check the missingness map
## missmap(combined.df, rank.order = F)
## write out a csv file for Python
write.table(combined.df, "~/Google Drive/GIT/democratizationsimulation/Data/Polity_Data.csv",
             quote=FALSE, sep=",", na="-10000", row.names=FALSE)







## library(wbstats)
## ## get the GDPpc contant 2010$ and Population total indicators from Development Indicators
## wb.df = wb(indicator=c('NY.GDP.PCAP.KD', 'SP.POP.TOTL'),
##            country='countries_only', start=1960, end=2016)

## wb.df = wb.df %>%
##     select(country, date, value, indicator) %>%
##     spread(key = indicator, value=value) %>%
##     rename(year = date, GDPpc = 'GDP per capita (constant 2010 US$)', pop = 'Population, total')
## ## reshape the data in the right format for GDPpc
## wb.gdp <- wb.df %>%
##     select(-pop) %>%
##     gather(key = key, value=value, na.rm=TRUE, GDPpc) %>%
##     unite(year, key, year) %>%
##     spread(key = year, value=value)
## ## same for population
## wb.pop <- wb.df %>%
##     select(-GDPpc) %>%
##     gather(key = key, value=value, na.rm=TRUE, pop) %>%
##     unite(year, key, year) %>%
##     spread(key = year, value=value)
## ## join them
## wb.df <- join(wb.gdp, wb.pop)

## ## match by wb.df$country and Polity.df$country
## wb.df$country[wb.df$country == "Venezuela, RB"] <- "Venezuela"
## wb.df$country[wb.df$country == "Timor-Leste"] <- "East Timor"
## wb.df$country[wb.df$country == "Lao P DR"] <- "Laos"
## wb.df$country[wb.df$country == "Congo, Democratic Republic"] <- "Congo Kinshasa"
## wb.df$country[wb.df$country == "Congo, Republic"] <- "Congo Brazzaville"
## wb.df$country[wb.df$country == "Bosnia and Herzegovina"] <- "Bosnia"
## wb.df$country[wb.df$country == "Myanmar"] <- "Myanmar (Burma)"
## wb.df$country[wb.df$country == "Cabo Verde"] <- "Cape Verde"
## wb.df$country[wb.df$country == "Syrian"] <- "Syria"
## wb.df$country[wb.df$country == "Russian Federation"] <- "Russia"
## wb.df$country[wb.df$country == "United Arab Emirates"] <- "UAE"
## wb.df$country[wb.df$country == "Kyrgyz Republic"] <- "Kyrgyzstan"
## wb.df$country[wb.df$country == "Cote d'Ivoire"] <- "Ivory Coast"
## wb.df <- data.frame(rbind(wb.df, wb.df[wb.df$country == "Germany", ]))
## wb.df$country[length(mpd$country)] <- "Germany West"

## ## Fix for the two Serbias; Serbia & Monte (1992-2005) and Serbia (2006-2015)
## wb.df <- data.frame(rbind(wb.df, wb.df[wb.df$country == "Serbia", ]))
## wb.df$country[length(wb.df$country)] <- "Serbia and Montenegro"
## ## Fix for two Sudans, North Sudan will simply be the new Sudan, South is too small
## wb.df <- data.frame(rbind(wb.df, wb.df[wb.df$country == "Sudan", ]))
## wb.df$country[length(wb.df$country)] <- "Sudan-North"



## Polity.df$country[!Polity.df$country %in% wb.df$country]





## ## Countries not in WB Dev Indicators: Taiwan, Yugoslavia, USSR, Czechoslovakia, North and South Yemen. All of them will come from MPD for the years available. North Yemen data can come from MPD (1950-1990) as the South was very small and poor (not an independent state?).
## wb.df <- read.csv("~/Google Drive/GIT/democratizationsimulation/Data/Datasets/WBGDPpcModified.csv",
##                 na.strings="", check.names = FALSE)
## colnames(wb.df) <- sapply(colnames(wb.df), function(x) {
##     if (!is.na(as.numeric(x))) {
##         paste(string="GDPpc_", x, sep="")
##     }
##     else x
## })
## wb.df$Country_Name <- as.character(wb.df$Country_Name)
## ## match by wb.df$Country_Name and Polity.df$country
## wb.df$Country_Name[wb.df$Country_Name == "Venezuela, RB"] <- "Venezuela"
## wb.df$Country_Name[wb.df$Country_Name == "Timor-Leste"] <- "East Timor"
## wb.df$Country_Name[wb.df$Country_Name == "Lao P DR"] <- "Laos"
## wb.df$Country_Name[wb.df$Country_Name == "Congo, Democratic Republic"] <- "Congo Kinshasa"
## wb.df$Country_Name[wb.df$Country_Name == "Congo, Republic"] <- "Congo Brazzaville"
## wb.df$Country_Name[wb.df$Country_Name == "Bosnia and Herzegovina"] <- "Bosnia"
## wb.df$Country_Name[wb.df$Country_Name == "Myanmar"] <- "Myanmar (Burma)"
## wb.df$Country_Name[wb.df$Country_Name == "Cabo Verde"] <- "Cape Verde"
## wb.df$Country_Name[wb.df$Country_Name == "Syrian"] <- "Syria"
## wb.df$Country_Name[wb.df$Country_Name == "Russian Federation"] <- "Russia"
## wb.df$Country_Name[wb.df$Country_Name == "United Arab Emirates"] <- "UAE"
## wb.df$Country_Name[wb.df$Country_Name == "Kyrgyz Republic"] <- "Kyrgyzstan"
## ## Fix for the two Serbias; Serbia & Monte (1992-2005) and Serbia (2006-2015)
## wb.df <- data.frame(rbind(wb.df, wb.df[wb.df$Country_Name == "Serbia", ]))
## wb.df$Country_Name[178] <- "Serbia and Montenegro"
## ## Fix for two Sudans, North Sudan will simply be the new Sudan, South is too small
## wb.df <- data.frame(rbind(wb.df, wb.df[wb.df$country == "Sudan", ]))
## wb.df$country[length(wb.df$country)] <- "Sudan-North"

## Polity.df$country[!Polity.df$country %in% wb.df$country]
################################################################################



## ### So, first merge wb.df with mpd; only when a country is not present in wb.df use wbppp.df instead

## ## How to transition between the two GDP databases, one based on 1990$ and the other on 2010$
## ## for each country, pick such a value that the summed distance after the adjustment is minimized? or just one value - first available value of intersection set;
## ## fit a linear model without intercept for the intersection period?
## mpd.gdp <- select(mpd, -starts_with('pop_'))
## coef.frame <- data_frame('Country' = NA, 'Intercept' = NA, 'Coef' = NA)
## i <- 1
## for (country in wb.df$Country_Name[wb.df$Country_Name %in% mpd.gdp$country]){
##     par(ask=TRUE)
##     coef.frame[i, 1] = country
##     ## How to transition between the two GDP databases, one based on 1990$ and the other on 2010$
##     mpd.gdp.series <- mpd.gdp[mpd.gdp$country == country, ]
##     range.y <- range(t(mpd.gdp.series[-1]), na.rm=TRUE)
##     plot(t(mpd.gdp.series[-1]), x=c(1946:2016), col="green", xlim=c(1946, 2016),
##          ylim = c(range.y[1] - 3000, range.y[2] + 3000))
##     wb.series <- wb.df[wb.df$Country_Name == country, ]
##     points(t(wb.series[-1]), col="blue", x=c(1960:2016))
##     mp <- t(mpd.gdp.series[-1]); mp <- mp[15:length(mp)]
##     wb <- t(wb.series[-1])
##     if (!all(is.na(wb)) && !all(is.na(mp)) && dim(na.omit(data.frame(wb=wb, mp=mp)))[1] != 0) {
##         coefs <- coef(lm(mp ~ wb))
##         coef.frame[i, 2:3] <- coefs
##         points(t(wb)*coefs[2] + coefs[1], col="red", x=c(1960:2016))
##         text(labels = country, x=1980, y=mean(range.y, na.rm=TRUE) + 2000, cex=2)
##         text(labels = round(coefs,3), x=1960, y=mean(range.y, na.rm=TRUE) + 2000, cex=1)
##     } else {print(country);  coef.frame[i, 2:3] = NA}
##     i = i + 1
## }








## ## Alternative WB DEV indicators
## wbppp.df <- read.csv("~/Google Drive/GIT/democratizationsimulation/Data/Datasets/WBGDPpcPPP2011Modified.csv",
##                 na.strings="", check.names = FALSE)
## colnames(wbppp.df) <- sapply(colnames(wbppp.df), function(x) {
##     if (!is.na(as.numeric(x))) {
##         paste(string="GDPpc_", x, sep="")
##     }
##     else x
## })
## wbppp.df$Country_Name <- as.character(wbppp.df$Country_Name)
## ## match by wbppp.df$Country_Name and Polity.df$country
## wbppp.df$Country_Name[wbppp.df$Country_Name == "Venezuela, RB"] <- "Venezuela"
## wbppp.df$Country_Name[wbppp.df$Country_Name == "Timor-Leste"] <- "East Timor"
## wbppp.df$Country_Name[wbppp.df$Country_Name == "Lao PDR"] <- "Laos"
## wbppp.df$Country_Name[wbppp.df$Country_Name == "Congo, Democratic Republic"] <- "Congo Kinshasa"
## wbppp.df$Country_Name[wbppp.df$Country_Name == "Congo, Republic"] <- "Congo Brazzaville"
## wbppp.df$Country_Name[wbppp.df$Country_Name == "Bosnia and Herzegovina"] <- "Bosnia"
## wbppp.df$Country_Name[wbppp.df$Country_Name == "Myanmar"] <- "Myanmar (Burma)"
## wbppp.df$Country_Name[wbppp.df$Country_Name == "Cabo Verde"] <- "Cape Verde"
## wbppp.df$Country_Name[wbppp.df$Country_Name == "Syrian"] <- "Syria"
## wbppp.df$Country_Name[wbppp.df$Country_Name == "Russian Federation"] <- "Russia"
## wbppp.df$Country_Name[wbppp.df$Country_Name == "United Arab Emirates"] <- "UAE"
## wbppp.df$Country_Name[wbppp.df$Country_Name == "Kyrgyz Republic"] <- "Kyrgyzstan"
## ## Fix for the two Serbias; Serbia & Monte (1992-2005) and Serbia (2006-2015)
## wbppp.df <- data.frame(rbind(wbppp.df, wbppp.df[wbppp.df$Country_Name == "Serbia", ]))
## wbppp.df$Country_Name[178] <- "Serbia and Montenegro"
## ## Fix for two Sudans, North Sudan will simply be the new Sudan, South is too small
## wbppp.df <- data.frame(rbind(wbppp.df, wbppp.df[wbppp.df$Country_Name == "Sudan", ]))
## wbppp.df$Country_Name[179] <- "Sudan-North"
## Polity.df$country[!Polity.df$country %in% wbppp.df$Country_Name]


## ## How to transition between the two GDP databases, one based on 1990$ and the other on 2011$
## ## for each country, pick such a value that the summed distance after the adjustment is minimized? or just one value - first available value of intersection set;
## ## fit a linear model without intercept for the intersection period?
## exp.vector <- c()
## for (country in wbppp.df$Country_Name[wbppp.df$Country_Name %in% mpd$country]){
##     par(ask=FALSE)
##     ## How to transition between the two GDP databases, one based on 1990$ and the other on 2011$
##     mpd.series <- mpd[mpd$country == country, ]
##     range.y <- range(t(mpd.series[-1]), na.rm=TRUE)
##     ## plot(t(mpd.series[-1]), x=c(1946:2010), col="green", xlim=c(1946, 2015),
##     ##      ylim = c(range.y[1] - 3000, range.y[2] + 10000))
##     wbppp.series <- wbppp.df[wbppp.df$Country_Name == country, ]
##     ## points(t(wbppp.series[-1]), col="blue", x=c(1960:2015))
##     mp <- t(mpd.series[-1]); mp <- mp[15:length(mp)]
##     wb <- t(wbppp.series[-1]); wb <- wb[1:51]
##     if (!all(is.na(wb)) && !all(is.na(mp)) && dim(na.omit(data.frame(wb=wb, mp=mp)))[1] != 0) {
##         exp.factor <- coef(lm(wb ~ mp - 1))
##         exp.vector <- c( exp.vector, exp.factor)
##         ## points(t(mpd.series[-1])*exp.factor, col="red", x=c(1946:2010))
##         ## text(labels = country, x=1980, y=mean(range.y, na.rm=TRUE) + 2000, cex=3)
##         ## text(labels = round(exp.factor,3), x=1960, y=mean(range.y, na.rm=TRUE) + 2000, cex=3)
##     } else {print(country); exp.vector <- c(exp.vector, NA)}
## }
## ## What coversion factor or multiplier to use between the two datasets for countries for which we do not have an overlap?
## overlap.names <- wbppp.df$Country_Name[wbppp.df$Country_Name %in% mpd$country]
## overlap.names[exp.vector > 3] # remove the outliers, mostly oil countries
## hist(exp.vector[exp.vector < 3])
## multiplier <- mean(exp.vector[exp.vector < 3], na.rm=T)
## ## answer - about 1.77
## ## convert MPD to WB GDP pc PPP 2011 constant dollars using the exp.vector where available and the multiplier for the others
## converter <- function(x){
##     if (as.character(x[1]) %in% overlap.names){
##         model.multiplier <- as.numeric(exp.vector[overlap.names == as.character(x[1])])
##         if (is.na(model.multiplier)) {model.multiplier <- multiplier }
##         x[2:length(x)] <- sapply(x[2:length(x)], function(y){
##             ifelse(is.na(y), NA, as.numeric(y) * model.multiplier)})
##     } else {
##             x[2:length(x)] <- sapply(x[2:length(x)], function(y){
##                 ifelse(is.na(y), NA, as.numeric(y) * multiplier)})
##     }
##     x[2:length(x)] <- round(x[2:length(x)], 0)
##     x
## }
## ## create a multiplier column
## for (i in 1:nrow(mpd)){
##     mpd[i, ] <- converter(mpd[i, ])
## }
## mpd.con.df <- mpd


## ## check visually:
## for (country in wbppp.df$Country_Name){
##     par(ask=TRUE)
##     wbppp.series <-  wbppp.df[wbppp.df$Country_Name == country, ]
##     wb.series <- wb.df[wb.df$Country_Name == country, ]
##     if(!all(is.na(wbppp.series[-1]))) {
##         wbppp.series <- as.numeric(c(rep(NA,14), wbppp.series[-1]))
##         wb.series <- as.numeric(c(rep(NA,14), wb.series[-1]))
##         plot(t(wbppp.series), x=c(1946:2015), col="green", type='l',
##              ylim = c(0, max(wbppp.series, na.rm=T) + 10000))
##         lines(t(wb.series), x=c(1946:2015), col="purple", type='l')
##         text(labels = country, x=1980, y=mean(wbppp.series, na.rm=TRUE) + 2000, cex=3)
##         if (country %in% mpd.con.df$Country_Name){
##             mpd.series <- mpd.con.df[mpd.con.df$Country_Name == country, ]
##             if(!all(is.na(mpd.series[2:length(mpd.series)]))){
##                 lines(t(mpd.series[-1]), col="blue", x=c(1946:2010), cex=1.5)
##             }
##         }
##     ## mp <- t(mpd.series[-1]); mp <- mp[15:length(mp)]
##     ## wb <- t(wbppp.series[-1]); wb <- wb[1:51]
##     } else print(country)
## }


## ## Merge WBDevInd and MPD into Polity data, first WBDev, then fill in NA with MPD; special fix for Cuba is needed
## ## create empty GDP columns and add to the Polity.df
## cols <- colnames(Polity.df)
## number.of.cols <- dim(wbppp.df)[2] - 1
## combined.df <- cbind(Polity.df, matrix(NA, nrow=dim(Polity.df)[1], ncol=number.of.cols + 14)) ## all the way to 1946
## ## rename gdp columns
## colnames(combined.df)[(length(cols) + 1):dim(combined.df)[2]] <- paste("GDPpc_", 1946:2015, sep="")
## ## replace the available values from WBppp and then from MPD
## for (i in 1:nrow(combined.df)){
##     country <- combined.df$country[i]
##     if (country %in% wbppp.df$Country_Name){
##         combined.df[i, (length(cols) + 15):dim(combined.df)[2]] <- wbppp.df[wbppp.df$Country_Name == country, -1]
##     }
##     if (country %in% mpd.con.df$Country_Name){
##         target.vector <- combined.df[i, (length(cols) + 1):(dim(combined.df)[2] - 5)]
##         missing.values <- is.na(target.vector) ## only till 2010
##         replacement.vector <- mpd.con.df[mpd.con.df$Country_Name == country, -1]
##         if (all(missing.values)){
##             target.vector <- replacement.vector
##         } else {
##             target.vector[missing.values] <- replacement.vector[missing.values]
##         }
##         combined.df[i, (length(cols) + 1):(dim(combined.df)[2] - 5)] <- target.vector
##     }
## }


## ## check the missingness map
## missmap(combined.df, rank.order = F)



## ## ## check it out:
## ## write.table(combined.df, file="~/Google Drive/GIT/democratizationsimulation/Data/Combined.csv", row.names=F,sep=",")
## write.table(combined.df, "~/Google Drive/GIT/democratizationsimulation/Data/Polity_Data.csv",
##              quote=FALSE, sep=",", na="-10000", row.names=FALSE)


## ## write a function to determine if  FEATUREID contains no duplicates in every year
## for (i in 1946:2016){
##     feat <- Polity.df[, paste("FEATUREID_", i, sep="")]
##     feat <- feat[!is.na(feat)]
##     print(all(!duplicated(feat)))
## }



## ## new package to try out: countrycode
## library(countrycode)
## countrycode(Polity.data$ccode, 'p4_ccode', 'cown')


## how to forecast GDPpc for each country (take a few trend values (-1% per year to 3% per year?))
library(timeSeries)
library(forecast)
library(ggplot2)
library(tidyr)

# forecast?
mpd.ts <- mpd %>%
    select(country, year, GDPpc) %>%
    spread(key=country, value=GDPpc)

mpd.ts <- ts(mpd.ts, start=1946, end=2016, frequency=1)

future.gdp <- forecast(mpd.ts, h=50, robust = TRUE, additive.only=TRUE, level=c(0), damped=FALSE)

plot(future.gdp$forecast$'Afghanistan')

library(corrplot)
C <- mpd.ts %>% cor(use='pairwise.complete.obs')
diag(C) <- NA
mean(C, na.rm=T)

corrplot(C)

us <- mpd %>%
    filter(country == 'United States')

country.gdp <- mpd %>%
    filter(country == 'United States') %>%
    select(GDPpc)
fit <- ets(country.gdp$GDPpc, model='AAN',  damped=FALSE)
forecast <- forecast(fit, h=50, level=75)
summary(forecast)
plot(forecast)


## random walk forecast with drift:
rwf(us$GDPpc, h=20, drift=TRUE)


## arima forecast
fit.ar <- auto.arima(us$GDPpc)
auto.arima(mpd.ts)
forecast.ar <- forecast(fit.ar, h=20)
plot(forecast.ar)
