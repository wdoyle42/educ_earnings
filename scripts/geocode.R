################################################################################
##
## <FILE> geocode.R
## <AUTH> Will Doyle and Benjamin Skinner
##
## REPLICATION for:
##
## Doyle, W.R. and Skinner, B.T. (2016) Estimating the Education-Earnings
##   Equation Using Geographic Variation. Economics of Education Review.
##
## PURPOSE:
##
## The purpose of this file is to link NLSY97 respondents with their location
## at age 17 as well as other geographic characteristics.
##
################################################################################

## clear memory
rm(list = ls())

## libraries
libs <- c('foreign')
lapply(libs, require, character.only = TRUE)

## relative directory paths
ddir <- '../data/'

## quick functions
`%+%` <- function(a,b) paste(a, b, sep = '')

## =============================================================================
## LOAD DATA
## =============================================================================

## cost and location data
wcostlist <- get(load(ddir %+% 'weightedcost.Rda'))

## nearest school data
nearestlist <- get(load(ddir %+% 'nearestschool.Rda'))

## county level variables; convert fips format for later merge
cdata <- get(load(ddir %+% 'county_medinc_data.Rda'))
cdata$fips <- sprintf(fmt = '%05d', cdata$fips)

## NLSY97 public use data
df_age <- get(load(ddir %+% 'nlsy97_age.Rda'))

## yearly data
df_year <- get(load(ddir %+% 'nlsy97_year.rda'))

## NLSY97 location data (restricted); lower names
geoc <- read.csv(ddir %+% 'Location_R15.csv')
names(geoc) <- tolower(names(geoc))

## =============================================================================
## GET COUNTY AT AGE 17
## =============================================================================

## init lists of relevant columns, first year, and yearly location
county_list <- c('geo01', paste0('geo01.', 1:14))
state_list <- c('geo02', paste0('geo02.', 1:14))
year <- 1997
yearloc <- list()

## loop through each period covered in R15 location data
for (i in 1:15){

    message('Now working with data from ' %+% year)

    ## add in leading 0s as needed, paste state and county to 5-digit fips
    temp_fips <- paste0(sprintf('%02d', unlist(geoc[state_list[i]])),
                        sprintf('%03d', unlist(geoc[county_list[i]]))
                        )

    ## Any non-numeric results are NA
    temp_fips[is.na(as.numeric(temp_fips))] <- NA

    ## add to geoc data
    geoc[paste0('fips', year)] <- temp_fips

    ## add to list
    yearloc[[as.character(year)]] <- cbind(seq(1, 8984, 1), temp_fips, year)

    ## iterate year
    year <- year + 1
}

## bind into single dataset; set names
yearloc <- data.frame(do.call('rbind', yearloc))
names(yearloc) <- c('pubid', 'fips', 'year')

## convert column data types
yearloc$pubid <- as.character(yearloc$pubid)
yearloc$fips  <- as.character(yearloc$fips)
yearloc$year  <- as.integer(as.character(yearloc$year))

## subset full NLSY97 data to month respondent turns 17
df_17 <- df_age[df_age$agem == 17,]

## order age 17 and geoc data sets by unique ids
df_17 <- df_17[order(df_17$pubid),]
geoc <- geoc[order(geoc$pubid),]

## init county fips column; make sure is character vector
df_17$fips <- NA
mode(df_17$fips) <- 'character'

## match county fips code to that when respondent was 17
df_17$fips[df_17$year == 1997] <- geoc$fips1997[df_17$year == 1997]
df_17$fips[df_17$year == 1998] <- geoc$fips1998[df_17$year == 1998]
df_17$fips[df_17$year == 1999] <- geoc$fips1999[df_17$year == 1999]
df_17$fips[df_17$year == 2000] <- geoc$fips2000[df_17$year == 2000]
df_17$fips[df_17$year == 2001] <- geoc$fips2001[df_17$year == 2001]

## =============================================================================
## COST AND INVERSE LOG DISTANCE MEASURES
## =============================================================================

## create a translator df
options(stringsAsFactors = FALSE)
geo17 <- data.frame(df_17$pubid, df_17$year, df_17$fips)
names(geo17) <- c('pubid','year','fips')
mode(geo17$fips) <- 'character'

## -----------------------------------------------
## two-year data loop
## -----------------------------------------------

geo_update2 <- NULL
for (i in 1997:2001){
    tempdata <- data.frame(wcostlist[paste0('y', i, '_pub2yr_popcenter')])
    names(tempdata) <- c('fips', paste0(c(
                                     'allcost',
                                     'allfte',
                                     'allild',
                                     'instatecost',
                                     'instatefte',
                                     'isild'),'.2yr'))

    tempdata$fips <- sprintf(fmt = '%05d', tempdata$fips)
    geo17_year <- geo17[geo17$year == i,]
    geo17_year <- merge(geo17_year, tempdata, by = c('fips'), all.x = TRUE)
    geo_update2 <- rbind(geo_update2, geo17_year)
}

## -----------------------------------------------
## four-year data loop
## -----------------------------------------------

geo_update4 <- NULL
for (i in 1997:2001){
    tempdata <- data.frame(wcostlist[paste0('y', i, '_pub4yr_popcenter')])
    names(tempdata) <- c('fips', paste0(c(
                                     'allcost',
                                     'allfte',
                                     'allild',
                                     'instatecost',
                                     'instatefte',
                                     'isild'),'.4yr'))

    tempdata$fips <- sprintf(fmt = '%05d', tempdata$fips)
    geo17_year <- geo17[geo17$year == i,]
    geo17_year <- merge(geo17_year, tempdata, by = c('fips'), all.x = TRUE)
    geo_update4 <- rbind(geo_update4, geo17_year)
}

## -----------------------------------------------
## all data loop
## -----------------------------------------------

geo_update_all <- NULL
for (i in 1997:2001){
    tempdata <- data.frame(wcostlist[paste0('y',i,'_all_popcenter')])
    names(tempdata) <- c('fips', paste0(c(
                                     'allcost',
                                     'allfte',
                                     'allild',
                                     'instatecost',
                                     'instatefte',
                                     'isild'),'.allinst'))

    tempdata$fips <- sprintf(fmt = '%05d', tempdata$fips)
    geo17_year <- geo17[geo17$year == i,]
    geo17_year <- merge(geo17_year, tempdata, by = c('fips'), all.x = TRUE)
    geo_update_all <- rbind(geo_update_all, geo17_year)
}

## =============================================================================
## NEAREST AND INDICATOR MEASURES
## =============================================================================

## -----------------------------------------------
## two-year data loop
## -----------------------------------------------

nearest2 <- NULL
for (i in 1997:2001){
    tempdata <- data.frame(nearestlist[paste0('y', i, '_pub2yr_popcenter')])
    tempdata$fips <- sprintf(fmt = '%05d', as.numeric(rownames(tempdata)))
    names(tempdata) <- c(paste0(c(
        'neardist_all',
        'neardist_ins',
        'haveschool'), '.2yr'), 'fips')

    geo17_year <- geo17[geo17$year == i,]
    geo17_year <- merge(geo17_year, tempdata, by = c('fips'), all.x = TRUE)
    nearest2 <- rbind(nearest2, geo17_year)
}

## -----------------------------------------------
## four-year data loop
## -----------------------------------------------

nearest4 <- NULL
for (i in 1997:2001){
    tempdata <- data.frame(nearestlist[paste0('y', i, '_pub4yr_popcenter')])
    tempdata$fips <- sprintf(fmt = '%05d', as.numeric(rownames(tempdata)))
    names(tempdata) <- c(paste0(c(
        'neardist_all',
        'neardist_ins',
        'haveschool'), '.4yr'), 'fips')

    geo17_year <- geo17[geo17$year == i,]
    geo17_year <- merge(geo17_year, tempdata, by = c('fips'), all.x = TRUE)
    nearest4 <- rbind(nearest4, geo17_year)
}

## -----------------------------------------------
## all institutions data loop
## -----------------------------------------------

nearest_all <- NULL
for (i in 1997:2001){
    tempdata <- data.frame(nearestlist[paste0('y', i, '_all_popcenter')])
    tempdata$fips <- sprintf(fmt = '%05d', as.numeric(rownames(tempdata)))
    names(tempdata) <- c(paste0(c(
        'neardist_all',
        'neardist_ins',
        'haveschool'), '.allinst'), 'fips')

    geo17_year <- geo17[geo17$year == i,]
    geo17_year <- merge(geo17_year, tempdata, by = c('fips'), all.x = TRUE)
    nearest_all <- rbind(nearest_all, geo17_year)
}

## =============================================================================
## MERGES
## =============================================================================

## add county-level data to geo17 data
geo_county <- merge(geo17, cdata, by = 'fips', all.x = TRUE)

## drop select columns from each of the data files; makes for cleaner merge
dropvars <- c('year', 'fips')

geo_update2 <- geo_update2[,!names(geo_update2) %in% dropvars]
geo_update4 <- geo_update4[,!names(geo_update4) %in% dropvars]
geo_update_all <- geo_update_all[,!names(geo_update_all) %in% dropvars]
nearest2 <- nearest2[,!names(nearest2) %in% dropvars]
nearest4 <- nearest4[,!names(nearest4) %in% dropvars]
nearest_all <- nearest_all[,!names(nearest_all) %in% dropvars]

## leaving in fips from county
geo_county <- geo_county[,!names(geo_county) %in% c('year')]

## merge geographic data with full dataset
df_year <- merge(df_year, geo_update2, by = 'pubid')
df_year <- merge(df_year, geo_update4, by = 'pubid')
df_year <- merge(df_year, geo_update_all, by = 'pubid')
df_year <- merge(df_year, nearest2, by = 'pubid')
df_year <- merge(df_year, nearest4, by = 'pubid')
df_year <- merge(df_year, nearest_all, by = 'pubid')
df_year <- merge(df_year, geo_county, by = 'pubid')

## =============================================================================
## CREATE CENSUS REGION/DIVISION VARIABLES
## =============================================================================

## create county and state fips columns; region column
df_year$countyfips <- df_year$fips
df_year$statefips <- substr(df_year$countyfips, 1, 2)
df_year$region <- NA

## New England
df_year$region[df_year$statefips %in% c('09',
                                        '23',
                                        '25',
                                        '33',
                                        '44',
                                        '50')] <- 'NE'

## Mid Atlantic
df_year$region[df_year$statefips %in% c('36',
                                        '34',
                                        '42')] <- 'MidAtl'
## Midwest East North Central
df_year$region[df_year$statefips %in% c('17',
                                        '18',
                                        '26',
                                        '39',
                                        '55')] <- 'MW-ENC'

## Midwest West North Central
df_year$region[df_year$statefips %in% c('19',
                                        '20',
                                        '27',
                                        '29',
                                        '31',
                                        '38',
                                        '46')] <- 'MW-WNC'

## South-South Atlantic
df_year$region[df_year$statefips %in% c('10',
                                        '11',
                                        '12',
                                        '13',
                                        '24',
                                        '37',
                                        '45',
                                        '51',
                                        '54')] <- 'South-Atlantic'

## South East South Central
df_year$region[df_year$statefips %in% c('01',
                                        '21',
                                        '28',
                                        '47')] <- 'South-ESC'

## South West South Central
df_year$region[df_year$statefips %in% c('05',
                                        '22',
                                        '40',
                                        '48')] <- 'South-WSC'

## West
df_year$region[df_year$statefips %in% c('04',
                                        '08',
                                        '16',
                                        '30',
                                        '32',
                                        '35',
                                        '49',
                                        '56')] <- 'West-WSC'
## Pacific
df_year$region[df_year$statefips %in% c('02',
                                        '06',
                                        '15',
                                        '41',
                                        '53')] <- 'Pacific'

## indicator for Southern region
df_year$south <- NA
df_year$south[df_year$region %in% c('South-Atlantic',
                                    'South-ESC',
                                    'South-WSC')] <- 1
df_year$south[!df_year$region %in% c('South-Atlantic',
                                     'South-ESC',
                                     'South-WSC')] <- 0
df_year$south[is.na(df_year$fips) == TRUE] <- NA

## =============================================================================
## LIVED IN SMSA AT AGE 17
## =============================================================================

## init first year; loop
year <- 1997
for (i in 0:14){

    message('Working with data from ' %+% year)

    ## add in leading 0s as needed, paste state and county to 5-digit fips
    temp_smsa <- rep(NA, length(geoc$geo03.1))

    if (i == 0) {
        temp_smsa[geoc['geo03'] == -4] <- 0
        temp_smsa[geoc['geo03'] > 0] <- 1
        temp_smsa[is.na(geoc['geo03']) == TRUE] <- NA
    } else {
        temp_smsa[geoc[paste0('geo03.', i)] == -4] <- 0
        temp_smsa[geoc[paste0('geo03.', i)] > 0] <- 1
        temp_smsa[is.na(geoc[paste0('geo03.', i)]) == TRUE] <- NA
    }
    ## add column
    geoc[paste0('smsa', year)] <- temp_smsa
    ## iterate year
    year <- year + 1
}

## assign for value for year respondent was 17
df_17$smsa <- NA
df_17$smsa[df_17$year == 1997] <- geoc$smsa1997[df_17$year == 1997]
df_17$smsa[df_17$year == 1998] <- geoc$smsa1998[df_17$year == 1998]
df_17$smsa[df_17$year == 1999] <- geoc$smsa1999[df_17$year == 1999]
df_17$smsa[df_17$year == 2000] <- geoc$smsa2000[df_17$year == 2000]
df_17$smsa[df_17$year == 2001] <- geoc$smsa2001[df_17$year == 2001]

## create data frame; set names; merge
df_17_smsa <- data.frame(df_17$pubid, df_17$smsa)
names(df_17_smsa) <- c('pubid','smsa')
df_year <- merge(df_year, df_17_smsa, by = 'pubid')

## =============================================================================
## WRITE TO STATA FILE FOR ANALYSIS
## =============================================================================

write.dta(df_year, file = ddir %+% 'geo_result.dta',
          version = 10,
          convert.factors = 'labels')

## =============================================================================
## END SCRIPT
################################################################################

