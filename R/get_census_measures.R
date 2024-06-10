
#'Extract Key Census Measures
#'
#' Gathers a number of useful census measures. 
#' Note that this function leverages the 'get_acs' function from the 'tidycensus' package.
#' 
#' tidycensus relies on the US Census Bureau API to access Census data. 
#' In order to use tidycensus, you need to obtain an API key from the US Census Bureau and provide it when making requests for data. 
#' This API key is used to authenticate your access to the Census Bureau's data and services.
#' 
#' To set the API key run the following code: 'Sys.setenv(CENSUS_KEY = YOUR KEY)'
#' 
#' @encoding UTF-8
#' 
#' 
#' @param geo_level The geography level. See https://walker-data.com/tidycensus/articles/basic-usage.html for available levels
#' @param surv The survey used. Includes all American Community Surveys (1-year,3-year,etc.)
#' @param state The U.S. State containing the desired geography
#' @param counties The counties for which data is to be extracted. Default is NULL
#' @param year the year for which data is to be gathered. Default is 2020. 
#'
#' @returns A dataframe with key census indicators
#'
#' @export
get_key_census_metric <- function(geo_level,
                                  surv,
                                  state,
                                  counties = NULL,
                                  year = 2020){
  #Necessary pacakges
  require(tidycensus)
  require(tidyverse)
  
  if(year >=2019){ 
    census_data <- 
    get_acs(geography = geo_level, 
            survey= surv,
            state = state, 
            county = counties,
            year = year,
            output="wide", 
            variables = c("B01001_001","B11001_001","B17017_002","B19058_002","B09010_002",
                          "B25003_002","B25003_003","B11001_006","B23025_005","B07201_002",
                          "B07201_003","B02001_002","B02001_003","B03003_003","B25123_002",
                          "B25123_008","B01001_006","B01001_007","B01001_008","B01001_009",
                          "B01001_010","B19013_001","B19301_001","B19083_001","B23025_002")) %>% 
      mutate(pop_male_15_to_24 = B01001_006E+B01001_007E+B01001_008E+B01001_009E+B01001_010E) %>% 
      select(-c("B01001_006E","B01001_007E","B01001_008E","B01001_009E","B01001_010E")) %>% 
      select(-ends_with("M")) %>% 
      rename(tot_pop = B01001_001E,
             tot_hh  = B11001_001E,
             hh_poverty = B17017_002E,
             hh_snap = B19058_002E,
             hh_pbassist = B09010_002E,
             hh_ownhome = B25003_002E,
             hh_rent = B25003_003E,
             hh_femhead = B11001_006E,
             pop_unemp = B23025_005E,
             pop_samehouse_1y = B07201_002E,
             pop_diffhouse_1y = B07201_003E,
             pop_white = B02001_002E,
             pop_black = B02001_003E,
             pop_hisp = B03003_003E,
             hh_physcond_own = B25123_002E,
             hh_physcond_rent = B25123_008E,
             hh_med_income = B19013_001E,
             hh_pc_income = B19301_001E,
             gini_index = B19083_001E,
             labor_force_pop = B23025_002E)
  }else if(year < 2019){
    census_data <- 
            get_acs(geography = geo_level, 
            survey= surv,
            state = state, 
            county = counties,
            year = year, 
            output="wide",
            variables = c("B01003_001","B25001_001","B17017_002","B19058_002","B09010_002",
                          "B25003_002","B25003_003","B11001_006","B23025_005","B07201_002",
                          "B07201_003","B02001_002","B02001_003","B03003_003","B25123_002",
                          "B25123_008","B01001_006","B01001_007","B01001_008","B01001_009",
                          "B01001_010","B19013_001","B19301_001","B19083_001","B23025_002")) %>% 
      mutate(pop_male_15_to_24 = B01001_006E+B01001_007E+B01001_008E+B01001_009E+B01001_010E) %>% 
      select(-c("B01001_006E","B01001_007E","B01001_008E","B01001_009E","B01001_010E")) %>% 
      select(-ends_with("M")) %>% 
      rename(tot_pop = B01003_001E,
             tot_hh  = B25001_001E,
             hh_poverty = B17017_002E,
             hh_snap = B19058_002E,
             hh_pbassist = B09010_002E,
             hh_ownhome = B25003_002E,
             hh_rent = B25003_003E,
             hh_femhead = B11001_006E,
             pop_unemp = B23025_005E,
             pop_samehouse_1y = B07201_002E,
             pop_diffhouse_1y = B07201_003E,
             pop_white = B02001_002E,
             pop_black = B02001_003E,
             pop_hisp = B03003_003E,
             hh_physcond_own = B25123_002E,
             hh_physcond_rent = B25123_008E,
             hh_med_income = B19013_001E,
             hh_pc_income = B19301_001E,
             gini_index = B19083_001E,
             labor_force_pop = B23025_002E)
  }
  #returns census_data
  return(census_data)
}
