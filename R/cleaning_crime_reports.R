
#'Crime Data Cleaning
#'
#'Aggregates crime incident data by producing counts within certain time periods.
#'
#' @encoding UTF-8
#'
#' @param data The crime incident data to be aggregated
#' @param crime_var Categorical variable distinguishing different types of crime. This variable will be used for aggregating.
#' @param date_var Indicator representing the time of a given crime incident 
#' @param date_format Vector indicating the format of dates. Uses lubricate date format. See documentation of lubridate for available options
#' @param time_period Time interval at which data will be aggregated. Available options are "year","year_month", and "year_quarter"
#' @param geography Specified geography for additional grouping if desired. If specified, data will nest time intervals within geographies.

#' @returns A grouped dataframe of counts with a column for each crime type in "crime_var"
#' 
#' @examples 
#' aggregated_crime_data <- clean_crime_reports(raw_crime_data, crime_category,incident_date,"ymd_HS","year_quarter","bg")
#' 
#' 
#' 
#' @export
clean_crime_reports <-  function(data,
                                 crime_var,
                                 date_var,
                                 date_format,
                                 time_period = "year",
                                 geography = NULL){
  #Load necessary packages
  suppressPackageStartupMessages(require(tidyverse))
  suppressPackageStartupMessages(require(reshape2))
  suppressPackageStartupMessages(require(zoo))
  #Produces error message if data, crime variable, and time variable are not specified 
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if(is.null(data)|is.null(crime_var)|is.null(date_var)){
    stop('At least one of data, crime_var, or time_var was not specified. All are required')
  }
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Create subset of data including only the time variable and the crime variable
  if(is.null(geography)){
  data_subset <- 
    data %>% 
    dplyr::select(all_of(date_var), all_of(crime_var)) %>% 
    rename(crime = crime_var)
  } else if(!is.null(geography)){
    data_subset <- 
      data %>% 
      dplyr::select(all_of(date_var), all_of(crime_var),all_of(geography)) %>% 
      rename(crime = crime_var, geo = geography)
  }
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Produce error message is date format is not specified
  if(is.null(date_format)){
    stop('Please specify a date format. For example: "ymd_HS"')
  }
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Produce error message if invalid date format is specified
  if(!(time_period %in% c("year","year_month","year_quarter"))){
    stop('Invalid time period specified. Must be one of year, year_month, or year_quarter')
  }
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Parse Date and delete old date variable
  if(is.null(geography)){
  date_parsed <-
    data_subset %>% 
    mutate(parsed_date = parse_date_time(!!sym(date_var),orders = date_format)) %>% 
    dplyr::select(c("parsed_date",crime))
  } else if(!is.null(geography)){
    date_parsed <-
      data_subset %>% 
      mutate(parsed_date = parse_date_time(!!sym(date_var),orders = date_format)) %>% 
      dplyr::select(c(geo,"parsed_date",crime))
  }
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Use pivot_wider to convert the dataframe into wide format so that each column represents a crime type and 
  #the values correspond to the count of each crime that was observed during a particular date
  widened_data <- 
    date_parsed %>% 
    pivot_wider(names_from = crime, values_from = crime, values_fn = length, values_fill = 0)
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Creates a column according to the specified time period (i.e., year, month, etc)
  #Groups according to specified time period by counting crime instances of each crime type
  if(!(time_period %in% c("year","year_month","year_quarter"))){
    stop('invalid date format')
  }
  #Step 1 in this process is creating a time indicator variable reflecting the user's input
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if(is.null(geography)){  
  if(time_period=="year"){
    grouped_data <-
      widened_data %>% 
      mutate(year = lubridate::year(parsed_date)) %>% 
      dplyr::select(-c("parsed_date")) %>% 
      group_by(year) %>% 
      summarise_all(.funs = list(~sum(!is.na(.)))) %>%
      rename_with(~paste0(., "_count"), -year)
  }else if(time_period=="year_month"){
    grouped_data <-
      widened_data %>% 
      mutate(year_month = as.yearmon(parsed_date,"%Y-%m")) %>% 
      dplyr::select(-c("parsed_date")) %>% 
      group_by(year_month ) %>% 
      summarise_all(.funs = list(~sum(!is.na(.)))) %>%
      rename_with(~paste0(., "_count"),-year_month)
  }else if(time_period=="year_quarter"){
    grouped_data <-
      widened_data %>% 
      mutate(year_quarter = as.yearqtr(parsed_date)) %>% 
      dplyr::select(-c("parsed_date")) %>% 
      group_by(year_quarter) %>% 
      summarise_all(.funs = list(~sum(!is.na(.)))) %>%
      rename_with(~paste0(., "_count"), -year_quarter)
  }
} else if(!is.null(geography)){
  if(time_period=="year"){
    grouped_data <-
      widened_data %>% 
      mutate(year = lubridate::year(parsed_date)) %>% 
      dplyr::select(-c("parsed_date")) %>% 
      group_by(geo,year) %>% 
      summarise_all(.funs = list(~sum(!is.na(.)))) %>%
      rename_with(~paste0(., "_count"), -year)
  }else if(time_period=="year_month"){
    grouped_data <-
      widened_data %>% 
      mutate(year_month = as.yearmon(parsed_date,"%Y-%m")) %>% 
      dplyr::select(-c("parsed_date")) %>% 
      group_by(geo,year_month ) %>% 
      summarise_all(.funs = list(~sum(!is.na(.)))) %>%
      rename_with(~paste0(., "_count"),-year_month)
  }else if(time_period=="year_quarter"){
    grouped_data <-
      widened_data %>% 
      mutate(year_quarter = as.yearqtr(parsed_date)) %>% 
      dplyr::select(-c("parsed_date")) %>% 
      group_by(geo,year_quarter) %>% 
      summarise_all(.funs = list(~sum(!is.na(.)))) %>%
      rename_with(~paste0(., "_count"), -year_quarter)
  }
}
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Clean column names by replacing all symbols and blank spaces with "_"
  names(grouped_data) <- gsub("[^[:alnum:]]+", "_", tolower(names(grouped_data)))
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  return(grouped_data)
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
}


