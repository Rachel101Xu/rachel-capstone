# Purpose: Computation or variable-creation helper functions that are applicable
#   to any project
# All helper functions ought to be in alphabetical order

library(data.table)
library(dplyr)
library(stringr)
library(tidyr)

# Define_month_frac() defines the month_frac variable as numeric and continuous
Define_month_frac <- function(data) {
  return(data %>% mutate(month_frac = floor(date/100) + ((date-1) %% 12)/12 ) ) 
}

# Define_round_bounds defines bounds for the min & max of a matrix (or vector), 
#   rounded based on scale (number of digits)
Define_round_bounds <- function(mat, scale) {
  max_diff <- ceiling(max(mat) / scale) * scale
  min_diff <- floor(min(mat) / scale) * scale
  bound <- max(max_diff, abs(min_diff))
  return(c(-1*bound, bound)) 
}

# Gen_ihs() generates inverse hyperbolic sine (log with a 0 correction) versions  
#   of all of the potential response variables 
# Note: Do not use inverse hyperbolic since, instead have separate regressions
#   for prob. of 0 value and for log (conditional on positive value)
Gen_ihs <- function(data, vnames) {
  new_data <- data
  for(v in vnames) {
    new_data <- new_data %>% mutate(vv = eval(parse(text=v))) %>%
      mutate(!!paste0("ihs_", v) := log(vv + sqrt(vv^2 + 1))) %>%
      select(-vv)
  }
  return(new_data)
}

# Get_rates() creates three columns from the following two raw data files:
#   selic_file: location of the daily selic data file in CSV format
#   cpi_file: location of the CPI data file in CSV format
# Returns a data frame of the following form:
#   month in the form of yyyymm numeric
#   selic as a numeric (float) variable equal to the average SELIC rate in the month
#   inflation as a numeric (float) variable equal to the year-over-year change in CPI
Get_rates <- function(selic_file, cpi_file) {
  df_selic <- read.csv(selic_file) %>% filter(!is.na(selic_rate)) %>%
    mutate(year_month = as.numeric(substr(date, 1, 4))*100 + as.numeric(substr(date, 6, 7))) %>%
    group_by(year_month) %>% summarize(selic = mean(selic_rate))
  
  cpi_raw <- read.csv(cpi_file) %>% rename(cpi=BRACPIALLMINMEI) %>%
    mutate(prev_cpi = lag(cpi, n = 12, order_by = row_number())) %>%
    mutate(year_month = as.numeric(substr(DATE, 1, 4))*100 + as.numeric(substr(DATE, 6, 7))) %>%
    mutate(inf = (cpi/prev_cpi - 1) * 100 )
  
  return(full_join(df_selic, cpi_raw, by="year_month"))
}
