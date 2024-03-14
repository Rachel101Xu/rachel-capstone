# Purpose: Helper functions for panel operations, e.g. filling in missing values,
#   define lags, that are applicable to any project. 
#   Note that aggregation code is in fac_aggr_fnc.R
# All helper functions ought to be in alphabetical order

library(data.table)
library(dplyr)
library(tidyr)

# Define_lags() defines the cumulative sum, past 3 months, and past month for 
#   each variable in vars_main, grouping by the variables in ids, across time
# Inputs:
#   ids is the vector of names of the identifier variables that we do not want to fill in
#   time is the name of the time variable
#   vars_to_lag is the vector of names of variables whose values we want to define lags
# Note: ensure that the combination of ids and time uniquely identifies observations in the data
Define_lags <- function(data, ids, time, vars_to_lag) {
  return(data %>% arrange(across(all_of(c(ids, time)))) %>% group_by(across(all_of(ids))) %>%
    # cumsum includes the current time period, do not want that
    mutate(across(all_of(vars_to_lag), ~ cumsum(ifelse(is.na(.), 0, .)) - ., .names="{col}_cs")) %>%
    mutate(across(all_of(vars_to_lag), ~ frollsum(., 4, align="right", na.rm=T) - ., .names="{col}_s3m")) %>%
    mutate(across(all_of(vars_to_lag), ~ frollsum(., 2, align="right", na.rm=T) - ., .names="{col}_s1m"))
  )
}

# Fill_in_panel() fills in all non-present values of a variable for a given
#   group of IDs
# Inputs:
#   ids is the vector of names of the identifier variables that we do not want to fill in
#   var_fill is the name of the variable that we want to fill in, usually year_month
#   vars_val is the vector of names of variables whose values we want to fill in with 0 (standard is NA)
# Note: ensure that the combination of ids and var_fill uniquely identifies observations in the data
Fill_in_panel <- function(data, ids, var_fill, vars_val) {
  # First create a correspondence between the IDs and a numeric panel ID
  panel_xwalk <- data %>% group_by(across(all_of(ids))) %>% summarize(id=cur_group_id())
  uniq_id <- unique(panel_xwalk$id)
  # Fill in the panel
  uniq_fill <- unique(data[[var_fill]])
  return(CJ(id=uniq_id, vfill=uniq_fill) %>% rename(!!var_fill := vfill) %>%
    left_join(panel_xwalk, by="id") %>%
    left_join(data, by=c(ids, var_fill)) %>%
    mutate(across(all_of(vars_val), .fns = ~replace_na(., 0))) %>% select(-id)
  )
}