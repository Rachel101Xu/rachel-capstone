# Purpose: Define functions to aggregate variables that are used across multiple files
#   in the factoring project
# If a function is only used in a single file, it is contained in that file
# All helper functions ought to be in alphabetical order

library(data.table)
library(dplyr)
library(tidyr)

# Helper function to aggregate numeric variables
Aggr_numvars <- function(data, group_vars, include_long=FALSE, include_coob=FALSE) {
  stubs <- c("val_", "ir_", "mat_")
  # For computing weighted average interest rate, exclude obs with ir = 0
  # Similarly, loans should not have 0 maturity
  ret <- data %>% 
    mutate(val_ns = ifelse(ir_newop_short==0, 0, val_newop_short),
      val_rs = ifelse(ir_resp_short==0, 0, val_resp_short)) %>%
    mutate(ir_ns = ir_newop_short * val_ns,
      ir_rs = ir_resp_short * val_rs,
      mat_ns = mat_newop_short * val_ns,
      mat_us = days_until_due_short * val_rs,
      mat_os = days_overdue_short * val_rs) 
  if(include_long) {
    ret <- ret %>% 
      mutate(val_nl = ifelse(ir_newop_long==0, 0, val_newop_long),
             val_rl = ifelse(ir_resp_long==0, 0, val_resp_long)) %>%
      mutate(ir_nl = ir_newop_long * val_nl,
      ir_rl = ir_resp_long * val_rl,
      mat_nl = mat_newop_long * val_nl,
      mat_ul = days_until_due_long * val_rl,
      mat_ol = days_overdue_long * val_rl) 
  } else {
    ret <- ret %>% select(-any_of(ends_with("_long")))
  }
  if(include_coob) {
    ret <- ret %>% mutate(coob_ns = pct_coob_newop_short * val_newop_short,
      coob_rs = pct_coob_resp_short * val_resp_short)
    stubs <- c(stubs, "coob_")
    if(include_long) {
      ret <- ret %>% mutate(coob_nl = pct_coob_newop_long * val_newop_long,
        coob_rl = pct_coob_resp_long * val_resp_long) 
    }
  }
  ret <- ret %>% group_by_at(vars(all_of(group_vars))) %>%
    summarize(across(starts_with(stubs), ~sum(., na.rm=T))) %>%
    mutate(ir_newop_short = ifelse(val_ns==0, 0, ir_ns/val_ns)) %>%
    mutate(ir_resp_short = ifelse(val_rs==0, 0, ir_rs/val_rs)) %>%
    mutate(mat_newop_short = ifelse(val_ns==0, 0, mat_ns/val_ns)) %>%
    mutate(days_until_due_short = ifelse(val_rs==0, 0, mat_us/val_rs)) %>%
    mutate(days_overdue_short = ifelse(val_rs==0, 0, mat_os/val_rs)) 
  if(include_coob) {
    ret <- ret %>% mutate(pct_coob_newop_short = ifelse(val_newop_short==0, 0, coob_ns/val_newop_short)) %>%
      mutate(pct_coob_resp_short = ifelse(val_resp_short==0, 0, coob_rs/val_resp_short)) 
  }
  if(include_long) {
    ret <- ret %>% mutate(ir_newop_long = ifelse(val_nl==0, 0, ir_nl/val_nl)) %>%
      mutate(ir_resp_long = ifelse(val_rl==0, 0, ir_rl/val_rl)) %>%
      mutate(mat_newop_long = ifelse(val_newop_long==0, 0, mat_nl/val_nl)) %>%
      mutate(days_until_due_long = ifelse(val_rl==0, 0, mat_ul/val_rl)) %>%
      mutate(days_overdue_long = ifelse(val_rl==0, 0, mat_ol/val_rl))
    if(include_coob) {
      ret <- ret %>%
        mutate(pct_coob_newop_long = ifelse(val_newop_long==0, 0, coob_nl/val_newop_long)) %>%
        mutate(pct_coob_resp_long = ifelse(val_resp_long==0, 0, coob_rl/val_resp_long)) 
    }
  }
  return(ret)
}

