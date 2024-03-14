# Purpose: Define variables on the fund and factoring data, e.g. NAV, 
#   weighted average interest rate

library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)

# Define_combined_fof_foc() ensures no NA values for value variables and defines
# variables for total issuance and outstanding, summing across fof and foc
# Input data is merged between FOF and FOC where FOC variables have _fidc suffix
Define_combined_fof_foc <- function(data) {
  return(data %>% 
    # Replace all missing values with 0; our data are comprehensive
    # so any value that is made NA by the merge is actually 0
    # i.e. if an obs is not in one dataset, it's not in the dataset because the value is 0
    mutate_at(vars(matches("^val_")), ~replace(., is.na(.), 0)) %>%
    mutate(val_newop = val_newop_short + val_newop_short_fidc) %>%
    mutate(val_resp = val_resp_short + val_resp_short_fidc) %>%
    mutate(ir_newop = (ir_newop_short * val_newop_short + ir_newop_short_fidc * val_newop_short_fidc) / val_newop) %>%  
    mutate(ir_resp = (ir_resp_short * val_resp_short + ir_resp_short_fidc * val_resp_short_fidc) / val_resp) %>%
    mutate(mat_newop = (mat_newop_short * val_newop_short + mat_newop_short_fidc * val_newop_short_fidc) / val_newop) %>%  
    mutate(days_until_due = (days_until_due_short * val_resp_short + days_until_due_short_fidc * val_resp_short_fidc) / val_resp)   
  )
}

# Define the variables created after aggregation of boleto (trade credit) with summarize()
Define_tc_after_aggr <- function(data) {
  return(data %>% mutate(maturity_wmean = wsum / payment) %>% 
    mutate(maturity_wmean_tc = wsum / payment_tc) %>%
    mutate(maturity_wmean_g30 = wsum_g30 / payment_g30) %>%
    mutate(maturity_wmean_g60 = wsum_g60 / payment_g60) )
}

# Define the variables created before aggregation of boleto (trade credit) with summarize()
Define_tc_before_aggr <- function(data) {
  return(data %>% mutate(is_tc = (maturity_bin!="Upfront")) %>%
    mutate(is_g30 = !(maturity_bin %in% c("Upfront", "[1, 15]", "[16, 30]"))) %>%
    mutate(is_g60 = !(maturity_bin %in% c("Upfront", "[1, 15]", "[16, 30]", "[31, 60]"))) %>%
    mutate(payment_tc = payment * is_tc) %>%
    mutate(payment_g30 = payment * is_g30) %>%
    mutate(payment_g60 = payment * is_g60) %>%
    mutate(wsum = maturity_wmean * payment) %>%
    mutate(wsum_g30 = maturity_wmean * payment_g30) %>%
    mutate(wsum_g60 = maturity_wmean * payment_g60))
}

# Define the maturity bins for trade credit on the boleto transaction level data
Define_tc_mat_bin_wide <- function(data) {
  return(data %>% mutate(maturity_bin = case_when(
    maturity==0 ~ 'Upfront',
    maturity>=1 & maturity<=15 ~ '[1, 15]',
    maturity>=16 & maturity<=30 ~ '[16, 30]',
    maturity>=31 & maturity<=60 ~ '[31, 60]',
    maturity>=61 & maturity<=90 ~ '[61, 90]',
    maturity>=91 & maturity<=180 ~ '[91, 180]',
    maturity>=181 & maturity<=365 ~ '[181, 365]',
    maturity>365 ~ 'Over a year'
  )) %>% mutate(maturity_wmean = maturity))
}

# Get_fund_NAV_1m() returns a table with the net asset value = NAV = total assets of each fund,
#   the cash on hand for the given snapshot month, and the return from the previous month,
#   taking as given the cash on hand for the previous month, issuance in the given month, flows in the given month, and
#   repayment in the given month
# Cash on hand = 
#   (1) previous cash on hand * return on cash
#   - (2) amount that the fund spent on receivables (val_newop_seller)
#   + (3) amount that the fund received in payment (val_paid)
#   + (4) net inflow
# NAV = cash on hand + face value of assets outstanding and not yet defaulted
# Return: current NAV / (previous NAV + net inflow)
# The equations above implicitly assume that all flows occur at the end of the month; fine approximation when the return on cash is low and the ratio of flows to NAV is low
# Inputs: 
#   df_nav_prev is at the fund level, with NAV and cash balance for the month before snap_month
#   df_snap is at the fund-asset level, with the status of each asset at the end of snap_month
#   df_issue is at the fund-asset level, with the interest rate and value of each asset upon issuance in snap_month
#   df_flow is at the fund level, with the net inflow for snap_month
#   snap_month is the month in yyyymm numeric form, where we want the snapshot at the end of the month
#   overdue_disc is a number, the proportion by which overdue balances are discounted for calculating NAV (to reflect present value of expected collections)
#   return_cash is a number, equal to the monthly percentage return on cash, e.g. 1 to denote 1%
Get_fund_NAV_1m <- function(df_nav_prev, df_snap, df_issue, df_flow, snap_month, overdue_disc, return_cash) {
  df_nav_prev <- df_nav_prev %>% rename(cash_prev=cash, nav_prev=nav)
  
  # Keep track of what the fund has received and is still owed 
  fund_snap <- df_snap %>% filter(year_month == snap_month) %>%
    # Sum across assets, by fund
    group_by(fund_cnpj8) %>%
    # Only record liabilities that are not overdue
    summarize(val_revenue=sum(val_paid), val_outstanding=sum(val_resp * (days_until_due > 0)), val_overdue=sum(val_resp * (days_until_due == 0)))
  
  # Compute how much the fund paid as a factor to firms in new issuance
  fund_spent <- df_issue %>% filter(year_month == snap_month) %>%
    group_by(fund_cnpj8) %>%
    summarize(val_factored = sum(val_newop))
  
  # Add net inflows between the initial date and the last day of snap_month inclusive (or strictly earlier than the first day of the next month)
  fund_flow <- df_flow %>% filter(year_month == snap_month) %>%
    group_by(fund_cnpj8) %>%
    summarize(val_inflow_net = sum(vl_inflow_net))
  
  # Join (merge) the datasets and define NAV
  fund_merged <- df_nav_prev %>% 
    full_join(fund_spent, by=c("fund_cnpj8")) %>%
    full_join(fund_snap, by=c("fund_cnpj8")) %>%
    full_join(fund_flow, by=c("fund_cnpj8")) %>%
    # The data are comprehensive, so all NA values should be 0 (they represent fund-month where no operations occurred, i.e. payments are 0)
    mutate(across(.cols = everything(), ~coalesce(., 0))) %>%
    mutate(cash = round(cash_prev*(1+return_cash/100) - val_factored + val_revenue + val_inflow_net, 2)) %>%
    mutate(nav = round(cash + val_outstanding + val_overdue*overdue_disc, 2)) %>%
    mutate(return = round( (nav / (nav_prev + val_inflow_net) - 1)*100, 2)) %>%
    mutate(year_month = snap_month)
  return(fund_merged)
}

# Get_fund_NAV() is a wrapper function for Get_fund_NAV_1m() that computes NAV for all 
#   months after the initial date 
# Inputs:
#   df_fund_stat is at the fund level for the first day with data, with variable snap_amount = the initial cash on hand 
#   df_fac_snap is at the fund-asset-month level, with the status of each asset at the end of each month
#   df_fac_issue is at the fund-asset level, with the interest rate and value of each asset upon issuance
#   df_fund_flows_m is at the fund-month level, with the net inflow
#   return_cash_vec is a vector equal to the monthly percentage return on cash in each month, e.g. 1 to denote 1%
#   overdue_disc_vec is vector of the proportion by which overdue balances are discounted for calculating NAV (to reflect present value of expected collections)
#   initial_date is the first day with data (see df_fund_stat)
#   funds is a vector of the unique CNPJ corresponding to funds (subset of id_factor values for df_fac_snap and df_fac_issue)
Get_fund_NAV <- function(df_fund_stat, df_fac_snap, df_fac_issue, df_fund_flows_m, 
    return_cash_vec=rep(1, 60), overdue_disc_vec=rep(0.5, 60),
    initial_date=as.Date("2019-01-01"), funds=unique(df_fund_flows_m$fund_cnpj8)) {
  months <- sort(unique(df_fac_snap$year_month))
  stopifnot(length(months) == length(return_cash_vec) & length(months) == length(overdue_disc_vec))
  df_cash <- df_fund_stat %>% filter(snap_date == initial_date) %>% group_by(fund_cnpj8) %>%
    summarize(cash = sum(snap_amount)) %>% mutate(nav = cash)
  df_nav_all <- NULL
  for(m in 1:length(months)) {
    df_nav <- Get_fund_NAV_1m(df_nav_prev = df_cash, 
      df_snap = df_fac_snap %>% filter(id_factor %in% funds) %>% rename(fund_cnpj8=id_factor) %>% filter(year_month == months[m]),
      df_issue = df_fac_issue %>% filter(id_factor %in% funds) %>% rename(fund_cnpj8=id_factor) %>% filter(year_month == months[m]),
      df_flow = df_fund_flows_m %>% filter(year_month == months[m]),
      snap_month = months[m],
      return_cash = return_cash_vec[m],
      overdue_disc = overdue_disc_vec[m]
    )
    df_cash <- df_nav %>% select(fund_cnpj8, cash, nav)
    df_nav_all <- bind_rows(df_nav_all, df_nav)
  }
  return(df_nav_all %>% arrange(fund_cnpj8, year_month) %>% 
    relocate(fund_cnpj8, year_month, nav, cash, return) %>%
    mutate(return=ifelse(year_month==months[1], NA, return))
  )
}

# Get_fund_portfolio() reports the amount that each fund is owed in each month from each firm, 
#   with an additional row for each fund reporting the cash on hand = difference between the NAV
#   and the total amount that the fund is owed by firms
# Input:
#   df_nav is at the fund-month level, giving the net asset value
#   df_fac_snap is at the fund-asset-month level, with the status of each asset at the end of each month
#   funds is a vector of the unique CNPJ corresponding to funds (subset of id_factor values for df_fac_snap and df_fac_issue)
Get_fund_portfolio <- function(df_nav, df_fac_snap, funds=unique(df_fund_flows_m$fund_cnpj8)) {
  df_asset <- df_fac_snap %>% 
    filter(id_factor %in% funds) %>% rename(fund_cnpj8=id_factor) %>%
    mutate(asset = paste0(seller_cnpj8, "-", buyer_cnpj8)) %>%
    group_by(fund_cnpj8, year_month, asset) %>%
    summarize(val_asset = sum(val_resp), ir = sum(ir * val_resp) / sum(val_resp),
      maturity = sum(maturity * val_resp) / sum(val_resp))
  return(df_nav %>% mutate(asset = "cash") %>%
    rename(val_asset = cash) %>%
    bind_rows(df_asset) %>%
    select(fund_cnpj8, asset, year_month, val_asset, ir) %>%
    arrange(fund_cnpj8, asset, year_month) 
  )
}

# Get_wmean_ir() computes the weighted average interest rate
#   for a given ID variable (e.g. the average price at which a factor
#   purchases receivables) for the portfolio at a given point in time, e.g. from Get_fund_portfolio() 
# Inputs:
#   df_portfolio is at the fund-month level, see Get_fund_portfolio()
#   snap_month is a date object, for the time of the snapshot
#   id_which is a string equal to the name of an ID variable, e.g. "seller_cnpj8"
#   is_amount_weight is a boolean, TRUE if weight by amount borrowed
#   is_maturity_weight is a boolean, TRUE if weight by maturity
Get_wmean_ir <- function(
    df_portfolio, snap_month, id_which="id_factor", is_amount_weight=TRUE, is_maturity_weight=FALSE
) {
  output <- df_portfolio %>% filter(year_month == snap_month) %>% mutate(weight = 1)
  if(is_amount_weight) {
    output <- output %>% mutate(weight = val_asset * weight) 
  }
  if(is_maturity_weight) {
    output <- output %>% mutate(weight = maturity * weight) 
  }
  return(output %>% 
    group_by(across(all_of(id_which)), year_month) %>%
    summarize(ir_wmean = weighted.mean(ir, w=weight), 
    num_contracts = n())
  )
}
