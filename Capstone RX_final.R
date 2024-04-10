# Capstone Project Rachel Xu (rx100)
# Note: first part of the file is creating mock data for each of the relevant datasets for analysis (end at line 370), second part is the analysis for Capstone project

# install.packages("ReIns")
# install.packages("gghighlight")
# install.packages("lme4")
# install.packages("Matrix")
# install.packages("lmtest")
# install.packages("gridExtra")

library(data.table)
library(dplyr)
library(lubridate)
library(ReIns)
library(tidyr)
library(ggplot2)
library(sandwich)
library(lmtest)
library(gridExtra)

########## Creating Mock Data for each of the relevant datasets for analysis
# created by hhz@mit.edu
# Note: See the helper functions in data_helper_fnc.R


set.seed(10)

source("Helper/Data Prep/fac_def_var_fnc.R")
source("Helper/Data Prep/generic_comp_fnc.R")
source("Helper/Data Prep/scr_aggr_fnc.R")
source("Helper/Figure/plot_loan_portfolio.R")

dir_bcb_ss <- ""

# Adjust these as desired
n_funds <- 10
n_banks <- 2
n_buyer <- 20
n_seller <- 10
n_obs_inv <- 10^4
n_obs_flow <- 10^3

# Assume that the price of receivables at the buyer-seller-time (bst) level is defined as
#   interest rate = SELIC + default risk + spread
#   spread = fac_int * e^(fac_coef_micro*(fund demand for debtor's receivables) + fac_coef_macro*(fund demand for all receivables))
# Intuition is that when demand is high, the spread goes to 0 (competition between investors)
# fac_int is the maximum possible spread, when there is no fund demand
# Micro refers to just the asset (buyer for non-recourse, seller for recourse)
# Macro refers to all factoring
fac_int <- 100
fac_coef_micro <- -1e-5
fac_coef_macro <- -1e-7

p_fac_beta <- 0.005 # increase in probability of factoring for each additional day of invoice maturity
p_early_repay <- 0.1

yr_start <- 2017
yr_end <- 2023
month_start <- 201701
month_end <- 202312
num_months <- 84

# Do not adjust these
buyer_vec <- 1:n_buyer
seller_vec <- 1:n_seller
fund_vec <- (10^3+1):(10^3+n_funds)
bank_vec <- (10^4+1):(10^4+n_banks)
days <- seq(ym(month_start), ym(month_end), by="day")
months <- sort(unique(format(days, "%Y-%m")))
months_num <- as.numeric(gsub("-", "", months))

# No need to adjust, can change if file structure changes
path_selic <- paste0(dir_bcb_ss, "daily_selic.csv")
path_inf <- paste0(dir_bcb_ss, "BRACPIALLMINMEI.csv")
df_rates <- Get_rates(path_selic, path_inf) # from generic_comp_fnc.R

# Assume firm ID "cnpj8" ids sorted such that default risk is increasing
Define_default <- function(data) {
  return(data %>% mutate(default_prob_nr = buyer_cnpj8/(n_buyer*5)) %>% 
    mutate(default_prob_r = seller_cnpj8/(n_seller*5))
  )
}
Aggr_hash_to_month <- function(data) {
  return(data %>% mutate(val_ir_newop_numer = ir * val_newop, val_mat_newop_numer = maturity * val_newop) %>%
    mutate(val_ir_resp_numer = ir * val_resp, val_dud_resp_numer = days_until_due * val_resp) %>%
    summarize(across(starts_with("val"), sum)) %>%
    mutate(ir_newop = val_ir_newop_numer/val_newop, maturity_newop = val_mat_newop_numer/val_newop) %>%
    mutate(ir_resp = val_ir_resp_numer/val_resp, days_until_due_resp = val_dud_resp_numer/val_resp)
  )
}


# 1.1) df_fund_flows lists all of the flows to & from the receivables funds,
#     indexed by the fund (i.e. the factor) and the shareholder (i.e. institutional investor)
# Variable names:
#   id_flow is the unique identifier within this dataset
#   fund_cnpj8 is the fund ID, matches to id_factor in df_firms_factoring
#   id_shareholder is the shareholder ID
#   flow date refers to when the flow occurred
#   flow_amount is how much $ the counterparty added to the fund, can be negative
df_fund_flows <- data.frame(
  id_flow = 1:n_obs_flow,
  fund_cnpj8 = sample((10^3+1):(10^3+n_funds), n_obs_flow, replace=TRUE),
  id_shareholder = sample((10^5+1):(10^5+10), n_obs_flow, replace=TRUE),
  flow_year = sample(yr_start:yr_end, n_obs_flow, replace=TRUE),
  flow_month = sample(1:12, n_obs_flow, replace=TRUE),
  flow_day = sample(1:28, n_obs_flow, replace=TRUE),
  flow_amount = rnorm(n_obs_flow, 10^4, 10^4)
) %>% mutate(
  group_shareholder = id_shareholder >= 10^5+5,
  flow_date = as.Date(paste(flow_year, flow_month, flow_day, sep="-"))
)

head(df_fund_flows)
dim(df_fund_flows)



# 1.2) df_fund_flows_m is the aggregate (across shareholders) net inflow by fund by month
df_fund_flows_m <- df_fund_flows %>% mutate(year_month = flow_year*100 + flow_month) %>%
  group_by(fund_cnpj8, year_month) %>% summarize(vl_inflow_net = sum(flow_amount))
fund_vec <- sort(unique(df_fund_flows_m$fund_cnpj8))

head(df_fund_flows_m)
dim(df_fund_flows_m)

# df_fund_flows_panel fills in the panel for the definition of lagged flows
df_fund_flows_panel <- expand.grid(year_month=unique(df_fund_flows_m$year_month),
  fund_cnpj8=fund_vec) %>% left_join(df_fund_flows_m) %>%
  mutate(vl_inflow_net = replace_na(vl_inflow_net,0)) %>%
  arrange(fund_cnpj8, year_month)
head(df_fund_flows_panel9)

# 2) df_fund_stat gives the amount that each counterparty has invested in
#   each fund at the initial date
# each fund's sum across counterparties is the fund's initial net worth = net asset value (NAV)
df_fund_stat <- data.frame(
  fund_cnpj8 = rep(fund_vec, each=10),
  id_counterparty = rep((10^5+1):(10^5+10), times=5),
  snap_date = as.Date(ym(month_start)),
  snap_amount = runif(length(fund_vec)*10, 0, 10^4)
) 
head(df_fund_stat)

# For the purpose of calculating a fund's purchase, assume that each fund 
#   purchases receivables worth its expected cash balance minus half the previous cash balance
#   (representing outstanding receivables); ignore any monthly returns
df_fund_cash_initial <- df_fund_stat %>% group_by(fund_cnpj8) %>% 
  summarize(cash_init = sum(snap_amount)) %>% mutate(year_month = month_start)
df_fund_target_pur <- full_join(df_fund_cash_initial, df_fund_flows_panel, by=c("fund_cnpj8", "year_month")) %>%
  arrange(fund_cnpj8, year_month) %>% group_by(fund_cnpj8) %>% 
  mutate(cash_init = max(cash_init, na.rm=T), total_flow = cumsum(vl_inflow_net)) %>%
  mutate(cash = cash_init + total_flow) %>%
  arrange(fund_cnpj8, year_month) %>% group_by(fund_cnpj8) %>% 
  mutate(target_pur = ifelse(year_month==month_start, cash/2, pmax(cash - lag(cash)/2, 0))) %>% 
  select(fund_cnpj8, year_month, target_pur) %>%
  arrange(fund_cnpj8, year_month)

# Define each fund's exposure share to each debtor for each type of financing
num_fund_firm_pairs <- length(fund_vec) * length(c(buyer_vec, seller_vec))
df_fund_firm_exposure <- expand.grid(fund_vec, c(buyer_vec, paste0("sel_", seller_vec))) %>%
  rename(fund_cnpj8=Var1, debtor_cnpj8=Var2) %>%
  mutate(is_recourse = ifelse(grepl("sel_", debtor_cnpj8), 1, 0)) %>%
  mutate(debtor_cnpj8 = gsub("sel_", "", debtor_cnpj8)) %>%
  # The shares sum to less than 1 across funds, and are unrestricted across firms
  # In practice, funds probably want exposure to firms corresponding to the
  # type of flow (e.g. to senior vs subordinated class)
  mutate(share_of_fund = runif(num_fund_firm_pairs, 0, 0.05)) %>% 
  group_by(fund_cnpj8) %>% mutate(total_share = sum(share_of_fund)) %>%
  mutate(share_of_fund = ifelse(share_of_fund > 1, share_of_fund/total_share, share_of_fund)) %>%
  select(-total_share) %>%
  mutate(across(ends_with("cnpj8"), as.numeric)) %>%
  mutate(uniq_id_pair = fund_cnpj8*10^6 + is_recourse*10^5 + debtor_cnpj8)
uniq_firm_fund_pairs <- unique(df_fund_firm_exposure$uniq_id_pair)

# Merge to fund purchases to compute fund-driven purchases by CNPJ8
df_fund_firm <- CJ(uniq_firm_fund_pairs, months_num) %>%
  rename(uniq_id_pair=uniq_firm_fund_pairs, year_month=months_num) %>%
  full_join(df_fund_firm_exposure, by=c("uniq_id_pair")) %>%
  full_join(df_fund_target_pur, by=c("fund_cnpj8", "year_month")) %>%
  mutate(expected_pur = share_of_fund * target_pur) 
df_firm_exposure <- df_fund_firm %>%
  group_by(debtor_cnpj8, is_recourse, year_month) %>%
  summarize(expected_pur = sum(expected_pur)) %>%
  group_by(year_month) %>% mutate(aggr_pur = sum(expected_pur)) %>% ungroup() %>%
  mutate(buyer_cnpj8 = ifelse(is_recourse==0, debtor_cnpj8, NA)) %>%
  mutate(seller_cnpj8 = ifelse(is_recourse==1, debtor_cnpj8, NA)) %>%
  Define_default() %>% 
  mutate(default_prob = ifelse(is_recourse==0, default_prob_nr, default_prob_r)) %>%
  left_join(df_rates, by="year_month") %>%
  # And the interest rates by debtor (assume that factors charge the same price)
  mutate(ir = selic + default_prob + fac_int*exp(fac_coef_micro*expected_pur + fac_coef_macro*aggr_pur)) %>%
  select(year_month, debtor_cnpj8, seller_cnpj8, buyer_cnpj8, is_recourse, ir, expected_pur, aggr_pur)
df_firm_ir_r <- df_firm_exposure %>% filter(is_recourse==1) %>%
  select(year_month, seller_cnpj8, ir) %>% rename(ir_r = ir)
df_firm_ir_nr <- df_firm_exposure %>% filter(is_recourse==0) %>%
  select(year_month, buyer_cnpj8, ir) %>% rename(ir_nr = ir)

head(df_fund_firm)
head(df_firm_exposure)

# 3) df_invoice is the individual issuance of invoices
# Variable names:
#   hash_invoice is the unique identifier within this dataset
#   seller_cnpj8 is the identifier for the firm who sells the good and the receivable
#   buyer_cnpj8 is the identifier for the firm who buys the good and owes the money
#   maturity is the number of days between contract date and payment due date
#   payment is the amount contracted between the two firms
df_invoice <- data.frame(
  hash_invoice = 1:n_obs_inv,
  seller_cnpj8 = sample(seller_vec, n_obs_inv, replace=TRUE),
  buyer_cnpj8 = sample(buyer_vec, n_obs_inv, replace=TRUE),  
  maturity = round(rtexp(n_obs_inv, rate=0.1, endpoint=300) + runif(n_obs_inv, 0, 60)),
  date_issue = sample(days, n_obs_inv, replace=TRUE)
) %>% mutate(payment = round(rexp(n_obs_inv, rate=10^-5))) %>%
  mutate(year_month = year(date_issue)*100 + month(date_issue)) %>%
  mutate(date_due = date_issue + maturity)

# 3.1) df_sbm is the monthly summary of invoice issuance by seller-buyer pair
df_sbm <- df_invoice %>% Define_tc_mat_bin_wide() %>% Define_tc_before_aggr() %>%
  group_by(seller_cnpj8, buyer_cnpj8, year_month) %>%
  summarize(across(starts_with(c("payment", "wsum")), ~sum(., na.rm=T))) %>%
  Define_tc_after_aggr()

# 3.2) df_fac_issue is the factoring of individual invoices (that we cannot observe)
#     indexed by the seller (who wants financing and sells the receivable), 
#     the buyer (whose risk determines the price), 
#     and the factor (who buys the receivable; assume this is a receivables fund)
# Assume that the probability of factoring is linear in maturity with intercept p_fac_alpha and slope p_fac_beta, does not depend on recourse
# Assume that if an invoice is factored, the entirety is sold to the factor
# Assume that each invoice is factored on the same day it is contracted
# Variable names:
#   hash_oper is the unique identifier within this dataset
#   seller_cnpj8 is the identifier for the firm who sells the good and the receivable
#   buyer_cnpj8 is the identifier for the firm who buys the good and owes the money
#   id_factor is the identifier for the entity who buys the receivable
#   year_month refers to the month when the contract was originated
#   maturity is the number of days between contract date and payment due date
#   ir_newop is the implied annual interest rate in %, defined later
#   val_newop is how much $ of receivable sold by the seller to the factor
#   val_newop_seller is how much $ the seller received for the receivable, defined later
df_fac_issue <- df_invoice %>%
  # Assume that larger values of firm ID correspond to higher default risk, 
  #   so non-recourse factoring is mostly invoices with high seller ID and low buyer ID,
  #   recourse factoring is mostly invoices with low seller ID
  mutate(nr_rand = rnorm(n_obs_inv, n_buyer/2, 5)) %>%
  mutate(r_rand = rnorm(n_obs_inv, 0, 5)) %>%
  mutate(nr_score = (seller_cnpj8 - buyer_cnpj8) + nr_rand) %>%
  mutate(r_score = n_seller - seller_cnpj8 + r_rand) %>%
  mutate(fac_score = pmax(nr_score, r_score) / (n_buyer + n_seller) / 2) %>%
  mutate(prob_fac = pmax(pmin(p_fac_beta * maturity + fac_score, 1), 0)) %>%
  mutate(is_factored = rbinom(n_obs_inv, 1, prob_fac)) %>%
  mutate(is_recourse = ifelse(nr_score>r_score, 0, 1)) %>%
  filter(is_factored == 1) %>% 
  rename(val_newop = payment) 
n_obs_fac <- dim(df_fac_issue)[1]
df_fac_issue <- df_fac_issue %>%
  # Assume that 95% of issuance is by banks and 5% is by funds
  mutate(is_fund = rbinom(n_obs_fac, 1, 0.05)) %>%
  mutate(id_bank = sample(bank_vec, n_obs_fac, replace=TRUE)) %>%
  mutate(id_fund = sample(fund_vec, n_obs_fac, replace=TRUE)) %>%
  mutate(id_factor = ifelse(is_fund==1, id_bank, id_fund)) %>%
  # Merge in interest rates 
  left_join(df_firm_ir_r, by=c("year_month", "seller_cnpj8")) %>%
  left_join(df_firm_ir_nr, by=c("year_month", "buyer_cnpj8")) %>%
  mutate(ir = ifelse(is_recourse==1, ir_r, ir_nr)) %>%
  select(hash_invoice, id_factor, is_fund, seller_cnpj8, buyer_cnpj8, is_recourse, year_month, date_issue, val_newop, ir, maturity, date_due)

# 3.3) df_fac_fund_pur is the purchase of invoices from banks by funds
#   in response to flows; TODO fill this in later

# 4.1) df_fac_snap is a snapshot of the assets outstanding (the balance sheet of the fund)
#   with an observation for each month until the asset is paid off
df_fac_snap <- df_fac_issue %>% Define_default() %>%
  # Define the actual number of days that it takes a firm to pay off a loan; 
  #   assume that default probability is increasing in firm ID, but
  #   days defaulted is uncorrelated with firm ID
  mutate(is_default_nr = rbinom(n_obs_fac, 1, default_prob_nr)) %>%
  mutate(is_default_r = rbinom(n_obs_fac, 1, default_prob_r)) %>%
  mutate(days_late = runif(n_obs_fac, 0, 180)) %>%
  mutate(days_late = ifelse( (is_recourse==1 & is_default_r==1) | (is_recourse==0 & is_default_nr==1), days_late, 0 )) %>%
  rowwise() %>%
  mutate(month_starts = list(seq(
    floor_date(date_issue, unit="month"), 
    floor_date(date_issue + maturity + days_late, unit="month"), 
    by="month"))) %>%
  unnest(month_starts) %>%
  # Now define year_month for each end-of-month snapshot
  mutate(year_month = as.numeric(paste0(substr(month_starts, 1, 4), substr(month_starts, 6, 7)))) %>%
  mutate(date_snap = ceiling_date(month_starts, "month") - days(1)) %>%
  # Define the days until due (as of end-of-month snapshot)
  mutate(days_until_due = pmax(0, date_due - date_snap)) %>%
  mutate(days_overdue = pmax(0, date_snap - date_due)) %>%
  arrange(hash_invoice, year_month) %>% group_by(hash_invoice) %>% 
  mutate(first_obs = (row_number() == 1)) %>%
  mutate(last_obs = (row_number() == n()))
nrow <- nrow(df_fac_snap)
df_fac_snap <- df_fac_snap %>% ungroup() %>%
  # Include interest; this is just an approximation, see SQL query for exact interest due
  mutate(val_due = val_newop * (1 + ir*(maturity/365)/100)) %>%
  # Decrement some of the values to represent early repayment, of a random share between 0 and 1
  mutate(repay_share = rbinom(nrow, 1, p_early_repay) * runif(nrow, 0, 1)) %>%
  mutate(val_resp = val_due * (1-repay_share)) %>%
  group_by(hash_invoice) %>% 
  # Ensure that val_resp is decreasing over time
  mutate(val_resp = round(cummin(val_resp),2)) %>%
  ungroup() %>%
  # Let everything be paid off in the final month
  mutate(val_resp = ifelse(last_obs == 1, 0, val_resp)) %>%
  # Define how much has been paid so far
  arrange(hash_invoice, year_month) %>% group_by(hash_invoice) %>%
  mutate(lag_resp = lag(val_resp)) %>% ungroup() %>%
  mutate(val_paid = ifelse(first_obs==1, val_due, lag_resp) - val_resp) %>%
  # When val_paid is close to 0, it should be 0, computational error
  mutate(val_paid = ifelse(abs(val_paid) < 0.1, 0, val_paid)) %>%
  # Let val_newop only be positive in the month of the new operation
  mutate(val_newop = ifelse(first_obs==1, val_newop, 0)) %>%
  # Only show the observations that are not in the future
  filter(year_month <= month_end) 

# Aggregate to the debtor by factor by month level
df_fac_nr <- df_fac_snap %>% filter(is_recourse == 0) %>%
  group_by(id_factor, is_fund, seller_cnpj8, buyer_cnpj8, year_month) %>%
  Aggr_hash_to_month()
df_fac_r <- df_fac_snap %>% filter(is_recourse == 1) %>%
  group_by(id_factor, is_fund, seller_cnpj8, year_month) %>%
  Aggr_hash_to_month()

# 4.2) Define the datasets corresponding to what's on the server
fof_fsbm_nr <- df_fac_nr %>% filter(is_fund==1) %>% rename(fund_cnpj8=id_factor) %>% select(-is_fund)
fof_fsm_rec <- df_fac_r %>% filter(is_fund==1) %>% rename(fund_cnpj8=id_factor) %>% select(-is_fund)
foc_fsbm_nr <- df_fac_nr %>% filter(is_fund==0) %>% rename(lender_cgf=id_factor) %>% select(-is_fund)
foc_fsm_rec <- df_fac_r %>% filter(is_fund==0) %>% rename(lender_cgf=id_factor) %>% select(-is_fund)

# 5) Mock SCR data for other types of working capital financing, for use with plot_loan_portfolio.R
# Mock dataset, uniquely indexed by security ID and current date:
#   id_firm = numeric, unique identifier of firms
#   id_security = numeric, unique identifier of securities
#   issue_month = numeric, integer denoting when a security was issued/originated
#   issue_day = numeric, integer "" ""
#   current_month = numeric, integer denoting the time of a "snapshot" at the firm's portfolio
#   current_day = numeric, "" ""
#   loan_rate = numeric, percentage (without % sign) interest rate
#   loan_borrowed = numeric, amount borrowed in thousands of BRL
#   loan_owe = numeric, amount owed on the loan in thousands of BRL on the current date
#   loan_overdue = numeric, amount overdue on the loan in thousands of BRL on the current date
#   until_due_days = numeric, remaining time in days until loan is due (can be negative)
#   loan_type = character string, type of loan
Mock_SCR <- function(n_obs=1000) {
  return(data.frame(
    id_firm = sample(1:5, n_obs, replace=TRUE),
    id_security = 1:n_obs,
    issue_month = c(floor(runif(n_obs/2, 1, 7)), floor(runif(n_obs/2, 5, 7))),
    issue_day = floor(runif(n_obs, 1, 29)),
    current_month = rep(9, times=n_obs),
    current_day = rep(1, times=n_obs),
    loan_rate = runif(n_obs, 0, 100),
    loan_borrowed = runif(n_obs, 0, 500),
    maturity = round(runif(n_obs, 30, 365), digits=0),
    loan_type = sample(c("Factoring", "Line of Credit", "Term Loan"), n_obs, replace=TRUE)
  ) %>% 
  mutate(loan_owe = runif(n_obs, 0, 1) * loan_borrowed) %>%
  mutate(issue_date = as.Date(paste("2023", issue_month, issue_day, sep="-"))) %>%
  mutate(current_date = as.Date(paste("2023", current_month, current_day, sep="-"))) %>%
  mutate(until_due_days = issue_date - current_date + maturity) %>%
  mutate(loan_overdue = ifelse(until_due_days < 0, loan_owe, 0)) %>%
  mutate(loan_overdue_30 = ifelse(until_due_days < -30, loan_owe, 0)))
}
mock_scr <- Mock_SCR()

######################### mock data end #######################



################### Start of Capstone project #################
######### understand variables

# important variables from different data frames
# don't need to run this part
# get selic from df_rates, which is from 197912 to 202402, has year_month
head(df_rates)
dim(df_rates)
summary(df_rates)

# get flows (vl_inflow_net) from df_fund_flows_panel, has year_month and fund_cnpj8 (1001-1010)
head(df_fund_flows_panel)
dim(df_fund_flows_panel)
summary(df_fund_flows_panel)
table(df_fund_flows_panel$year_month)

# get purchase (expected_pur) from df_fund_firm
head(df_fund_firm)
summary(df_fund_firm)
dim(df_fund_firm)
table(df_fund_firm$fund_cnpj8)

################ data frame prep ##############

# Prepare fund NAV df (variables see fac_def_var_fnc.R)
df_nav <- Get_fund_NAV(df_fund_stat, df_fac_snap, df_fac_issue, df_fund_flows_panel, 
  return_cash_vec=rep(1, num_months), overdue_disc_vec=rep(0.5, num_months),
  initial_date=as.Date(ym(month_start)), funds=fund_vec)

# Group by year_month and fund_cnpj8, calculate aggregate_receivables_purchase
df_receivables_purchase <- df_fund_firm %>%
  group_by(year_month, fund_cnpj8) %>%
  summarize(
    share_of_fund = first(share_of_fund),
    aggregate_receivables_purchase = sum(expected_pur, na.rm = TRUE)
  ) %>%
  ungroup()

# View the new dataset
print(df_receivables_purchase)


# merge data sets: df_rates, df_fund_flows_panel, df_fund_firm, and df_nav
ndata1 <- merge(df_rates, df_fund_flows_panel, by = "year_month")
ndata2 <- merge(ndata1, df_receivables_purchase, by = c("year_month", "fund_cnpj8"), all = TRUE)
ndata3 <- merge(ndata2, df_nav, by = c("year_month", "fund_cnpj8"), all = TRUE)

# modify the date in ndata3 such that it is date object
ndata3 <- ndata3 %>%
  select(-val_inflow_net) %>%
  mutate(DATE = as.Date(DATE))

print(class(ndata3$DATE)) # all the dates are the first day of eacch month of 2017-01 to 2023-08

dim(ndata3)
head(ndata3)
summary(ndata3$year_month)

########## Regressions #########
# Set the printing options to disable scientific notation
options(scipen = 999)


# regressions incorporated in the poster
reg1 <- lm(aggregate_receivables_purchase ~ vl_inflow_net + factor(fund_cnpj8) + factor(year_month), data = ndata3)
summary(reg1)
BIC(reg1)

# account for economic condition
reg2 <- lm(aggregate_receivables_purchase ~ vl_inflow_net + selic + cpi + factor(fund_cnpj8) + factor(year_month), data = ndata3)
summary(reg2)
BIC(reg2)

# account for firm performance and size in addition to economic condition
reg3 <- lm(aggregate_receivables_purchase ~ vl_inflow_net + cpi + nav + factor(fund_cnpj8) + factor(year_month), data = ndata3)
summary(reg3)
BIC(reg3)
anova(reg2,reg3)



# mixed effect model to account for firms appearing multiple times

# Fit the mixed-effects model, doesn't work rn, need to fix!!!!!!
library(lme4)
install.packages("Matrix")

reg_mixed_model <- lmer(aggregate_receivables_purchase ~ vl_inflow_net + selic + nav + return + (1 | fund_cnpj8), data = ndata3)

# Print the summary of the model
summary(model)


# mixed model substitution
# Run regression with robust standard errors
reg_mixed <- lm(aggregate_receivables_purchase ~ vl_inflow_net + selic + nav + return, data = ndata3)
robust_se <- sqrt(diag(vcovHC(reg_mixed)))

# Print coefficients with robust standard errors
coeftest(reg_mixed, vcov. = vcovHC)

# View other model diagnostics
summary(model)

########## Graphs ##########
# Create histogram for aggregate_receivables_purchase and vl_inflow_net
explore_purchase <- ggplot(ndata3, aes(x = aggregate_receivables_purchase)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(x = "Aggregate Receivables Purchase", y = "Frequency") +
  theme_minimal()

explore_inflow <- ggplot(ndata3, aes(x = vl_inflow_net)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(x = "Aggregate Inflows", y = "Frequency") +
  theme_minimal()
table(ndata3$vl_inflow_net)

grid.arrange(explore_purchase, explore_inflow, nrow = 1)

# Count the number of negative values in vl_inflow_net
negative_count <- sum(ndata3$vl_inflow_net < 0)
print(negative_count)

# residual v. fitted plot
library(scales)
plot(reg3$fitted.values,reg3$resid, 
     pch=18, col=alpha("skyblue",0.50),
     xlab="Fitted Values",
     ylab="Residuals",
     main="Residuals vs Fitted Values")
abline(0,0)


###### graph for four indicators
library(ggplot2)

# Create scatter plot for vl_inflow_net

plot_vl_inflow_net <-ggplot(data = ndata3, aes(x = vl_inflow_net, y = aggregate_receivables_purchase, color = factor(fund_cnpj8))) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 0.6) +
  labs(x = "Firm Aggregate Inflow", y = "Aggregate Receivables Purchase", title = "Agg Receivables Purchase v. Firm Aggregate Inflow") +
  scale_color_discrete(name = "Firm", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  theme_minimal() +
  theme(  
    text = element_text(size = 10),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 10, hjust = 0.5)
  )
  
  
plot_selic <- ggplot(data = ndata3, aes(x = selic, y = aggregate_receivables_purchase, color = factor(fund_cnpj8))) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 0.6) +
  labs(x = "Brazilian Targeted Federal Funds Rate (%)", y = "Aggregate Receivables Purchase", title = "Agg Receivables Purchase v. Federal Funds Rate") +
  scale_color_discrete(name = "Firm", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  theme_minimal() +
  theme(  
    text = element_text(size = 10),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 10, hjust = 0.5)
  )

plot_cpi <- ggplot(data = ndata3, aes(x = cpi, y = aggregate_receivables_purchase, color = factor(fund_cnpj8))) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 0.6) +
  labs(x = "CPI", y = "Aggregate Receivables Purchase", title = "Agg Receivables Purchase v. CPI") +
  scale_color_discrete(name = "Firm", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  theme_minimal() +
  theme(  
    text = element_text(size = 10),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 10, hjust = 0.5)
  )
  
plot_nav <- ggplot(data = ndata3, aes(x = nav, y = aggregate_receivables_purchase, color = factor(fund_cnpj8))) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(method = "lm", se = FALSE, size = 0.6) +
  labs(x = "Firm's Total Asset", y = "Aggregate Receivables Purchase", title = "Agg Receivables Purchase v. Firm's Total Asset") +
  scale_color_discrete(name = "Firm", labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  theme_minimal() +
  theme(  
    text = element_text(size = 10),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 10, hjust = 0.5)
  )

grid.arrange(plot_vl_inflow_net, plot_selic, plot_cpi, plot_nav, nrow = 2)












