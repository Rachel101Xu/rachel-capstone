# Purpose: Visualize fund flows and the impact of flows on firms' factoring prices 

library(dplyr)
library(ggplot2)

# Plot_flow_fund() is a line graph 
#   x-axis equal to month, cast as date (see example code)
#   y-axis equal to the total flow_amount (across shareholders within group)
#   separate line for each group (group_shareholder)
Plot_flow_fund <- function(data=df_fund_flows, group_colors, group_labels) {
  flow_over_time <- data %>% 
    mutate(xvar = paste0(flow_year, "-", sprintf("%02d", flow_month), "-01")) %>% 
    group_by(xvar, group_shareholder) %>% 
    summarise(total_flow_amount = sum(flow_amount)) %>% 
    mutate(date = as.Date(xvar)) %>%
    ggplot(aes(x=date, y=total_flow_amount, color=group_shareholder, group=group_shareholder)) +
    geom_line() +
    xlab("\nTime") + ylab("Net Inflow Amount (BRL)\n") + labs(color="Shareholder Type") +
    theme_minimal() +
    scale_color_manual(values=group_colors, labels=group_labels) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  return(flow_over_time)
}

Plot_flow_fund(df_fund_flows, group_colors=c("TRUE"="blue", "FALSE"="red"), group_labels=c("TRUE"="Institutional", "FALSE"="Retail"))

print(df_firm_exposure)
head(df_fund_firm)



# Task 1.1	•	shows the seasonality of fund flows: net flows on y-axis (maybe separate lines by fund size or sectoral exposure), time on x-axis

Plot_net_flow_fund <- function(data=df_fund_flows_panel) {
  flow_over_time <- data %>%
    mutate(year_month = as.character(year_month), 
           year_month = as.Date(paste0(year_month, "01"), format = "%Y%m%d"),
           group_variable = ifelse(vl_inflow_net >= 0, "Positive", "Negative")) %>%
    ggplot(aes(x=year_month, y=vl_inflow_net, color=group_variable, group=group_variable)) +
    geom_line() +
    xlab("\nTime") + ylab("Net Flow\n") +
    labs(color="Net Flow Group") +
    scale_color_manual(values=c("Positive"="blue", "Negative"="red")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  
  return(flow_over_time)
}

# Example usage:
Plot_net_flow_fund(df_fund_flows_panel)




head(df_nav)



# Task 1.2
head(df_fund_flows_panel)
head(df_fund_firm)
summary(df_fund_flows_panel)
summary(df_fund_firm)



plot_aggregate_flow_and_receivables <- function(df_fund_flows_panel, df_fund_firm) {
  # Step 1: Aggregate data for fund flows
  aggregate_flow <- df_fund_flows_panel %>%
    group_by(year_month, fund_cnpj8) %>%
    summarise(aggregate_flow = sum(vl_inflow_net)) %>%
    ungroup()
  
  # Step 2: Aggregate data for receivables purchase by FIDCs
  aggregate_receivables_purchase <- df_fund_firm %>%
    group_by(year_month, fund_cnpj8) %>%
    summarise(aggregate_receivables_purchase = sum(expected_pur)) %>%
    ungroup()
  
  # Step 3: Merge the two datasets by year_month and fund_cnpj8
  merged_data <- merge(aggregate_flow, aggregate_receivables_purchase, by = c("year_month", "fund_cnpj8"), all = TRUE)
  
  # Convert year_month to date object
  merged_data$year_month <- as.Date(paste0(merged_data$year_month, "01"), format = "%Y%m%d")

  
  print(head(merged_data))
  
  # Step 4: Plot the lines for both aggregates
  ggplot(merged_data, aes(x = year_month)) +
    geom_line(aes(y = aggregate_flow, color = "Aggregate Flow")) +
    geom_line(aes(y = aggregate_receivables_purchase, color = "Aggregate Receivables Purchase by FIDCs")) +
    xlab("Month") + ylab("Aggregate") +
    labs(color = "Variable") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
}

# Example usage:
plot_aggregate_flow_and_receivables(df_fund_flows_panel, df_fund_firm)

## averaged

plot_aggregate_flow_and_receivables <- function(df_fund_flows_panel, df_fund_firm) {
  library(dplyr)
  library(ggplot2)
  
  # Step 1: Aggregate data for fund flows
  aggregate_flow <- df_fund_flows_panel %>%
    group_by(year_month, fund_cnpj8) %>%
    summarise(aggregate_flow = sum(vl_inflow_net)) %>%
    ungroup()
  
  # Step 2: Aggregate data for receivables purchase by FIDCs
  aggregate_receivables_purchase <- df_fund_firm %>%
    group_by(year_month, fund_cnpj8) %>%
    summarise(aggregate_receivables_purchase = sum(expected_pur)) %>%
    ungroup()
  
  # Step 3: Merge the two datasets by year_month and fund_cnpj8
  merged_data <- merge(aggregate_flow, aggregate_receivables_purchase, by = c("year_month", "fund_cnpj8"), all = TRUE)
  
  # Convert year_month to date object
  merged_data$year_month <- as.Date(paste0(merged_data$year_month, "01"), format = "%Y%m%d")
  
  # Step 4: Group by year_month again and calculate the average aggregate_flow and aggregate_receivables_purchase
  merged_data <- merged_data %>%
    group_by(year_month) %>%
    summarise(aggregate_flow = mean(aggregate_flow, na.rm = TRUE),
              aggregate_receivables_purchase = mean(aggregate_receivables_purchase, na.rm = TRUE))
  
  print(head(merged_data))
  
  # Step 5: Plot the lines for both aggregates
  ggplot(merged_data, aes(x = year_month)) +
    geom_line(aes(y = aggregate_flow, color = "Aggregate Flow")) +
    geom_line(aes(y = aggregate_receivables_purchase, color = "Aggregate Receivables Purchase by FIDCs")) +
    xlab("Month") + ylab("Aggregate") +
    labs(color = "Variable") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
}

# Example usage:
plot_aggregate_flow_and_receivables(df_fund_flows_panel, df_fund_firm)







Plot_flow_fund <- function(data=df_fund_flows, group_colors, group_labels) {
  flow_over_time <- data %>% 
    mutate(xvar = paste0(flow_year, "-", sprintf("%02d", flow_month), "-01")) %>% 
    group_by(xvar, group_shareholder) %>% 
    summarise(total_flow_amount = sum(flow_amount)) %>% 
    mutate(date = as.Date(xvar)) %>%
    ggplot(aes(x=date, y=total_flow_amount, color=group_shareholder, group=group_shareholder)) +
    geom_line() +
    xlab("\nTime") + ylab("Net Inflow Amount (BRL)\n") + labs(color="Shareholder Type") +
    theme_minimal() +
    scale_color_manual(values=group_colors, labels=group_labels) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  return(flow_over_time)
}



# Example year_month variable
year_month <- "201710"

# Convert year_month to date object
date_object <- as.Date(paste0(year_month, "01"), format = "%Y%m%d")

# Output
date_object





plot_aggregate_flow_and_receivables <- function(df_fund_flows_panel, df_fund_firm) {
  library(dplyr)
  library(ggplot2)
  
  # Step 1: Aggregate data for fund flows
  aggregate_flow <- df_fund_flows_panel %>%
    group_by(year_month, fund_cnpj8) %>%
    summarise(aggregate_flow = sum(vl_inflow_net)) %>%
    ungroup()
  
  # Step 2: Aggregate data for receivables purchase by FIDCs
  aggregate_receivables_purchase <- df_fund_firm %>%
    group_by(year_month, fund_cnpj8) %>%
    summarise(aggregate_receivables_purchase = sum(expected_pur)) %>%
    ungroup()
  
  # Step 3: Merge the two datasets by year_month and fund_cnpj8
  merged_data <- merge(aggregate_flow, aggregate_receivables_purchase, by = c("year_month", "fund_cnpj8"), all = TRUE)
  
  # Convert year_month to date format
  merged_data <- merged_data %>%
    mutate(year_month = as.Date(paste0(year_month, "01"), format = "%Y%m%d"))
  
  # Print table of fund_cnpj8 if needed
  # print(table(merged_data$fund_cnpj8))
  
  # Step 4: Plot the lines for both aggregates
  ggplot(merged_data, aes(x = year_month)) +
    geom_line(aes(y = aggregate_flow, color = "Aggregate Flow")) +
    geom_line(aes(y = aggregate_receivables_purchase, color = "Aggregate Receivables Purchase by FIDCs")) +
    xlab("Month") + ylab("Aggregate") +
    labs(color = "Variable") +
    theme_minimal()
}

# Example usage:
plot_aggregate_flow_and_receivables(df_fund_flows_panel, df_fund_firm)








# explore relevant data sets
head(df_fund_flows)
head(df_fund_flows_panel) ## aggregate flow 
dim(df_fund_flows)
dim(df_fund_flows_panel)
table(df_fund_flows_panel$fund_cnpj8) # each fund (10) appears 84 times
table(df_fund_flows$fund_cnpj8)

head(df_firm_exposure)

head(df_fund_flows)
dim(df_fund_flows)
summary(df_fund_flows)

head(df_fund_firm)
dim(df_fund_firm)
table(df_fund_firm$share_of_fund)

## bar plot
Plot_flow_fund <- function(data=df_fund_flows, group_colors, group_labels) {
  flow_over_time <- data %>% 
    mutate(month = sprintf("%02d", flow_month)) %>% 
    group_by(flow_year, month, group_shareholder) %>% 
    summarise(total_flow_amount = sum(flow_amount)) %>%
    ungroup() %>%
    mutate(month = factor(month, levels = sprintf("%02d", 1:12))) %>%
    ggplot(aes(x=month, y=total_flow_amount, fill=group_shareholder)) +
    geom_bar(stat="identity", position="dodge") +
    xlab("\nMonth") + ylab("Net Inflow Amount (BRL)\n") + labs(fill="Shareholder Type") +
    theme_minimal() +
    scale_fill_manual(values=group_colors, labels=group_labels) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  
  return(flow_over_time)
}

Plot_flow_fund(df_fund_flows, group_colors=c("TRUE"="blue", "FALSE"="red"), group_labels=c("TRUE"="Institutional", "FALSE"="Retail"))



## Line plot: correlation between aggregate 
head(df_fund_flows)
head(df_fund_flows_panel)

Plot_flow_vs_purchase <- function(flow_data, purchase_data) {
  # Aggregate flows by month
  aggregate_flows <- flow_data %>%
    mutate(year_month = as.character(year_month)) %>%
    group_by(year_month) %>%
    summarise(aggregate_flow = sum(vl_inflow_net))
  
  # Aggregate purchases by month
  aggregate_purchases <- purchase_data %>%
    group_by(year_month) %>%
    summarise(aggregate_purchase = sum(expected_pur))
  
  # Merge aggregate flows and purchases
  merged_data <- inner_join(aggregate_flows, aggregate_purchases, by = "year_month")
  
  # Plotting
  ggplot(merged_data, aes(x = year_month, group = 1)) +
    geom_line(aes(y = aggregate_flow, color = "Aggregate Flow")) +
    geom_line(aes(y = aggregate_purchase, color = "Aggregate Purchase by FIDCs")) +
    xlab("\nYear-Month") + ylab("Amount\n") +
    scale_color_manual(values = c("Aggregate Flow" = "blue", "Aggregate Purchase by FIDCs" = "red")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
}

# Example usage:
Plot_flow_vs_purchase(df_fund_flows_panel, df_fund_firm)


