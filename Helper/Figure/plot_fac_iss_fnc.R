# Purpose: Helper functions for the issuance of factoring, either by banks/fintechs (FOC) or funds (FOF)

library(dplyr)
library(ggplot2)

# Plot_hist_fac() is a histogram for issuance of factoring (sales of receivables)
#   x-axis equal to bins of the specified variable of interest
#   y-axis is the proportion of invoices if is_val_weight==FALSE, otherwise the proportion of invoice volume
Plot_hist_fac <- function(data=df_fac_issue, x_name="ir", x_width=1, x_title="Annual interest Rate (%)", is_val_weight=TRUE) {
  ylab_suf <- ifelse(is_val_weight, "Factoring Volume", "Factoring Transactions")
  # Construct the bins for the histogram
  df_plot <- data %>% mutate(plot_bin = round(!!rlang::sym(x_name) / x_width) * x_width) %>%
    # Define the y-variable
    mutate(y_v1 = ifelse(is_val_weight, val_newop, 1)) %>%
    group_by(plot_bin) %>% summarize(y_v2 = sum(y_v1)) %>% ungroup() %>%
    # Normalize by the total value, so that the sum across bars is 1
    mutate(y = y_v2/sum(y_v2))
  hist_fac <- ggplot(df_plot, aes(x=plot_bin, y=y)) + 
    geom_bar(stat="identity", position="identity", width=x_width) + 
    scale_x_continuous(name=paste0("\n", x_title)) +
    scale_y_continuous(name=paste0("Proportion of ", ylab_suf, "\n")) +
    theme_minimal() 
  return(hist_fac)
}
Plot_hist_fac(df_fac_issue, x_name="ir", x_width=2)
Plot_hist_fac(df_fac_issue, x_name="maturity", x_width=1, x_title="Maturity (Days)", is_val_weight=FALSE)

