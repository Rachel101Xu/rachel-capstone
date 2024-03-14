# Purpose: Helper functions for funds' purchases of boletos

library(dplyr)
library(ggplot2)

# Plot_hist_fund_pur shows the distribution across funds of total factoring purchases
#   x-axis is bin of FIDC purchase volume
#   y-axis is proportion of FIDCs
# data is at the fund level
Plot_hist_fund_pur <- function(data, x_type, title_suffix="") {
  if(x_type=="tot") {
    x_var <- "vl_bought_tot"
    pref <- "Total "
  } else if(x_type=="nr") {
    x_var <- "vl_bought_nr"
    pref <- "Non-Recourse "
  } else if(x_type=="r") {
    x_var <- "vl_bought_r"
    pref <- "Recourse "
  }
  return(ggplot(data, aes(x=!!rlang::sym(x_var), y=stat(density*width))) + 
    geom_histogram(binwidth=50) + 
    scale_y_continuous(name="Proportion of FIDCs") +
    labs(title=paste0(pref, "Factoring Purchase Volume\nby Fund", title_suffix), 
      x="Purchases (Nominal Million BRL = 200k USD)") + theme_minimal()
  ) 
}
