# Purpose: General plotting functions not specific to a particular purpose

library(dplyr)
library(ggplot2)

# Plot_scatter_gen shows the distribution across observations in the relationship
#   between x and y at the observation-level, with different colors by group and
#   different size by weight
Plot_scatter_gen <- function(data, x_var="maturity_r", y_var="ir_r", group_var="is_fund_open",
  weight_var="vl_bought_r", x_title="Maturity (Days)", y_title="Interest Rate (%)", 
  weight_title="Purchases\n(Million USD)", 
  group_title="Fund Type", group_colors=c("Open"="purple4", "Closed"="gold4")) {
  return(ggplot(data, aes(x=!!rlang::sym(x_var), y=!!rlang::sym(y_var), 
    color=!!rlang::sym(group_var), size=!!rlang::sym(weight_var), weight=!!rlang::sym(weight_var))) +
  geom_point() + 
  #geom_smooth(method="lm", se=FALSE, size=0.6) +
  labs(x=x_title, y=y_title, color=group_var, size=weight_title) +
  scale_color_manual(values=group_colors) +
  scale_size_continuous(range=c(0.2, 2)) +
  theme_minimal()
  )
}

Plot_scatter_gen