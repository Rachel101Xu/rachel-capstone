library(dplyr)
library(ggplot2)
library(gghighlight)

### Firm-level loan portfolio figures
# Plot_stacked_bars() creates stacked bar chart examining a given variable in question across different maturities
#   data = dataset for plotting
#   yvar = y-axis variable
#   xvar = x-axis variable
#   xlabel = x-axis variable label
#   ylabel = y-axis variable label
#   group = variable that delineates groups (i.e. one line per group)
#   position_type = specifies style of bar chart representation
#   colors = named vector with colors of groups
# Returns the ggplot stacked bar chart object
Plot_stacked_bars <- function(data, yvar, xvar="maturity", xlabel="Days Until Due", ylabel="Fraction of Loans\n", 
  group_var="loan_type", group="Loan Type", group_colors=c("Factoring"="brown", "Line of Credit"="navy", "Term Loan"="darkgray"),
  position_type="fill") {
  ggplot(data, aes_string(fill=group_var, y=yvar, x=xvar)) + 
    geom_bar(position=position_type, stat="identity") +
    theme_minimal() +
    xlab(xlabel) + ylab(ylabel) + labs(fill=group) + 
    scale_fill_manual(values=group_colors) +
    theme(axis.text.x = element_text(angle=45, vjust=0.8, hjust=0.5))
}


# Plot_line_maturity() prepares firm-level yield and delinquency curves
#   Line graph where each line & color corresponds to a type of WC financing,
#   x-axis is maturity
#   y-axis is average interest rate (weighted by amount borrowed)
Plot_line_maturity <- function(data, yvar, xlabel="\nMaturity (Days)", ylabel="Fraction of Loans\n", 
  group_var="loan_type", group="Loan Type", group_colors=c("Factoring"="brown", "Line of Credit"="navy", "Term Loan"="darkgray")) {
  ggplot(data, aes_string(x="maturity", y=yvar, color=group_var, group=group_var)) + 
    geom_line() +
    xlab(xlabel) + ylab(ylabel) + labs(color=group) +
    theme_minimal() +
    scale_x_discrete(limits = data$maturity) +
    scale_color_manual(values=group_colors) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
}

# Together, these graphs should motivate why firms use different types of WCF
#   at different maturities, and show a pattern of banks' financing

