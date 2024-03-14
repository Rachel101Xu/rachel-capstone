# Purpose: Helper functions for boletos (interfirm payment volume and trade credit)

library(dplyr)
library(ggplot2)

# Plot_hist_tc_mat() is a histogram defined on the invoice transaction-level df_invoice
#   that shows the most common individual values of maturity, split across multiple
#   graphs for readability
# Each graph has 
#   x-axis equal to day
#   y-axis equal to total volume (sum of payment) with the same y-axis ticks
# This code is already done on the server, need to export

# Plot_hist_bol_vol() is a histogram defined on the firm-level invoice data
#   that shows total inflows and outflows 
#   (to/from other firms, or in general, whether using invoice_network or invoice_in/_out)
#   x-axis is amount of payment
#   y-axis is number of firms