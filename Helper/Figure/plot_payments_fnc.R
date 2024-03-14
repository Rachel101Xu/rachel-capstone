# Purpose: Catalog helper functions for general forms of plots
# Note: Some function inputs may be specific to the payments dataset
library(ggplot2)
library(stringr)

# Create_plot_gif() combines a series of plots into a gif
# Parameters:
#   - dir_in is the string name of the directory with the exact set of plots
#   - dir_out is the string name of where you want to save the gif
#   - gif_name is the string name of the gif file without the .gif ending
#   - fps is the number of frames per second in the gif
Create_plot_gif_magick <- function(dir_in, dir_out, gif_name, fps=1) {
  plots <- list.files(dir_in, full.names=TRUE)
  plot_read <- lapply(plots, image_read)
  plot_joined <- image_join(plot_read)
  gif <- image_animate(plot_joined, fps=fps)
  image_write(image=gif, path=paste0(dir_out, gif_name, ".gif"))
}
# Create_gif() is an incomplete wrapper function for Create_plot_gif()
Create_gif <- function(dir_in){
  files <- list.files(dir_in, full.names=TRUE)
  
  this_plot <- Plot_hist(this_data, "log_total_payment", 
                           "Log Total Inflow (BRL)",
                           "Number of CNPJ")
}
  
# Plot_grid() is a general wrapper function for a choropleth plot on a grid 
#   (e.g. using colors to highlight flows in an input-output matrix)
# Parameters:
#   matrix = values on a grid in matrix format
#   legendtitle = title of the legend
#   lims = min and max values (for the color scale)
#   rotate_x = whether the x-axis label should be rotated 90 degrees
# Returns the ggplot object
Plot_grid_payments <- function(matrix, legendtitle, lims=c(-1,1), rotate_x=FALSE){
  if(rotate_x) {
    x_angle <- 90
  } else {
    x_angle <- 0
  }
  x = reshape2::melt(matrix)
  colorbreaks = c(lims[1], 0, lims[2])
  names(x) = c("debtor", "creditor", "color")
  x <- x %>% mutate(across(c(creditor, debtor), as.character))
  g_grid_plot = ggplot(x, aes(creditor, debtor, fill=color, label=color)) +
    geom_tile() +
    theme_minimal() +
    scale_fill_gradient2(low="red", mid="white", high="violet", limits=lims, 
    breaks=colorbreaks, labels=as.character(colorbreaks)) + 
    xlab("\nCreditor Sector") + ylab("Debtor Sector\n") + 
    guides(x = guide_axis(angle = x_angle)) + labs(fill=legendtitle)
  return(g_grid_plot)
}

# Plot_hist() is a general wrapper function for a histogram
# Parameters:
#   data = dataset for plotting
#   dstr_var = variable whose distribution we wish to show
#   dstr_varlab = label (title) of the variable
#   w_var = name of the weight variable
#   y_axislab = label (title) of the y-axis
# Returns the ggplot object
Plot_hist <- function(data, dstr_var, dstr_varlab, w_var="NA",
  y_axislab="Number of sector pairs",
  xlb=3, xub=21, xstep=3, nbins=36){
  if(w_var == "NA") {
    data <- data %>% mutate(unit_weight = 1)
    w_var <- "unit_weight"
  }
  g_plot_hist <- data %>%
    ggplot(aes_string(x=dstr_var, weight=w_var)) +
    geom_histogram(position="identity", bins=nbins) + 
    xlab(paste0("\n", dstr_varlab)) + ylab(paste0(y_axislab, "\n")) +
    theme_minimal() + theme(panel.grid.minor=element_blank()) +
    scale_x_continuous(breaks=seq(xlb, xub, xstep), limits=c(xlb, xub))
  return(g_plot_hist)
}

# Plot_flows() plots the flows of payments over time
# Parameters:
#   data = dataset for plotting
#   yvar = y-axis variable
#   yvarlab = y-axis variable label
#   groupvar = variable that delineates groups (i.e. one line per group)
#   groupname = group variable label
#   colors = named vector with colors of groups
#   xvarlab = x-axis variable label (default is x-variable to be time in months)
# Returns the ggplot object
Plot_flows <- function(data, yvar, yvarlab, groupvar, groupname, colors, xvarlab="Month", ggtitle="") {
# Need to ensure that we have a complete panel, i.e. every possible combination
#   of group and date in the dataset. Missing to be replaced by 0 because
#   our dataset is supposed to be comprehensive
  g_plot_flows <- data %>% 
    mutate(date=as.character(date)) %>% 
    mutate(date=paste0(date, "01")) %>% # normalize each month's date to the first day
    mutate(group_v = eval(parse(text=groupvar))) %>%
    mutate(y_v = eval(parse(text=yvar))) %>%
    complete(date, group_v) %>% 
    mutate(y_v = replace(y_v, is.na(y_v), 0)) %>%
    mutate(date_v=as.Date(date, format="%Y%m%d")) %>%
    mutate(group_fac=factor(group_v)) %>%
    ggplot( aes(x=date_v, y=y_v, group=group_v, color=group_fac)) + 
      geom_line() + 
      theme_minimal() +
      ggtitle(ggtitle) +
      scale_x_date(name=xvarlab, date_labels = "%m/%Y") + 
      scale_color_manual(values=colors) +
      ylab(yvarlab) + labs(color=groupname) 
  return(g_plot_flows) 
}

# Plot_stacked_area() is the same as Plot_flows except it creates a stacked
#   area plot instead of a line plot
# Parameters:
#   data = dataset for plotting
#   yvar = y-axis variable
#   yvarlab = y-axis variable label
#   groupvar = variable that delineates groups (i.e. one line per group)
#   groupname = group variable label
#   colors = named vector with colors of groups
#   xvarlab = x-axis variable label (default is x-variable to be time in months)
# Returns the ggplot object
Plot_stacked_area <- function(data, yvar, yvarlab, groupvar, groupname, colors, xvarlab="Month") {
  g_plot_area <- data %>% 
  mutate(date=paste0(date, "01")) %>% # normalize each month's date to the first day
  mutate(date=as.Date(date, format="%Y%m%d")) %>%
  mutate(group_fac=factor(eval(parse(text=groupvar)))) %>%
  ggplot( aes_string(x="date", y=yvar, group=groupvar, fill="group_fac")) + 
    geom_area() + 
    theme_minimal() +
    scale_x_date(name=xvarlab, date_labels = "%m/%Y") + 
    scale_fill_manual(values=colors) +
    ylab(yvarlab) + labs(fill=groupname) 
  return(g_plot_area) 
}


