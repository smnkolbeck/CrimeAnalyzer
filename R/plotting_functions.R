
#' Plot distributions
#' 
#' Plots the distribution of a specified categorical indicator
#' 
#' @encoding UTF-8
#' 
#' @param data Data used for plotting
#' @param variable The variable for which the distribution is to be plotted
#' @param threshold_value Only categories with a percentage of observations greater than the specified threshold will be plotted. Threshold takes the form of a 
#'
#' @returns a ggplot object 
#'
#' @export
distribution_bar <- function(data,
                             variable,
                             value ="count",
                             threshold_value=0){
require(dplyr)
require(ggplot2)
  
  #Stops function if 'variable' is not in data
  if(!(variable %in% names(data))){
    stop("'variable' is not in 'data'")
  }
  
  #Prepares data for plotting by grouping and aggregating indicator
  grouped_data <-
    data %>% 
    group_by(!!sym(variable)) %>% 
    summarise(count = n()) %>% 
    filter(!is.na(!!sym(variable))) %>% 
    mutate(percent = prop.table(count)) %>% 
    filter(percent > threshold_value)
  
  if(value == "count"){
  #plots indicator
    distribution_plot <- 
     ggplot(grouped_data,aes(x=!!sym(variable),y=count))+
     geom_bar(stat="identity",color="black",fill = "grey", alpha = 0.8) +
     theme_minimal(base_family="serif") +
     theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5)) +
     ylab("Count")
  }else if(value == "percent"){
    distribution_plot <-
      ggplot(grouped_data,aes(x=!!sym(variable),y=percent))+
      geom_bar(stat="identity",color="black",fill = "grey", alpha = 0.8) +
      theme_minimal(base_family="serif")  +
      theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5)) +
      ylab("Percent") +
      scale_y_continuous(labels = scales::percent)
      
  }

return(distribution_plot)
  
}

#'
#'Plot Time Trends
#'
#'Plots the longitudinal trend of a specified indicator
#'
#'
#'
#' @param data Data used for plotting
#' @param time_indicator The time indicator that will appear on the x-axis. 
#' @param smoothing_method Smoothing method that will be passed to geom_smooth. See documentation for geom_smooth: https://ggplot2.tidyverse.org/reference/geom_smooth.html
#' @param linecolor The color of the trend line. Default is "black"
#' @param linwidth The width of the trend line.
#' @param text_size The size of the text. Default is 14
#' @param theme The plot theme. See for available options: https://ggplot2.tidyverse.org/reference/ggtheme.html
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param group Grouping variable. Default is null. Plots distinct time trends for each group if desired
#' @param group_colors Color palette used to distinguish each group.
#' 
#' @returns A ggplot object
#' 
#' @export
plot_timetrend <- function(data,
                           y_var,
                           time_indicator,
                           smoothing_method = NULL,
                           linecolor = "black",
                           linewidth = NULL,
                           text_size = 14,
                           theme = "minimal",
                           xlab = "Time Period",
                           ylab = "Value",
                           group = NULL,
                           group_colors = "Set1"){
  
  suppressPackageStartupMessages(require(tidyverse))
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(zoo))
  #Create plot data
  plot_data <-
    data %>% 
    select(c(time_indicator,y_var)) %>% 
    rename(time = time_indicator,value = y_var)
  
  #Create plot
  if(is.null(group)){
    plot <- 
      ggplot(plot_data,aes(x = time,y=value)) +
      geom_point() +
      geom_line(colour = linecolor,linewidth = linewidth) +
      theme(text = element_text(family = "serif", size = text_size))
  }else if(!is.null(group)){
    plot <- 
      ggplot(plot_data,aes(x = time,y=value,group = !!sym(group))) +
      geom_point() +
      geom_line(colour = linecolor,linewidth = linewidth) +
      theme(text = element_text(family = "serif", size = text_size)) +
      scale_color_brewer(palette = "Set1")
    
  }
  
  
  
  #Add theme according to user input
  theme <- switch(theme,
                  minimal = theme_minimal(),
                  bw = theme_bw(),
                  classic = theme_classic(),
                  dark = theme_dark(),
                  light = theme_light(),
                  default = theme_gray())  # Default theme if no match
  
  plot <- plot + theme
  #Add Smoothing
  if(!is.null(smoothing_method)){
    plot <- plot + geom_smooth(method = smoothing_method, se=FALSE, color="black")
  }
  
  #Add axis labels:
  plot <- plot + xlab(xlab) + ylab(ylab)
  
  
  
  return(plot)
}
