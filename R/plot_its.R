#' Plot Interrupted Time Series Trend
#'
#' Plots the time trend of an indicator before and after an intervention at time t
#'
#' @encoding UTF-8
#' 
#' @param data Data used for plotting
#' @param time_var Time variable. Can be expressed as an "integer" or "Date"
#' @param outcome Variable name of the outcome of interest 
#' @param treat_start The start time of the treatment 
#' @param smooth Whether to add a smoothed trend line, Default is NULL
#' @param xlab X-axis Title
#' @param ylab Y-axis Title
#' @param treat_label_y The heigh at which the "Treatment" label appears. 
#'
#' @export

plot_its <- function(data,
                     time_var,
                     outcome,
                     treat_start,
                     smooth = FALSE,
                     xlab="Time",
                     ylab="Value",
                     treat_label_y = NULL
){
  #Sets the treatment label to the max of the outcome if no value is supplied
  if (is.null(treat_label_y)) {
    treat_label_y <- max(data[[outcome]], na.rm = TRUE) # Default to max value of outcome variable
  }
  
  its_plot <- 
    data %>% 
    ggplot(.,aes_string(x=time_var,y=outcome)) +
    geom_line(linewidth=0.8)  +
    geom_vline(xintercept = treat_start, linetype = "dashed", color = "red",linewidth = 1.2) +
    annotate("text", x = treat, y = treat_label_y, label = "Treatment", vjust = 1.5, color = "red", angle = 90) +
    theme_bw(base_family = "serif")+
    labs(x=xlab,
         y=ylab)
  #Adjusts x-axis according to time input
  if(class(time_var=="integer") | class(time_var)=="numeric") {
    its_plot <-
      its_plot +
      scale_x_continuous(breaks = seq(1,max(time_var),1))
  }else if(class(time_var)=="Date"){
    its_plot <-
      its_plot +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  }
  
  if(smooth==TRUE){
    its_plot <-
      its_plot +
      geom_smooth(method = "lm")
  }
  
  return(its_plot)
}
