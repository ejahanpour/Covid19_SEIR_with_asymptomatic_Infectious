
##' This page will calculate the Re based on the death rate. as death numbers are more accurate and observable than cases or hospitalization
##' we are going to consider this metrics to calculat the Re and will compare that with the other three-based numbers
##' 
##' 

library(readxl)
library(dplyr)
library(EpiEstim)
library(plotly)

# serial interval is the parameter can be applied to calculate the posterior values for the Re
mean_si <- 4.7
std_si <- 2.9
sliding_window <- 7 # for prior distribution of Re calcualtion




calculate_Re_from_death <- function(death_data, mean_si, std_si, sliding_window) {

  onset_to_death <- 13
  #find first non_zero event
  first_non_zero <- min(which(death_data$number_reported > 0))
  death_data <- death_data[first_non_zero:nrow(death_data), ]
  # plot(death_data$number_reported, type = 'l')
  
  # make sure we have data for all the days in between min and max date
  death_data <- death_data %>%
    mutate(date = as.Date(date)) %>%
    tidyr::complete(date = seq.Date(min(date), max(date), by = 'day'), fill = list(number_reported = 0))
  
  # plot the death epi curve
  # fig <- plot_ly(data = death_data, x = ~date, y = ~number_reported, type = 'bar', name = 'death counts',
  #                          marker = list(color = 'steel blue')) %>%
  #   add_trace(x = ~date, y = ~cumulative_number, type = 'scatter', mode = 'lines', name = 'cumulatove', yaxis = 'y2',
  #                          line = list(color = 'red')) %>%
  #   layout(title = 'actual counts vs cumulative numbers',
  #           xaxis = list(title = ""),
  #           yaxis = list(side = 'left', title = 'actual death counts', showgrid = FALSE, zeroline = FALSE),
  #           yaxis2 = list(side = 'right', overlaying = "y", title = 'cumulative death counts', showgrid = FALSE, zeroline = FALSE))
  # 
  # fig


  # calculate the Re with different time window
  T <- nrow(death_data)
  t_start <- seq(2, T - sliding_window)
  t_end <- t_start + sliding_window
  data_for_Re <- death_data %>%
    select(date, number_reported) %>%
    rename(dates = date, I = number_reported)
  # dataframe <- dataframe %>%
  #   tidyr::complete(dates = seq.Date(min(dates), max(dates), by = 'day'), fill = list(I = 0)) 
  # dataframe$dates <- as.Date(dataframe$dates)
  missouri_re_parametric_si <- estimate_R(data_for_Re,
                                          method = 'parametric_si',
                                          config = make_config(list(mean_si = mean_si, std_si = std_si, 
                                                                    t_start = t_start, t_end = t_end)))
  
  death_estimated_Re <- missouri_re_parametric_si$R
  death_estimated_Re$dates <- min(data_for_Re$dates) - onset_to_death + death_estimated_Re$t_end - 1
  death_estimated_Re$all_one <- 1
  
  
  fig <- plot_ly(data = death_estimated_Re, x = ~dates, y = ~`Quantile.0.975(R)`, type = 'scatter', mode = 'lines', 
                 name = 'UCL', line = list(color = 'transparent')) %>%
    add_trace(y = ~`Quantile.0.025(R)`, type = 'scatter', mode = 'lines',
              fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', name = 'LCL', line = list(color = 'transparent')) %>%
    add_trace(y = ~`Mean(R)`, type = 'scatter', mode = 'lines', line = list(color = 'black'), name = 'Re') %>%
    add_trace(y = ~all_one, type = 'scatter', mode = 'lines', name = 'unit line', line = list(color = 'red', dash = 'dash')) %>%
    layout(title = paste0('Covid-19 effective reproductive number (Re) St Louis, Missouri. 2020'), 
           xaxis = list(title = 'day'),
           yaxis = list(title = 'effective reproducion number'))
  return(fig)
}



