library(EpiEstim)
library(plotly)
library(tidyr)
library(zoo)
library(HDInterval)
library(purrr)
source('handler_functions/count_smoother.R')
## One of the parameters that can be used to see how disease is being transmitted is Estimated Instantaneous Number

# serial interval is the parameter can be applied to calculate the posterior values for the Re
mean_si <- 4.7
std_si <- 2.9

calculate_Re_From_SI <- function(dataframe, mean_si, std_si, county, region) {
  # dataframe <- all_cases[all_cases$LATEST_COUNTED_DHSS_REGION == 'STATE', -1]
  dataframe <- dataframe[ , c('ONSET_DATE', 'smoothed_cases')]
  colnames(dataframe) <- c('dates', 'I')
  dataframe$dates <- as.Date(dataframe$dates)
  # dataframe <- dataframe %>%
  #   tidyr::complete(dates = seq.Date(min(dates), max(dates), by = 'day'), fill = list(I = 0)) 
  # dataframe$dates <- as.Date(dataframe$dates)
  missouri_re_parametric_si <- estimate_R(dataframe,
                                          method = 'parametric_si',
                                          config = make_config(list(mean_si = mean_si, std_si = std_si)))
  # estimated_Re <- head(missouri_re_parametric_si$R, -7)
  estimated_Re <- missouri_re_parametric_si$R
  estimated_Re$dates <- as.Date(min(dataframe$dates)) + estimated_Re$t_end - 1
  estimated_Re$all_one <- 1
  fig <- plot_ly(data = estimated_Re, x = ~dates, y = ~`Quantile.0.975(R)`, type = 'scatter', mode = 'lines', name = 'UCL', 
                 line = list(color = 'transparent')) %>%
    add_trace(y = ~`Quantile.0.025(R)`, type = 'scatter', mode = 'lines',
              fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', name = 'LCL', line = list(color = 'transparent')) %>%
    add_trace(y = ~`Mean(R)`, type = 'scatter', mode = 'lines', line = list(color = 'black'), name = 'Re') %>%
    add_trace(y = ~all_one, type = 'scatter', mode = 'lines', name = 'unit line', line = list(color = 'red', dash = 'dash')) %>%
    layout(title = paste0('Covid-19 effective reproductive number (Re), ', county, ', ', tolower(region), ' district, Missouri. 2020'), 
           xaxis = list(title = 'day'),
           yaxis = list(title = 'effective reproducion number'))

  return(fig)
}

calculate_Re_with_uniform_prior <- function(dataframe) {
  #' calculate the Re with uniform prior for Re and a fixed 
  #' 
  # dataframe <- all_cases[all_cases$LATEST_COUNTED_DHSS_REGION == 'STATE', c('ONSET_DATE', 'CASES', 'smoothed_cases')]
  # dataframe <- as.data.frame(dataframe)
  dataframe$ONSET_DATE <- as.Date(dataframe$ONSET_DATE)
  # dataframe <- dataframe %>%
  #   tidyr::complete(ONSET_DATE = seq.Date(min(ONSET_DATE), max(ONSET_DATE), by = 'day'), fill = list(CASES = 0)) 
  # set up the prior probabilities
  Re_min <- 0
  Re_max <- 8
  serial_interval <- 4.7
  Re_prior <- seq(Re_min, Re_max, by = 0.1)
  gamma <- 1 / serial_interval
  observed_cases <- dataframe$smoothed_cases
  
  # likelihoods under prior probability
  likelihoods <- data.frame(day = 0:(length(observed_cases) - 1), cases = observed_cases) %>%
    tidyr::crossing(r_e = Re_prior) %>%
    group_by(r_e) %>%
    mutate(lambda = lag(cases) * exp(gamma * (r_e - 1))) %>%
    ungroup() %>%
    mutate(likelihood_r_e = dpois(lambda = lambda, x = cases)) %>%
    # ignore day 0 
    filter(day > 0)
    
  # ggplot(data = likelihoods[likelihoods$day == 30,], aes(x = r_e, y = likelihood_r_e)) +
  #   geom_line()
  # 
  # posterior_for_re_1_day_30 <- likelihoods[likelihoods$r_e == 1,] %>%
  #   mutate(last_7_sum = exp(zoo::rollapply(log(likelihood_r_e), width = 7, sum, partial = TRUE, align = 'right')))

  # since the posterior of the last seven days are the sum of 
  posterior_interval = 7
  # posterior probability for Re based on likelihood
  posteriors <- likelihoods %>%
    group_by(r_e) %>%
    arrange(day) %>%
    mutate(posterior = exp(zoo::rollapply(log(likelihood_r_e), posterior_interval, sum, partial = TRUE, align = 'right'))) %>%
    # mutate(posterior = zoo::rollapply(likelihood_r_e, posterior_interval, prod, partial = TRUE, align = "left")) %>%
    group_by(day) %>%
    mutate(posterior = posterior / sum(posterior)) %>%
    mutate(posterior = ifelse(is.na(posterior), 0, posterior)) %>%
    ungroup() %>%
    filter(day > posterior_interval)
   
  # ggplot(data = posteriors[posteriors$day == 30,], aes(x = r_e, y = posterior)) +
  #   geom_line()
  
  
  # now select the 95% interval for the posterior probability of the R_e and visualize that
  posterior_intervals <- posteriors %>%
    group_by(day) %>%
    summarize(r_e_simulated = list(sample(x = r_e, size = 10000, prob = posterior, replace = TRUE)), 
           r_e_most_likely = Re_prior[which.max(posterior)]) %>%
    mutate(r_e_low = purrr::map_dbl(r_e_simulated, ~hdi(.x)[1]),
           r_e_high = purrr::map_dbl(r_e_simulated, ~hdi(.x)[2])) %>%
    select(-r_e_simulated)
    
  # posterior_intervals <- head(posterior_intervals, -7)
  posterior_intervals$day <- as.Date(min(dataframe$ONSET_DATE)) + posterior_intervals$day 
  posterior_intervals$Region <- unique(dataframe$LATEST_COUNTED_DHSS_REGION)[1]
  
  return(posterior_intervals)
  # ggplot(data = posterior_intervals, aes(x = day, y = r_e_most_likely)) +
  #   geom_line()
  
  # ggplot(data = posteriors[posteriors$day == 10,], aes(x = r_e, y = posterior, group = day)) +
  #   geom_line()
}


