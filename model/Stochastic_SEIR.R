source("class/individual_class.R")
source("handler_functions/daily_case_handlers.R")
library(plotly)

Stochastic_SEIR <- function(N = 1000000, first_case_date = '03/23/2020', first_case_count = 1, simulation_time = 30, 
                            beta0 = 1, bata1 = 0.5, beta2 = 0.3, beta3 = 0.1, 
                            alpha = 0.3, alpha_p = 0.6, 
                            rho1 = 0.5, rho2 = 0.4, rho3 = 0.2, 
                            resusciptible = 0,
                            incubation_period = 5, asymptomatic_duration = 6, 
                            mild_duration = 6, severe_duration = 6, critical_duration = 8) {
  #' Gets the SEIR model with Asymptomatic patients and run simulation to estimate the number of susceptible, infected, and recovered per day
  #'
  #'
  #'
  # I assume that after mild infection is done, the individual did the test and confirmed positive
  
  actual_simulation_time <- simulation_time + incubation_period + mild_duration
  S <- rep(N, actual_simulation_time)
  E <- I0 <- I1 <- I2 <- I3 <- R <- D <- rep(0, actual_simulation_time)
  population_stat <- data.frame(S, E, I0, I1, I2, I3, R, D)
  population_stat[1, 'S'] = N
  death_threshold <- alpha_p*rho1*rho2*rho3
  critical_threshold <- alpha_p*rho1*rho2
  severe_threshold <- alpha_p*rho1
  mild_threshold <- alpha_p
  asymptomatic_threshold <- alpha + alpha_p
  # first cases:
  ind <- create_new_batch(new_cases = first_case_count, incubation_period, critical_threshold, death_threshold, severe_threshold, mild_threshold,
                               ritical_duration, severe_duration, mild_duration,
                               asymptomatic_threshold, asymptomatic_duration)
  population_stat <- update_population(t = 1, exposed_individual = ind, population_stat = population_stat, new_cases = first_case_count)
# rest of the simulation
  for (t in 2:10) {
    new_exposed_cases <- new_exposed(beta0 = beta0, beta1 = beta1, beta2 = beta2, beta3 = beta3, 
                                     S = population_stat[t, 'S'], I0 = population_stat[t, 'I0'], I1 = population_stat[t, 'I1'],
                                     I2 = population_stat[t, 'I2'], I3 = population_stat[t, 'I3'], N = N
                                     )
    print(new_exposed_cases)
    if (new_exposed_cases > 0) {
      ind <- create_new_batch(new_cases = new_exposed_cases, incubation_period, critical_threshold, death_threshold, severe_threshold, mild_threshold,
                                   ritical_duration, severe_duration, mild_duration,
                                   asymptomatic_threshold, asymptomatic_duration)
      population_stat <- update_population(t = t, exposed_individual = ind, population_stat = population_stat, new_cases = new_exposed_cases)
    }
    # }
  }
  population_stat <- population_stat[1:(simulation_time + incubation_period + mild_duration), ]
  date_range <- seq(as.Date(first_case_date, format = '%m/%d/%Y') - (incubation_period + mild_duration - 1), 
                    as.Date(first_case_date, format = '%m/%d/%Y') + simulation_time, "days")
  population_stat['Date'] = date_range
  return(population_stat)
}


plot_stats <- function(population_stat) {
  fig <- plot_ly(population_stat, x = ~Date, y = ~I1, type = 'scatter', mode = 'lines', name = 'Mild infection')
  fig %>%
    add_trace(y = ~I2, name = 'Severe infection') %>%
    add_trace(y = ~D, name = 'Recovered')
  
}


if(FALSE)
{
  N = 1000000; first_case_date = '03/23/2020'; first_case_count = 1; simulation_time = 90; 
  beta0 = 1; beta1 = 1; beta2 = 0.3; beta3 = 0.1; 
  alpha = 0.3; alpha_p = 0.6; 
  rho1 = 0.5; rho2 = 0.4; rho3 = 0.2; 
  resusciptible = 0;
  incubation_period = 5; asymptomatic_duration = 6; 
  mild_duration = 6; severe_duration = 6; critical_duration = 8
}