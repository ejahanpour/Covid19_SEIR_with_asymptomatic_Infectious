source("class/new_individual.R")
source("handler_functions/daily_case_handlers.R")
library(plotly)

Stochastic_SEIR <- function(N = 100000, first_case_date = "2020-02-02", first_case_count = 1, simulation_time = 30) {
  #' Gets the SEIR model with Asymptomatic patients and run simulation to estimate the number of susceptible, infected, and recovered per day
  #'
  #'
  #'
  # I assume that after mild infection is done, the individual did the test and confirmed positive
  
  ####################  PARAMETERS ####################
  beta0_min = 0.5; beta0_max = 1.5
  beta1_min = 0.5; beta1_max = 1
  beta2_min = 0.2; beta2_max = 0.5
  beta3_min = 0.1; beta3_max = 0.3
  
  
  day_one_infected <- create_individuals_with_infections(case_count = first_case_count)
  sink("log.txt", append=TRUE) #open sink file and add output
  max_incubation <- max(unlist(lapply(day_one_infected, function(x) max(x$incubation_period))))
  actual_simulation_time <- simulation_time + max_incubation 
  S <- rep(N, actual_simulation_time)
  E <- I0 <- I1 <- I2 <- I3 <- R <- D <- rep(0, actual_simulation_time)
  population_stat <- data.frame(S, E, I0, I1, I2, I3, R, D)
  initial_population <- population_stat 
  initial_population$S <- 0  
  
  # create the list of daily individual to susceptable infectious contact rate per day per disease stage 
  daily_infectious_stat <- list(beta0 = runif(actual_simulation_time, beta0_min, beta0_max),
                                beta1 = runif(actual_simulation_time, beta1_min, beta1_max),
                                beta2 = runif(actual_simulation_time, beta2_min, beta2_max),
                                beta3 = runif(actual_simulation_time, beta3_min, beta3_max)
  )
  
  
  t <- 1
  
  #update the Susceptible cases based on the infected number
  population_stat[(t + 1):nrow(population_stat), 'S'] <- population_stat[(t + 1):nrow(population_stat), 'S'] - first_case_count
  # update the population_stat dataframe based on the infected individual metrics
  population_stat <- population_stat + Reduce('+', lapply(day_one_infected, update_daily_cases, initial_population, t = 1))
  # find the number of new infected individuals on day t
  
  
# rest of the simulation
  for (t in 2:(actual_simulation_time - 1)) {
    new_exposed_cases <- new_exposed(beta0 = daily_infectious_stat$beta0[t], beta1 = daily_infectious_stat$beta1[t], beta2 = daily_infectious_stat$beta2[t], 
                                     beta3 = daily_infectious_stat$beta3[t], 
                                     S = population_stat[t, 'S'], I0 = population_stat[t, 'I0'], I1 = population_stat[t, 'I1'],
                                     I2 = population_stat[t, 'I2'], I3 = population_stat[t, 'I3'], N = N
                                     )
    if (new_exposed_cases > 0) {
      day_t_infected <- create_individuals_with_infections(case_count = new_exposed_cases)
      #update the Susceptible cases based on the infected number
      population_stat[(t + 1):nrow(population_stat), 'S'] <- population_stat[(t + 1):nrow(population_stat), 'S'] - new_exposed_cases
      # update the population_stat dataframe based on the infected individual metrics
      population_stat <- population_stat + Reduce('+', lapply(day_t_infected, update_daily_cases, initial_population, t = t))
      # find the number of new infected individuals on day t
    }
    # }
  }
  # population_stat <- population_stat[1:(simulation_time + incubation_max), ]
  date_range <- seq(as.Date(first_case_date) - (max_incubation - 1), 
                    as.Date(first_case_date) + simulation_time, "days")
  population_stat['Date'] = date_range
  return(population_stat)
}


create_individuals_with_infections <- function(case_count) {
  ###' Create case count number of infected individuals based on the clinical prior distributions
  ###' @param case_count: <integer> number of infected individuals per day
  ###' @return infected_individual_list: <list> list of infected_individuals based on the clincal metrics
  ###' 
  
  ########### Parameters ###############
  incubatin_min = 4; incubation_max = 6 # https://annals.org/aim/fullarticle/2762808/incubation-period-coronavirus-disease-2019-covid-19-from-publicly-reported
  asymp_min = 8; asymp_max = 17 # https://www.businessinsider.com/mild-coronavirus-cases-high-fever-dry-cough-2020-3
  mild_to_severe  = 10
  mild_to_recover = 17 
  severe_to_critical = 4
  severe_to_recover = 7
  critical_to_recover = 3
  alpha0 = 0.3
  alpha_p = 1 - alpha0
  alpha1 = 0.39 # https://www.thelancet.com/action/showPdf?pii=S1473-3099%2820%2930232-2
  alpha2 = 0.77
  alpha3 = 0.014 / (alpha_p * alpha1 * alpha2) # https://www.nature.com/articles/s41591-020-0822-7 1.4% of the symptomatics
  # probabilities of (asymptomatic, mild, severe, critical, death)
  severity_level <- c(alpha0, alpha_p*(1-alpha1), alpha_p*alpha1*(1- alpha2), alpha_p*alpha1*alpha2*(1 - alpha3), alpha_p*alpha1*alpha2*alpha3)

  
  individual_feature_list = list(disease_severity = sample(1:5, size = case_count, prob = severity_level, replace = TRUE),
                                 incubation_period = sample(size = case_count, x = incubatin_min:incubation_max, replace = TRUE),
                                 asymptomatic_to_recover = sample(size = case_count, x = asymp_min:asymp_max, replace = TRUE),
                                 mild_to_severe = rep(mild_to_severe, case_count),
                                 mild_to_recover = rep(mild_to_recover, case_count),
                                 severe_to_critical = rep(severe_to_critical, case_count),
                                 severe_to_recover = rep(severe_to_recover, case_count),
                                 critical_to_recover = rep(critical_to_recover, case_count)
  )
  infected_individuals_list <- purrr::transpose(individual_feature_list)
  return(infected_individuals_list)
}

update_daily_cases <- function(daily_infected_individuals, population_stat, t) {
  ###' Updates the population stat of the community based on the nubmer of individuals got infected on day t and their stochastic features
  ###'@param daily_infected_individuals: <list> list of the clinical features for each infected individual (disease_severity, time_for_each_disease_stage, ...)
  ###'@param population_stat: <dataframe> dataframe of simulation days and number of Susceptable, Infected, Recovered and Deseased at each day
  ###'@param t: <integer> day when population_stat is being updated
  ###'@return population_stat: <dataframe> updated population_stat based on the features of individuals being infected
  ###'
  initial_population_rows = nrow(population_stat)
  population_stat[(t + 1):(t+daily_infected_individuals$incubation_period), 'E'] <- 
    population_stat[(t + 1):(t+daily_infected_individuals$incubation_period), 'E'] + 1
  if (daily_infected_individuals$disease_severity == 1) {
    population_stat[(t + daily_infected_individuals$incubation_period + 1):(t + daily_infected_individuals$incubation_period + daily_infected_individuals$asymptomatic_to_recover), 'I0'] <- 
      population_stat[(t + daily_infected_individuals$incubation_period + 1):(t + daily_infected_individuals$incubation_period + daily_infected_individuals$asymptomatic_to_recover), 'I0'] + 1
    population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$asymptomatic_to_recover + 1):nrow(population_stat), 'R'] <- 
      population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$asymptomatic_to_recover + 1):nrow(population_stat), 'R'] + 1
    
  } else if(daily_infected_individuals$disease_severity == 2) {  # mild cases
    population_stat[(t + daily_infected_individuals$incubation_period + 1):(t+ daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_recover), 'I1'] <- 
      population_stat[(t + daily_infected_individuals$incubation_period + 1):(t+ daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_recover), 'I1'] + 1
    population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_recover + 1):nrow(population_stat), 'R'] <- 
      population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_recover + 1):nrow(population_stat), 'R'] + 1
  } else if(daily_infected_individuals$disease_severity == 3) {   # severe cases
    # first patient goes to mild phase
    population_stat[(t + daily_infected_individuals$incubation_period + 1):(t+ daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe), 'I1'] <- 
      population_stat[(t + daily_infected_individuals$incubation_period + 1):(t+ daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe), 'I1'] + 1
    # next S(he) will go to severe phase (hospitalized)
    population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + 1):
                      (t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + daily_infected_individuals$severe_to_recover), 'I2'] <- 
      population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + 1):
                        (t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + daily_infected_individuals$severe_to_recover), 'I2'] + 1
    # finally S(he) will be recovered
    population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + daily_infected_individuals$severe_to_recover + 1):nrow(population_stat), 'R'] <- 
      population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + daily_infected_individuals$severe_to_recover + 1):nrow(population_stat), 'R'] + 1
  } else if(daily_infected_individuals$disease_severity %in% c(4, 5)) {   # critical cases
    # first patient goes to mild phase
    population_stat[(t + daily_infected_individuals$incubation_period + 1):(t+ daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe), 'I1'] <- 
      population_stat[(t + daily_infected_individuals$incubation_period + 1):(t+ daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe), 'I1'] + 1
    # next S(he) will go to severe phase (hospitalized)
    population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + 1):
                      (t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + daily_infected_individuals$severe_to_critical), 'I2'] <- 
      population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + 1):
                        (t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + daily_infected_individuals$severe_to_critical), 'I2'] + 1
    # then S(he) will go to critical phase
    population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe  + daily_infected_individuals$severe_to_critical + 1):
                      (t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + daily_infected_individuals$severe_to_critical + daily_infected_individuals$critical_to_recover), 'I3'] <- 
      population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe  + daily_infected_individuals$severe_to_critical + 1):
                        (t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + daily_infected_individuals$severe_to_critical + daily_infected_individuals$critical_to_recover), 'I3'] + 1      
    # next S(he) will be either recovered or deseased :(
    if (daily_infected_individuals$disease_severity == 4) {
      population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + daily_infected_individuals$severe_to_critical + daily_infected_individuals$critical_to_recover + 1):nrow(population_stat), 'R'] <- 
        population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + daily_infected_individuals$severe_to_critical + daily_infected_individuals$critical_to_recover + 1):nrow(population_stat), 'R'] + 1      
    } else {
      population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + daily_infected_individuals$severe_to_critical + daily_infected_individuals$critical_to_recover + 1):nrow(population_stat), 'D'] <- 
        population_stat[(t + daily_infected_individuals$incubation_period + daily_infected_individuals$mild_to_severe + daily_infected_individuals$severe_to_critical + daily_infected_individuals$critical_to_recover + 1):nrow(population_stat), 'D'] + 1      
      
    }
  }
  return(population_stat[1:initial_population_rows , ])
}


plot_stats <- function(population_stat) {
  fig <- plot_ly(population_stat, x = ~Date, y = ~I1, type = 'scatter', mode = 'lines', name = 'Mild infection')
  fig %>%
    add_trace(y = ~I2, name = 'Severe infection') %>%
    add_trace(y = ~D, name = 'Deceased')
  
}
