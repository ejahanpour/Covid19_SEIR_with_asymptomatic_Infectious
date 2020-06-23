new_exposed <- function(beta0, beta1, beta2, beta3, S, I0, I1, I2, I3, N, E) {
  #' Gets the values for beta0, beta1, beta2, beta3 along with the susciptable and infectious patients to calculate the 
  #' number of new cases per day
  #' Args: 
  #' beta0 = the rates at which asymptomatic person is dealing with society
  #' beta1 = the rates at which mild infectious person is interacting with society
  #' beta2 = the rates at which severe infectious person is interacting with society
  #' beta3 = the rates at which critical infectious person is dealing with society
  #' S = number of susciptable person in population
  #' I0 = number of asymptomatic person in the society
  #' I1 = number of mild infectious person in the society
  #' I2 = number of severe infectious person in the society
  #' I3 = number of critical infectious person in the society
  #' N = population of the societyß
  new_exposed <- (S / N) * sum(beta0, E, beta0 * I0, beta1 * I1, beta2 * I2, beta3 * I3) 
  return(ceiling(new_exposed))
}

create_new_batch <- function(new_cases, incubation_period, critical_threshold, death_threshold, severe_threshold, mild_threshold,
                             ritical_duration, severe_duration, mild_duration,
                             asymptomatic_threshold, asymptomatic_duration) {
  random <- runif(new_cases)
  ind <- Individual$new(exposure_time = incubation_period,
                        critical = sum(random < critical_threshold),
                        critical_to_death = sum(random < death_threshold),
                        critical_to_recover = sum((random > death_threshold) & (random < critical_threshold)),
                        critical_time <- critical_duration,
                        severe = sum(random < severe_threshold),
                        severe_to_recover = sum((random < severe_threshold) & (random > critical_threshold)),
                        severe_time = severe_duration,
                        mild = sum(random < mild_threshold),
                        mild_to_recover <- sum((random < mild_threshold) & (random > severe_threshold)),
                        mild_time = mild_duration,
                        asymptomatic = sum((random > mild_threshold) & (random < asymptomatic_threshold)),
                        asymptomatic_time = asymptomatic_duration
  )
}


update_population <- function(t, exposed_individual, population_stat, new_cases) {
  population_stat[(t + 1):nrow(population_stat), 'S'] = population_stat[(t + 1):nrow(population_stat), 'S'] - new_cases
  population_stat[(t + 1):(t + incubation_period), 'E'] = population_stat[(t + 1):(t + incubation_period), 'E'] + new_cases
  population_stat[(t + 1 + incubation_period):(t + incubation_period + asymptomatic_duration), 'I0'] = 
    population_stat[(t + 1 + incubation_period):(t + incubation_period + asymptomatic_duration), 'I0'] + as.numeric(ind$asymptomatic)
  population_stat[(t + 1 + incubation_period):(t + incubation_period + mild_duration), 'I1'] = 
    population_stat[(t + 1 + incubation_period):(t + incubation_period + mild_duration), 'I1'] + as.numeric(ind$mild)
  population_stat[(t + 1 + incubation_period + mild_duration):(t + incubation_period + mild_duration + severe_duration), 'I2'] = 
    population_stat[(t + 1 + incubation_period + mild_duration):(t + incubation_period + mild_duration + severe_duration), 'I2'] + as.numeric(ind$severe)
  population_stat[(t + 1 + incubation_period + mild_duration + severe_duration):(t + incubation_period + mild_duration + severe_duration + critical_duration), 'I3'] = 
    population_stat[(t + 1 + incubation_period + mild_duration + severe_duration):(t + incubation_period + mild_duration + severe_duration + critical_duration), 'I3'] + as.numeric(ind$critical)
  # if (random > death_threshold) { # the individual survives
  # asymptotic recovery
  population_stat[(t + 1 + incubation_period + asymptomatic_duration):nrow(population_stat), 'R'] = 
    population_stat[(t + 1 + incubation_period + asymptomatic_duration), 'R'] + ind$asymptomatic
  # mild to recovery
  population_stat[(t + 1 + incubation_period + as.numeric(ind$asymptomatic) * asymptomatic_duration + as.numeric(ind$mild) * mild_duration + 
                     as.numeric(ind$severe) * severe_duration + as.numeric(ind$critical)*critical_duration):nrow(population_stat), 'R'] = 
    population_stat[(t + 1 + incubation_period + as.numeric(ind$asymptomatic) * asymptomatic_duration + as.numeric(ind$mild) * mild_duration + 
                       as.numeric(ind$severe) * severe_duration + as.numeric(ind$critical)*critical_duration):nrow(population_stat), 'R'] + new_cases - ind$critical_to_death
  # } else {
  population_stat[(t + 1 + incubation_period + mild_duration + severe_duration + critical_duration):nrow(population_stat), 'D'] = 
    population_stat[(t + 1 + incubation_period + mild_duration + severe_duration + critical_duration):nrow(population_stat), 'D'] + ind$critical_to_death
  # }
  return(population_stat)
}