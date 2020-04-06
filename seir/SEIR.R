library(ggplot2)


SEIR_model <- function(I1_0 = 2, N = 10000, 
                       beta0 = 0.8, beta1 = 0.6, beta2 = 0.1, beta3 = 0.1, alpha = 0.8, 
                       gamma1 = 0.0727, gamma2 = 0.1397, gamma3 = 0.0109341, 
                       exposure_time =  3, asymptomatic_to_recover = 6,
                       symptomatic_to_test = 3, test_to_hosp = 3, hosp_to_ICU = 6, ICU_time = 8,
                       sim_time = 60, time_0 = '03/23/2020', time_from_infect_to_report = 10
                       ) {
  
  # I1_0 = 2; N = 10000; beta0 = 0.8; beta1 = 0.6; beta2 = 0.1; beta3 = 0.1; alpha = 0.8;
  # gamma1 = 0.0727; gamma2 = 0.1397; gamma3 = 0.0109341;
  # exposure_time =  3; asymptomatic_to_recover = 6;
  # symptomatic_to_test = 3; test_to_hosp = 3; hosp_to_ICU = 6; ICU_time = 8;
  # sim_time = 60; time_0 = '03/23/2020'; time_from_infect_to_report = 10
  ###' SEIR model gets the infected data at time 0 and model the infetion spread over time (projected)
  ###' I1_0: <int> number of infected patient at time 0
  ###' N: <int> population of the community
  ###' beta: <float> rate of spread which represents the probability of transmitting disease between a susceptible and an exposed individual
  ###' alpha: <float> incubation rate: rate of latent individuals becoming infectious (symptomatic + asmpromatic). average duration of incubation = 1/alpha
  ###' gamma1: <float> recovery rate from mild  infected individuals
  ###' rho1: <float> rate of (mild & asymptomatic) infected individuals to hospitatlization
  ###' gamma2: <float> recovery rate from hospitalized cases
  ###' rho2: <float> rate of hospitalized cases ending up in ICU
  ###' gamma3: <float> recovery rate from ICU cases
  ###' mu: <float> mortality rate of ICU cases
  ###' time_from_infect_to_report: <int> time it takes from being infected to being confirmed
  # rho0 <- 1 - gamma0
  rho1 <- 1 - gamma1 
  rho2 <- 1 - gamma2
  rho3 <- 1 - gamma3
  mu <- 1 - gamma3
  update_sim_time <- sim_time + exposure_time + symptomatic_to_test
  S <- E <- I0 <- I1 <- I2 <- I3 <- R <- D <- rep(0, update_sim_time)  
  mild_to_recover <- symptomatic_to_test + test_to_hosp + hosp_to_ICU 
  hosp_to_recover <- hosp_to_ICU
  # estimate the exposed and susceptible at time 0 
  I1[exposure_time + symptomatic_to_test] <- I1_0
  E[1] <- I1_0 / (1 - alpha)
  # I0[exposure_time:(exposure_time + symptomatic_to_test - 1)] <- I0[exposure_time] / (exposure_time * alpha)
  S[1] <- N - E[1]  # number of susceptible at time 0
  # for (t in 1:(update_sim_time - 1))
  # # for (t in 1:30)
  # {
  #   S[t + 1] = S[t] - S[t] * sum(beta0 * I0[t], beta1 * I1[t], beta2 * I2[t], beta3 * I3[t]) / N - R[t] - D[t]
  #   exposure_to_asymp <- ifelse(t - exposure_time < 1, 0, E[t - exposure_time])
  #   E[t + 1] = E[t] - alpha * exposure_to_asymp +  S[t] * sum(beta0 * I0[t], beta1 * I1[t], beta2 * I2[t], beta3 * I3[t]) / N
  #   asymp_to_rec <- ifelse(t - asymptomatic_to_recover < 1, 0, I0[t - asymptomatic_to_recover])
  #   I0[t + 1] = I0[t] + alpha * E[t] -  asymp_to_rec 
  #   asymps <- ifelse(t - mild_to_recover < 1, 0, I1[t - mild_to_recover])
  #   symps <- ifelse(t - symptomatic_to_test < 1, 0, I1[t - symptomatic_to_test])
  #   I1[t + 1] = I1[t] + (1 - alpha) * E[t] - rho1 * asymps - gamma1 * symps
  #   hosp_rec <- ifelse(t - hosp_to_recover < 1, 0, I2[t - hosp_to_recover])
  #   hosp_icu <- ifelse(t - hosp_to_ICU < 1, 0, I2[t - hosp_to_ICU])
  #   I2[t + 1] = I2[t] + gamma1 * symps - rho2 * hosp_rec  - gamma2 * hosp_icu
  #   icu_rec <- ifelse(t - ICU_time < 1, 0, I3[t - ICU_time])
  #   I3[t + 1] = I3[t] + gamma2 * hosp_icu  - rho3 * icu_rec - gamma3 * icu_rec
  #   # print(c(E[t + 1], I0[t + 1], I1[t + 1], I2[t +1], I3[t +1]))
  #   R[t + 1] = asymp_to_rec + rho1 * asymps + rho2 * hosp_rec + rho3 * icu_rec
  #   D[t + 1] = gamma3 * icu_rec
  # }
  
  for (t in 1:(update_sim_time - 1))
    # for (t in 1:30)
  {
    S[t + 1] = S[t] - S[t] * sum(beta0 * I0[t], beta1 * I1[t], beta2 * I2[t], beta3 * I3[t]) / N - min(N, R[t] + D[t])
    E[t:(t + exposure_time)] = E[t:(t + exposure_time)] + alpha * exposure_to_asymp +  S[t] * sum(beta0 * I0[t], beta1 * I1[t], beta2 * I2[t], beta3 * I3[t]) / N
    I0[(t + exposure_time):(t + exposure_time + asymptomatic_to_recover)] =+ E[t] * alpha 
    I1[(t + exposure_time):(t + exposure_time + symptomatic_to_test + test_to_hosp)] =+ E[t] * (1 - alpha)
    I2[(t + exposure_time + symptomatic_to_test + test_to_hosp):(t + exposure_time + symptomatic_to_test + test_to_hosp + hosp_to_ICU)] =+ 
      gamma1 * I1[t + exposure_time + symptomatic_to_test + test_to_hosp ]
    I3[(t + exposure_time + symptomatic_to_test + test_to_hosp + hosp_to_ICU):(t + exposure_time + symptomatic_to_test + test_to_hosp + hosp_to_ICU + ICU_time)] =+ 
      gamma2 * I2[t + exposure_time + symptomatic_to_test + test_to_hosp] 
    R[(t + exposure_time + asymptomatic_to_recover):length(R)] = R[(t + exposure_time + asymptomatic_to_recover):length(R)] +
      I0[t + exposure_time]
    R[(t + exposure_time + mild_to_recover):length(R)] = R[(t + exposure_time + mild_to_recover):length(R)] + 
      rho1 * I1[t + exposure_time]
    R[(t + exposure_time + symptomatic_to_test + test_to_hosp + hosp_to_ICU):length(R)] = 
      R[(t + exposure_time + symptomatic_to_test + test_to_hosp + hosp_to_ICU):length(R)] + rho2 * I2[t + exposure_time + symptomatic_to_test + test_to_hosp]
    R[(t + exposure_time + symptomatic_to_test + test_to_hosp + hosp_to_ICU + ICU_time):length(R)] =
      R[(t + exposure_time + symptomatic_to_test + test_to_hosp + hosp_to_ICU + ICU_time):length(R)] + rho3 * I3[t + exposure_time + symptomatic_to_test + test_to_hosp + hosp_to_ICU]
    D[(t + exposure_time + symptomatic_to_test + test_to_hosp + hosp_to_ICU + ICU_time):length(R)] =
      D[(t + exposure_time + symptomatic_to_test + test_to_hosp + hosp_to_ICU + ICU_time):length(R)] + gamma3 * I3[t + exposure_time + symptomatic_to_test + test_to_hosp + hosp_to_ICU]
  }
  
  date_range <- seq(as.Date(time_0, format = '%m/%d/%Y') - (exposure_time + symptomatic_to_test - 1), 
                    as.Date(time_0, format = '%m/%d/%Y') + sim_time, "days")
  cases = I1[1:length(date_range)] + I2[1:length(date_range)] + I3[1:length(date_range)]
  df <- data.frame(day = date_range, cases = cases)
  return(df)
  
}

test <- SEIR_model(I1_0 = 2, N = 10000, 
                               beta0 = 0.4, beta1 = 0.3, beta2 = 0.2, beta3 = 0.1, alpha = 0.8, 
                               gamma1 = 0.0727, gamma2 = 0.1397, gamma3 = 0.0109341, 
                               exposure_time =  3, asymptomatic_to_recover = 6,
                               symptomatic_to_test = 3, test_to_hosp = 3, hosp_to_ICU = 6, ICU_time = 8,
                               sim_time = 60, time_0 = '03/23/2020', time_from_infect_to_report = 10
)

test$row <- 1:length(test$cases)
counts <- missouri_nyt %>%
  filter(COUNTY == 'Adair') %>%
  select(DATE, CASES)
counts$DATE <- as.Date(counts$DATE)
test <- merge(test, counts, by.x = 'day', by.y = 'DATE', all.x = TRUE)
ggplot(data = test, aes(x = day)) + 
  geom_line(aes(y = cases, colour = 'Simulated')) + 
  geom_line(aes(y = CASES, colour = 'Observed')) +
  ggtitle('Projected cases per day') +
  scale_colour_manual("", 
                      breaks = c("Simulated", "Observed"),
                      values = c("Steelblue", "black")) 
  
