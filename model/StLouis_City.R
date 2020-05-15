source('model/New_stochastic_SEIR.R')
source('handler_functions/Re_calculation.R')
require(foreach)
require(doParallel)

observed_data <- read.csv('data/MDHSS_region_cases.csv', stringsAsFactors = FALSE) %>%
  filter(county == 'St louis') %>%
  arrange(ONSET_DATE) 

# sim_time = nrow(observed_data)
sim_time <- 130
first_date <- observed_data$ONSET_DATE[1]
first_day_count <- observed_data$CASES[1]
Population <- 302838   # Wikipedia

observed_data <- observed_data %>%
  mutate(ONSET_DATE = as.Date(ONSET_DATE)) %>%
  tidyr::complete(ONSET_DATE = seq.Date(min(ONSET_DATE), max(ONSET_DATE), by = 'day'), fill = list(CASES = 0)) %>%
  mutate(smoothed_cases = CASES) # for Re calculation

Re_df <- calculate_Re_From_SI(dataframe = observed_data, mean_si = 4.7, std_si = 2.9, 
                              region = 'EASTERN', county = 'St louis', return_df = TRUE)

# parallel set up
cl <- parallel::detectCores() - 2
# cl <- makeCluster(4)
registerDoParallel(cl)
sim_results <- list()

sim_results <- foreach(i = 1:20) %dopar%
  Stochastic_SEIR(N = Population, first_case_date = first_date, first_case_count = first_day_count, 
                  simulation_time = sim_time, Re_df =  Re_df)


# sim_results <- replicate(10, Stochastic_SEIR(N = Population, first_case_date = first_date, first_case_count = first_day_count, simulation_time = 20))

simulated_infections <- data.frame(do.call(cbind, lapply(sim_results, function(x) x$I0 + x$I1 + x$I2 + x$I3)))
simulated_infections$median <- apply(simulated_infections, 1, median, na.rm = T)
simulated_infections$lcl <- apply(simulated_infections, 1, function(x) quantile(x[x>=1], probs=.05, na.rm = T))
simulated_infections$ucl <- apply(simulated_infections, 1, function(x) quantile(x[x>=1], probs=.95, na.rm = T))
simulated_infections$ONSET_DATE <- as.character(seq(as.Date(first_date), 
                                       as.Date(first_date) + sim_time - 1, "days"))

observed_data$ONSET_DATE <- as.character(observed_data$ONSET_DATE)
observed_data <- merge(simulated_infections[c('median', 'lcl', 'ucl', 'ONSET_DATE')], observed_data, by = 'ONSET_DATE', all.x = TRUE) %>%
  dplyr::mutate_all(~replace(., is.na(.), 0)) 


ggplot(data = observed_data, aes(x = ONSET_DATE, group = 1)) +
  geom_line(aes(y = CASES, colour = 'steelblue')) +
  geom_line(aes(y = median, colour = 'black')) + 
  geom_ribbon( aes(ymin=lcl,ymax=ucl), fill="gray", alpha="0.5") + 
  scale_color_discrete(name = "Data", labels = c("observed", "estimated"))



test <- Stochastic_SEIR(N = Population, first_case_date = first_date, first_case_count = first_day_count, 
                        simulation_time = sim_time, Re_df = Re_df)
test <- test[1:sim_time,]
test$estimated_infected <- test$I0 + test$I1 + test$I2 + test$I3
test <- merge(test, observed_data, by.x = 'Date', by.y = 'ONSET_DATE', all.x = TRUE)
test$observed = test$CASES
ggplot(data = test, aes(x = Date)) +
  geom_line(aes(y = observed, color = 'steelblue')) +
  geom_line(aes(y = estimated_infected, color = 'red')) +
  scale_color_discrete(name = "Data", labels = c("observed", "estimated"))
