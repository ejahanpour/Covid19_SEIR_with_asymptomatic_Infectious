source('model/New_stochastic_SEIR.R')
# library(doSNOW)
require(foreach)
require(doParallel)

observed_data <- read.csv('data/MDHSS_region_cases.csv', stringsAsFactors = FALSE) %>%
  filter(county == 'St louis') %>%
  arrange(ONSET_DATE)

sim_time = nrow(observed_data)
first_date <- observed_data$ONSET_DATE[1]
first_day_count <- observed_data$CASES[1]
Population <- 302838   # Wikipedia

# sim_results <- replicate(10, Stochastic_SEIR(N = Population, first_case_date = first_date, first_case_count = first_day_count, simulation_time = 20))
# parallel set up
cl <- makeCluster(4)
registerDoParallel(cl)
sim_results <- list()
sim_results <- foreach(i = 1:30) %dopar%
  Stochastic_SEIR(N = Population, first_case_date = first_date, first_case_count = first_day_count, simulation_time = sim_time)

test <- do.call(cbind, lapply(sim_results, function(x) x$I0 + x$I1 + x$I2 + x$I3))
