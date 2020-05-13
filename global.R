library(dplyr)
source('handler_functions/count_smoother.R')
source('handler_functions/Re_calculation.R')

regional_cases <- read.csv('data/MDHSS_region_cases.csv', stringsAsFactors = FALSE)
regional_hosp <- read.csv('data/MDHSS_region_hosp.csv', stringsAsFactors = FALSE)
regional_death <- read.csv('data/MDHSS_region_death.csv', stringsAsFactors = FALSE)

all_cases <- regional_cases %>%
  filter(county == 'All') %>%
  group_by(ONSET_DATE) %>%
  mutate(CASES = sum(CASES), 
         LATEST_COUNTED_DHSS_REGION = 'STATE', 
         county = 'All') %>%
  # select(c(ONSET_DATE, CASES)) %>%
  ungroup() %>%
  unique() %>%
  rbind(regional_cases) %>%
  smooth_observation()

all_hosp <- regional_hosp %>%
  group_by(ONSET_DATE) %>%
  mutate(CASES = sum(CASES), 
         LATEST_COUNTED_DHSS_REGION = 'STATE') %>%
  # select(c(ONSET_DATE, CASES)) %>%
  ungroup() %>%
  unique() %>%
  rbind(regional_hosp) %>%
  # smooth_observation()
  mutate(smoothed_cases = CASES)

all_death <- regional_death %>%
  group_by(ONSET_DATE) %>%
  mutate(CASES = sum(CASES), 
         LATEST_COUNTED_DHSS_REGION = 'STATE') %>%
  # select(c(ONSET_DATE, CASES)) %>%
  ungroup() %>%
  unique() %>%
  rbind(regional_death) %>%
  # no need to smooth the death date
  mutate(smoothed_cases = CASES)


posterior_Re <- all_cases %>%
  mutate(LATEST_COUNTED_DHSS_REGION = paste(LATEST_COUNTED_DHSS_REGION, county, sep = '|')) %>%
  split(.$LATEST_COUNTED_DHSS_REGION) %>%
  map(calculate_Re_with_uniform_prior) %>%
  bind_rows() %>%
  separate(col= Region, into = c("LATEST_COUNTED_DHSS_REGION", "county"), sep = '\\|', extra = "merge")
