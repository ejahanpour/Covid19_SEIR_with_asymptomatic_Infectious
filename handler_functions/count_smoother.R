library(smoother)

smooth_observation <- function(dataframe) 
{
  # dataframe <- all_cases[all_cases$LATEST_COUNTED_DHSS_REGION == 'STATE', c('ONSET_DATE', 'CASES')]
  # dataframe <- regional_cases
  dataframe$ONSET_DATE <- as.Date(dataframe$ONSET_DATE)
  smoothed_dataframe <- dataframe %>%
    group_by(LATEST_COUNTED_DHSS_REGION, county) %>%
    arrange(ONSET_DATE) %>%
    tidyr::complete(ONSET_DATE = seq.Date(min(ONSET_DATE), max(ONSET_DATE), by = 'day'), fill = list(CASES = 0)) %>%
    mutate(new_cases = c(CASES[1], diff(CASES))) %>%
    mutate(new_cases_smooth = round(smoother::smth(new_cases, window = 6, tail = TRUE))) %>%
    # replace the last elements of smoothed cases (NAs with zeros or real values)
    # mutate(new_cases_smooth = c(new_cases_smooth[-((n()-3):n())], new_cases[((n()-3):n())])) %>%
    # remove the cases where one is identified between lots of zero proceeding elements
    # that seems to be more of a travel cases 
    # mutate(new_cases_smooth = if_else(is.na(new_cases_smooth), new_cases, new_cases_smooth))%>%
    # mutate(smoothed_cases = replace(new_cases_smooth, which(new_cases_smooth < 0), 0)) %>%
    # replace the first three elements of the smoothed column with those of the real case
    mutate(smoothed_cases = cumsum(c(new_cases[1:3], new_cases_smooth[4:n()]))) %>%
    slice((min(which(new_cases_smooth != 0))):(max(which(!is.na(new_cases_smooth))))) %>%
    ungroup() %>%
    select(LATEST_COUNTED_DHSS_REGION, ONSET_DATE, county, CASES, smoothed_cases) %>%
    # remove negatives to avoid errors in generating poisson distribution
    # the smoothing sometimes returns zero which causes issue with Poisson posterior and then sampling from the posterior
    mutate(smoothed_cases = if_else(smoothed_cases <= 0, 1, smoothed_cases))

  return(smoothed_dataframe)
  # ggplot(data = smoothed_dataframe, aes(x = ONSET_DATE)) +
  #   geom_line(aes(y = CASES), color = 'red', linetype = 'dashed') +
  #   geom_line(aes(y = smoothed_cases), color = 'steelblue')
}

