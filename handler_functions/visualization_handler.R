plot_stats <- function(population_stat) {
  fig <- plot_ly(population_stat, x = ~Date, y = ~I1, type = 'scatter', mode = 'lines', name = 'Mild infection')
  fig %>%
    add_trace(y = ~I2, name = 'Severe infection') %>%
    add_trace(y = ~D, name = 'Recovered')
  return(fig)
}

plot_SEIR_predictions <- function() {
  #' this funciton reads in the outputs from the SEIR model and visualize is for Shiny app
  #' 
  #' 
  predicted_values <- read.csv('data/StLouis_model_output.csv', stringsAsFactors = FALSE)
  
  fig <- plot_ly(data = predicted_values, x = ~ONSET_DATE, y = ~ucl, type = 'scatter', mode = 'lines', name = 'UCL', 
                 line = list(color = 'transparent')) %>%
    add_trace(y = ~lcl, type = 'scatter', mode = 'lines',
              fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', name = 'LCL', line = list(color = 'transparent')) %>%
    add_trace(y = ~`median`, type = 'scatter', mode = 'lines', line = list(color = 'green'), name = 'SEIR_prediction') %>%
    add_trace(y = ~CASES, type = 'scatter', mode = 'lines', name = 'Observation', line = list(color = 'steelblue')) %>%
    layout(title = 'Covid-19 predicted cases for St Louis', 
           xaxis = list(title = 'day'),
           yaxis = list(title = 'counts'))
  return(fig)
  
}
