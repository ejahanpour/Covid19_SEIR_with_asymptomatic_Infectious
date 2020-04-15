plot_stats <- function(population_stat) {
  fig <- plot_ly(population_stat, x = ~Date, y = ~I1, type = 'scatter', mode = 'lines', name = 'Mild infection')
  fig %>%
    add_trace(y = ~I2, name = 'Severe infection') %>%
    add_trace(y = ~D, name = 'Recovered')
  return(fig)
}
