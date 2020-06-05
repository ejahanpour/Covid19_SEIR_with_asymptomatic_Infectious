#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# source("model/SEIR.R")
source("model/Stochastic_SEIR.R")
source("handler_functions/visualization_handler.R")
source("handler_functions/Re_calculation.R")
source("handler_functions/count_smoother.R")
source("handler_functions/death_related_Re.R")
library(shiny)
library(dplyr)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   create_region_stats <- reactive({ 
      region_hosp <- all_hosp %>%
         filter((LATEST_COUNTED_DHSS_REGION == input$region) & (county == input$counties)) %>%
         mutate(ONSET_DATE = as.Date(ONSET_DATE)) %>%
         rename(hospitalized = CASES, smoothed_hospitalized = smoothed_cases) %>%
         select(-LATEST_COUNTED_DHSS_REGION)
      region_death <- all_death %>%
         filter((LATEST_COUNTED_DHSS_REGION == input$region) & (county == input$counties)) %>%
         rename(death = CASES, smoothed_death = smoothed_cases) %>%
         select(-LATEST_COUNTED_DHSS_REGION)
      # merge and create the main plot for the data
      region_data <- all_cases %>%
         filter((LATEST_COUNTED_DHSS_REGION == input$region) & (county == input$counties)) %>%
         merge(region_hosp, by = c('ONSET_DATE', 'county'), all.x = TRUE) %>%
         merge(region_death, by = c('ONSET_DATE', 'county'), all.x = TRUE) %>%
         mutate(hospitalized = ifelse(is.na(smoothed_hospitalized), 0, smoothed_hospitalized),
                death = ifelse(is.na(smoothed_death), 0 , smoothed_death)) 
      # region_data <- head(region_data, -1)
      })
   
   reported_death <- read.csv('data/MDHSS_reported_death.csv')
   
   output$countyControls <- renderUI({
      counties <- unique(all_cases[all_cases$LATEST_COUNTED_DHSS_REGION == input$region, ]$county)
      selectInput("counties", "Choose County", choices = counties, selected = 'All')
   })
   
   output$distPlot <- renderPlotly({
      if(input$smoother == FALSE) {
         fig <- plot_ly(create_region_stats(), x = ~ONSET_DATE, y = ~CASES, type = 'scatter', mode = 'lines', name = 'confirmed cases', 
                        hovertemplate = '%{x} confirmed cases: %{y}') %>%
            add_trace(y = ~ hospitalized, name = 'hospitalized cases', hovertemplate = 'hospitalized cases: %{y}') %>%
            add_trace(y = ~ death, name = 'death cases'
                      # , hovertemplate = 'death number: %{y}'
            ) %>%
            layout(title = 'Confirmed, hospitalized and death cases per onset date',
                   xaxis = list(title = 'Onset Date'), 
                   yaxis = list(title = 'Counts'))
      } else {
         fig <- plot_ly(create_region_stats(), x = ~ONSET_DATE, y = ~smoothed_cases, type = 'scatter', mode = 'lines', name = 'confirmed cases', 
                        hovertemplate = '%{x} confirmed cases: %{y}') %>%
            add_trace(y = ~ smoothed_hospitalized, name = 'hospitalized cases', hovertemplate = 'hospitalized cases: %{y}') %>%
            add_trace(y = ~ smoothed_death, name = 'death cases'
                      # , hovertemplate = 'death number: %{y}'
            ) %>%
            layout(title = 'Confirmed, hospitalized and death cases per onset date',
                   xaxis = list(title = 'Onset Date'), 
                   yaxis = list(title = 'Counts'))
         
      }
       fig
       
   })
   
   output$re <- renderPlotly({
      
      if (input$uniform_prior == FALSE) {
         
         calculate_Re_From_SI(dataframe = create_region_stats(), mean_si = as.numeric(input$mean_si), std_si = as.numeric(input$std_si), 
                              region = input$region, county = input$counties, sliding_window = input$sliding_window)
      } else {
         region_posterior <- posterior_Re[(posterior_Re$LATEST_COUNTED_DHSS_REGION == input$region) & 
                                          (posterior_Re$county == input$counties), ]
         region_posterior$all_one <- 1
         fig <- plot_ly(data = region_posterior, x = ~day, y = ~r_e_high, type = 'scatter', mode = 'lines', name = 'UCL', 
                        line = list(color = 'transparent')) %>%
            add_trace(y = ~r_e_low, type = 'scatter', mode = 'lines',
                      fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', name = 'LCL', line = list(color = 'transparent')) %>%
            add_trace(y = ~r_e_most_likely, type = 'scatter', mode = 'lines', line = list(color = 'black'), name = 'Re') %>%
            add_trace(y = ~all_one, type = 'scatter', mode = 'lines', name = 'unit line', line = list(color = 'red', dash = 'dash')) %>%
            layout(title = paste0('Covid-19 effective reproductive number (Re), ', input$counties, ', ', tolower(input$region), ' district, Missouri. 2020'), 
                   xaxis = list(title = 'day'),
                   yaxis = list(title = 'effective reproducion number'))
      } 
      
   })
   
   output$death_re <- renderPlotly({
      calculate_Re_from_death(death_data = reported_death, mean_si = as.numeric(input$mean_si),
                              std_si = as.numeric(input$std_si), sliding_window = input$sliding_window)
   })
   
   
   output$seir <- renderPrint({
      "coming Soon..."
   })

})
