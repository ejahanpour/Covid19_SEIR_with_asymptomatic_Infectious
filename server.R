#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("model/SEIR.R")
library(shiny)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    missouri_nyt <- read.csv('data/missouri_nyt.csv', stringsAsFactors = FALSE)
    missouri_population <- read.csv('data/missouri_population.csv', stringsAsFactors = FALSE)
    
    get_infected_counts <- reactive({ 
        # the demographic data has a County added to the name of the county
        counts <- missouri_nyt %>%
            filter(COUNTY == input$county) %>% 
            # filter(COUNTY == 'St. Louis city') %>%
        select(DATE, CASES)
        first_record <- counts %>%
            filter(DATE == min(DATE)) 
        list(counts, first_record[1, 'CASES'], first_record[1, 'DATE'])
    })
    
    get_population <- reactive({
        selected_county <- if_else(grepl('city', tolower(input$county)), input$county, paste(input$county, 'County'))
        N <- missouri_population %>%
            filter(COUNTY == selected_county) %>%
            # filter(COUNTY == 'Marion County') %>%
            select(TOTAL_POPULATION)
        N[1, 1]
        # county_population <- N[1, 1]
        
    })
   output$distPlot <- renderPlot({
        I <- SEIR_model(I1_0 = get_infected_counts()[[2]], N = get_population(), 
                        beta0 = input$beta0, beta1 = input$beta1, beta2 = input$beta2, beta3 = input$beta3, alpha = input$alpha, 
                        gamma1 = 0.0727, gamma2 = 0.1397, gamma3 = 0.0109341, 
                        exposure_time =  3, asymptomatic_to_recover = 6,
                        symptomatic_to_test = 3, test_to_hosp = 3, hosp_to_ICU = 6, ICU_time = 8,
                        sim_time = 60, time_0 = get_infected_counts()[[3]], time_from_infect_to_report = 10
        )
        # I <- I[1:30, ]
        observed_data <- get_infected_counts()[[1]]
        observed_data$DATE <- as.Date(observed_data$DATE)
        
        combined_cases <- merge(I, observed_data, by.x = 'day', by.y = 'DATE', all.x = TRUE)
        RMSE <- sqrt(sum((combined_cases$cases - combined_cases$CASES)^2, na.rm = TRUE))
        ggplot(data = combined_cases, aes(x = day, y = cases)) + 
            geom_line(aes(y = cases, colour = 'simulated')) + 
            geom_line(aes(y = CASES, color = 'observed')) +
            ggtitle(paste('Projected cases per day, RMSE:', round(RMSE, 2))) + 
            scale_colour_manual("", 
                                breaks = c("simulated", "observed"),
                                values = c("Steelblue", "black")) 
        })


})
