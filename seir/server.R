#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("SEIR.R")
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    missouri_nyt <- read.csv('../data/missouri_nyt.csv')
    missouri_population <- read.csv('../data/missouri_population.csv')
    
    get_infected_counts <- reactive({
        counts <- missouri_nyt %>%
            filter(paste(COUNTY, 'County') == input$county) %>% # the demographic data has a County added to the name of the county
            select(DATE, CASES)
        print(counts)
        counts
    })
    
    get_population <- reactive({
        county_cases <- missouri_nyt %>%
            filter(COUNTY == input$county)
        N <- missouri_population %>%
            filter(COUNTY == input$county) %>%
            # filter(COUNTY == 'Marion County') %>%
            select(TOTAL_POPULATION)
        N[1, 1]
        # county_population <- N[1, 1]
        
    })
   output$distPlot <- renderPlot({

        I <- SEIR_model(I1_0 = 2, N = get_population(), 
                        beta0 = input$beta0, beta1 = input$beta1, beta2 = input$beta2, beta3 = input$beta3, alpha = 0.5, 
                        gamma1 = 0.0727, gamma2 = 0.1397, gamma3 = 0.0109341, 
                        exposure_time =  3, asymptomatic_to_recover = 6,
                        symptomatic_to_test = 3, test_to_hosp = 3, hosp_to_ICU = 6, ICU_time = 8,
                        sim_time = 60, time_0 = '03/23/2020', time_from_infect_to_report = 10
        )
        # I <- I[1:30, ]
        observed_data <- get_infected_counts()
        observed_data$DATE <- as.Date(observed_data$DATE)
        
        combined_cases <- merge(I, observed_data, by.x = 'day', by.y = 'DATE', all.x = TRUE)
        ggplot(data = combined_cases, aes(x = day, y = cases)) + 
            geom_line(aes(y = cases, colour = 'simulated')) + 
            geom_line(aes(y = CASES, color = 'observed')) +
            ggtitle('Projected cases per day') + 
            scale_colour_manual("", 
                                breaks = c("simulated", "observed"),
                                values = c("Steelblue", "black")) 
        })

})
