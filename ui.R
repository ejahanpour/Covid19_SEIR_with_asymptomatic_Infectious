#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source('global.R')
library(plotly)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Missouri Regional Covid-19 DESTEM"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput('region', 'Select Region', choices = c(sort(unique(regional_cases$LATEST_COUNTED_DHSS_REGION)), 'STATE')),
            uiOutput("countyControls"),
            conditionalPanel(condition = 'input.tabselected == 1',
                             checkboxInput("smoother", "Smooth Data", value = FALSE)),
            # conditionalPanel(condition = "input.tabselected==3", 
            #                  sliderInput("alpha", "percentage of asymptomatic infectious", min = 0, max = 1, value = 0.5)),
            # conditionalPanel(condition = "input.tabselected==3", 
            #                  sliderInput("beta0", "Beta 0", min = 0, max = 1, value = 0.7)),
            # conditionalPanel(condition = "input.tabselected==3", 
            #                  sliderInput("beta1", "Beta 1", min = 0, max = 1, value = 0.5)),
            # conditionalPanel(condition = "input.tabselected==3", 
            #                  sliderInput("beta2", "Beta 2", min = 0, max = 1, value = 0.3)),
            # conditionalPanel(condition = "input.tabselected==3", 
            #                  sliderInput("beta3", "Beta 3", min = 0, max = 1, value = 0.1)),
            conditionalPanel(condition = "input.tabselected==2",
                             sliderInput('sliding_window', 'slinging window for Re calculation (days)', min = 5, max = 15, value = 7, step = 1)),
            conditionalPanel(condition = "input.tabselected==2",
                             checkboxInput('uniform_prior', 'Consider a  Uniform Prior for Serial Intervals', value = FALSE)),
            conditionalPanel(condition = "input.tabselected==2 & input.uniform_prior==false",
                             textInput('mean_si', 'Select the mean value for Serial Interval', value = 4.7)),
            conditionalPanel(condition = "input.tabselected==2 & input.uniform_prior==false",
                             textInput('std_si', 'Select the standard deviation value for Serial Interval', value = 2.9))
            ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                id = 'tabselected',
                tabPanel("Confirmed Cases", value = 1, plotlyOutput ("distPlot")),
                tabPanel('Estimated Re', value = 2, plotlyOutput('re'), 
                         conditionalPanel(condition = 'input.region=="EASTERN"', 
                                          plotlyOutput('death_re'))), 
                tabPanel('SEIR projections', value = 3, plotlyOutput('seir'))
                )
            
        )
    )
))
