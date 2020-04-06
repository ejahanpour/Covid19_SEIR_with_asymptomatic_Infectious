#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
missouri_population <- read.csv('../data/missouri_population.csv')
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Estimation of Infectious cases"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput('county', 'Select County', choices = sort(unique(missouri_population$COUNTY))),
            sliderInput("beta0", "Beta 0", min = 0, max = 1, value = 0.4),
            sliderInput("beta1", "Beta 1", min = 0, max = 1, value = 0.3),
            sliderInput("beta2", "Beta 2", min = 0, max = 1, value = 0.2),
            sliderInput("beta3", "Beta 3", min = 0, max = 1, value = 0.1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
