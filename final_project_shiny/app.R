#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reactable)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(15))

# Define UI for application that draws a histogram
ui <- navbarPage(
  "School safety",
  
  fluidPage(

    # Application title
    titlePanel("School Crime and Safety Survey: 2006, 2008, 2016, 2018"),

    sidebarPanel(
      selectInput('xCol', 'X', names(batch)),
      selectInput('yCol', 'Y', names(batch))),
    
    # Shows the plot
    mainPanel(plotOutput('plot'))
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Get the data from the variables declared on the ui.R file
  df <- reactive({batch[, c(input$xCol, input$yCol)]})
  
  # Create the plot
  output$plot <- renderPlot({plot(df(), pch = 20, cex = 3, col = "blue",
                                  main = "2005-2006; 2007-2008; 2015-2016; 2017-2018 Survey Results")})

  }

# Run the application 
shinyApp(ui = ui, server = server)


