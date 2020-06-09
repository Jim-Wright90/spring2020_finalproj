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

files <- dir_ls(here::here("data_1"), glob = "*.csv")

batch <- map_df(files, read_csv)

numeric_plot <- function(data, x, y){
  
  ggplot(data = {{data}}, aes(x = {{x}}, y = {{y}})) +
    geom_point(alpha = .4, color = 'gray70') +
    geom_smooth(method = 'lm', aes(color = year), formula = {{y}} ~ {{x}})
}

numeric_plot(batch, c0534, c0536) 

# Define UI for application that draws a histogram
ui <- navbarPage(
  "School safety",
  
  fluidPage(

    # Application title
    titlePanel("School Crime and Safety Survey: 2006, 2008, 2016, 2018"),

    sidebarPanel(
      selectInput(inputId = 'x', label = 'X', choices = names(batch)),
      selectInput(inputId = 'y', label = 'Y', choices = names(batch))),
    
    # Shows the plot
    mainPanel(plotOutput('plot'))
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$plot <- renderPlot(numeric_plot(batch, input$x, input$y))
}


# Run the application 
shinyApp(ui = ui, server = server)


