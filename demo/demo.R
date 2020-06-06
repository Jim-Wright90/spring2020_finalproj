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
ui <- fluidPage(
  
  # Application title
  titlePanel("School Crime and Safety Survey Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      radioButtons("var",
                   "Facet distributions by:",
                   choices = c(
                     "None" = "none",
                     "Year" = "year",
                     "Percentage of Minority Enrollment" = "Percentage of Minority Enrollment",
                     "School Level" = "School Level",
                     "School Urbanicity" = "School Urbanicity",
                     "Total Enrollment" = "Total Enrollment"
                   ),
                   selected = "none")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("ggplot_dist")),
        tabPanel("Table Summary", dataTableOutput("dist_smry"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  d <- d %>% 
    mutate(year = factor(year),
           "Percentage of Minority Enrollment" = factor("Percentage of Minority Enrollment"),
           "School Level" = factor("School Level"),
           "School Urbanicity" = factor("School Urbanicity"),
           "Total Enrollment" = factor("Total Enrollment"))
  
  output$ggplot_dist <- renderPlot({
    
    p <- ggplot(d, aes(`Percentage of College Bound Students`)) +
      geom_histogram(bins = input$bins,
                     fill = "cornflowerblue",
                     alpha = 0.7,
                     color = "gray40") 
    
    if(input$var != "none") {
      p <- p +
        facet_wrap(input$var)
    }
    p
  })
  
  output$dist_smry <- renderDataTable({
    if(input$var == "none") {
      d %>%
        summarize(Mean = mean(`Percentage of College Bound Students`),
                  SD   = sd(`Percentage of College Bound Students`),
                  Min  = min(`Percentage of College Bound Students`),
                  Max  = max(`Percentage of College Bound Students`)) %>%
        mutate_if(is.numeric, round, 2) %>%
        datatable(rownames = FALSE)
    }
    else {
      d %>% 
        group_by(!!sym(input$var)) %>% 
        summarize(Mean = mean(`Percentage of College Bound Students`),
                  SD   = sd(`Percentage of College Bound Students`),
                  Min  = min(`Percentage of College Bound Students`),
                  Max  = max(`Percentage of College Bound Students`)) %>% 
        mutate_if(is.numeric, round, 2) %>% 
        datatable(rownames = FALSE)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
