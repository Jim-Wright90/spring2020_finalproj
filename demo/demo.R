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
                     "Percentage of Minority Enrollment" = "fr_catmn",
                     "School Level" = "fr_lvel",
                     "School Urbanicity" = "fr_urban",
                     "Total Enrollment" = "fr_size"
                   ),
                   selected = "none")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("ggplot_dist")),
        tabPanel("Table Summary", reactableOutput("dist_smry"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  d_shiny <- d_shiny %>% 
    mutate(year = factor(year),
           fr_catmn = factor(fr_catmn),
           fr_urban = factor(fr_urban),
           fr_lvel = factor(fr_lvel),
           fr_size = factor(fr_size))
  
  output$ggplot_dist <- renderPlot({
    
    p <- ggplot(d_shiny, aes(`Percentage of College Bound Students`)) +
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
  
  output$dist_smry <- renderReactable({
    if(input$var == "none") {
      d_shiny %>%
        summarize(Mean = mean(`Percentage of College Bound Students`),
                  SD   = sd(`Percentage of College Bound Students`),
                  Min  = min(`Percentage of College Bound Students`),
                  Max  = max(`Percentage of College Bound Students`)) %>%
        mutate_if(is.numeric, round, 2) %>%
        reactable(rownames = FALSE)
    }
    else {
      d_shiny %>% 
        group_by(!!sym(input$var)) %>% 
        summarize(Mean = mean(`Percentage of College Bound Students`),
                  SD   = sd(`Percentage of College Bound Students`),
                  Min  = min(`Percentage of College Bound Students`),
                  Max  = max(`Percentage of College Bound Students`)) %>% 
        mutate_if(is.numeric, round, 2) %>% 
        reactable(rownames = FALSE)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
