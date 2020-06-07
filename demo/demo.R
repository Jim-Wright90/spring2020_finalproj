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
library(tidyverse)
library(rio)
library(here)


safe06 <- import(here("spss_files", "2006_school_safety.sav"),
                 setclass = "tbl_df") %>% 
  characterize() %>% 
  janitor::clean_names() 

safe08 <- import(here('spss_files', '2008_school_safety.sav'),
                 setclass = 'tbl_df') %>% 
  characterize() %>% 
  janitor::clean_names()

safe16 <- import(here("spss_files", "pu_ssocs16.sas7bdat"),
                 setclass = 'tbl_df') %>% 
  characterize() %>% 
  janitor::clean_names() %>% 
  mutate(fr_ubran = as.factor(fr_urban),
         fr_size = as.factor(fr_size),
         year = as.factor(c0578_yy))

safe18 <- import(here('spss_files', '2018_school_safety.sav'),
                 setclass = 'tbl_df') %>% 
  characterize() %>% 
  janitor::clean_names()


all_safe <- bind_rows("2006" = safe06, 
                      "2008" = safe08, 
                      #"15-16" = safe16,
                      "2018" = safe18, 
                      .id = "year")

head(all_safe)

d <- all_safe %>% 
  select(year, schid, c0508, c0510, c0514, outsus06, outsus08, outsus18, c0534, c0568, fr_catmn, fr_lvel, fr_urban, fr_size)

d <- d %>% 
  rename(`Total Insubordinations` = c0508,
         `Total Removals for Insubordination` = c0510,
         `Total Suspensions for Insubordination` = c0514,
         `Total Suspensions 2006` = outsus06,
         `Total Suspensions 2008` = outsus08,
         `Total Suspensions 2018` = outsus18,
         `Percentage of College Bound Students` = c0534,
         `Average Percentage Daily Attendance` = c0568,
         `Percentage of Minority Enrollment` = fr_catmn,
         `School Level` = fr_lvel,
         `School Urbanicity` = fr_urban,
         `Total Enrollment` = fr_size) 


head(d)

d_shiny <- all_safe %>% 
  select(year, schid, c0508, c0510, c0514, outsus06, outsus08, outsus18, c0534, c0568, fr_catmn, fr_lvel, fr_urban, fr_size) %>% 
  rename(`Percentage of College Bound Students` = c0534)

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
           fr_catmn = forcats::fct_explicit_na(fr_catmn),
           fr_urban = forcats::fct_explicit_na(fr_urban),
           fr_lvel = forcats::fct_explicit_na(fr_lvel),
           fr_size = forcats::fct_explicit_na(fr_size))
  
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
