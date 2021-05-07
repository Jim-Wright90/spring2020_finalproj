library(shiny)
library(shinydashboard)
library(tidyverse)
library(here)
library(rio)
library(fs)
library(ggridges)
library(glue)
library(reactable)
library(DT)
library(shinythemes)
library(rsconnect)
theme_set(theme_minimal(15))

# We draw our analytic data from the four year School Survey on Crime and Safety datasets (05-06, 07-08, 15-16, 17-18), downloaded from the NCES website, https://nces.ed.gov/surveys/ssocs/. 
# We select variables of interest that are available across all four years. 
# They are urbanicity, school size, percentage of low-performing students, percentage of college-going students, and total numbers of a) incidents reported, b) incidents reported to police, and c) out-of-school suspensions.

# import data frames and extract the variables of interest:

sf06 <- import(here("data_1", "pu_ssocs06_spss.sav"),
               setclass = "tbl_df") %>%
  select(urbanicity = FR_LOC4,
         size = FR_SIZE,
         low_performing = C0532,
         college_going = C0534,
         incidents = INCID06,
         incidents_police = INCPOL06,
         suspension = OUTSUS06) %>% 
  mutate(year = "2005-2006")
sf08 <- import(here("data_1", "pu_ssocs08_spss.sav"),
               setclass = "tbl_df") %>% 
  select(urbanicity = FR_URBAN,
         size = FR_SIZE,
         low_performing = C0532,
         college_going = C0534,
         incidents = INCID08,
         incidents_police = INCPOL08,
         suspension = OUTSUS08)%>% 
  mutate(year = "2007-2008")
sf16 <- import(here("data_1", "pu_ssocs16.sas7bdat"),
               setclass = "tbl_df")%>% 
  select(urbanicity = FR_URBAN,
         size = FR_SIZE,
         low_performing = C0532,
         college_going = C0534,
         incidents = INCID16,
         incidents_police = INCPOL16,
         suspension = OUTSUS16)%>% 
  mutate(year = "2015-2016")
sf18 <- import(here("data_1", "pu_ssocs18.sav"),
               setclass = "tbl_df")%>% 
  select(urbanicity = FR_URBAN,
         size = FR_SIZE,
         low_performing = C0532,
         college_going = C0534,
         incidents = INCID18,
         incidents_police = INCPOL18,
         suspension = OUTSUS18)%>% 
  mutate(year = "2017-2018")

four_year <- bind_rows("05-06" = sf06, "07-08" = sf08, "15-16" = sf16, "17-18" = sf18, .id = "dataset") %>%
  pivot_longer(cols = c(incidents, incidents_police, suspension),
               names_to = "safety_indicators",
               values_to = "total") %>% 
  mutate(urbanicity = recode(urbanicity, '1' = "City", '2' = "Suburb", '3' = "Town", '4' = "Rural"),
         size = recode(size, '1' = "<300", '2' = "300-499", '3' = "500-999", '4' = "1,000+"),
         size = fct_relevel(size, "<300", "300-499", "500-999", "1,000+"),
         low_performing_1 = ifelse(low_performing >= 50, "Higher-performing", "Lower-performing"),
         college_going_1 = ifelse(college_going >= 50, "More College-bound", "Less College-bound"))

by_year <- four_year %>% 
  group_by(year) %>%
  nest()


#data import for college going tab

# plots to be used in the app:

full_plot1 <- four_year %>%
  group_by(year, safety_indicators) %>% 
  mutate(mean = mean(total)) %>% 
  ungroup() %>% 
  mutate(year = readr::parse_number(year)) %>% 
  ggplot(aes(year, mean))+
  geom_line(aes(color = safety_indicators), size = 0.9)+
  geom_point()+
  scale_x_continuous(limits = c(2004, 2018),
                     breaks = c(2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018))+
  labs(title = "School Safety from 2005 to 2018",
       x = "School Year",
       y = "Average Number of Cases in School") +
  scale_color_discrete(name = "Safety Indicators", 
                       labels = c("Incidents", "Incidents reported to police", "Suspension"))

plot1 <- function(x){
  x %>%
    ggplot(aes(size, total))+
    geom_col(aes(fill = safety_indicators),
             position = "dodge")+
    facet_wrap(~urbanicity)+
    theme_minimal()+
    labs(title = "School Safety, School Size, and Urbanicity",
         x = "School Size",
         y = "Total Number of Cases") +
        scale_fill_brewer(palette = "Set2",
                      name = "Safety Indicators", 
                      labels = c("Incidents", "Incidents reported to police", "Suspension"))
    
}

full_plot2 <- plot1(four_year)

plot1_by_year <- by_year %>% 
  mutate(plot = map2(data, year, ~plot1(.x)+
                       labs(subtitle = glue("(Surveyed School Year: {.y})"))))

plot06 <- plot1_by_year$plot[[1]]
plot08 <- plot1_by_year$plot[[2]]
plot16 <- plot1_by_year$plot[[3]]
plot18 <- plot1_by_year$plot[[4]]

fs::dir_create(here::here("thuy_testapp", "indicators"))


files <- plot1_by_year$year
paths <- here::here("thuy_testapp", "indicators", glue("{files}.png"))
paths

walk2(paths, plot1_by_year$plot, ggsave,
      width = 9.5, 
      height = 6.5,
      dpi = 500)


create_plot <- function(df, var, input) {
  p <- ggplot(df, aes({{var}})) +
    geom_histogram(bins = input$bins2,
                   fill = "cornflowerblue",
                   alpha = 0.7,
                   color = "white") +
    scale_x_continuous("Percentage of Students", labels = function(x) paste0(x, "%")) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(color = "gray80")) +
    labs(x = "Percentage of Students",
         y = "Number of Schools", 
         title = "Percentage of Students Scoring Below 15th Percentile on Standardized Assessment") 
  
  if(input$var2 != "none") {
    p <- p +
      facet_wrap(input$var2)
  }
  p
}

create_react <- function(df, var, input) {
  if(input$var2 == "none") {
    df %>% 
      summarize(Mean = mean({{var}}),
                SD = sd({{var}}),
                Min = min({{var}}),
                Max = max({{var}})) %>% 
      mutate_if(is.numeric, round, 2) %>% 
      reactable(rownames = FALSE)
  }
  else{
    df %>% 
      group_by(!!sym(input$var2)) %>% 
      summarize(Mean = mean({{var}}),
                SD = sd({{var}}),
                Min = min({{var}}),
                Max = max({{var}})) %>% 
      mutate_if(is.numeric, round, 2) %>% 
      reactable(rownames = FALSE)
  }
}



# the app:


ui <- navbarPage(
  theme = shinytheme("flatly"),
  "School Crime and Safety Survey (Data Source: https://nces.ed.gov/surveys/ssocs/)",
  
  tabPanel(
    "Four Year Overview",
    fluidPage(
      plotOutput("full_plot1"), plotOutput("full_plot2"), DTOutput("four_year")
    )
  ),
  tabPanel(
    "2005-2006 Dataset",
    fluidPage(
      plotOutput("plot06"), DTOutput("sf06")
    )
  ), 
  tabPanel(
    "2007-2008 Dataset",
    fluidPage(
      plotOutput("plot08"), DTOutput("sf08")
    )
  ),
  tabPanel(
    "2015-2016 Dataset",
    fluidPage(
      plotOutput("plot16"), DTOutput("sf16")
    )
  ),
  tabPanel(
    "2017-2018 Dataset",
    fluidPage(
      plotOutput("plot18"), DTOutput("sf18")
    )
  ),
  tabPanel(
    "School Characteristics and School Safety",
    fluidPage(
      sidebarPanel(
        radioButtons("var1",
                     "Factors correlated with school safety:",
                     choices = c(
                       "Urbanicity" = "urbanicity",
                       "School Size" = "size",
                       "Percentage of Low-Performing Students" = "low_performing_1",
                       "Percentage of College-Going Students" = "college_going_1"
                     ),
                     selected = "size")
      ),
      mainPanel(
        plotOutput("plots")
      )
    )
  ),
  tabPanel(
    "College Bound Students",
    fluidPage(
      sliderInput("bins2",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      radioButtons("var2",
                   "Facet distributions by:",
                   choices = c(
                     "None" = "none",
                     "Year" = "year",
                     "Urbanicity" = "urbanicity",
                     "School Size" = "size"
                   ),
                   selected = "none"),
      tabsetPanel(
        tabPanel("Plot", plotOutput("ggplot_dist")), 
        tabPanel("Table", reactableOutput("dist_smry"))
      )
    )
  ),
  tabPanel(
    "Low Performing Students",
    fluidPage(
      sliderInput("bins3",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      radioButtons("var3",
                   "Facet distributions by:",
                   choices = c(
                     "None" = "none",
                     "Year" = "dataset",
                     "Urbanicity" = "urbanicity",
                     "School Size" = "size"
                   ),
                   selected = "none"),
      tabsetPanel(
        tabPanel("Plot", plotOutput("low_perf")), 
        tabPanel("Table", reactableOutput("smry_low_perf"))
      )
    )
  )
)

server <- function(input, output, session){
  
  output$full_plot1 <- renderPlot({full_plot1})
  output$full_plot2 <- renderPlot({full_plot2})
  output$four_year <- renderDT({four_year})
  output$plot06 <- renderPlot({plot06})
  output$sf06 <- renderDT({sf06})
  output$plot08 <- renderPlot({plot08})
  output$sf08 <- renderDT({sf08})
  output$plot16 <- renderPlot({plot16})
  output$sf16 <- renderDT({sf16})
  output$plot18 <- renderPlot({plot18})
  output$sf18 <- renderDT({sf18})
  
  output$plots <- renderPlot({
    ggplot(four_year, aes(x = safety_indicators, y = total))+
      geom_col(color = "mediumseagreen", alpha = 0.7)+
      facet_wrap(input$var1)+
      labs(x = "School Safety Indicators",
           y = "Total Number of Cases",
           title = "School Characteristics Correlated with School Safety")
  })
  
  output$ggplot_dist <- renderPlot(create_plot(four_year, college_going, input))
  
  output$dist_smry <- renderReactable(create_react(four_year, college_going, input))
  
  output$low_perf <- renderPlot({
    
    p <- ggplot(four_year, aes(low_performing)) +
      geom_histogram(bins = input$bins3,
                     fill = "cornflowerblue",
                     alpha = 0.7,
                     color = "white") +
      scale_x_continuous("Percentage of Students", labels = function(x) paste0(x, "%")) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_line(color = "gray80")) +
      labs(x = "Percentage of Students",
           y = "Number of Schools", 
           title = "Percentage of Students Scoring Below 15th Percentile on Standardized Assessment") 
    
    if(input$var3 != "none") {
      p <- p +
        facet_wrap(input$var3)
    }
    p
  })
  
  output$smry_low_perf <- renderReactable({
    if(input$var3 == "none") {
      four_year %>%
        summarize(Mean = mean(low_performing),
                  SD   = sd(low_performing),
                  Min  = min(low_performing),
                  Max  = max(low_performing)) %>%
        mutate_if(is.numeric, round, 2) %>%
        reactable(rownames = FALSE)
    }
    else {
      four_year %>% 
        group_by(!!sym(input$var3)) %>% 
        summarize(Mean = mean(low_performing),
                  SD   = sd(low_performing),
                  Min  = min(low_performing),
                  Max  = max(low_performing)) %>% 
        mutate_if(is.numeric, round, 2) %>% 
        reactable(rownames = FALSE)
    }
  })
  
}


shinyApp(ui = ui, server = server)