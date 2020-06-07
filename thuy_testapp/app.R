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

theme_set(theme_minimal(15))

# We draw our analytic data from the four year School Survey on Crime and Safety datasets (05-06, 07-08, 15-16, 17-18), downloaded from the NCES website, https://nces.ed.gov/surveys/ssocs/. 
# We select variables of interest that are available across all four years. 
# They are urbanicity, school size, percentage of low-performing students, percentage of college-going students, and total numbers of a) incidents reported, b) incidents reported to police, and c) out-of-school suspensions.

# First, we import data frames and extract the variables of interest:

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
         size = fct_relevel(size, "<300", "300-499", "500-999", "1,000+"))


# next, we produce plots to be used in the app:

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
       y = "Average Number of Cases in School",
       fill = "Safety Indicators")

plot1 <- function(x){
  x %>%
    ggplot(aes(size, total))+
    geom_col(aes(fill = safety_indicators),
             position = "dodge")+
    facet_wrap(~urbanicity)+
    theme_minimal()+
    scale_fill_brewer(palette = "Set2")+
    labs(title = "School Safety, School Size, and Urbanicity",
         x = "School Size",
         y = "Total Number of Cases",
         fill = "Safety Indicators")
}

full_plot2 <- plot1(four_year)

plot1_by_year <- four_year %>% 
  group_by(year) %>%
  nest() %>% 
  mutate(plot = map2(data, year, ~plot1(.x)+
                       labs(subtitle = glue("(Surveyed School Year: {.y})"))))

plot06 <- plot1_by_year$plot[[1]]
plot08 <- plot1_by_year$plot[[2]]
plot16 <- plot1_by_year$plot[[3]]
plot18 <- plot1_by_year$plot[[4]]


# finally, we write the app:


ui <- navbarPage(
    "School Crime and Safety Survey (Data Source: https://nces.ed.gov/surveys/ssocs/)",
    
    tabPanel(
      "Four Year Overview",
      fluidPage(
        plotOutput("full_plot1"),
        plotOutput("full_plot2"),
        DTOutput("four_year")
      )
    ),
    tabPanel(
        "2005-2006",
        fluidPage(
          plotOutput("plot06"),
          DTOutput("sf06")
        )
    ), 
    tabPanel("2007-2008",
            fluidPage(
                plotOutput("plot08"),
                DTOutput("sf08")
        )
    ),
    tabPanel("2015-2016",
            fluidPage(
                plotOutput("plot16"),
                DTOutput("sf16")
        )
    ),
    tabPanel("2017-2018",
            fluidPage(
                plotOutput("plot18"),
                DTOutput("sf18")
        )
    )
)

# I write the server section this way due to the two reasons I personally felt useful:
# if we put all the code in here, every time we change our code we're risking messting up the huge amount of () and {}.
# and if we write functions to produce bunch of plots, we cannot put the function in the server section. this way we only need to specify the names of the plots created by the function

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
        
    }


shinyApp(ui = ui, server = server)