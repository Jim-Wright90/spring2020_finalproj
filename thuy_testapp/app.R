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

by_year <- four_year %>% 
  group_by(year) %>%
  nest()

ui <- navbarPage(
    "School Crime and Safety Survey (Data Source: https://nces.ed.gov/surveys/ssocs/)",
    
    tabPanel(
      "Four Year Overview",
      fluidPage(
        DTOutput("four_year")
      )
    ),
    tabPanel(
        "2005-2006",
        fluidPage(
            DTOutput("sf06")
        )
    ), 
    tabPanel(
        "2007-2008",
        fluidPage(
            DTOutput("sf08")
        )
    ),
    tabPanel(
        "2015-2016",
        fluidPage(
            DTOutput("sf16")
        )
    ),
    tabPanel(
        "2017-2018",
        fluidPage(
            DTOutput("sf18")
        )
    ),
    
    collapsible = TRUE
)

server <- function(input, output, session){
        
        output$four_year <- renderDT({
            four_year
        })
        
        output$sf06 <- renderDT({
          sf06
        })
        
        output$sf08 <- renderDT({
            sf08
        })
        
        output$sf16 <- renderDT({
            sf16
        })
        
        output$sf18 <- renderDT({
            sf18
        })
        
    }


shinyApp(ui = ui, server = server)