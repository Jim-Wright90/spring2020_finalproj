library(reactable)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(DT)
library(tidyverse)

ui <- navbarPage(
    "School Crime and Safety Survey: 2006, 2008, 2016, 2018",
    
    tabPanel(
        "Safety Survey 2006",
        fluidPage(
            DTOutput("sf06")
        )
    ), 
    tabPanel(
        "Safety Survey 2008",
        fluidPage(
            DTOutput("sf08")
        )
    ),
    tabPanel(
        "Safety Survey 2016",
        fluidPage(
            DTOutput("sf16")
        )
    ),
    tabPanel(
        "Safety Survey 2018",
        fluidPage(
            DTOutput("sf18")
        )
    ),
    
    collapsible = TRUE
)

server <- function(input, output, session){
        
        output$sf06 <- renderDT({
            import(here("data_1", "pu_ssocs06_spss.sav"),
                   setclass = "tbl_df") %>%
                select(urbanicity = FR_LOC4,
                       size = FR_SIZE,
                       low_performing = C0532,
                       college_going = C0534,
                       incidents = INCID06,
                       incidents_police = INCPOL06,
                       suspension = OUTSUS06) %>% 
                mutate(year = "2006")
            
        })
        
        output$sf08 <- renderDT({
            import(here("data_1", "pu_ssocs08_spss.sav"),
                   setclass = "tbl_df") %>% 
                select(urbanicity = FR_URBAN,
                       size = FR_SIZE,
                       low_performing = C0532,
                       college_going = C0534,
                       incidents = INCID08,
                       incidents_police = INCPOL08,
                       suspension = OUTSUS08)%>% 
                mutate(year = "2008")
        })
        
        output$sf16 <- renderDT({
            import(here("data_1", "pu_ssocs16.sas7bdat"),
                   setclass = "tbl_df")%>% 
                select(urbanicity = FR_URBAN,
                       size = FR_SIZE,
                       low_performing = C0532,
                       college_going = C0534,
                       incidents = INCID16,
                       incidents_police = INCPOL16,
                       suspension = OUTSUS16)%>% 
                mutate(year = "2016")
        })
        
        output$sf18 <- renderDT({
            import(here("data_1", "pu_ssocs18.sav"),
                   setclass = "tbl_df")%>% 
                select(urbanicity = FR_URBAN,
                       size = FR_SIZE,
                       low_performing = C0532,
                       college_going = C0534,
                       incidents = INCID18,
                       incidents_police = INCPOL18,
                       suspension = OUTSUS18)%>% 
                mutate(year = "2018")
        })
    }


shinyApp(ui = ui, server = server)