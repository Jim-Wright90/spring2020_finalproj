library(reactable)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(DT)
library(tidyverse)

ui <- navbarPage(
    "School Crime and Safety Survey",
    navbarMenu(
        "Safety Survey Data by year",
    tabPanel("2006",
            fluidPage(
                plotOutput("plot06"),
                DTOutput("sf06")
        )
    ), 
    tabPanel("2008",
            fluidPage(
                plotOutput("plot08"),
                DTOutput("sf08")
        )
    ),
    tabPanel("2016",
            fluidPage(
                plotOutput("plot16"),
                DTOutput("sf16")
        )
    ),
    tabPanel("2018",
            fluidPage(
                plotOutput("plot18"),
                DTOutput("sf18")
        )
    )
)
)

server <- function(input, output, session){
        
    output$plot06 <- renderPlot({
        four_year %>% 
            filter(year == "2006") %>% 
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
            
        })
        
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
        
    output$plot08 <- renderPlot({
        four_year %>% 
            filter(year == "2008") %>% 
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
        
        output$plot16 <- renderPlot({
            four_year %>% 
                filter(year == "2016") %>% 
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
        
         
         output$plot18 <- renderPlot({
             four_year %>% 
                 filter(year == "2018") %>% 
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