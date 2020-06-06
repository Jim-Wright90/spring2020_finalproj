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


sf06 <- import(here("data_1", "pu_ssocs06_spss.sav"),
               setclass = "tbl_df") %>%
  select(urbanicity = FR_LOC4,
         size = FR_SIZE,
         low_performing = C0532,
         college_going = C0534,
         incidents = INCID06,
         incidents_police = INCPOL06,
         suspension = OUTSUS06) %>% 
  mutate(year = "2006")
sf08 <- import(here("data_1", "pu_ssocs08_spss.sav"),
               setclass = "tbl_df") %>% 
  select(urbanicity = FR_URBAN,
         size = FR_SIZE,
         low_performing = C0532,
         college_going = C0534,
         incidents = INCID08,
         incidents_police = INCPOL08,
         suspension = OUTSUS08)%>% 
  mutate(year = "2008")
sf16 <- import(here("data_1", "pu_ssocs16.sas7bdat"),
               setclass = "tbl_df")%>% 
  select(urbanicity = FR_URBAN,
         size = FR_SIZE,
         low_performing = C0532,
         college_going = C0534,
         incidents = INCID16,
         incidents_police = INCPOL16,
         suspension = OUTSUS16)%>% 
  mutate(year = "2016")
sf18 <- import(here("data_1", "pu_ssocs18.sav"),
               setclass = "tbl_df")%>% 
  select(urbanicity = FR_URBAN,
         size = FR_SIZE,
         low_performing = C0532,
         college_going = C0534,
         incidents = INCID18,
         incidents_police = INCPOL18,
         suspension = OUTSUS18)%>% 
  mutate(year = "2018")
four_year <- bind_rows("05-06" = sf06, "07-08" = sf08, "15-16" = sf16, "17-18" = sf18, .id = "dataset") %>%
  pivot_longer(cols = c(incidents, incidents_police, suspension),
               names_to = "safety_indicators",
               values_to = "total") %>% 
  mutate(urbanicity = recode(urbanicity, '1' = "City", '2' = "Suburb", '3' = "Town", '4' = "Rural"),
         size = recode(size, '1' = "<300", '2' = "300-499", '3' = "500-999", '4' = "1,000+"),
         size = fct_relevel(size, "<300", "300-499", "500-999", "1,000+"))



