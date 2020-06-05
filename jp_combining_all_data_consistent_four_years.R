# Got all the columns with the same names but different years to match for all years

sf06 <- import(here("data_1", "pu_ssocs06_spss.sav"),
               setclass = "tbl_df") %>%
  janitor::clean_names() %>% 
  rename_at(vars(crisis06:viopol06), ~str_replace(., '06', '')) %>% 
  rename(year = c0578_yy)

sf08 <- import(here("data_1", "pu_ssocs08_spss.sav"),
               setclass = "tbl_df") %>%
  janitor::clean_names() %>% 
  rename_at(vars(crisis08:viopol08), ~str_replace(., '08', '')) %>% 
  rename(year = c0578_yy)

sf16 <- import(here("data_1", "pu_ssocs16.sas7bdat"),
               setclass = "tbl_df") %>%
  janitor::clean_names() %>% 
  rename_at(vars(crisis16:viopol16), ~str_replace(., '16', '')) %>% 
  rename(year = c0578_yy)

sf18 <- import(here("data_1", "pu_ssocs18.sav"),
               setclass = "tbl_df") %>%
  janitor::clean_names() %>% 
  rename_at(vars(crisis18:viopol18), ~str_replace(., '18', '')) %>% 
  rename(year = c0578_yy)


sf_list <- list(sf06, sf08, sf16, sf18)
Reduce(intersect, lapply(sf_list, names))

# write.csv(sf06, 'sf06.csv')
# write.csv(sf08, 'sf08.csv')
# write.csv(sf16, 'sf16.csv')
# write.csv(sf18, 'sf18.csv')

