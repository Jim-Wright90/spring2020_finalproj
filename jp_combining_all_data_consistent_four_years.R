# Got all the columns with the same names but different years to match for all years

sf06 <- import(here("data_1", "pu_ssocs06_spss.sav"),
               setclass = "tbl_df") %>%
  janitor::clean_names() %>% 
  rename_at(vars(crisis06:viopol06), ~str_replace(., '06', '')) %>% 
  rename(year = c0578_yy) %>% 
  select(-matches('_r|_r2')) %>% 
  mutate(schid = as.character(schid))

sf08 <- import(here("data_1", "pu_ssocs08_spss.sav"),
               setclass = "tbl_df") %>%
  janitor::clean_names() %>% 
  rename_at(vars(crisis08:viopol08), ~str_replace(., '08', '')) %>% 
  rename(year = c0578_yy) %>% 
  select(-matches('_r|_r2')) %>% 
  mutate(schid = as.character(schid))

sf16 <- import(here("data_1", "pu_ssocs16.sas7bdat"),
               setclass = "tbl_df") %>%
  janitor::clean_names() %>% 
  rename_at(vars(crisis16:viopol16), ~str_replace(., '16', '')) %>% 
  rename(year = c0578_yy) %>% 
  select(-matches('_r|_r2')) %>% 
  mutate(schid = as.character(schid))

sf18 <- import(here("data_1", "pu_ssocs18.sav"),
               setclass = "tbl_df") %>%
  janitor::clean_names() %>% 
  rename_at(vars(crisis18:viopol18), ~str_replace(., '18', '')) %>% 
  rename(year = c0578_yy) %>% 
  select(-matches('_r|_r2')) %>% 
  mutate(schid = as.numeric(schid))


sf_list <- list(sf06, sf08, sf16, sf18)
Reduce(intersect, lapply(sf_list, names))


map_chr(sf06, class)
map_chr(sf08, class)
map_chr(sf16, class)
map_chr(sf18, class) 

# write.csv(sf06, 'sf06.csv')
# write.csv(sf08, 'sf08.csv')
# write.csv(sf16, 'sf16.csv')
# write.csv(sf18, 'sf18.csv')


files <- dir_ls(here::here("data_1"), glob = "*.csv")

batch <- map_df(files, read_csv)
batch

batch <- batch %>% 
  mutate(urbanicity = as.factor(fr_urban),
         size = as.factor(fr_size),
         year = as.factor(year),
         low_performing = c0532,
         college_going = c0534,
         incidents = as.factor(incid),
         incidents_police = as.factor(incpol),
         suspension = as.factor(outsus),
         urbanicity = recode(urbanicity, '1' = "City", '2' = "Suburb", '3' = "Town", '4' = "Rural"),
         size = recode(size, '1' = "<300", '2' = "300-499", '3' = "500-999", '4' = "1,000+"),
         size = fct_relevel(size, "<300", "300-499", "500-999", "1,000+"))



numeric_plot <- function(df, x, y){
  plot_num <- {{df}} %>% 
    ggplot(aes({{x}}, {{y}})) +
    geom_point(alpha = .4, color = 'gray70') +
    geom_smooth(method = 'lm')
  
  if(!as.numeric({{x}}) & !as.numeric({{y}})){
    stop()
  }
  else{
    plot_num
  }
  return(plot_num)
}

batch %>% 
