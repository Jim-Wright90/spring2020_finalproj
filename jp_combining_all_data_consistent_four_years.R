# Got all the columns with the same names but different years to match for all years

options(digits = 2)

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




numeric_plot <- function(data, x, y){
  r2 <- summary(lm(y ~ x, data))
  
  ggplot(data = {{data}}, aes(x = {{x}}, y = {{y}})) +
    geom_point(alpha = .4, color = 'gray70') +
    geom_smooth(method = 'lm', formula = {{y}} ~ {{x}}) +
    annotate('text', x = 0, y = 0, label = glue('italic(R) ^ 2 == {r2}'), parse = TRUE)
}

numeric_plot(batch, c0534, c0536)


bar_plot <- function(data, group, x, y){
  data %>% 
    group_by({{group}}) %>% 
    mutate(mean_value = mean({{y}})) %>% 
    ungroup() %>% 
    ggplot(aes({{x}}, {{y}})) +
    geom_col()
}

batch %>% 
  bar_plot(urbanicity, urbanicity, c0534) +
  geom_col(aes(fill = urbanicity)) +
  labs(x = 'X Value',
       y = 'Y Value',
       Title = 'A Title',
       caption = 'R2') +
  coord_flip() +
  geom_text(aes(label = round(mean_value, digits = 2)), hjust = -.2, size = 4)

# did not group by year though. Can easily change the function if necessary.

year_plot <- function()

  