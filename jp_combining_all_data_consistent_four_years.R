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
# batch

names(batch)

batch <- batch %>% 
  mutate(crisis = as.factor(crisis),
         distot = as.factor(distot),
         incidents = as.factor(incid),
         incidents_police = as.factor(incpol),
         othact = as.factor(othact),
         suspension = as.factor(outsus),
         probwk = as.factor(probwk),
         removl = as.factor(removl),
         stuoff = as.factor(stuoff),
         svinc = as.factor(svinc),
         svpol = as.factor(svpol),
         transf = as.factor(transf),
         vioinc = as.factor(vioinc),
         viopol = as.factor(viopol),
         fr_catmn = as.factor(fr_catmn),
         fr_loc4 = as.factor(fr_loc4),
         fr_lvel = as.factor(fr_lvel),
         size = as.factor(fr_size),
         urbanicity = as.factor(fr_urban),
         year = as.factor(year),
         low_performing = c0532,
         college_going = c0534,
         urbanicity = recode(urbanicity, '1' = "City", '2' = "Suburb", '3' = "Town", '4' = "Rural"),
         size = recode(size, '1' = "<300", '2' = "300-499", '3' = "500-999", '4' = "1,000+"),
         size = fct_relevel(size, "<300", "300-499", "500-999", "1,000+"))


map_chr(batch, class)

model <- lm(c0536 ~ c0534, batch)
association <- model$coef[2]

numeric_plot <- function(data, x, y){

  ggplot(data = {{data}}, aes(x = {{x}}, y = {{y}})) +
    geom_point(alpha = .4, color = 'gray70') +
    geom_smooth(method = 'lm', formula = {{y}} ~ {{x}})
}

numeric_plot(batch, c0534, c0536) +
  geom_text(x = 0, y = 75, label = association, parse = TRUE, color = 'red')


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



batch %>% 
  pivot_longer(cols = c(incid, incpol, outsus),
               names_to = "safety_indicators",
               values_to = "total") %>% 
  group_by(year, safety_indicators) %>% 
  mutate(total = as.numeric(total),
         mean_value = mean(total)) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  ggplot(aes(year, mean_value))+
  geom_line(aes(color = safety_indicators), size = 1) +
  geom_point()+
  scale_x_continuous(limits = c(2006, 2018),
                     breaks = c(2006, 2008, 2010, 2012, 2014, 2016, 2018)) +
  labs(title = "Average Values of Data from 2005-2006\nto 2017-2018",
       x = "School Year",
       y = "Average Number of Cases in School",
       fill = "Variable of Interest")

names(batch)

year_plot <- function(data, factor_long){
  {{data}} %>% 
  pivot_longer(cols = {{factor_long}},
               names_to = 'group',
               values_to = 'total') %>% 
    group_by(year, group) %>% 
    mutate(total = as.numeric(total),
           mean_value = mean(total)) %>% 
    ungroup(year, {{group}}) %>% 
    mutate(year = as.numeric(as.character(year))) %>% 
    ggplot(aes(year, mean_value)) +
    geom_line(aes(color = group), size = 1) +
    geom_point()+
    scale_x_continuous(limits = c(2006, 2018),
                       breaks = c(2006, 2008, 2010, 2012, 2014, 2016, 2018)) +
    labs(title = "Average Values of Data from 2005-2006\nto 2017-2018",
         x = "School Year",
         y = "Average Number of Cases in School",
         fill = "Variable of Interest")
}

year_plot(data = batch, factor_long = urbanicity)
  

year_plot_comparing_2 <- function(data, factor_long, factor_long2){
  {{data}} %>% 
    pivot_longer(cols = c({{factor_long}}, {{factor_long2}}),
                 names_to = 'groups',
                 values_to = 'total') %>% 
    group_by(year, group) %>% 
    mutate(total = as.numeric(total),
           mean_value = mean(total)) %>% 
    ungroup(year, {{group}}) %>% 
    mutate(year = as.numeric(as.character(year))) %>% 
    ggplot(aes(year, mean_value)) +
    geom_line(aes(color = group), size = 1) +
    geom_point()+
    scale_x_continuous(limits = c(2006, 2018),
                       breaks = c(2006, 2008, 2010, 2012, 2014, 2016, 2018)) +
    labs(title = "Average Values of Data from 2005-2006\nto 2017-2018",
         x = "School Year",
         y = "Average Number of Cases in School",
         fill = "Variable of Interest")
}

year_plot_comparing_2(data = batch, factor_long = urbanicity, factor_long2 = incidents)


year_plot_comparing_3 <- function(data, factor_long, factor_long2, factor_long3){
  {{data}} %>% 
    pivot_longer(cols = c({{factor_long}}, {{factor_long2}}, {{factor_long3}}),
                 names_to = 'groups',
                 values_to = 'total') %>% 
    group_by(year, group) %>% 
    mutate(total = as.numeric(total),
           mean_value = mean(total)) %>% 
    ungroup(year, {{group}}) %>% 
    mutate(year = as.numeric(as.character(year))) %>% 
    ggplot(aes(year, mean_value)) +
    geom_line(aes(color = group), size = 1) +
    geom_point()+
    scale_x_continuous(limits = c(2006, 2018),
                       breaks = c(2006, 2008, 2010, 2012, 2014, 2016, 2018)) +
    labs(title = "Average Values of Data from 2005-2006\nto 2017-2018",
         x = "School Year",
         y = "Average Number of Cases in School",
         fill = "Variable of Interest")
}

year_plot_comparing_3(data = batch, factor_long = incidents, factor_long2 = incidents_police, factor_long3 = suspension)

# FIND A WAY TO HAVE RADIO BUTTONS TO CHOOSE DIFFERENT YEARS.
# THAT WAY, WE CAN REMOVE THE FOUR YEARS AND JUST HAVE THAT AS AN OPTION



variables_by_urbanicity <- function(data, factor_long, factor_long2, factor_long3){
  {{data}} %>% 
    pivot_longer(cols = c({{factor_long}}, {{factor_long2}}, {{factor_long3}}),
                 names_to = 'group',
                 values_to = 'total') %>% 
    group_by(year, group) %>% 
    mutate(total = as.numeric(total),
           mean_value = mean(total)) %>% 
    ungroup(year, {{group}}) %>% 
    ggplot(aes(group, total))+
    geom_col(aes(fill = group),
             position = "dodge")+
    facet_wrap(~urbanicity)+
    theme_minimal()+
    scale_fill_brewer(palette = "Set2")+
    labs(title = "School Safety, School Size, and Urbanicity",
         x = "School Size",
         y = "Total Number of Cases",
         fill = "Groups")
}

variables_by_urbanicity(data = batch, factor_long = incidents, factor_long2 = incidents_police, factor_long3 = suspension) +
  coord_flip()
