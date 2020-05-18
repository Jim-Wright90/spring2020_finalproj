safe06 <- import(here( data ,  '2006_school_safety.sav' ),
                 setclass =  tbl_df ) %>% 
  characterize() %>% 
  janitor::clean_names() 

safe08 <- import(here('data', '2008_school_safety.sav'),
                 setclass = 'tbl_df') %>% 
  characterize() %>% 
  janitor::clean_names()

safe18 <- import(here('data', '2018_school_safety.sav'),
                 setclass = 'tbl_df') %>% 
  characterize() %>% 
  janitor::clean_names()

all_safe <- bind_rows( 05-06 Survey  = safe06, 
                       07-08 Survey  = safe08, 
                       17-18 Survey  = safe18, 
                      .id =  dataset)



all_datasets_have <- inspectdf::inspect_na(all_safe) %>% 
  filter(!pcnt > 1) %>% 
  filter(col_name != 'dataset')

col_names_in_all <- str_sub(all_datasets_have$col_name)




data06 <- import(here( data ,  '2006_school_safety.sav' ),
                 setclass =  tbl_df ) %>% 
  characterize() %>% 
  janitor::clean_names() %>% 
  dplyr::select(col_names_in_all) %>% 
  arrange(schid)

data08 <- import(here('data', '2008_school_safety.sav'),
                 setclass = 'tbl_df') %>% 
  characterize() %>% 
  janitor::clean_names() %>% 
  dplyr::select(col_names_in_all) %>% 
  arrange(schid)

data18 <- import(here('data', '2018_school_safety.sav'),
                 setclass = 'tbl_df') %>% 
  characterize() %>% 
  janitor::clean_names() %>% 
  dplyr::select(col_names_in_all) %>% 
  arrange(schid)

# write.csv(data06, 'safe06.csv')
# write.csv(data08, 'safe08.csv')
# write.csv(data18, 'safe18.csv')

safe06 <- read_csv('https://raw.githubusercontent.com/Jim-Wright90/spring2020_finalproj/master/data/safe06.csv')
safe08 <- read_csv('https://raw.githubusercontent.com/Jim-Wright90/spring2020_finalproj/master/data/safe08.csv')
safe18 <- read_csv('https://raw.githubusercontent.com/Jim-Wright90/spring2020_finalproj/master/data/safe18.csv')

safe06 <- safe06 %>% 
  rename(rowid = X1,
         school = schid,
         c0110,
         c0112,
         c0114,
         c0116,
         c0120,
         c0122,
         c0134,
         c0136,
         c0138,
         c0140,
         c0142,
         c0144,
         c0146,
         c0150,
         c0158,
         c0162,
         c0166,
         c0170,
         c0174,
         c0176,
         c0186,
         c0190,
         c0192,
         c0196,
         c0198,
         c0204,
         c0206,
         c0208,
         c0210,
         c0212,
         c0214,
         c0216,
         c0218,
         c0266,
         c0268,
         c0270,
         c0272,
         c0274,
         c0276,
         c0280,
         c0282,
         c0284,
         c0286,
         c0288,
         c0290,
         c0292,
         c0294,
         c0296,
         c0298,
         c0300,
         c0302
         c0304
         c0374
         c0376
         c0378
         c0380
         c0382
         c0384
         c0386,
         c0402,
         c0404,
         c0406,
         c0408,
         c0410,
         c0412,
         c0422,
         c0424,
         c0426,
         c0428,
         c0430,
         c0432,
         c0434,
         c0436,
         c0442,
         c0444,
         c0446,
         c0448,
         c0450,
         c0452,
         c0454,
         c0456,
         c0532,
         c0534,
         c0536,
         c0538,
         c0560,
         c0562,
         c0568,
         c0570,
         c0572,
         c0578,
         c0578_yy,
         fr_urban,
         fr_size,
         finalwgt,
         ic0110,
         ic0112,
         ic0114,
         ic0116,
         ic0120,
         ic0122,
         ic0134,
         ic0136,
         ic0138,
         ic0140,
         ic0142,
         ic0144,
         ic0146,
         ic0150,
         ic0158,
         ic0162, 
         ic0166,
         ic0170,
         ic0174,
         ic0176,
         ic0186,
         ic0190,
         ic0192,
         ic0196,
         ic0198,
         ic0204,
         ic0206,
         ic0208,
         ic0210,
         ic0212,
         ic0214,
         ic0216,
         ic0218,
         ic0266,  
         ic0268,
         ic0270,
         ic0272,
         ic0274,
         ic0276,
         ic0280,
         ic0282,
         ic0284,
         ic0286,
         ic0288,
         ic0290,
         ic0292,
         ic0294,
         ic0296,
         ic0298,
         ic0300,
         ic0302,
         ic0304,
         ic0374,
         ic0376,
         ic0378,
         ic0380,
         ic0382,
         ic0384,
         ic0386,
         ic0402,
         ic0404,
         ic0406,
         ic0408,
         ic0410,
         ic0412,
         ic0422,
         ic0424,
         ic0426,
         ic0428,
         ic0430,
         ic0432,
         ic0434,
         ic0436,
         ic0442,
         ic0444,
         ic0446,
         ic0448,
         ic0450,
         ic0452,
         ic0454,
         ic0456,
         ic0532,
         ic0534,
         ic0536,
         ic0538,
         ic0560,
         ic0562,
         ic0568,
         ic0570,
         ic0572)


