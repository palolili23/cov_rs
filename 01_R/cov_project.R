
# Set directory -----------------------------------------------------------
# Libraries ---------------------------------------------------------------
library(here)
source(here::here("01_R", "00_libraries.R"))


# basic -------------------------------------------------------------------

# basic downloaded from wiki: https://epi-wiki.erasmusmc.nl/wiki/ergowiki/index.php/Ergobasics
# select needed variables from the basic file and determine age at study entry

basic <- read_sav(here::here("00_raw_data", "basic", "RoterdamStudy_Basics2014.sav"))

basic <- basic %>% 
  mutate(sex = labelled(sex, c(Female = 1, Male = 0)))

basic <- basic %>%
  select(ergoid, rs_cohort, sex, date_of_birth, startdat) %>%
  mutate(age_0 = round(as.numeric(as.period((date_of_birth %--% startdat ), "years"), "years"), 2))

export(basic, here::here("02_clean_data", "basic.Rdata"))

export()
### remove everything from our environment except for the complete_db
rm(list=ls()[! ls() %in% c("basic")])

# visits ------------------------------------------------------------------

#visits downloaded from wiki

rs1_1 <- import("./00_raw_data/visits/Ergo1ResponseDetail_(22-jan-2015)_excerpt.sav")
rs1_2 <- import("./00_raw_data/visits/Ergo2ResponseDetail_(22-jan-2015)_excerpt.sav")
rs1_3 <- import("./00_raw_data/visits/e3_(3)_RESPONS_(22-feb-2016)_excerpt.sav")
rs1_4 <- import("./00_raw_data/visits/e4_(4)_RESPONS_(12-mar-2018)_excerpt.sav")
rs1_5 <- import("./00_raw_data/visits/e5_(5)_RESPONS_(22-jun-2016)_excerpt.sav")
rs1_6 <- import("./00_raw_data/visits/e6_(6)_RESPONS_(10-feb-2017)_EXCERPT.sav")
rs2_1 <- import("./00_raw_data/visits/ep_(1)_RESPONS_(15-jan-2019)_excerpt.sav")
rs3_1 <- import("./00_raw_data/visits/ej_(1)_RESPONS_(04-apr-2016)_excerpt.sav")

##### Separate visitdates dataset into cohorts

#1. Separate rs1_4 into rs1, rs2

rs2_2 <- rs1_4 %>%
  filter(rs_cohort == 2)

rs1_4 <- rs1_4 %>%
  filter(rs_cohort == 1)

#2. Separate rs1_5 into rs1, rs2, rs3
rs3_2 <- rs1_5 %>%
  filter(rs_cohort == 3)

rs2_3 <- rs1_5 %>%
  filter(rs_cohort == 2)

rs1_5 <- rs1_5 %>%
  filter(rs_cohort == 1)

#3. rs1_6 into rs1 and rs2 
rs2_4 <- rs1_6 %>%
  filter(rs_cohort == 2)

rs1_6 <- rs1_6 %>%
  filter(rs_cohort == 1)


#### Merge rs1

rs1 <- list(rs1_1, rs1_2, rs1_3, rs1_4, rs1_5, rs1_6)

rs1_vis <- reduce(rs1, left_join, by = c("ergoid", "rs_cohort"))

### Filter in rs1_vis the dates we want
### NOTE: RS1_2 had only 1 center visit and no home interview!

rs1 <- rs1_vis %>%
  select(ergoid, rs_cohort, e1_aintdat, e1_acen1dat, e1_acen2dat, 
         e2_bcendat, e3_3493, e3_3494, e3_3495, e4_3493, e4_3494, e4_3495, 
         e5_3493, e5_3494, e5_3495, e6_3493, e6_3494, e6_3495)

rs1_interview <- rs1_vis %>%
  select(ergoid, rs_cohort, e1_aintdat, e2_bcendat, e3_3493, e4_3493, e5_3493, e6_3493) %>% 
  rename(e1 = e1_aintdat, e2 = e2_bcendat, e3 = e3_3493, e4 = e4_3493, e5 = e5_3493, e6 = e6_3493)

### merge r2 visits

rs2 <- list(rs2_1, rs2_2, rs2_3, rs2_4)

rs2_vis <- reduce(rs2, left_join, by = c("ergoid", "rs_cohort"))

rs2_interview <- rs2_vis %>%
  select(ergoid, rs_cohort , ep_3493, e4_3493, e5_3493, e6_3493) %>% 
  rename(e3 = ep_3493, e4 = e4_3493, e5 = e5_3493, e6 = e6_3493)
  

#### Merge rs3 visits

rs3_vis <- rs3_1  %>%
  left_join(rs3_2, by = c("ergoid", "rs_cohort"))

rs3_interview <- rs3_vis %>%
  select(ergoid, rs_cohort , ej_3493, e5_3493) %>% 
  rename(e4 = ej_3493, e5 = e5_3493)

##merge cohorts together for interview dates (this gives extra cases, meaning extra rows, therefore we use bind_rows instead of left_join merging)

visits <- rs1_interview %>% 
  bind_rows(rs2_interview) %>% 
  bind_rows(rs3_interview)

rm(list=ls()[! ls() %in% c("visits")])

export(visits, here::here("02_clean_data", "visits.Rdata"))

# complete labels of the variables
# label(visits)[1:8] <- list("Identification","Rotterdam Study cohort","Interview date Ergo1","Center visit date Ergo2","Interview date Ergo3 and Ergo PLUS","Interview date Ergo4 and Ergo Jong","Interview date Ergo5","Interview date Ergo6") 

# bmi ---------------------------------------------------------------------

bmi1_1 <- import("./00_raw_data/anthro/e1_ANTHROPO_(15-jun-2011).sav")
bmi1_2 <- import("./00_raw_data/anthro/e2_(2)_ANTHROPO_(26-apr-2011).sav")
bmi1_3 <- import("./00_raw_data/anthro/e3_(3)_HARTVAAT_(25-feb-2013)_ANTHROPO.sav")
bmi1_4 <- import("./00_raw_data/anthro/e4_(4)_UITSCHR_(06-nov-2014)_ANTHROPO-PART.sav")
bmi1_5 <- import("./00_raw_data/anthro/e5_(5)_ANTHROPO_(10-dec-2015).sav")
bmi1_6 <- import("./00_raw_data/anthro/e6_(6)_ANTHROPO_(25-apr-2017).sav")
bmi2_1 <- import("./00_raw_data/anthro/ep_(1)_LICHONDZ_(18-oct-2012)_ANTHROPO.sav")
bmi3_1 <- import("./00_raw_data/anthro/ej_(1)_UITSCHR_(23-feb-2010)_ANTHROPO-PART.sav")

#separate bmi4 into rs1 and rs2
bmi2_2 <- bmi1_4 %>%
  filter(rs_cohort == 2)

bmi1_4 <- bmi1_4 %>%
  filter(rs_cohort == 1)

#2. Separate bmi_5 into rs1, rs2, rs3
bmi3_2 <- bmi1_5 %>%
  filter(rs_cohort == 3)

bmi2_3 <- bmi1_5 %>%
  filter(rs_cohort == 2)

bmi1_5 <- bmi1_5 %>%
  filter(rs_cohort == 1)

#3. Separate bmi_6 into rs1 and rs2
bmi1_6 <- bmi1_6 %>%
  filter(rs_cohort ==1)

bmi2_4 <- bmi1_6 %>%
  filter(rs_cohort ==2)

### Merge cohorts for rs1
bmi1 <- list(bmi1_1, bmi1_2, bmi1_3, bmi1_4, bmi1_5, bmi1_6)

rs1_bmi <- reduce(bmi1, left_join, by = c("ergoid", "rs_cohort"))

dim(rs1_bmi)

### Merge cohorts for rs2
bmi2 <- list(bmi2_1, bmi2_2, bmi2_3, bmi2_4)
rs2_bmi <- reduce(bmi2, left_join, by = c("ergoid", "rs_cohort"))

###Merge cohorts for rs3

bmi3 <- bmi3_1 %>%
  left_join(bmi3_2, by= c("ergoid", "rs_cohort"))

#### Select main variables from each set

rs1_bmi <- rs1_bmi%>%
  select(ergoid, rs_cohort, e1_aahgt, e1_aawgt, e2_229, e2_230, e3_229, e3_230, e4_229, e4_230, e5_229, e5_230,e6_229,e6_230) %>%
  rename(hgt1 = e1_aahgt, hgt2 = e2_229, hgt3 = e3_229, hgt4 = e4_229, hgt5 = e5_229, hgt6 = e6_229,
         wgt1 = e1_aawgt, wgt2 = e2_230, wgt3 = e3_230, wgt4 = e4_230, wgt5 = e5_230, wgt6 = e6_230)

rs2_bmi <- rs2_bmi%>%
  select(ergoid, rs_cohort, ep_229, ep_230, e4_229, e4_230, e5_229, e5_230,e6_229,e6_230) %>%
  rename(hgt3 = ep_229, hgt4 = e4_229, hgt5 = e5_229, hgt6 = e6_229,
         wgt3 = ep_230, wgt4 = e4_230, wgt5 = e5_230, wgt6 = e6_230)


rs3_bmi <- bmi3%>%
  select(ergoid,  rs_cohort, ej_229, ej_230, e5_229, e5_230) %>%
  rename(hgt4 = ej_229, hgt5 = e5_229,
         wgt4 = ej_230, wgt5 = e5_230)

### bind bmi

rs_bmi <- rs1_bmi %>%
  bind_rows(rs2_bmi) %>%
  bind_rows(rs3_bmi)

rs_bmi <- rs_bmi %>%
  mutate(bmi1 = (wgt1/((hgt1/100)^2))) %>%
  mutate(bmi2 = (wgt2/((hgt2/100)^2))) %>%
  mutate(bmi3 = (wgt3/((hgt3/100)^2))) %>%
  mutate(bmi4 = (wgt4/((hgt4/100)^2))) %>%
  mutate(bmi5 = (wgt5/((hgt5/100)^2))) %>%
  mutate(bmi6 = (wgt6/((hgt6/100)^2)))

bmi <- rs_bmi %>%
  select(ergoid, rs_cohort, bmi1, bmi2, bmi3, bmi4, bmi5, bmi6)

label(bmi) <- as.list(c("Identification",rep("Body Mass Index (BMI) in kg/m^2",6))) 

export(bmi, here::here("02_clean_data", "bmi.Rdata"))

# education ---------------------------------------------------------------
# education file from wiki: UNESCO classification: https://epi-wiki.erasmusmc.nl/wiki/ergowiki/index.php/Education

educ <- import("./00_raw_data/education/Education RS-I-II-III (UNESCO class)_(12-MAR-2015).sav")

label(educ) <- list("Identification","Highest obtained education level: 0 = Low, 1 = Intermediate, 2 = High")

export(educ, here::here("02_clean_data", "educ.Rdata"))
# smoke anything ----------------------------------------------------------
smoke1_1 <- import("./00_raw_data/smoke/e1_intvw_SMOKING_(23-nov-2011).sav")
smoke1_2 <- import("./00_raw_data/smoke/e2_intvw_SMOKING_(23-nov-2011).sav")
smoke1_3 <- import("./00_raw_data/smoke/e3_intvw_SMOKING_(11-nov-2011).sav")
smoke1_4 <- import("./00_raw_data/smoke/e4_intvw_SMOKING_(04-nov-2011).sav")
smoke1_5 <- import("./00_raw_data/smoke/e5_intvw_SMOKING_(04-sep-2014).sav")
smoke2_1 <- import("./00_raw_data/smoke/ep_intvw_SMOKING_(30-sep-2011).sav")
smoke3_1 <- import("./00_raw_data/smoke/ej_intvw_SMOKING_(28-mar-2011).sav")
smoke1_6 <- import("./00_raw_data/smoke/e6_intvw_SMOKING_(20-feb-2017).sav")

### break 4 and 5 into

#separate smoke4 into rs1 and rs2
smoke2_2 <- smoke1_4 %>%
  filter(rs_cohort == 2)

smoke1_4 <- smoke1_4 %>%
  filter(rs_cohort == 1)

#2. Separate smoke_5 into rs1, rs2, rs3
smoke3_2 <- smoke1_5 %>%
  filter(rs_cohort == 3)

smoke2_3 <- smoke1_5 %>%
  filter(rs_cohort == 2)

smoke1_5 <- smoke1_5 %>%
  filter(rs_cohort == 1)

#3. Separate smoke_6 into rs1 and rs2
smoke1_6 <- smoke1_6 %>%
  filter(rs_cohort == 1)

smoke2_4 <- smoke1_6 %>% 
  filter(rs_cohort == 2)

### Merge cohorts for rs1
smoke1 <- list(smoke1_1, smoke1_2, smoke1_3, smoke1_4, smoke1_5, smoke1_6)

rs1_smoke<- reduce(smoke1, left_join, by = c("ergoid", "rs_cohort"))

### Merge cohorts for rs2
smoke2 <- list(smoke2_1, smoke2_2, smoke2_3, smoke2_4)

rs2_smoke <- reduce(smoke2, left_join, by = c("ergoid", "rs_cohort"))

###Merge cohorts for rs3

rs3_smoke <- smoke3_1 %>%
  left_join(smoke3_2, by= c("ergoid", "rs_cohort"))

########### Select variables and code smoking 0 never, 1 former, 2 current, NA missing
rs1_smoke <- rs1_smoke%>%
  mutate(smoke1 = ifelse((e1_ai7_20 == 0 & e1_ai7_30 == 0), 0, NA),
         smoke1 = ifelse((e1_ai7_20 == 0 & e1_ai7_30 == 1), 1, smoke1),
         smoke1 = ifelse(e1_ai7_20 == 1, 2, smoke1),
         smoke2 = ifelse(e2_b0cg == 3 & e2_b0pi == 3 & e2_b0ct == 3, 0, NA),
         smoke2 = ifelse((e2_b0cg == 2 | e2_b0pi == 2 | e2_b0ct == 2), 1, smoke2),
         smoke2 = ifelse((e2_b0cg == 1 | e2_b0pi == 1 | e2_b0ct == 1), 2, smoke2),
         smoke3 = ifelse(e3_cicg == 0 & e3_cipi == 0 & e3_cict == 0 & e3_cictps == 0, 0, NA),
         smoke3 = ifelse((e3_cicg == 1 |e3_cipi == 1 |(e3_cict == 0 & e3_cictps == 1)), 1, smoke3),
         smoke3 = ifelse((e3_cicg == 2 |e3_cicg == 3 | e3_cipi == 2 | e3_cipi == 3 | e3_cict == 1), 2, smoke3),
         smoke4 = ifelse(e4_dicg == 0 & e4_dipi == 0 & e4_dict == 0, 0, NA),
         smoke4 = ifelse((e4_dicg == 1 |e4_dipi == 1 | e4_dict == 2 | e4_dict == 3), 1, smoke4),
         smoke4 = ifelse((e4_dicg == 2 |e4_dicg == 3 | e4_dipi == 2 | e4_dipi == 3 | e4_dict == 1), 2, smoke4),
         smoke5 = ifelse(e5_EILF6 == 0 & e5_EILFE == 0 & e5_EILF5 == 0 & e5_EILF4 == 0, 0, NA), 
         smoke5 = ifelse(((e5_EILF6 == 0 & e5_EILFE == 1)| e5_EILF5 == 1 | e5_EILF4 == 1 | e5_EILF4 == 2 | e5_EILF4 == 3), 1, smoke5),
         smoke5 = ifelse((e5_EILF6 == 1 | e5_EILF5 == 2 | e5_EILF4 == 4 | e5_EILF4 == 5 | e5_EILF4 == 6), 2, smoke5),
         smoke6 = ifelse((e6_EILF6 == 0 & e6_EILSE5 == 0 & e6_EILF5 == 0 & e6_EILF4 == 0), 0, NA),
         smoke6 = ifelse(((e6_EILF6 == 0 & (e6_EILSE5 == 1 |e6_EILSE5 == 2))| e6_EILF5 == 1 | e6_EILF4 == 1 | e6_EILF4 == 2 | e6_EILF4 == 3), 1, smoke6),
         smoke6 = ifelse((e6_EILF6 == 1 | e6_EILF5 == 2 | e6_EILF4 == 4 | e6_EILF4 == 5 | e6_EILF4 == 6), 2, smoke6)) %>% 
  select(ergoid, smoke1, smoke2, smoke3, smoke4, smoke5)

rs2_smoke <- rs2_smoke%>%
  mutate(smoke3 = ifelse(ep_lf4 == 0 & ep_lf5 == 0 & ep_lf6 == 0 & ep_lf6e == 0, 0, NA),
         smoke3 = ifelse((ep_lf4 == 1 | ep_lf5 == 1 | (ep_lf6 == 0 & ep_lf6e == 1)), 1, smoke3),
         smoke3 = ifelse((ep_lf4 == 2 | ep_lf5 == 2 | ep_lf6 == 1), 2, smoke3),
         smoke4 = ifelse(e4_dicg == 0 & e4_dipi == 0 & e4_dict == 0, 0, NA), 
         smoke4 = ifelse((e4_dicg == 1 |e4_dipi == 1 | e4_dict == 2 | e4_dict == 3), 1, smoke4), 
         smoke4 = ifelse((e4_dicg == 2 |e4_dicg == 3 | e4_dipi == 2 | e4_dipi == 3 | e4_dict == 1), 2, smoke4),
         smoke5 = ifelse(e5_EILF6 == 0 & e5_EILFE == 0 & e5_EILF5 == 0 & e5_EILF4 == 0, 0, NA), 
         smoke5 = ifelse(((e5_EILF6 == 0 & e5_EILFE == 1)| e5_EILF5 == 1 | e5_EILF4 == 1 | e5_EILF4 == 2 | e5_EILF4 == 3), 1, smoke5),
         smoke5 = ifelse((e5_EILF6 == 1 | e5_EILF5 == 1 | e5_EILF4 == 4 | e5_EILF4 == 5 | e5_EILF4 == 6), 2, smoke5),
         smoke6 = ifelse(e6_EILF6 == 0 & e6_EILSE5 == 0 & e6_EILF5 == 0 & e6_EILF4 == 0, 0, NA),
         smoke6 = ifelse(((e6_EILF6 == 0 & (e6_EILSE5 == 1 |e6_EILSE5 == 2))| e6_EILF5 == 1 | e6_EILF4 == 1 | e6_EILF4 == 2 | e6_EILF4 == 3), 1, smoke6),
         smoke6 = ifelse((e6_EILF6 == 1 | e6_EILF5 == 2 | e6_EILF4 == 4 | e6_EILF4 == 5 | e6_EILF4 == 6), 2, smoke6)) %>%
  select(ergoid, rs_cohort, smoke3, smoke4, smoke5, smoke6)
  
  
rs3_smoke <- rs3_smoke%>%
  mutate(smoke4 = ifelse(ej_yilf4 == 0 & ej_yilf5 == 0 & ej_yilf6 == 0 & ej_yilfe == 0, 0, NA),
         smoke4 = ifelse((ej_yilf4 == 1 | ej_yilf4 == 2 | ej_yilf4 == 3 | ej_yilf5 == 1 | (ej_yilf6 == 0 & ej_yilfe == 1)), 1, smoke4),
         smoke4 = ifelse((ej_yilf4 == 4 | ej_yilf4 == 5 | ej_yilf4 == 6 | ej_yilf5 == 2 | ej_yilf6 == 1), 2, smoke4),
         smoke5 = ifelse(e5_EILF6 == 0 & e5_EILFE == 0 & e5_EILF5 == 0 & e5_EILF4 == 0, 0, NA),
         smoke5 = ifelse(((e5_EILF6 == 0 & e5_EILFE == 1)| e5_EILF5 == 1 | e5_EILF4 == 1 | e5_EILF4 == 2 | e5_EILF4 == 3), 1, smoke5),
         smoke5 = ifelse((e5_EILF6 == 1 | e5_EILF5 == 1 | e5_EILF4 == 4 | e5_EILF4 == 5 | e5_EILF4 == 6), 2, smoke5)) %>% 
  select(ergoid, rs_cohort, smoke4, smoke5)


smoke <- rs1_smoke %>% 
  bind_rows(rs2_smoke) %>% 
  bind_rows(rs3_smoke)

export(smoke, here::here("02_clean_data", "smoke.Rdata"))

# smoke cigarrets ---------------------------------------------------------
smoke1_1 <- import("./00_raw_data/smoke/e1_intvw_SMOKING_(23-nov-2011).sav")
smoke1_2 <- import("./00_raw_data/smoke/e2_intvw_SMOKING_(23-nov-2011).sav")
smoke1_3 <- import("./00_raw_data/smoke/e3_intvw_SMOKING_(11-nov-2011).sav")
smoke1_4 <- import("./00_raw_data/smoke/e4_intvw_SMOKING_(04-nov-2011).sav")
smoke1_5 <- import("./00_raw_data/smoke/e5_intvw_SMOKING_(04-sep-2014).sav")
smoke2_1 <- import("./00_raw_data/smoke/ep_intvw_SMOKING_(30-sep-2011).sav")
smoke3_1 <- import("./00_raw_data/smoke/ej_intvw_SMOKING_(28-mar-2011).sav")
smoke1_6 <- import("./00_raw_data/smoke/e6_intvw_SMOKING_(20-feb-2017).sav")

### break 4 and 5 into 
#separate smoke4 into rs1 and rs2
smoke2_2 <- smoke1_4 %>%
  filter(rs_cohort == 2)

smoke1_4 <- smoke1_4 %>%
  filter(rs_cohort == 1)

#2. Separate smoke_5 into rs1, rs2, rs3
smoke3_2 <- smoke1_5 %>%
  filter(rs_cohort == 3)

smoke2_3 <- smoke1_5 %>%
  filter(rs_cohort == 2)

smoke1_5 <- smoke1_5 %>%
  filter(rs_cohort == 1)

#3. Separate smoke_6 into rs1 and rs2
smoke1_6 <- smoke1_6 %>%
  filter(rs_cohort == 1)

smoke2_4 <- smoke1_6 %>% 
  filter(rs_cohort == 2)


### Merge cohorts for rs1
smoke1 <- list(smoke1_1, smoke1_2, smoke1_3, smoke1_4, smoke1_5, smoke1_6)

rs1_cig<- reduce(smoke1, left_join, by = c("ergoid"))

### Merge cohorts for rs2
smoke2 <- list(smoke2_1, smoke2_2, smoke2_3, smoke2_4)

rs2_cig <- reduce(smoke2, left_join, by = c("ergoid"))

###Merge cohorts for rs3

rs3_cig <- smoke3_1 %>%
  left_join(smoke3_2, by= c("ergoid"))

########### Select variables and code smoking 0 never, 1 former, 2 current, 3 missing
rs1_cig <- rs1_cig %>%
  mutate(
    smoke_cig1 = ifelse((e1_ai7_20 == 0 & e1_ai7_30 == 0), 0, NA),
    smoke_cig1 = ifelse((e1_ai7_20 == 0 &
                           e1_ai7_30 == 1), 1, smoke_cig1),
    smoke_cig1 = ifelse(e1_ai7_20 == 1, 2, smoke_cig1),
    smoke_cig2 = ifelse(e2_b0ct == 3, 0, NA),
    smoke_cig2 = ifelse(e2_b0ct == 2, 1, smoke_cig2),
    smoke_cig2 = ifelse(e2_b0ct == 1, 2, smoke_cig2),
    smoke_cig3 = ifelse((e3_cict == 0 &
                           e3_cictps == 0), 0, NA),
    smoke_cig3 = ifelse((e3_cict == 0 &
                           e3_cictps == 1), 1, smoke_cig3),
    smoke_cig3 = ifelse(e3_cict == 1, 2, smoke_cig3),
    smoke_cig4 = ifelse(e4_dict == 0, 0, NA),
    smoke_cig4 = ifelse((e4_dict == 2 |
                           e4_dict == 3), 1, smoke_cig4),
    smoke_cig4 = ifelse(e4_dict == 1, 2, smoke_cig4),
    smoke_cig5 = ifelse(e5_EILF6 == 0 & e5_EILFE == 0, 0, NA),
    smoke_cig5 = ifelse((e5_EILF6 == 0 &
                           e5_EILFE == 1), 1, smoke_cig5),
    smoke_cig5 = ifelse(e5_EILF6 == 1, 2, smoke_cig5),
    smoke_cig6 = ifelse((e6_EILF6 == 0 &
                           e6_EILSE5 == 0), 0, NA),
    smoke_cig6 = ifelse((
      e6_EILF6 == 0 & (e6_EILSE5 == 1 | e6_EILSE5 == 2)
    ), 1, smoke_cig6),
    smoke_cig6 = ifelse((e6_EILF6 == 1), 2, smoke_cig6)
  ) %>%
  select(ergoid, starts_with("smoke_cig"))

# pattern_cig_rs1<- rs1_cig %>% 
#   filter(!is.na(smoke_cig1)) %>% 
#   mutate(smoke_cig2 = ifelse(is.na(smoke_cig2), smoke_cig1, smoke_cig2),
#          smoke_cig3 = ifelse(is.na(smoke_cig3), smoke_cig2, smoke_cig3),
#          smoke_cig4 = ifelse(is.na(smoke_cig4), smoke_cig3, smoke_cig4),
#          smoke_cig5 = ifelse(is.na(smoke_cig5), smoke_cig4, smoke_cig5),
#          smoke_cig6 = ifelse(is.na(smoke_cig6), smoke_cig5, smoke_cig6)) %>% 
#   count(smoke_cig1, smoke_cig2, smoke_cig3, smoke_cig4, smoke_cig5, smoke_cig6)

######## rs2
rs2_cig <- rs2_cig %>%
  mutate(smoke_cig3 = ifelse((ep_lf6 == 0 & ep_lf6e == 0), 0, NA),
         smoke_cig3 = ifelse((ep_lf6 == 0 & ep_lf6e == 1), 1, smoke_cig3),
         smoke_cig3 = ifelse(ep_lf6 == 1, 2, smoke_cig3),
         smoke_cig4 = ifelse(e4_dict == 0, 0, NA),
         smoke_cig4 = ifelse((e4_dict == 2 | e4_dict == 3), 1, smoke_cig4),
         smoke_cig4 = ifelse(e4_dict == 1, 2, smoke_cig4),
         smoke_cig5 = ifelse(e5_EILF6 == 0 & e5_EILFE == 0, 0, NA),
         smoke_cig5 = ifelse((e5_EILF6 == 0 & e5_EILFE == 1), 1, smoke_cig5),
         smoke_cig5 = ifelse(e5_EILF6 == 1, 2, smoke_cig5),
         smoke_cig6 = ifelse((e6_EILF6 == 0 & e6_EILSE5 == 0), 0, NA),
         smoke_cig6 = ifelse((e6_EILF6 == 0 & (e6_EILSE5 == 1 | e6_EILSE5 == 2)), 1, smoke_cig6),
         smoke_cig6 = ifelse((e6_EILF6 == 1), 2, smoke_cig6)) %>%
  select(ergoid, smoke_cig3, smoke_cig4, smoke_cig5, smoke_cig6)

# pattern_cig_rs2<- rs2_cig %>% 
#   filter(!is.na(smoke_cig3)) %>% 
#   mutate(smoke_cig3 = ifelse(is.na(smoke_cig3), smoke_cig4, smoke_cig5, smoke_cig6),
#          smoke_cig4 = ifelse(is.na(smoke_cig4), smoke_cig5, smoke_cig6),
#          smoke_cig5 = ifelse(is.na(smoke_cig5), smoke_cig6)) %>% 
#   count(smoke_cig3, smoke_cig4, smoke_cig5, smoke_cig6)

############## rs3

rs3_cig <- rs3_cig %>%
  mutate(smoke_cig4 = ifelse(ej_yilf6 == 0 & ej_yilfe == 0, 0, NA),
         smoke_cig4 = ifelse(ej_yilf6 == 0 & ej_yilfe == 1, 1, smoke_cig4),
         smoke_cig4 = ifelse(ej_yilf6 == 1, 2, smoke_cig4),
         smoke_cig5 = ifelse((e5_EILF6 == 0 & e5_EILFE == 0), 0, NA),
         smoke_cig5 = ifelse((e5_EILF6 == 0 & e5_EILFE == 1), 1, smoke_cig5),
         smoke_cig5 = ifelse(e5_EILF6 == 1, 2, smoke_cig5)) %>%
  select(ergoid, smoke_cig4, smoke_cig5)

# pattern_cig_rs3<- rs3_cig %>% 
#   filter(!is.na(smoke_cig4)) %>% 
#   mutate(smoke_cig5 = ifelse(is.na(smoke_cig5), smoke_cig4, smoke_cig5)) %>% 
#   count(smoke_cig4, smoke_cig5)

### binding all

smoke_cig <- rs1_cig %>% 
  bind_rows(rs2_cig) %>% 
  bind_rows(rs3_cig)

export(smoke_cig, here::here("02_clean_data", "smoke_cig.Rdata"))



# errors in smoking variable ----------------------------------------------

# error <- rs1_smoke %>% 
#   filter(ergoid == 574001 | ergoid == 1944001 | ergoid == 3603002 | ergoid == 3607001 | ergoid == 3796002) %>% 
#   select(ergoid, e1_ai7_20, e1_ai7_30, e2_b0cg, e2_b0ct, e2_b0pi, e3_cicg, e3_cipi, e3_cict, e3_cictps, e4_dicg, e4_dipi, e4_dict, e5_EILF6, e5_EILFE, e5_EILF4, e5_EILF5, starts_with("smoke"))
# 
# error <- error %>% 
#   mutate(smoke2 = ifelse(e2_b0cg == 3 & e2_b0pi == 3 & e2_b0ct == 3, 0, NA), 
#          smoke2 = ifelse((e2_b0cg == 2 | e2_b0pi == 2 | e2_b0ct == 2), 1, smoke2),
#          smoke2 = ifelse((e2_b0cg == 1 | e2_b0pi == 1 | e2_b0ct == 1), 2, smoke2))
# glimpse(error)
# 
# pattern_smoke_rs1<- rs1_smoke %>% 
#   filter(!is.na(smoke1)) %>% 
#   mutate(smoke2 = ifelse(is.na(smoke2), smoke1, smoke2),
#          smoke3 = ifelse(is.na(smoke3), smoke2, smoke3),
#          smoke4 = ifelse(is.na(smoke4), smoke3, smoke4),
#          smoke5 = ifelse(is.na(smoke5), smoke4, smoke5)) %>% 
#   count(smoke1, smoke2, smoke3, smoke4, smoke5)



# vital status ------------------------------------------------------------

vital_status <- import(here::here("00_raw_data", "vital_status", "fp_VitalStatus_(24-MAY-2018).sav"))
vital_status <- mort %>%
  select(ergoid, fp_mortdat, fp_censordate) %>% 
  rename(mort_date = fp_mortdat, censor_date = fp_censordate)

export(vital_status, here::here("02_clean_data", "vital_status.Rdata"))

# alcohol intake ---------------------------------------------------------

oh1_1 <- import(here::here("00_raw_data", "alcohol", "e1_FFQ_AlcoholGramPerday_inclGLAZEN.sav"))
oh1_2 <- import(here::here("00_raw_data", "alcohol", "e2_intvw_Alcoholperday_25-10-2013.sav"))
oh1_3 <- import(here::here("00_raw_data", "alcohol", "e3_intvw_Alcoholperday_24-10-2017.sav"))
oh1_4 <- import(here::here("00_raw_data", "alcohol", "e4_intvw_Alcoholperday_22-11-2013.sav"))
oh1_5 <- import(here::here("00_raw_data", "alcohol", "e5intvw_Alcoholperday_11-07-2014.sav"))
oh1_6 <- import(here::here("00_raw_data", "alcohol", "e6_AlcoholGramsDay_FFQ_energy_(13-dec-2018).sav"))
oh2_1 <- import(here::here("00_raw_data", "alcohol", "ep_intvw_Alcoholperday_22-11-2013.sav"))
oh3_1 <- import(here::here("00_raw_data", "alcohol", "ej_intvw_ALCOHOLGRAMSPERDAY_(14072014).sav"))

#separate oh1_4 into rs1 and rs2
oh2_2 <- oh1_4 %>%
  filter(rs_cohort == 2)

oh1_4 <- oh1_4 %>%
  filter(rs_cohort == 1)

#2. Separate bmi_5 into rs1, rs2, rs3
oh3_2 <- oh1_5 %>%
  filter(rs_cohort == 3)

oh2_3 <- oh1_5 %>%
  filter(rs_cohort == 2)

oh1_5 <- oh1_5 %>%
  filter(rs_cohort == 1)

#3 Separate bmi_6 into rs1 and rs2

oh1_6 <- oh1_6 %>%
  filter(rs_cohort == 1)

oh2_4 <- oh1_6 %>%
  filter(rs_cohort == 2)

### Merge cohorts for rs1
oh1 <- list(oh1_1, oh1_2, oh1_3, oh1_4, oh1_5, oh1_6)

rs1_oh <- reduce(oh1, left_join, by = c("ergoid"))

dim(rs1_oh)

### Merge cohorts for rs2
oh2 <- list(oh2_1, oh2_2, oh2_3, oh2_4)

rs2_oh <- reduce(oh2, left_join, by = c("ergoid"))

dim(rs2_oh)

### Merge cohorts for rs3
oh3 <- list(oh3_1, oh3_2)

rs3_oh <- reduce(oh3, left_join, by = c("ergoid"))

dim(rs3_oh)

### Select alcohol intake

rs1_oh <- rs1_oh %>% 
  select(ergoid, antalc, e2_Alc_Tot, e3_Alc_Tot, e4_Alc_Tot, e5_Alc_tot, e6_alc_item_sum) %>% 
  rename(oh1 = antalc,
         oh2 = e2_Alc_Tot,
         oh3 = e3_Alc_Tot,
         oh4 = e4_Alc_Tot,
         oh5 = e5_Alc_tot,
         oh6 = e6_alc_item_sum)

rs2_oh <- rs2_oh %>% 
  select(ergoid, ep_Alc_Tot, e4_Alc_Tot, e5_Alc_tot,e6_alc_item_sum) %>% 
  rename(
    oh3 = ep_Alc_Tot,
    oh4 = e4_Alc_Tot,
    oh5 = e5_Alc_tot,
    oh6 = e6_alc_item_sum)

rs3_oh <- rs3_oh %>% 
  select(ergoid, ej_Alc_tot, e5_Alc_tot) %>% 
  rename(
    oh4 = ej_Alc_tot,
    oh5 = e5_Alc_tot)

### bind together

alcohol <- rs1_oh %>% 
  bind_rows(rs2_oh) %>% 
  bind_rows(rs3_oh)

label(alcohol) <- as.list(c("Identification",rep("Total alcohol intake g/day",6))) 

export(oh, here::here("02_clean_data", "alcohol.Rdata"))



### download from the wiki: https://epi-wiki.erasmusmc.nl/wiki/ergowiki/index.php/Hypertension
ht <- import(here::here("00_raw_data", "hypertension", "HT2018_analysisfile_(15-may-2018).sav"))

ht <- ht %>% 
  select (ergoid, rs_cohort, e1_systolicBP, e1_HT2018, e2_systolicBP, e2_HT2018, e3_systolicBP, e3_HT2018, e4_systolicBP, e4_HT2018, e5_systolicBP, e5_HT2018, ep_systolicBP, ep_HT2018, ej_systolicBP, ej_HT2018,e6_systolicBP, e6_HT2018)

ht1 <- ht %>%
  filter(rs_cohort == 1) %>%
  select(ergoid, contains("systolicBP"), contains("HT2018")) %>%
  rename(
    sbp1 = e1_systolicBP,
    sbp2 = e2_systolicBP,
    sbp3 = e3_systolicBP,
    sbp4 = e4_systolicBP,
    sbp5 = e5_systolicBP,
    sbp6 = e6_systolicBP,
    ht1 = e1_HT2018,
    ht2 = e2_HT2018,
    ht3 = e3_HT2018,
    ht4 = e4_HT2018,
    ht5 = e5_HT2018,
    ht6 = e6_HT2018
  )


ht2 <- ht %>% 
  filter(rs_cohort == 2) %>%
  select(ergoid, contains("systolicBP"), contains("HT2018")) %>%
  rename(
    sbp3 = ep_systolicBP,
    sbp4 = e4_systolicBP,
    sbp5 = e5_systolicBP,
    sbp6 = e6_systolicBP,
    ht3 = ep_HT2018,
    ht4 = e4_HT2018,
    ht5 = e5_HT2018,
    ht6 = e6_HT2018
  )

ht3 <- ht %>% 
  filter(rs_cohort == 3) %>%
  select(ergoid, contains("systolicBP"), contains("HT2018")) %>%
  rename(
    sbp4 = ej_systolicBP,
    sbp5 = e5_systolicBP,
    ht4 = ej_HT2018,
    ht5 = e5_HT2018
  )
  
hypertension <- ht1 %>% 
  bind_rows(ht2) %>% 
  bind_rows(ht3) %>%
  select(ergoid, starts_with("sbp"), starts_with("ht"))

label(hypertension)<- as.list(c("Identification",rep("Systolic Blood Pressure mmHg",6),rep("Hypertension, resting BP > 140/90 or taking BP lowering meds",6)))

export(hypertension, here::here("02_clean_data", "hypertension.RData"))


# Lipids ------------------------------------------------------------------

chol1_1 <- import(here::here("00_raw_data", "cholesterol",  "e1_CHOLESTEROL_(10.03.2010).sav"))
chol1_3 <- import(here::here("00_raw_data", "cholesterol",  "e3_(3)_LAB_(10-mar-2010).sav"))
chol1_4 <- import(here::here("00_raw_data", "cholesterol",  "e4_(4)_LAB_(10-mar-2010)b.sav"))
chol1_5 <- import(here::here("00_raw_data", "cholesterol",  "e5_(5)_LAB_(29-aug-2014)r.sav"))
chol2_1 <- import(here::here("00_raw_data", "cholesterol",  "ep_(1)_LAB_(15-mar-2010).sav"))
chol3_1 <- import(here::here("00_raw_data", "cholesterol",  "ej_(1)_LAB_(11-jun-2009)r.sav"))

#chol1_6 <- import("./Cholesterol/e6_(6)_LAB_(10-jun-2016)r.sav") # No cholesterol data in that dataset yet, also not on the wiki yet

#Separate chol1_5 into rs1, rs2, rs3

chol3_2 <- chol1_5 %>%
  filter(rs_cohort == 3)

chol2_3 <- chol1_5 %>%
  filter(rs_cohort == 2)

chol1_5 <- chol1_5 %>%
  filter(rs_cohort == 1)

#separate chol1_4 into rs1 and rs2
chol2_2 <- chol1_4 %>%
  filter(rs_cohort == 2)

chol1_4 <- chol1_4 %>%
  filter(rs_cohort == 1)


#separate the main variables
chol1_1 <- chol1_1 %>% 
  select(ergoid, e1_al7_chl, e1_al7_hdl) %>%
  rename(chol1)
  setnames(old = c("e1_al7_chl", "e1_al7_hdl"), new=c("chol1", "hdl1"))

chol1_3 <- chol1_3 %>% 
  select(ergoid, e3_3845, e3_4107 ) %>%
  setnames(old = c("e3_3845", "e3_4107"), new=c("chol3", "hdl3"))

chol1_4 <- chol1_4 %>% 
  select(ergoid, rs_cohort, e4_3845, e4_4107 ) %>%
  setnames(old = c("e4_3845", "e4_4107"), new=c("chol4", "hdl4"))

chol1_5 <- chol1_5 %>% 
  select(ergoid, rs_cohort, e5_3845, e5_4107 ) %>%
  setnames(old = c("e5_3845", "e5_4107"), new=c("chol5", "hdl5"))

#chol1_6 <- chol1_6 %>% 
#  select(ergoid, rs_cohort, e6_3845, e6_4107 ) %>%
#  setnames(old = c("e6_3845", "e6_4107"), new=c("chol6", "hdl6"))

chol2_1 <- chol2_1 %>% 
  select(ergoid, ep_3845, ep_4107) %>%
  setnames(old = c("ep_3845", "ep_4107"), new=c("chol3", "hdl3"))

chol3_1 <- chol3_1 %>% 
  select(ergoid, ej_3845, ej_4107) %>%
  setnames(old = c("ej_3845", "ej_4107"), new=c("chol4", "hdl4"))


#separate chol1_6 into rs1 and rs2

#chol2_4 <- chol1_6 %>% 
#  filter(rs_cohort == 2) %>%
#  select(ergoid,chol6, hdl6)

#chol1_6 <- chol1_6 %>% 
#  filter(rs_cohort == 1) %>%
#  select(ergoid,chol6, hdl6)


### Merge cohorts for rs1
chol1 <- list(chol1_1, chol1_3, chol1_4, chol1_5)#, chol1_6)

rs1_chol <- reduce(chol1, left_join, by = c("ergoid"))

dim(rs1_chol)

### Merge cohorts for rs2
chol2 <- list(chol2_1, chol2_2, chol2_3)#,chol2_4)

rs2_chol <- reduce(chol2, left_join, by = c("ergoid"))

dim(rs2_chol)

### Merge cohorts for rs3
chol3 <- list(chol3_1, chol3_2)

rs3_chol <- reduce(chol3, left_join, by = c("ergoid"))

dim(rs3_chol)

### bind together

rs_chol <- rs1_chol %>% 
  bind_rows(rs2_chol) %>% 
  bind_rows(rs3_chol) %>% 
  select(ergoid, chol1, chol3, chol4, chol5, hdl1, hdl3, hdl4, hdl5)

label(rs_chol) <- as.list(c("Identification",rep("Cholesterol in serum (mmol/l)",4),rep("HDL-cholesterol in serum (mmol/l)",4)))
