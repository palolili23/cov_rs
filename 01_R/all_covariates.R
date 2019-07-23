# Open library and set direction ------------------------------------------
library(easypackages)
libraries("dplyr", "ggplot2", "purrr", "data.table", "statar", "lubridate", "data.table",
          "zoo", "rio", "tableone", "VIM", "tidyr", "fastDummies", "here", "janitor", "stringr", "readr")

set_here()
here()

# VISITS: Generates complete_db ---------------------------------------------------------
#visits downloaded from wiki

rs1_1 <- import("V:/Uitwissel/Paloma/covariates_project/visits/Ergo1ResponseDetail_(22-jan-2015)_excerpt.sav")
rs1_2 <- import("V:/Uitwissel/Paloma/covariates_project/visits/Ergo2ResponseDetail_(22-jan-2015)_excerpt.sav")
rs1_3 <- import("V:/Uitwissel/Paloma/covariates_project/visits/e3_(3)_RESPONS_(22-feb-2016)_excerpt.sav")
rs1_4 <- import("V:/Uitwissel/Paloma/covariates_project/visits/e4_(4)_RESPONS_(12-mar-2018)_excerpt.sav")
rs1_5 <- import("V:/Uitwissel/Paloma/covariates_project/visits/e5_(5)_RESPONS_(22-jun-2016)_excerpt.sav")
rs1_6 <- import("V:/Uitwissel/Paloma/covariates_project/visits/e6_(6)_RESPONS_(10-feb-2017)_EXCERPT.sav")
rs2_1 <- import("V:/Uitwissel/Paloma/covariates_project/visits/ep_(1)_RESPONS_(15-jan-2019)_excerpt.sav")
rs3_1 <- import("V:/Uitwissel/Paloma/covariates_project/visits/ej_(1)_RESPONS_(04-apr-2016)_excerpt.sav")

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

#rs2_4 <- rs1_6 %>%
#filter(rs_cohort == 2)

#rs1_6 <- rs1_6 %>%
# filter(rs_cohort == 1)


#### Merge rs1

rs1 <- list(rs1_1, rs1_2, rs1_3, rs1_4, rs1_5)#rs1_6)

rs1_vis <- reduce(rs1, left_join, by = c("ergoid", "rs_cohort"))

### Filter in rs1_vis the dates we want

rs1 <- rs1_vis %>%
  select(ergoid, rs_cohort, e1_aintdat, e1_acen1dat, e1_acen2dat, e2_bcendat, e3_3493, e3_3494, e3_3495, e4_3493, e4_3494, e4_3495, e5_3493, e5_3494, e5_3495) # e6_3493, e6_3494, e6_3495)

rs1_interview <- rs1_vis %>%
  select(ergoid, rs_cohort, e1_aintdat, e2_bcendat, e3_3493, e4_3493, e5_3493)#e6_3493)

### merge r2 visits

rs2 <- list(rs2_1, rs2_2, rs2_3)

rs2_vis <- reduce(rs2, left_join, by = c("ergoid", "rs_cohort"))

rs2_interview <- rs2_vis %>%
  select(ergoid, rs_cohort , ep_3493, e4_3493, e5_3493)

#### Merge rs3 visits

rs3_vis <- rs3_1  %>%
  left_join(rs3_2, by = c("ergoid", "rs_cohort"))

rs3_interview <- rs3_vis %>%
  select(ergoid, rs_cohort , ej_3493, e5_3493)

### rename variables
setnames(rs1_interview, old = c("e1_aintdat", "e2_bcendat", "e3_3493", "e4_3493", "e5_3493"), new = c("e1", "e2", "e3", "e4", "e5"))

setnames(rs2_interview, old = c("ep_3493", "e4_3493", "e5_3493"), new = c("e1", "e2", "e3"))

setnames(rs3_interview, old = c("ej_3493", "e5_3493"), new = c("e1", "e2"))

##merge cohorts

complete_db <- rs1_interview %>% 
  bind_rows(rs2_interview) %>% 
  bind_rows(rs3_interview)

rm(list=ls()[! ls() %in% c("complete_db")])

# BASIC ------------------------------------------------------------
#basic downloaded from wiki
basic <- import("V:/Uitwissel/Paloma/covariates_project/basic/RoterdamStudy_Basics2014.sav")

basic <- basic %>%
  select(ergoid, rs_cohort, sex, date_of_birth, startdat) %>%
  mutate(age_0 = (startdat - date_of_birth)/365.25) %>% 
  mutate(age_0 = as.numeric(round(age_0, digits = 2)))

complete_db <- complete_db %>%
  left_join(basic, by = c("ergoid", "rs_cohort"))

rm(list=ls()[! ls() %in% c("complete_db")])


# MMSE --------------------------------------------------------------------
#mmse from the V drive
mmse1 <- import("V:/HomeDir/040609(Paloma Rojas)/My Documents/Projects/Data/Cognition/MMSE_GMS_RS-I-1-6_update2015.sav")
mmse2 <- import("V:/HomeDir/040609(Paloma Rojas)/My Documents/Projects/Data/Cognition/MMSE_GMS_RS-II-1-4_update2017.sav")
mmse3 <- import("V:/HomeDir/040609(Paloma Rojas)/My Documents/Projects/Data/Cognition/MMSE_GMS_RS-III-1-2_update2015.sav")

### select mmse data from mmse datasets

mmse1 <- mmse1 %>%
  select(ergoid, ammse, bmmse, cmmse, dmmse, emmse)

mmse2 <- mmse2 %>%
  select(ergoid, MMSE_RS_II_1, MMSE_RS_II_2, MMSE_RS_II_3)

mmse3 <- mmse3 %>%
  select(ergoid, MMSE_RS_III_1, MMSE_RS_III_2)

### renaming three cohorts

setnames(mmse1, old = c("ammse", "bmmse", "cmmse", "dmmse", "emmse" ), new = c("mmse1", "mmse2", "mmse3", "mmse4", "mmse5"))

setnames(mmse2, old = c("MMSE_RS_II_1", "MMSE_RS_II_2", "MMSE_RS_II_3"), new = c("mmse1", "mmse2", "mmse3"))

setnames(mmse3, old = c("MMSE_RS_III_1", "MMSE_RS_III_2"), new = c("mmse1", "mmse2"))

##merge cohorts

mmse <- mmse1 %>% 
  bind_rows(mmse2) %>% 
  bind_rows(mmse3)

##merge with complete database

complete_db <- complete_db %>%
  left_join(mmse, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])

# DEMENTIA -------------------------------------------------
#dementia from the V drive
dem <- import("V:/HomeDir/040609(Paloma Rojas)/My Documents/Projects/Data/Cognition/Dementia_RSI-II-III_20171214.sav")
dem <- dem %>%
  select(ergoid, dementia_prevalent, dementia_incident, dementia_date) %>% 
  rename(dementia_prev = dementia_prevalent, dementia_inc = dementia_incident) %>% 
  mutate(dementia_inc = ifelse(is.na(dementia_inc), 2, dementia_inc))

complete_db <- complete_db %>%
  left_join(dem, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])
# DEMENTIA CI -------------------------------------------------------------
dem_CI <- import("V:/HomeDir/040609(Paloma Rojas)/My Documents/Projects/Data/Cognition/IC_ok_dementia.sav")
complete_db <- complete_db %>%
  left_join(dem_CI, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])
# VITAL STATUS ---------------------------------------------------------------
#mortality downloaded from wiki
mort <- import("V:/Uitwissel/Paloma/Data/Mortality/fp_VitalStatus_(24-MAY-2018).sav")
mort <- mort %>%
  select(ergoid, fp_mortdat, fp_censordate) %>% 
  rename(mort_date = fp_mortdat, censor_date = fp_censordate)

complete_db <- complete_db %>%
  left_join(mort, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])


# APOE--------------------------------------------------------------------
apoe <- import("V:/HomeDir/040609(Paloma Rojas)/My Documents/Projects/Data/Cognition/Apoe_RS.sav")
apoe <- apoe %>%
  select(ergoid, apoe4_RSI) %>% 
  rename(apoe4 = apoe4_RSI)

complete_db <- complete_db %>%
  left_join(apoe, by = c("ergoid"))
 
rm(list=ls()[! ls() %in% c("complete_db")])
# BMI ---------------------------------------------------------------------
#bmi downloaded from wiki
bmi_1 <- import("V:/Uitwissel/Paloma/Data/Anthro/e1_ANTHROPO_(15-jun-2011).sav")
bmi_2 <- import("V:/Uitwissel/Paloma/Data/Anthro/e2_(2)_ANTHROPO_(26-apr-2011).sav")
bmi_3 <- import("V:/Uitwissel/Paloma/Data/Anthro/e3_(3)_HARTVAAT_(25-feb-2013)_ANTHROPO.sav")
bmi_4 <- import("V:/Uitwissel/Paloma/Data/Anthro/e4_(4)_UITSCHR_(06-nov-2014)_ANTHROPO-PART.sav")
bmi_5 <- import("V:/Uitwissel/Paloma/Data/Anthro/e5_(5)_ANTHROPO_(10-dec-2015).sav")
bmi_6 <- import("V:/Uitwissel/Paloma/Data/Anthro/e6_(6)_ANTHROPO_(25-apr-2017).sav")
bmi2_ep_1 <- import("V:/Uitwissel/Paloma/Data/Anthro/ep_(1)_LICHONDZ_(18-oct-2012)_ANTHROPO.sav")
bmi3_ej_1 <- import("V:/Uitwissel/Paloma/Data/Anthro/ej_(1)_UITSCHR_(23-feb-2010)_ANTHROPO-PART.sav")

#separate bmi4 into rs1 and rs2
bmi2_2 <- bmi_4 %>%
  filter(rs_cohort == 2)

bmi1_4 <- bmi_4 %>%
  filter(rs_cohort == 1)

#2. Separate bmi_5 into rs1, rs2, rs3
bmi3_2 <- bmi_5 %>%
  filter(rs_cohort == 3)

bmi2_3 <- bmi_5 %>%
  filter(rs_cohort == 2)

bmi1_5 <- bmi_5 %>%
  filter(rs_cohort == 1)

### Merge cohorts for rs1
bmi1 <- list(bmi_1, bmi_2, bmi_3, bmi1_4, bmi1_5)

rs1_bmi <- reduce(bmi1, left_join, by = c("ergoid"))

dim(rs1_bmi)

### Merge cohorts for rs2
bmi2 <- list(bmi2_ep_1, bmi2_2, bmi2_3)
rs2_bmi <- reduce(bmi2, left_join, by = c("ergoid"))

###Merge cohorts for rs3

bmi3 <- bmi3_ej_1 %>%
  left_join(bmi3_2, by= c("ergoid"))

#### Select main variables from each set

rs1_bmi <- rs1_bmi%>%
  select(ergoid, e1_aahgt, e1_aawgt, e2_229, e2_230, e3_229, e3_230, e4_229, e4_230, e5_229, e5_230)

rs2_bmi <- rs2_bmi%>%
  select(ergoid, ep_229, ep_230, e4_229, e4_230, e5_229, e5_230)

rs3_bmi <- bmi3%>%
  select(ergoid, ej_229, ej_230, e5_229, e5_230)

### rename
setnames(rs1_bmi, old = c("e1_aahgt", "e2_229", "e3_229", "e4_229","e5_229", "e1_aawgt","e2_230" ,"e3_230", "e4_230","e5_230"), new = c("hgt1", "hgt2", "hgt3", "hgt4", "hgt5", "wgt1", "wgt2", "wgt3", "wgt4", "wgt5"))

setnames(rs2_bmi, old = c("ep_229", "e4_229", "e5_229", "ep_230", "e4_230", "e5_230"), new = c("hgt1", "hgt2", "hgt3", "wgt1", "wgt2", "wgt3"))

setnames(rs3_bmi, old = c("ej_229", "e5_229", "ej_230", "e5_230"), new = c("hgt1", "hgt2", "wgt1", "wgt2"))

### bind bmi

rs_bmi <- rs1_bmi %>% 
  bind_rows(rs2_bmi) %>% 
  bind_rows(rs3_bmi)

rs_bmi <- rs_bmi %>%
  mutate(bmi1 = (wgt1/((hgt1/100)^2))) %>%
  mutate(bmi2 = (wgt2/((hgt2/100)^2))) %>%
  mutate(bmi3 = (wgt3/((hgt3/100)^2))) %>%
  mutate(bmi4 = (wgt4/((hgt4/100)^2))) %>%
  mutate(bmi5 = (wgt5/((hgt5/100)^2)))

rs_bmi <- rs_bmi %>% 
  select(ergoid, bmi1, bmi2, bmi3, bmi4, bmi5)

dim(rs_bmi)

###merge with complete db

complete_db <- complete_db %>%
  left_join(rs_bmi, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])


# Education ---------------------------------------------------------------
educ <- import("V:/Uitwissel/Paloma/Data/Education/education_RS1_RS2_RS3_seven_and_three_point_scale.sav")

educ <- educ %>% 
  select(ergoid, education_three_levels)

complete_db <- complete_db %>%
  left_join(educ, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])

# SMOKE ANYTHING -----------------------------------------------------------------
smoke1_1 <- import("V:/Uitwissel/Paloma/Data/Smoking/e1_intvw_SMOKING_(23-nov-2011).sav")
smoke1_2 <- import("V:/Uitwissel/Paloma/Data/Smoking/e2_intvw_SMOKING_(23-nov-2011).sav")
smoke1_3 <- import("V:/Uitwissel/Paloma/Data/Smoking/e3_intvw_SMOKING_(11-nov-2011).sav")
smoke1_4 <- import("V:/Uitwissel/Paloma/Data/Smoking/e4_intvw_SMOKING_(04-nov-2011).sav")
smoke1_5 <- import("V:/Uitwissel/Paloma/Data/Smoking/e5_intvw_SMOKING_(04-sep-2014).sav")
smoke2_1 <- import("V:/Uitwissel/Paloma/Data/Smoking/ep_intvw_SMOKING_(30-sep-2011).sav")
smoke3_1 <- import("V:/Uitwissel/Paloma/Data/Smoking/ej_intvw_SMOKING_(28-mar-2011).sav")

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

### Merge cohorts for rs1
smoke1 <- list(smoke1_1, smoke1_2, smoke1_3, smoke1_4, smoke1_5)

rs1_smoke<- reduce(smoke1, left_join, by = c("ergoid"))

### Merge cohorts for rs2
smoke2 <- list(smoke2_1, smoke2_2, smoke2_3)

rs2_smoke <- reduce(smoke2, left_join, by = c("ergoid"))

###Merge cohorts for rs3

rs3_smoke <- smoke3_1 %>%
  left_join(smoke3_2, by= c("ergoid"))

rs1_cig <- rs1_cig %>%
  select(ergoid, e1_ai7_20, e1_ai7_30, e2_b0cg, e2_b0ct, e2_b0pi, e3_cicg, e3_cipi, e3_cict, e3_cictps, e4_dicg, e4_dipi, e4_dict, e5_EILF6, e5_EILFE, e5_EILF4, e5_EILF5)

########### Select variables and code smoking 0 never, 1 former, 2 current, 3 missing
rs1_smoke <- rs1_smoke %>%
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
         smoke5 = ifelse((e5_EILF6 == 1 | e5_EILF5 == 2 | e5_EILF4 == 4 | e5_EILF4 == 5 | e5_EILF4 == 6), 2, smoke5)) %>%
  select(ergoid, smoke1, smoke2, smoke3, smoke4, smoke5)

glimpse(rs1_smoke)
error <- rs1_smoke %>% 
  filter(ergoid == 574001 | ergoid == 1944001 | ergoid == 3603002 | ergoid == 3607001 | ergoid == 3796002) %>% 
  select(ergoid, e1_ai7_20, e1_ai7_30, e2_b0cg, e2_b0ct, e2_b0pi, e3_cicg, e3_cipi, e3_cict, e3_cictps, e4_dicg, e4_dipi, e4_dict, e5_EILF6, e5_EILFE, e5_EILF4, e5_EILF5, starts_with("smoke"))

error <- error %>% 
  mutate(smoke2 = ifelse(e2_b0cg == 3 & e2_b0pi == 3 & e2_b0ct == 3, 0, NA), 
         smoke2 = ifelse((e2_b0cg == 2 | e2_b0pi == 2 | e2_b0ct == 2), 1, smoke2),
         smoke2 = ifelse((e2_b0cg == 1 | e2_b0pi == 1 | e2_b0ct == 1), 2, smoke2))
glimpse(error)

pattern_smoke_rs1<- rs1_smoke %>% 
  filter(!is.na(smoke1)) %>% 
  mutate(smoke2 = ifelse(is.na(smoke2), smoke1, smoke2),
         smoke3 = ifelse(is.na(smoke3), smoke2, smoke3),
         smoke4 = ifelse(is.na(smoke4), smoke3, smoke4),
         smoke5 = ifelse(is.na(smoke5), smoke4, smoke5)) %>% 
  count(smoke1, smoke2, smoke3, smoke4, smoke5)

######## rs2
rs2_smoke <- rs2_smoke %>%
  mutate(smoke1 = ifelse(ep_lf4 == 0 & ep_lf5 == 0 & ep_lf6 == 0 & ep_lf6e == 0, 0, NA),
         smoke1 = ifelse((ep_lf4 == 1 | ep_lf5 == 1 | (ep_lf6 == 0 & ep_lf6e == 1)), 1, smoke1),
         smoke1 = ifelse((ep_lf4 == 2 | ep_lf5 == 2 | ep_lf6 == 1), 2, smoke1),
         smoke2 = ifelse(e4_dicg == 0 & e4_dipi == 0 & e4_dict == 0, 0, NA), 
         smoke2 = ifelse((e4_dicg == 1 |e4_dipi == 1 | e4_dict == 2 | e4_dict == 3), 1, smoke2), 
         smoke2 = ifelse((e4_dicg == 2 |e4_dicg == 3 | e4_dipi == 2 | e4_dipi == 3 | e4_dict == 1), 2, smoke2),
         smoke3 = ifelse(e5_EILF6 == 0 & e5_EILFE == 0 & e5_EILF5 == 0 & e5_EILF4 == 0, 0, NA), 
         smoke3 = ifelse(((e5_EILF6 == 0 & e5_EILFE == 1)| e5_EILF5 == 1 | e5_EILF4 == 1 | e5_EILF4 == 2 | e5_EILF4 == 3), 1, smoke3),
         smoke3 = ifelse((e5_EILF6 == 1 | e5_EILF5 == 1 | e5_EILF4 == 4 | e5_EILF4 == 5 | e5_EILF4 == 6), 2, smoke3)) %>%
  select(ergoid, smoke1, smoke2, smoke3)

############## rs3

rs3_smoke <- rs3_smoke %>%
  mutate(smoke1 = ifelse(ej_yilf4 == 0 & ej_yilf5 == 0 & ej_yilf6 == 0 & ej_yilfe == 0, 0, NA),
         smoke1 = ifelse((ej_yilf4 == 1 | ej_yilf4 == 2 | ej_yilf4 == 3 | ej_yilf5 == 1 | (ej_yilf6 == 0 & ej_yilfe == 1)), 1, smoke1),
         smoke1 = ifelse((ej_yilf4 == 4 | ej_yilf4 == 5 | ej_yilf4 == 6 | ej_yilf5 == 2 | ej_yilf6 == 1), 2, smoke1),
         smoke2 = ifelse(e5_EILF6 == 0 & e5_EILFE == 0 & e5_EILF5 == 0 & e5_EILF4 == 0, 0, NA),
         smoke2 = ifelse(((e5_EILF6 == 0 & e5_EILFE == 1)| e5_EILF5 == 1 | e5_EILF4 == 1 | e5_EILF4 == 2 | e5_EILF4 == 3), 1, smoke2),
         smoke2 = ifelse((e5_EILF6 == 1 | e5_EILF5 == 1 | e5_EILF4 == 4 | e5_EILF4 == 5 | e5_EILF4 == 6), 2, smoke2)) %>% 
  select(ergoid, smoke1, smoke2)

### binding all

rs_smoke <- rs1_smoke %>% 
  bind_rows(rs2_smoke) %>% 
  bind_rows(rs3_smoke)

dim(rs_smoke)

###merge with complete db

complete_db <- complete_db %>%
  left_join(rs_smoke, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])

# SMOKE CIG ---------------------------------------------------------------
smoke1_1 <- import("V:/Uitwissel/Paloma/Data/Smoking/e1_intvw_SMOKING_(23-nov-2011).sav")
smoke1_2 <- import("V:/Uitwissel/Paloma/Data/Smoking/e2_intvw_SMOKING_(23-nov-2011).sav")
smoke1_3 <- import("V:/Uitwissel/Paloma/Data/Smoking/e3_intvw_SMOKING_(11-nov-2011).sav")
smoke1_4 <- import("V:/Uitwissel/Paloma/Data/Smoking/e4_intvw_SMOKING_(04-nov-2011).sav")
smoke1_5 <- import("V:/Uitwissel/Paloma/Data/Smoking/e5_intvw_SMOKING_(04-sep-2014).sav")
smoke2_1 <- import("V:/Uitwissel/Paloma/Data/Smoking/ep_intvw_SMOKING_(30-sep-2011).sav")
smoke3_1 <- import("V:/Uitwissel/Paloma/Data/Smoking/ej_intvw_SMOKING_(28-mar-2011).sav")

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

### Merge cohorts for rs1
smoke1 <- list(smoke1_1, smoke1_2, smoke1_3, smoke1_4, smoke1_5)

rs1_cig<- reduce(smoke1, left_join, by = c("ergoid"))

### Merge cohorts for rs2
smoke2 <- list(smoke2_1, smoke2_2, smoke2_3)

rs2_cig <- reduce(smoke2, left_join, by = c("ergoid"))

###Merge cohorts for rs3

rs3_cig <- smoke3_1 %>%
  left_join(smoke3_2, by= c("ergoid"))

########### Select variables and code smoking 0 never, 1 former, 2 current, 3 missing
rs1_cig <- rs1_cig %>%
  mutate(smoke_cig1 = ifelse((e1_ai7_20 == 0 & e1_ai7_30 == 0), 0, NA),
         smoke_cig1 = ifelse((e1_ai7_20 == 0 & e1_ai7_30 == 1), 1, smoke_cig1),
         smoke_cig1 = ifelse(e1_ai7_20 == 1, 2, smoke_cig1),
         smoke_cig2 = ifelse(e2_b0ct == 3, 0, NA),
         smoke_cig2 = ifelse(e2_b0ct == 2, 1, smoke_cig2),
         smoke_cig2 = ifelse(e2_b0ct == 1, 2, smoke_cig2),
         smoke_cig3 = ifelse((e3_cict == 0 & e3_cictps == 0), 0, NA), 
         smoke_cig3 = ifelse((e3_cict == 0 & e3_cictps == 1), 1, smoke_cig3),
         smoke_cig3 = ifelse(e3_cict == 1, 2, smoke_cig3),
         smoke_cig4 = ifelse(e4_dict == 0, 0, NA),
         smoke_cig4 = ifelse((e4_dict == 2 | e4_dict == 3), 1, smoke_cig4),
         smoke_cig4 = ifelse(e4_dict == 1, 2, smoke_cig4),
         smoke_cig5 = ifelse(e5_EILF6 == 0 & e5_EILFE == 0, 0, NA),
         smoke_cig5 = ifelse((e5_EILF6 == 0 & e5_EILFE == 1), 1, smoke_cig5),
         smoke_cig5 = ifelse(e5_EILF6 == 1, 2, smoke_cig5)) %>% 
  select(ergoid, smoke_cig1, smoke_cig2, smoke_cig3, smoke_cig4, smoke_cig5)

pattern_cig_rs1<- rs1_cig %>% 
  filter(!is.na(smoke_cig1)) %>% 
  mutate(smoke_cig2 = ifelse(is.na(smoke_cig2), smoke_cig1, smoke_cig2),
         smoke_cig3 = ifelse(is.na(smoke_cig3), smoke_cig2, smoke_cig3),
         smoke_cig4 = ifelse(is.na(smoke_cig4), smoke_cig3, smoke_cig4),
         smoke_cig5 = ifelse(is.na(smoke_cig5), smoke_cig4, smoke_cig5)) %>% 
  count(smoke_cig1, smoke_cig2, smoke_cig3, smoke_cig4, smoke_cig5)

######## rs2
rs2_cig <- rs2_cig %>%
  mutate(smoke_cig1 = ifelse((ep_lf6 == 0 & ep_lf6e == 0), 0, NA),
         smoke_cig1 = ifelse((ep_lf6 == 0 & ep_lf6e == 1), 1, smoke_cig1),
         smoke_cig1 = ifelse(ep_lf6 == 1, 2, smoke_cig1),
         smoke_cig2 = ifelse(e4_dict == 0, 0, NA),
         smoke_cig2 = ifelse((e4_dict == 2 | e4_dict == 3), 1, smoke_cig2),
         smoke_cig2 = ifelse(e4_dict == 1, 2, smoke_cig2),
         smoke_cig3 = ifelse(e5_EILF6 == 0 & e5_EILFE == 0, 0, NA),
         smoke_cig3 = ifelse((e5_EILF6 == 0 & e5_EILFE == 1), 1, smoke_cig3),
         smoke_cig3 = ifelse(e5_EILF6 == 1, 2, smoke_cig3))%>%
  select(ergoid, smoke_cig1, smoke_cig2, smoke_cig3)

pattern_cig_rs2<- rs2_cig %>% 
  filter(!is.na(smoke_cig1)) %>% 
  mutate(smoke_cig2 = ifelse(is.na(smoke_cig2), smoke_cig1, smoke_cig2),
         smoke_cig3 = ifelse(is.na(smoke_cig3), smoke_cig2, smoke_cig3)) %>% 
  count(smoke_cig1, smoke_cig2, smoke_cig3)
############## rs3


rs3_cig <- rs3_cig %>%
  mutate(smoke_cig1 = ifelse(ej_yilf6 == 0 & ej_yilfe == 0, 0, NA),
         smoke_cig1 = ifelse(ej_yilf6 == 0 & ej_yilfe == 1, 1, smoke_cig1),
         smoke_cig1 = ifelse(ej_yilf6 == 1, 2, smoke_cig1),
         smoke_cig2 = ifelse((e5_EILF6 == 0 & e5_EILFE == 0), 0, NA),
         smoke_cig2 = ifelse((e5_EILF6 == 0 & e5_EILFE == 1), 1, smoke_cig2),
         smoke_cig2 = ifelse(e5_EILF6 == 1, 2, smoke_cig2)) %>%
  select(ergoid, smoke_cig1, smoke_cig2)

pattern_cig_rs3<- rs3_cig %>% 
  filter(!is.na(smoke_cig1)) %>% 
  mutate(smoke_cig2 = ifelse(is.na(smoke_cig2), smoke_cig1, smoke_cig2)) %>% 
  count(smoke_cig1, smoke_cig2)

### binding all

rs_cig <- rs1_cig %>% 
  bind_rows(rs2_cig) %>% 
  bind_rows(rs3_cig)

dim(rs_cig)

###merge with complete db

complete_db <- complete_db %>%
  left_join(rs_cig, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])

check <- complete_db %>% 
  filter(!is.na(smoke1) & !is.na(smoke_cig1)) %>% 
  mutate(smoke_cig2 = ifelse(is.na(smoke_cig2), smoke_cig1, smoke_cig2),
         smoke_cig3 = ifelse(is.na(smoke_cig3), smoke_cig2, smoke_cig3),
         smoke_cig4 = ifelse(is.na(smoke_cig4), smoke_cig3, smoke_cig4),
         smoke_cig5 = ifelse(is.na(smoke_cig5), smoke_cig4, smoke_cig5)) %>% 
  mutate(smoke2 = ifelse(is.na(smoke2), smoke1, smoke2),
         smoke3 = ifelse(is.na(smoke3), smoke2, smoke3),
         smoke4 = ifelse(is.na(smoke4), smoke3, smoke4),
         smoke5 = ifelse(is.na(smoke5), smoke4, smoke5))

error <- check %>% 
  filter(smoke2 == 0 & smoke_cig2 != 0)

# ALCOHOL INTAKE ---------------------------------------------------------
oh1_1 <- import("V:/Uitwissel/Paloma/Data/Alcohol/e1_FFQ_AlcoholGramPerday_inclGLAZEN.sav")
oh1_2 <- import("V:/Uitwissel/Paloma/Data/Alcohol/e2_intvw_Alcoholperday_25-10-2013.sav")
oh1_3 <- import("V:/Uitwissel/Paloma/Data/Alcohol/e3_intvw_Alcoholperday_24-10-2017.sav")
oh1_4 <- import("V:/Uitwissel/Paloma/Data/Alcohol/e4_intvw_Alcoholperday_22-11-2013.sav")
oh1_5 <- import("V:/Uitwissel/Paloma/Data/Alcohol/e5intvw_Alcoholperday_11-07-2014.sav")
oh2_1 <- import("V:/Uitwissel/Paloma/Data/Alcohol/ep_intvw_Alcoholperday_22-11-2013.sav")
oh3_1 <- import("V:/Uitwissel/Paloma/Data/Alcohol/ej_intvw_ALCOHOLGRAMSPERDAY_(14072014).sav")

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

### Merge cohorts for rs1
oh1 <- list(oh1_1, oh1_2, oh1_3, oh1_4, oh1_5)

rs1_oh <- reduce(oh1, left_join, by = c("ergoid"))

dim(rs1_oh)

### Merge cohorts for rs2
oh2 <- list(oh2_1, oh2_2, oh2_3)

rs2_oh <- reduce(oh2, left_join, by = c("ergoid"))

dim(rs2_oh)

### Merge cohorts for rs3
oh3 <- list(oh3_1, oh3_2)

rs3_oh <- reduce(oh3, left_join, by = c("ergoid"))

dim(rs3_oh)

### Select alcohol intake

rs1_oh <- rs1_oh %>% 
  select(ergoid, antalc, e2_Alc_Tot, e3_Alc_Tot, e4_Alc_Tot, e5_Alc_tot)

rs2_oh <- rs2_oh %>% 
  select(ergoid, ep_Alc_Tot, e4_Alc_Tot, e5_Alc_tot)

rs3_oh <- rs3_oh %>% 
  select(ergoid, ej_Alc_tot, e5_Alc_tot)

### Rename

setnames(rs1_oh, old = c("antalc", "e2_Alc_Tot", "e3_Alc_Tot", "e4_Alc_Tot", "e5_Alc_tot"), new = c("oh1", "oh2", "oh3", "oh4", "oh5"))

setnames(rs2_oh, old = c("ep_Alc_Tot", "e4_Alc_Tot", "e5_Alc_tot"), new = c("oh1", "oh2", "oh3"))

setnames(rs3_oh, old = c("ej_Alc_tot", "e5_Alc_tot"), new = c("oh1", "oh2"))

### bind together

rs_oh <- rs1_oh %>% 
  bind_rows(rs2_oh) %>% 
  bind_rows(rs3_oh)

sum <- CreateTableOne(data = rs_oh)
summary(sum)

###merge with complete db

complete_db <- complete_db %>%
  left_join(rs_oh, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])

# HYPERTENSION ------------------------------------------------------------
ht <- import("V:/Uitwissel/Paloma/Data/Hypertension/HT2018_analysisfile_(15-may-2018).sav")

ht <- ht %>% 
  select (ergoid, rs_cohort, e1_systolicBP, e1_HT2018, e1_bpldrug,
          e2_systolicBP, e2_HT2018, e2_bpldrug,
          e3_systolicBP, e3_HT2018, e3_bpldrug,
          e4_systolicBP, e4_HT2018, e4_bpldrug,
          e5_systolicBP, e5_HT2018, e5_bpldrug,
          ep_systolicBP, ep_HT2018, ep_bpldrug,
          ej_systolicBP, ej_HT2018, ej_bpldrug)

ht1 <- ht %>% 
  filter(rs_cohort == 1) %>%
  select(ergoid, e1_systolicBP, e1_HT2018, e1_bpldrug, e2_systolicBP, e2_HT2018, e2_bpldrug, e3_systolicBP, e3_HT2018, e3_bpldrug, e4_systolicBP, e4_HT2018,e4_bpldrug, e5_systolicBP, e5_HT2018, e5_bpldrug) %>% 
  setnames(old = c("e1_systolicBP", "e1_HT2018", "e2_systolicBP", "e2_HT2018", "e3_systolicBP", "e3_HT2018", "e4_systolicBP", "e4_HT2018", "e5_systolicBP", "e5_HT2018"), new = c("sbp1", "ht1", "sbp2", "ht2", "sbp3", "ht3", "sbp4", "ht4", "sbp5", "ht5")) %>% 
  rename(ht_drug1 = e1_bpldrug, ht_drug2 = e2_bpldrug, ht_drug3 = e3_bpldrug, ht_drug4 = e4_bpldrug, ht_drug5 = e5_bpldrug)

ht2 <- ht %>% 
  filter(rs_cohort == 2) %>%
  select(ergoid, ep_systolicBP, ep_HT2018, ep_bpldrug, e4_systolicBP, e4_HT2018, e4_bpldrug, e5_systolicBP, e5_HT2018, e5_bpldrug) %>% 
  setnames(old = c("ep_systolicBP", "ep_HT2018", "e4_systolicBP", "e4_HT2018", "e5_systolicBP", "e5_HT2018"), new = c("sbp1", "ht1", "sbp2", "ht2", "sbp3", "ht3")) %>% 
  rename(ht_drug1 = ep_bpldrug, ht_drug2 = e4_bpldrug, ht_drug3 = e5_bpldrug)

ht3 <- ht %>% 
  filter(rs_cohort == 3) %>%
  select(ergoid, ej_systolicBP, ej_HT2018,ej_bpldrug, e5_systolicBP, e5_HT2018, e5_bpldrug) %>% 
  setnames(old = c("ej_systolicBP", "ej_HT2018", "e5_systolicBP", "e5_HT2018"), new = c("sbp1", "ht1", "sbp2", "ht2")) %>% 
  rename(ht_drug1 = ej_bpldrug, ht_drug2 = e5_bpldrug)

rs_ht <- ht1 %>% 
  bind_rows(ht2) %>% 
  bind_rows(ht3) %>%
  select(ergoid, sbp1, sbp2, sbp3, sbp4, sbp5, ht1, ht2, ht3, ht4, ht5, ht_drug1, ht_drug2, ht_drug3, ht_drug4, ht_drug5)

check <- rs_ht %>% 
  filter(!is.na(ht1)) %>% 
  mutate(ht2 = ifelse(is.na(ht2), ht1, ht2),
         ht3 = ifelse(is.na(ht3), ht2, ht3),
         ht4 = ifelse(is.na(ht4), ht3, ht4),
         ht5 = ifelse(is.na(ht5), ht4, ht5)) %>% 
    count(ht1, ht2, ht3, ht4, ht5)

#merge in complete dataset
complete_db <- complete_db %>%
  left_join(rs_ht, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])

# CHOLESTEROL and HDL -------------------------------------------------------------
chol1_1 <- import("V:/Uitwissel/Paloma/Data/Cholesterol/e1_CHOLESTEROL_(10.03.2010).sav")
chol1_3 <- import("V:/Uitwissel/Paloma/Data/Cholesterol/e3_(3)_LAB_(10-mar-2010).sav")
chol1_4 <- import("V:/Uitwissel/Paloma/Data/Cholesterol/e4_(4)_LAB_(10-mar-2010)b.sav")
chol1_5 <- import("V:/Uitwissel/Paloma/Data/Cholesterol/e5_(5)_LAB_(29-aug-2014)r.sav")
chol2_1 <- import("V:/Uitwissel/Paloma/Data/Cholesterol/ep_(1)_LAB_(15-mar-2010).sav")
chol3_1 <- import("V:/Uitwissel/Paloma/Data/Cholesterol/ej_(1)_LAB_(11-jun-2009)r.sav")

#separate the main variables
chol1_1 <- chol1_1 %>% 
  select(ergoid, e1_al7_chl, e1_al7_hdl) %>%
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

chol2_1 <- chol2_1 %>% 
  select(ergoid, ep_3845, ep_4107) %>%
  setnames(old = c("ep_3845", "ep_4107"), new=c("chol1", "hdl1"))

chol3_1 <- chol3_1 %>% 
  select(ergoid, ej_3845, ej_4107) %>%
  setnames(old = c("ej_3845", "ej_4107"), new=c("chol1", "hdl1"))

#Separate chol1_5 into rs1, rs2, rs3

chol3_2 <- chol1_5 %>%
  filter(rs_cohort == 3) %>%
  rename(chol2 = chol5, hdl2 = hdl5) %>% 
  select(ergoid, chol2, hdl2)

chol2_3 <- chol1_5 %>%
  filter(rs_cohort == 2) %>% 
  rename(chol3 = chol5, hdl3 = hdl5) %>% 
  select(ergoid, chol3, hdl3)

chol1_5 <- chol1_5 %>%
  filter(rs_cohort == 1) %>% 
  select(ergoid, chol5, hdl5)

#separate chol1_4 into rs1 and rs2
chol2_2 <- chol1_4 %>%
  filter(rs_cohort == 2) %>% 
  rename(chol2=chol4, hdl2=hdl4) %>% 
  select(ergoid, chol2, hdl2)

chol1_4 <- chol1_4 %>%
  filter(rs_cohort == 1) %>% 
  select(ergoid, chol4, hdl4)


### Merge cohorts for rs1
chol1 <- list(chol1_1, chol1_3, chol1_4, chol1_5)

rs1_chol <- reduce(chol1, left_join, by = c("ergoid"))

dim(rs1_chol)

### Merge cohorts for rs2
chol2 <- list(chol2_1, chol2_2, chol2_3)

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
  select(ergoid, chol1, chol2, chol3, chol4, chol5, hdl1, hdl2, hdl3, hdl4, hdl5)

###merge with complete db

complete_db <- complete_db %>%
  left_join(rs_chol, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])




# DB medication -----------------------------------------------------------
md1_1 <- import("V:/Uitwissel/Paloma/Data/Farmaco/e1_MEDICATION_(08-jan-2010).sav")
md1_3 <- import("V:/Uitwissel/Paloma/Data/Farmaco/e3_MEDICATION_(27-jun-2012).sav")
md2_1 <- import("V:/Uitwissel/Paloma/Data/Farmaco/ep_MEDICATION_(12-jan-2010).sav")
md1_4 <- import("V:/Uitwissel/Paloma/Data/Farmaco/e4_MEDICATION_(20-jan-2010).sav")
md1_5 <- import("V:/Uitwissel/Paloma/Data/Farmaco/e5_MEDICATION_(12-MAR-2015).sav")
md3_1 <- import("V:/Uitwissel/Paloma/Data/Farmaco/ej_MEDICATION_(22-mar-2010).sav")

md1_1 <- md1_1 %>%
  select(ergoid, e1_a10)

md1_3 <- md1_3 %>%
  select(ergoid, e3_a10)

md2_1 <- md2_1 %>%
  select(ergoid, ep_a10)

md3_1 <- md3_1 %>%
  select(ergoid, ej_a10)

#1

md2_2 <- md1_4 %>%
  filter(rs_cohort == 2) %>%
  select(ergoid, e4_a10)

md1_4 <- md1_4 %>%
  filter(rs_cohort == 1) %>%
  select(ergoid, e4_a10)

md3_2 <- md1_5 %>%
  filter(rs_cohort == 3) %>%
  select(ergoid, e5_a10)

md2_3 <- md1_5 %>%
  filter(rs_cohort == 2) %>%
  select(ergoid, e5_a10)

md1_5<- md1_5 %>%
  filter(rs_cohort == 1) %>%
  select(ergoid, e5_a10)

### Merge cohorts for rs1
md1 <- list(md1_1, md1_3, md1_4, md1_5)

rs1_md <- reduce(md1, left_join, by = c("ergoid"))

rs1_md <- rs1_md %>%
  rename(db_med_1 = e1_a10, db_med_3 = e3_a10, db_med_4 = e4_a10, db_med_5 = e5_a10)

### Merge cohorts for rs2
md2 <- list(md2_1, md2_2, md2_3)

rs2_md <- reduce(md2, left_join, by = c("ergoid"))

dim(rs2_md)

rs2_md <- rs2_md %>%
  select(ergoid, db_med_ = ends_with("a10"))


### Merge cohorts for rs3
md3 <- list(md3_1, md3_2)

rs3_md <- reduce(md3, left_join, by = c("ergoid"))

dim(rs3_md)

rs3_md <- rs3_md %>%
  select(ergoid, db_med_ = ends_with("a10"))

### bind together

rs_md <- rs1_md %>%
  bind_rows(rs2_md) %>%
  bind_rows(rs3_md)

rs_md <- rs_md %>%
  select(ergoid, db_med_1, db_med_2, starts_with("db_med")) %>% 
  rename(db_med1 = db_med_1, db_med2 = db_med_2, db_med3 = db_med_3, db_med4 = db_med_4, db_med5 = db_med_5)

rs_md <- rs_md %>% 
  mutate(db_med1 = ifelse((db_med1 == 9), is.na(db_med1), db_med1),
         db_med3 = ifelse((db_med3 == 9), is.na(db_med3), db_med3),
         db_med4 = ifelse((db_med4 == 9), is.na(db_med4), db_med4))

###merge with complete db

complete_db <- complete_db %>%
  left_join(rs_md, by = c("ergoid"))


# Medication all-----------------------------------------------------
# md1_1 <- import("V:/Uitwissel/Paloma/Data/Farmaco/e1_MEDICATION_(08-jan-2010).sav")
# md1_3 <- import("V:/Uitwissel/Paloma/Data/Farmaco/e3_MEDICATION_(27-jun-2012).sav")
# md2_1 <- import("V:/Uitwissel/Paloma/Data/Farmaco/ep_MEDICATION_(12-jan-2010).sav")
# md1_4 <- import("V:/Uitwissel/Paloma/Data/Farmaco/e4_MEDICATION_(20-jan-2010).sav")
# md1_5 <- import("V:/Uitwissel/Paloma/Data/Farmaco/e5_MEDICATION_(12-MAR-2015).sav")
# md3_1 <- import("V:/Uitwissel/Paloma/Data/Farmaco/ej_MEDICATION_(22-mar-2010).sav")

# c01 cardiac therapy, c02 antihypertensives, c03 diuretics, c04 peripheral vasodilatators,
# c05 vasoprotectives, c07 betablockers, c08 calcium blockers, c09 ACE inhibitors

# md1_1 <- md1_1 %>%
#   select(ergoid, e1_a10, e1_c01, e1_c02, e1_c03, e1_c04, e1_c05, e1_c07)
# 
# md_e1 <- md1_1 %>% 
#   count(e1_c01, e1_c02, e1_c03, e1_c04, e1_c05, e1_c07) %>% 
#   arrange(desc(n))
# 
# md1_3 <- md1_3 %>%
#   select(ergoid, e3_a10, e3_c01, e3_c02, e3_c03, e3_c04, e3_c05, e3_c07, e3_c08, e3_c09)
# 
# md_e3 <- md1_3 %>% 
#   count(e3_c01, e3_c02, e3_c03, e3_c04, e3_c05, e3_c07, e3_c08, e3_c09) %>% 
#   arrange(desc(n))
# 
# md2_1 <- md2_1 %>%
#   select(ergoid, ep_a10, ep_c01, ep_c02, ep_c03, ep_c04, ep_c05, ep_c07, ep_c08, ep_c09)
# 
# md_e21 <- md2_1 %>% 
#   count(ep_c01, ep_c02, ep_c03, ep_c04, ep_c05, ep_c07, ep_c08, ep_c09) %>% 
#   arrange(desc(n))
# 
 
# md1_4 <- md1_4 %>%
#   select(ergoid, e4_a10, e4_c01, e4_c02, e4_c03, e4_c04, e4_c05, e4_c07, e4_c08, e4_c09)
# 
# md_e14 <- md1_4 %>% 
#   count(e4_c01, e4_c02, e4_c03, e4_c04, e4_c05, e4_c07, e4_c08, e4_c09)

### Filter from e4 and e5 into cohort 1 and 2

# md1_1 <- md1_1 %>%
#   select(ergoid, e1_a10, e1_c01, e1_c02)
# 
# md1_3 <- md1_3 %>%
#   select(ergoid, e3_a10, e3_c01, e3_c02)
# 
# md2_1 <- md2_1 %>% 
#   select(ergoid, ep_a10, ep_c01, ep_c02)
# 
# md3_1 <- md3_1 %>% 
#   select(ergoid, ej_a10, ej_c01, ej_c02)
# 
# #1
# 
# 
# md2_2 <- md1_4 %>%
#   filter(rs_cohort == 2) %>%
#   select(ergoid, e4_a10, e4_c01, e4_c02)
# 
# md1_4 <- md1_4 %>%
#   filter(rs_cohort == 1) %>%
#   select(ergoid, e4_a10, e4_c01, e4_c02)
# 
# md3_2 <- md1_5 %>%
#   filter(rs_cohort == 3) %>%
#   select(ergoid, e5_a10, e5_c01, e5_c02)
# 
# md2_3 <- md1_5 %>%
#   filter(rs_cohort == 2) %>%
#   select(ergoid, e5_a10, e5_c01, e5_c02)
# 
# md1_5<- md1_5 %>%
#   filter(rs_cohort == 1) %>%
#   select(ergoid, e5_a10, e5_c01, e5_c02)
# 
# ### Merge cohorts for rs1
# md1 <- list(md1_1, md1_3, md1_4, md1_5)
# 
# rs1_md <- reduce(md1, left_join, by = c("ergoid"))
#  
# rs1_md <- rs1_md %>% 
#   rename(cardiac_med_1 = e1_c01, cardiac_med_3 = e3_c01, cardiac_med_4 = e4_c01, cardiac_med_5 = e5_c01,
#          ht_med_1 = e1_c02, ht_med_3 = e3_c02, ht_med_4 = e4_c02, ht_med_5 = e5_c02,
#          db_med_1 = e1_a10, db_med_3 = e3_a10, db_med_4 = e4_a10, db_med_5 = e5_a10) 
# 
# ### Merge cohorts for rs2
# md2 <- list(md2_1, md2_2, md2_3)
# 
# rs2_md <- reduce(md2, left_join, by = c("ergoid"))
# 
# dim(rs2_md)
# 
# rs2_md <- rs2_md %>% 
#   select(ergoid, cardiac_med_ = ends_with("c01"), ht_med_ = ends_with("c02"), db_med_ = ends_with("a10")) 
#   
# 
# ### Merge cohorts for rs3
# md3 <- list(md3_1, md3_2)
# 
# rs3_md <- reduce(md3, left_join, by = c("ergoid"))
# 
# dim(rs3_md)
# 
# rs3_md <- rs3_md %>% 
#   select(ergoid, cardiac_med_ = ends_with("c01"), ht_med_ = ends_with("c02"), db_med_ = ends_with("a10")) 
# 
# ### bind together
# 
# rs_md <- rs1_md %>% 
#   bind_rows(rs2_md) %>% 
#   bind_rows(rs3_md) 
# 
# rs_md <- rs_md %>%
#   select(ergoid, cardiac_med_1, cardiac_med_2, starts_with("cardiac_med"),ht_med_1, ht_med_2, starts_with("ht_med"),
#          db_med_1, db_med_2, starts_with("db_med"))

###merge with complete db

# complete_db <- complete_db %>%
#   left_join(rs_md, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])

# STROKE ------------------------------------------------------------------
stroke <- import("V:/HomeDir/040609(Paloma Rojas)/My Documents/Projects/Data/Cognition/Strokes RSI-III 01-01-2016 (28-08-2017).sav")
stroke <- stroke %>%
  select(ergoid, prev_stroke_2016, incid_stroke_2016, eventdate_2016)%>%
  rename(stroke_prev = prev_stroke_2016, stroke_inc = incid_stroke_2016, stroke_date = eventdate_2016)

complete_db <- complete_db %>%
  left_join(stroke, by = c("ergoid")) 

rm(list=ls()[! ls() %in% c("complete_db")])
# CVD comorbidities (missing cohort 3 for HF, pending data to attach) -----------------------------------------------------------
comorb <- import("V:/HomeDir/040609(Paloma Rojas)/My Documents/Projects/Data/HD/CHD_2014_24.06.2014.dta") ## also 3 cohorts
af <- import("V:/HomeDir/040609(Paloma Rojas)/My Documents/Projects/Data/HD/AFIB_prevalenceANDincidence_morbidityANDmortality_25.02.2016_KLASSIEK.sav") #only contains the cases for cohorts 1 - 3

## create same prev, inc, date variables as with other comorbidities and merge af in comorbidities
af <- af %>% 
  mutate(af_prev = ifelse((fupstat.1 == 0), 1, NA)) %>% 
  mutate(af_prev = ifelse(is.na(af_prev), 0, af_prev)) %>%
  mutate(af_inc = ifelse((fupstat.1 == 1), 1, NA)) %>% 
  mutate(af_inc = ifelse(is.na(af_inc), 0, af_inc)) %>%
  mutate(af_date = ifelse((fupstat.1 == 0), NA, eventdat.1)) %>%
  mutate(af_date = as.Date(af_date)) %>% 
  select(ergoid, af_prev, af_inc, af_date)

### select main variables

comorb <- comorb %>% 
  select(ergoid, prev_CABG_2014, inc_CABG_2014, eventdat_CABG_2014, prev_PCI_2014, inc_PCI_2014,  eventdat_PCI_2014, prev_MI_2014, inc_MI_2014, eventdat_MI_2014, prev_CHD_2014,  inc_totalCHD_2014, enddate_totalCHD_2014)

comorb <- comorb %>% 
  left_join(af, by = c("ergoid"))

###create only one variable (1 is event, 0 no)

comorb <- comorb %>% 
  mutate(hd_prev = ifelse((prev_CABG_2014 == 1 | prev_PCI_2014 == 1 | prev_MI_2014 == 1 | prev_CHD_2014 == 1 | af_prev == 1), 1, NA)) %>%
  mutate(hd_prev = ifelse((prev_CABG_2014 == 0 & prev_PCI_2014 == 0 & prev_MI_2014 == 0 & prev_CHD_2014 == 0 & (af_prev == 0 | is.na(af_prev))), 0, hd_prev))

comorb <- comorb %>% 
  mutate(hd_inc = ifelse((inc_CABG_2014 == 1 | inc_PCI_2014 == 1 | inc_MI_2014 == 1 | af_inc == 1), 1, NA))%>%
  mutate(hd_inc = ifelse((inc_CABG_2014 == 0 & inc_PCI_2014 == 0 & inc_MI_2014 == 0 & (af_inc == 0| is.na(af_inc))), 0, hd_inc))

comorb <- comorb %>% 
  mutate(hd_date = pmin(eventdat_CABG_2014, eventdat_PCI_2014, eventdat_MI_2014, af_date, na.rm = TRUE))

comorb <- comorb %>% 
  select(ergoid, hd_prev, hd_inc, hd_date) 

complete_db <- complete_db %>% 
  left_join(comorb, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])

# CANCER cancer is only available for 1 and 2nd cohort -------------------------------------------------------------

cancer <- import("V:/HomeDir/040609(Paloma Rojas)/My Documents/Projects/Data/cancer/Cancer_prevalent_incident_RSI-II.sav")

cancer <- cancer %>% 
  mutate(cancer_prev = ifelse(!is.na(Prevalent_cancer), 1, 0)) %>%  
  mutate(cancer_inc = ifelse((fupstat == 1 | fupstat == 8 & !is.na(eventdat)), 1, 0)) %>% 
  mutate(cancer_inc = ifelse((is.na(cancer_inc)), 0, cancer_inc)) %>% 
  mutate(cancer_deathdx = ifelse((fupstat == 8 & !is.na(eventdat)),1,0)) %>%
  setnames(old = c("eventdat", "Cancer_type", "ICD10"), new = c("cancer_date", "cancer_type", "cancer_icd10")) %>% 
  select(ergoid, cancer_prev, cancer_inc, cancer_deathdx, cancer_date, cancer_type, cancer_icd10)

complete_db <- complete_db %>% 
  left_join(cancer, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])


# Diabetes dx. ------------------------------------------------------------
diabetes_prev <- import("V:/HomeDir/040609(Paloma Rojas)/My Documents/Projects/Data/Diabetes/Prevalent_DM_RS1-3RS2-1RS3-1.sav") %>% 
  rename(diab_prev = prevalent_DM)

## Date of diabetes incidence only for rs1 and rs2
diabetes_inc <-import("V:/HomeDir/040609(Paloma Rojas)/My Documents/Projects/Data/Diabetes/rs12_diabetes.sav")

diabetes <- diabetes_prev %>%
  left_join(diabetes_inc, by = ("ergoid"))

### There are people with no diabetes and date of incident dm, which corresponds to their censoring date, so erase those dates
### There are some individuals that have diab_prev == 1 but are NA in diab_inc which also includes prevalent,
### I am adding those prev as 1

diabetes <- diabetes %>% 
  mutate(diabetes_date = as.Date(ifelse(incident_DM != 1, NA, incident_DM_date)),
         diabetes_inc = ifelse(incident_DM == 1, 1, 0),
         diabetes_prev = ifelse(incident_DM == 8 | diab_prev == 1, 1, 0)) %>% 
  select(ergoid, starts_with("diabetes"))

complete_db <- complete_db %>%
  left_join(diabetes, by = c("ergoid")) 

rm(list=ls()[! ls() %in% c("complete_db")])

# Export_wide -------------------------------------------------------------

export(complete_db, "V:/HomeDir/040609(Paloma Rojas)/My Documents/Projects/Data/Complete_dataset/db_wide.RData")

# Convert complete_db in LONG ---------------------------------------------
complete_db <- complete_db %>% 
  rename(cardiac_med1 = cardiac_med_1, cardiac_med2 = cardiac_med_2, cardiac_med3 = cardiac_med_3,
         cardiac_med4 = cardiac_med_4, cardiac_med5 = cardiac_med_5,
         db_med1 = db_med_1, db_med2 = db_med_2, db_med3 = db_med_3,
         db_med4 = db_med_4, db_med5 = db_med_5,
         ht_med1 = ht_med_1, ht_med2 = ht_med_2, ht_med3 = ht_med_3,
         ht_med4 = ht_med_4, ht_med5 = ht_med_5)
rename 
tv_var <- c("e1", "e2", "e3", "e4", "e5", "mmse1", "mmse2", "mmse3", "mmse4", "mmse5", 
            "bmi1", "bmi2", "bmi3", "bmi4", "bmi5", 
            "smoke1", "smoke2", "smoke3", "smoke4", "smoke5", 
            "smoke_cig1", "smoke_cig2", "smoke_cig3", "smoke_cig4", "smoke_cig5",
            "oh1", "oh2", "oh3", "oh4", "oh5",
            "sbp1", "sbp2", "sbp3", "sbp4", "sbp5", 
            "ht1", "ht2", "ht3", "ht4", "ht5",
            "chol1", "chol2", "chol3", "chol4", "chol5",
            "hdl1", "hdl2", "hdl3", "hdl4", "hdl5",
            "cardiac_med1", "cardiac_med2", "cardiac_med3", "cardiac_med4", "cardiac_med5",
            "ht_med1", "ht_med2", "ht_med3", "ht_med4", "ht_med5",
            "db_med1", "db_med2", "db_med3", "db_med4", "db_med5")

glimpse(complete_db)
db_long <- reshape(data = complete_db,
                   idvar = "ergoid",
                   varying = tv_var,
                   sep = "",
                   timevar = "visit",
                   times = c(1,2,3,4,5),
                   direction = "long")

# Export wide or long -------------------------------------------------------------
# export(db_long, "~/db_long.RData")
