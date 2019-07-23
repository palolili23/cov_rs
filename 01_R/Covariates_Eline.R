# Open library and set direction ------------------------------------------

setwd("V:\Uitwissel\Eline\CovariatesCovariates/")

#install.packages("tidyr", lib="V:/HomeDir/890029(E. Vinke)/Rpackages")
library("tidyr", lib = "V:/HomeDir/890029(E. Vinke)/Rpackages")
#library("dplyr",lib = "V:/HomeDir/890029(E. Vinke)/Rpackages")
#library("ggplot2")
#install.packages("rio", lib="V:/HomeDir/890029(E. Vinke)/Rpackages")
library ("rio",lib = "V:/HomeDir/890029(E. Vinke)/Rpackages")
#install.packages("data.table", lib="V:/HomeDir/890029(E. Vinke)/Rpackages")
library("data.table",lib = "V:/HomeDir/890029(E. Vinke)/Rpackages")
#install.packages("purr", lib="V:/HomeDir/890029(E. Vinke)/Rpackages")
library("purrr",lib = "V:/HomeDir/890029(E. Vinke)/Rpackages")
#library("lubridate")
#install.packages("reshape2", lib="V:/HomeDir/890029(E. Vinke)/Rpackages")
library("reshape2",lib = "V:/HomeDir/890029(E. Vinke)/Rpackages")
#install.packages("zoo", lib="V:/HomeDir/890029(E. Vinke)/Rpackages")
library("zoo",lib = "V:/HomeDir/890029(E. Vinke)/Rpackages")
#install.packages("Hmisc", lib="V:/HomeDir/890029(E. Vinke)/Rpackages")
library("Hmisc",lib = "V:/HomeDir/890029(E. Vinke)/Rpackages") #to be able to edit labels of column names
library("foreign",lib = "V:/HomeDir/890029(E. Vinke)/Rpackages")

# VISITS ---------------------------------------------------------
#visits downloaded from wiki

rs1_1 <- import("./Data/Visits/Ergo1ResponseDetail_(22-jan-2015)_excerpt.sav")
rs1_2 <- import("./Data/Visits/Ergo2ResponseDetail_(22-jan-2015)_excerpt.sav")
rs1_3 <- import("./Data/Visits/e3_(3)_RESPONS_(22-feb-2016)_excerpt.sav")
rs1_4 <- import("./Data/Visits/e4_(4)_RESPONS_(12-mar-2018)_excerpt.sav")
rs1_5 <- import("./Data/Visits/e5_(5)_RESPONS_(22-jun-2016)_excerpt.sav")
rs1_6 <- import("./Data/Visits/e6_(6)_RESPONS_(10-feb-2017)_EXCERPT.sav")
rs2_1 <- import("./Data/Visits/ep_(1)_RESPONS_(22-mar-2016)_excerpt.sav")
rs3_1 <- import("./Data/Visits/ej_(1)_RESPONS_(04-apr-2016)_excerpt.sav")

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
  select(ergoid, rs_cohort, e1_aintdat, e1_acen1dat, e1_acen2dat, e2_bcendat, e3_3493, e3_3494, e3_3495, e4_3493, e4_3494, e4_3495, e5_3493, e5_3494, e5_3495, e6_3493, e6_3494, e6_3495)

rs1_interview <- rs1_vis %>%
  select(ergoid, rs_cohort, e1_aintdat, e2_bcendat, e3_3493, e4_3493, e5_3493, e6_3493)

### merge r2 visits

rs2 <- list(rs2_1, rs2_2, rs2_3,rs2_4)

rs2_vis <- reduce(rs2, left_join, by = c("ergoid", "rs_cohort"))

rs2_interview <- rs2_vis %>%
  select(ergoid, rs_cohort , ep_3493, e4_3493, e5_3493, e6_3493)

#### Merge rs3 visits

rs3_vis <- rs3_1  %>%
  left_join(rs3_2, by = c("ergoid", "rs_cohort"))

rs3_interview <- rs3_vis %>%
  select(ergoid, rs_cohort , ej_3493, e5_3493)

### rename variables
setnames(rs1_interview, old = c("e1_aintdat", "e2_bcendat", "e3_3493", "e4_3493", "e5_3493","e6_3493"), new = c("e1", "e2", "e3", "e4", "e5","e6"))

setnames(rs2_interview, old = c("ep_3493", "e4_3493", "e5_3493","e6_3493"), new = c("e3", "e4", "e5","e6"))

setnames(rs3_interview, old = c("ej_3493", "e5_3493"), new = c("e4", "e5"))

##merge cohorts together for interview dates (this gives extra cases, meaning extra rows, therefore we use bind_rows instead of left_join merging)

complete_db <- rs1_interview %>% 
  bind_rows(rs2_interview) %>% 
  bind_rows(rs3_interview)

### remove everything from our environment except for the complete_db
rm(list=ls()[! ls() %in% c("complete_db")])

# BASIC ------------------------------------------------------------
#basic downloaded from wiki: https://epi-wiki.erasmusmc.nl/wiki/ergowiki/index.php/Ergobasics
basic <- import("./RotterdamStudy_Basics2014.sav")

# select needed variables from the basic file and determine age at study entry
basic <- basic %>%
  select(ergoid, rs_cohort, sex, date_of_birth, startdat) %>%
  mutate(age_0 = (startdat - date_of_birth)/365.25) %>% 
  mutate(age_0 = round(age_0, digits = 2))

# change label of age_0
label(basic$age_0) <- "Age at entering the Rotterdam Study"
label(basic$sex) <- "Sex, 0 = male; 1 = female"

# add these basic variables to the complete database, merging by ergoid and rs_cohort
complete_db <- complete_db %>%
  left_join(basic, by = c("ergoid", "rs_cohort"))

# complete labels of the variables
label(complete_db)[1:8] <- list("Identification","Rotterdam Study cohort","Interview date Ergo1","Center visit date Ergo2","Interview date Ergo3 and Ergo PLUS","Interview date Ergo4 and Ergo Jong","Interview date Ergo5","Interview date Ergo6") 

### remove everything from our environment except for the complete_db
rm(list=ls()[! ls() %in% c("complete_db")])


# MMSE --------------------------------------------------------------------
#mmse from the V drive # add links to wikipedia for these files
mmse1 <- import("./Data/Cognition/MMSE_GMS_RS-I-1-6_update2015.sav")
mmse2 <- import("./Data/Cognition/MMSE_GMS_RS-II-1-4_update2017.sav")
mmse3 <- import("./Data/Cognition/MMSE_GMS_RS-III-1-2_update2015.sav")

### select mmse data from mmse datasets

mmse1 <- mmse1 %>%
  select(ergoid, ammse, bmmse, cmmse, dmmse, emmse, fmmse)

mmse2 <- mmse2 %>%
  select(ergoid, MMSE_RS_II_1, MMSE_RS_II_2, MMSE_RS_II_3,MMSE_e6)

mmse3 <- mmse3 %>%
  select(ergoid, MMSE_RS_III_1, MMSE_RS_III_2)

### renaming three cohorts

setnames(mmse1, old = c("ammse", "bmmse", "cmmse", "dmmse", "emmse","fmmse" ), new = c("mmse1", "mmse2", "mmse3", "mmse4", "mmse5","mmse6"))

# trying to allign the mmse scores from the different cohorts, MMSE_RS_II_1 (ERGO PLUS) is named mmse3 here.
setnames(mmse2, old = c("MMSE_RS_II_1", "MMSE_RS_II_2", "MMSE_RS_II_3","MMSE_e6"), new = c("mmse3", "mmse4", "mmse5","mmse6"))

# trying to allign the mmse scores from the different cohorts, MMSE_RS_III_1 (ERGO JONG), is named mmse 4 here.
setnames(mmse3, old = c("MMSE_RS_III_1", "MMSE_RS_III_2"), new = c("mmse4", "mmse5"))

##merge cohorts

mmse <- mmse1 %>% 
  bind_rows(mmse2) %>% 
  bind_rows(mmse3)

label(mmse)<- as.list(c("Identification",rep("Mini Mental State Examination (MMSE)",6))) 

##merge with complete database

complete_db <- complete_db %>%
  left_join(mmse, by = c("ergoid"))

### remove everything from our environment except for the complete_db
rm(list=ls()[! ls() %in% c("complete_db")])

# DEMENTIA -------------------------------------------------
#dementia from the V drive: "V:/Neurology/Dementie"
#dem <- import("V:/Uitwissel/Paloma/Data/Cognition/Dementia_RSI-II-III_20171214.sav")

# select the dementia variables that you need and make variable names a bit shorter
#dem <- dem %>%
#  select(ergoid, dementia_prevalent, dementia_incident, dementia_date) %>% 
#  rename(dementia_prev = dementia_prevalent, dementia_inc = dementia_incident) %>% 
#  mutate(dementia_inc = ifelse(is.na(dementia_inc), 2, dementia_inc))
#label(dem$dementia_inc) <- "Incident dementia"

#complete_db <- complete_db %>%
#  left_join(dem, by = c("ergoid"))


### remove everything from our environment except for the complete_db
#rm(list=ls()[! ls() %in% c("complete_db")])
# VITAL STATUS ---------------------------------------------------------------
#mortality downloaded from wiki: https://epi-wiki.erasmusmc.nl/wiki/ergowiki/index.php/Fp_vital_status

#mort <- import("V:/Uitwissel/Paloma/Data/Mortality/fp_VitalStatus_(24-MAY-2018).sav")

# select the mortality variables that you need and make variable names a bit shorter
#mort <- mort %>%
#  select(ergoid, fp_mortdat, fp_censordate) %>% 
#  rename(mort_date = fp_mortdat, censor_date = fp_censordate)

#complete_db <- complete_db %>%
#  left_join(mort, by = c("ergoid"))

### remove everything from our environment except for the complete_db
#rm(list=ls()[! ls() %in% c("complete_db")])


# APOE--------------------------------------------------------------------
# From the V-drive? From the wiki? where?
apoe <- import("/scratch/evinke/Sustain/Rotterdam_Study_Data/datasets/Covariates/APOE/APOE_RS.sav")
apoe <- apoe %>%
  select(ergoid, apoe4)

label(apoe) <- list("Identification","Number of APOE4 alleles")
complete_db <- complete_db %>%
  left_join(apoe, by = c("ergoid"))
 
rm(list=ls()[! ls() %in% c("complete_db")])
# BMI ---------------------------------------------------------------------
#bmi downloaded from wiki: https://epi-wiki.erasmusmc.nl/wiki/ergowiki/index.php/Anthropometry

bmi_1 <- import("./Data/Anthro/e1_ANTHROPO_(15-jun-2011).sav")
bmi_2 <- import("./Data/Anthro/e2_(2)_ANTHROPO_(26-apr-2011).sav")
bmi_3 <- import("./Data/Anthro/e3_(3)_HARTVAAT_(25-feb-2013)_ANTHROPO.sav")
bmi_4 <- import("./Data/Anthro/e4_(4)_UITSCHR_(06-nov-2014)_ANTHROPO-PART.sav")
bmi_5 <- import("./Data/Anthro/e5_(5)_ANTHROPO_(10-dec-2015).sav")
bmi_6 <- import("./Data/Anthro/e6_(6)_ANTHROPO_(25-apr-2017).sav")
bmi2_ep_1 <- import("./Data/Anthro/ep_(1)_LICHONDZ_(18-oct-2012)_ANTHROPO.sav")
bmi3_ej_1 <- import("./Data/Anthro/ej_(1)_UITSCHR_(23-feb-2010)_ANTHROPO-PART.sav")

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

#3. Separate bmi_5 into rs1 and rs2
bmi1_6 <- bmi_6 %>%
  filter(rs_cohort ==1)

bmi2_4 <- bmi_6 %>%
  filter(rs_cohort ==2)

### Merge cohorts for rs1
bmi1 <- list(bmi_1, bmi_2, bmi_3, bmi1_4, bmi1_5,bmi1_6)

rs1_bmi <- reduce(bmi1, left_join, by = c("ergoid"))

dim(rs1_bmi)

### Merge cohorts for rs2
bmi2 <- list(bmi2_ep_1, bmi2_2, bmi2_3, bmi2_4)
rs2_bmi <- reduce(bmi2, left_join, by = c("ergoid"))

###Merge cohorts for rs3

bmi3 <- bmi3_ej_1 %>%
  left_join(bmi3_2, by= c("ergoid"))

#### Select main variables from each set

rs1_bmi <- rs1_bmi%>%
  select(ergoid, e1_aahgt, e1_aawgt, e2_229, e2_230, e3_229, e3_230, e4_229, e4_230, e5_229, e5_230,e6_229,e6_230)

rs2_bmi <- rs2_bmi%>%
  select(ergoid, ep_229, ep_230, e4_229, e4_230, e5_229, e5_230,e6_229,e6_230)

rs3_bmi <- bmi3%>%
  select(ergoid, ej_229, ej_230, e5_229, e5_230)

### rename
setnames(rs1_bmi, old = c("e1_aahgt", "e2_229", "e3_229", "e4_229","e5_229","e6_229", "e1_aawgt","e2_230" ,"e3_230", "e4_230","e5_230","e6_230"), new = c("hgt1", "hgt2", "hgt3", "hgt4", "hgt5","hgt6", "wgt1", "wgt2", "wgt3", "wgt4", "wgt5","wgt6"))

setnames(rs2_bmi, old = c("ep_229", "e4_229", "e5_229","e6_229", "ep_230", "e4_230", "e5_230","e6_230"), new = c("hgt3", "hgt4", "hgt5","hgt6", "wgt3", "wgt4", "wgt5","wgt6"))

setnames(rs3_bmi, old = c("ej_229", "e5_229", "ej_230", "e5_230"), new = c("hgt4", "hgt5", "wgt4", "wgt5"))

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

rs_bmi <- rs_bmi %>% 
  select(ergoid, bmi1, bmi2, bmi3, bmi4, bmi5, bmi6)

dim(rs_bmi)

label(rs_bmi) <- as.list(c("Identification",rep("Body Mass Index (BMI) in kg/m^2",6))) 
###merge with complete db

complete_db <- complete_db %>%
  left_join(rs_bmi, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])


# Education ---------------------------------------------------------------
# education file from wiki: UNESCO classification: https://epi-wiki.erasmusmc.nl/wiki/ergowiki/index.php/Education

educ <- import("./Data/Education/education_RS1_RS2_RS3_seven_and_three_point_scale.sav")

educ <- educ %>% 
  select(ergoid, education_three_levels)

label(educ) <- list("Identification","Highest obtained education level: 0 = Low, 1 = Intermediate, 2 = High")

complete_db <- complete_db %>%
  left_join(educ, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])


# SMOKING -----------------------------------------------------------------
smoke1_1 <- import("./Data/Smoking/e1_intvw_SMOKING_(23-nov-2011).sav")
smoke1_2 <- import("./Data/Smoking/e2_intvw_SMOKING_(23-nov-2011).sav")
smoke1_3 <- import("./Data/Smoking/e3_intvw_SMOKING_(11-nov-2011).sav")
smoke1_4 <- import("./Data/Smoking/e4_intvw_SMOKING_(04-nov-2011).sav")
smoke1_5 <- import("./Data/Smoking/e5_intvw_SMOKING_(04-sep-2014).sav")
smoke1_6 <- import("./Smoking/e6_intvw_SMOKING_(20-feb-2017).sav")
smoke2_1 <- import("./Data/Smoking/ep_intvw_SMOKING_(30-sep-2011).sav")
smoke3_1 <- import("./Data/Smoking/ej_intvw_SMOKING_(28-mar-2011).sav")

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
smoke1 <- list(smoke1_1, smoke1_2, smoke1_3, smoke1_4, smoke1_5,smoke1_6)

rs1_smoke<- reduce(smoke1, left_join, by = c("ergoid"))

### Merge cohorts for rs2
smoke2 <- list(smoke2_1, smoke2_2, smoke2_3,smoke2_4)

rs2_smoke <- reduce(smoke2, left_join, by = c("ergoid"))

###Merge cohorts for rs3

rs3_smoke <- smoke3_1 %>%
  left_join(smoke3_2, by= c("ergoid"))

########### Select variables and code smoking 0 never, 1 former, 2 current, 3 missing

rs1_smoke <- rs1_smoke %>%
  select(ergoid, e1_ai7_20, e1_ai7_30, e2_b0cg, e2_b0ct, e2_b0pi, e3_cicg, e3_cipi, e3_cict, e3_cictps, e4_dicg, e4_dipi, e4_dict, e5_EILF6, e5_EILFE, e5_EILF4, e5_EILF5,e6_EILF6, e6_EILSE5, e6_EILF4, e6_EILF5)

rs1_smoke <- rs1_smoke %>%
  mutate(smoke1 = ifelse(e1_ai7_20 == 0 & e1_ai7_30 == 0, 0, NA)) %>%
  mutate(smoke1 = ifelse((e1_ai7_20 == 0 | e1_ai7_30 == 1), 1, smoke1)) %>%
  mutate(smoke1 = ifelse(e1_ai7_20 == 1, 2, smoke1)) %>%
  mutate(smoke1 = ifelse(is.na(smoke1), 3, smoke1))

# rs1_smoke %>%
#   select (e1_ai7_20, e1_ai7_30, smoke1) %>%
#   head(25)

rs1_smoke <- rs1_smoke %>%
  mutate(smoke2 = ifelse(e2_b0cg == 3 & e2_b0pi == 3 & e2_b0ct == 3, 0, NA)) %>%
  mutate(smoke2 = ifelse((e2_b0cg == 2 | e2_b0pi == 2 | e2_b0ct == 2), 1, smoke2)) %>%
  mutate(smoke2 = ifelse((e2_b0cg == 1 | e2_b0pi == 1 | e2_b0ct == 1), 2, smoke2)) %>%
  mutate(smoke2 = ifelse(is.na(smoke2), 3, smoke2))

# rs1_smoke %>%
#   select (e2_b0cg, e2_b0pi, e2_b0ct, smoke2) %>%
#   head(25)

rs1_smoke <- rs1_smoke %>%
  mutate(smoke3 = ifelse(e3_cicg == 0 & e3_cipi == 0 & e3_cict == 0 & e3_cictps == 0, 0, NA)) %>%
  mutate(smoke3 = ifelse((e3_cicg == 1 |e3_cipi == 1 |(e3_cict == 0 & e3_cictps == 1)), 1, smoke3)) %>%
  mutate(smoke3 = ifelse((e3_cicg == 2 |e3_cicg == 3 | e3_cipi == 2 | e3_cipi == 3 | e3_cict == 1), 2, smoke3)) %>%
  mutate(smoke3 = ifelse(is.na(smoke3), 3, smoke3))

# rs1_smoke %>%
#   select (e3_cicg, e3_cipi, e3_cict, e3_cictps, smoke3) %>%
#   head(25)

rs1_smoke <- rs1_smoke %>%
  mutate(smoke4 = ifelse(e4_dicg == 0 & e4_dipi == 0 & e4_dict == 0, 0, NA)) %>%
  mutate(smoke4 = ifelse((e4_dicg == 1 |e4_dipi == 1 | e4_dict == 2 | e4_dict == 3), 1, smoke4)) %>%
  mutate(smoke4 = ifelse((e4_dicg == 2 |e4_dicg == 3 | e4_dipi == 2 | e4_dipi == 3 | e4_dict == 1), 2, smoke4)) %>%
  mutate(smoke4 = ifelse(is.na(smoke4), 3, smoke4))

# rs1_smoke %>%
#   select(e4_dicg, e4_dict, e4_dipi, smoke4) %>%
#   head(25)

rs1_smoke <- rs1_smoke %>%
  mutate(smoke5 = ifelse(e5_EILF6 == 0 & e5_EILFE == 0 & e5_EILF5 == 0 & e5_EILF4 == 0, 0, NA)) %>%
  mutate(smoke5 = ifelse(((e5_EILF6 == 0 & e5_EILFE == 1)| e5_EILF5 == 1 | e5_EILF4 == 1 | e5_EILF4 == 2 | e5_EILF4 == 3), 1, smoke5)) %>%
  mutate(smoke5 = ifelse((e5_EILF6 == 1 | e5_EILF5 == 2 | e5_EILF4 == 4 | e5_EILF4 == 5 | e5_EILF4 == 6), 2, smoke5)) %>%
  mutate(smoke5 = ifelse(is.na(smoke5), 3, smoke5))

# rs1_smoke %>%
#   select(e5_EILF4, e5_EILF5, e5_EILF6, e5_EILFE, smoke5) %>%
#   head(50)

rs1_smoke <- rs1_smoke %>%
  mutate(smoke6 = ifelse(e6_EILF6 == 0 & e6_EILSE5 == 0 & e6_EILF5 == 0 & e6_EILF4 == 0, 0, NA)) %>%
  mutate(smoke6 = ifelse(((e6_EILF6 == 0 & (e6_EILSE5 == 1 |e6_EILSE5 == 2))| e6_EILF5 == 1 | e6_EILF4 == 1 | e6_EILF4 == 2 | e6_EILF4 == 3), 1, smoke6)) %>%
  mutate(smoke6 = ifelse((e6_EILF6 == 1 | e6_EILF5 == 2 | e6_EILF4 == 4 | e6_EILF4 == 5 | e6_EILF4 == 6), 2, smoke6)) %>%
  mutate(smoke6 = ifelse(is.na(smoke6), 3, smoke6))

rs1_smoke <- rs1_smoke %>%
  select(ergoid, smoke1, smoke2, smoke3, smoke4, smoke5, smoke6)


######## rs2
rs2_smoke <- rs2_smoke %>%
  select(ergoid, ep_lf4, ep_lf5, ep_lf6, ep_lf6e, e4_dicg, e4_dipi, e4_dict, e5_EILF6, e5_EILFE, e5_EILF4, e5_EILF5,e6_EILF6, e6_EILSE5, e6_EILF4, e6_EILF5)


rs2_smoke <- rs2_smoke %>%
  mutate(smoke3 = ifelse(ep_lf4 == 0 & ep_lf5 == 0 & ep_lf6 == 0 & ep_lf6e == 0, 0, NA)) %>%
  mutate(smoke3 = ifelse((ep_lf4 == 1 | ep_lf5 == 1 | (ep_lf6 == 0 & ep_lf6e == 1)), 1, smoke3)) %>%
  mutate(smoke3 = ifelse((ep_lf4 == 2 | ep_lf5 == 2 | ep_lf6 == 1), 2, smoke3)) %>%
  mutate(smoke3 = ifelse(is.na(smoke3), 3, smoke3))

rs2_smoke <- rs2_smoke %>%
  mutate(smoke4 = ifelse(e4_dicg == 0 & e4_dipi == 0 & e4_dict == 0, 0, NA)) %>%
  mutate(smoke4 = ifelse((e4_dicg == 1 |e4_dipi == 1 | e4_dict == 2 | e4_dict == 3), 1, smoke4)) %>%
  mutate(smoke4 = ifelse((e4_dicg == 2 |e4_dicg == 3 | e4_dipi == 2 | e4_dipi == 3 | e4_dict == 1), 2, smoke4)) %>%
  mutate(smoke4 = ifelse(is.na(smoke4), 3, smoke4))

#rs2_smoke %>%
#select(e4_dicg, e4_dict, e4_dipi, smoke2) %>%
#head(25)

rs2_smoke <- rs2_smoke %>%
  mutate(smoke5 = ifelse(e5_EILF6 == 0 & e5_EILFE == 0 & e5_EILF5 == 0 & e5_EILF4 == 0, 0, NA)) %>%
  mutate(smoke5 = ifelse(((e5_EILF6 == 0 & e5_EILFE == 1)| e5_EILF5 == 1 | e5_EILF4 == 1 | e5_EILF4 == 2 | e5_EILF4 == 3), 1, smoke5)) %>%
  mutate(smoke5 = ifelse((e5_EILF6 == 1 | e5_EILF5 == 1 | e5_EILF4 == 4 | e5_EILF4 == 5 | e5_EILF4 == 6), 2, smoke5)) %>%
  mutate(smoke5 = ifelse(is.na(smoke5), 3, smoke5))

#rs2_smoke %>%
#select(e5_EILF4, e5_EILF5, e5_EILF6, e5_EILFE, smoke3) %>%
#head(50)
rs2_smoke <- rs2_smoke %>%
  mutate(smoke6 = ifelse(e6_EILF6 == 0 & e6_EILSE5 == 0 & e6_EILF5 == 0 & e6_EILF4 == 0, 0, NA)) %>%
  mutate(smoke6 = ifelse(((e6_EILF6 == 0 & (e6_EILSE5 == 1 |e6_EILSE5 == 2))| e6_EILF5 == 1 | e6_EILF4 == 1 | e6_EILF4 == 2 | e6_EILF4 == 3), 1, smoke6)) %>%
  mutate(smoke6 = ifelse((e6_EILF6 == 1 | e6_EILF5 == 2 | e6_EILF4 == 4 | e6_EILF4 == 5 | e6_EILF4 == 6), 2, smoke6)) %>%
  mutate(smoke6 = ifelse(is.na(smoke6), 3, smoke6))


rs2_smoke <- rs2_smoke %>%
  select(ergoid, smoke3, smoke4, smoke5,smoke6)

############## rs3
# # ej_yilfe ................. Have you used to smoke cigarettes? 1 no, 1 yes
# ej_yilf6 ................. Do you smoke cigarettes? 1 no, 1 yes
# ej_yilf5 ................. Do you smoke pipe? (If no, question): Have you never smoked pipe or have you ever smoked pipe, but have you stopped now? 
# 0, never, 1 before, 2 yes
# ej_yilf4 .................Do you smoke cigars? (If no, question): Have you never smoked cigars or have you ever smoked cigars but have you stopped? 
# 0, never, 1-3,before, 4 - 6 yes

rs3_smoke <- rs3_smoke %>%
  mutate(smoke4 = ifelse(ej_yilf4 == 0 & ej_yilf5 == 0 & ej_yilf6 == 0 & ej_yilfe == 0, 0, NA)) %>%
  mutate(smoke4 = ifelse((ej_yilf4 == 1 | ej_yilf4 == 2 | ej_yilf4 == 3 | ej_yilf5 == 1 | (ej_yilf6 == 0 & ej_yilfe == 1)), 1, smoke4)) %>%
  mutate(smoke4 = ifelse((ej_yilf4 == 4 | ej_yilf4 == 5 | ej_yilf4 == 6 | ej_yilf5 == 2 | ej_yilf6 == 1), 2, smoke4)) %>%
  mutate(smoke4 = ifelse(is.na(smoke4), 3, smoke4))

#rs3_smoke %>%
#select(ej_yilf6, ej_yilf4, ej_yilf5, ej_yilfe, smoke1)  %>%
#head(50)

# e5_EILF4 ................. Do you smoke cigars? (If no, question): Have you never smoked cigars or have you ever smoked cigars but have you stopped? 
# 0, never, 1 - 3 before, 4 - 6 yes
#e5_EILF5 ................. Do you smoke pipe? (If no, question): Have you never smoked pipe or have you ever smoked pipe, but have you stopped now? 
# 0, never, 1 before, 2 yes
# e5_EILFE ................. Have you used to smoke cigarettes? 
# 0, no, 1 yes
# e5_EILF6 ................. Do you smoke cigarettes? 
# 0, no, 1 yes


rs3_smoke <- rs3_smoke %>%
  mutate(smoke5 = ifelse(e5_EILF6 == 0 & e5_EILFE == 0 & e5_EILF5 == 0 & e5_EILF4 == 0, 0, NA)) %>%
  mutate(smoke5 = ifelse(((e5_EILF6 == 0 & e5_EILFE == 1)| e5_EILF5 == 1 | e5_EILF4 == 1 | e5_EILF4 == 2 | e5_EILF4 == 3), 1, smoke5)) %>%
  mutate(smoke5 = ifelse((e5_EILF6 == 1 | e5_EILF5 == 1 | e5_EILF4 == 4 | e5_EILF4 == 5 | e5_EILF4 == 6), 2, smoke5)) %>%
  mutate(smoke5 = ifelse(is.na(smoke5), 3, smoke5))

#rs3_smoke %>%
#select(e5_EILF4, e5_EILF5, e5_EILF6, e5_EILFE, smoke2) %>%
#head(50)

rs3_smoke <- rs3_smoke %>%
  select(ergoid, smoke4, smoke5)

### binding all

rs_smoke <- rs1_smoke %>% 
  bind_rows(rs2_smoke) %>% 
  bind_rows(rs3_smoke)

dim(rs_smoke)

label(rs_smoke) <- as.list(c("Identification", rep("Smoking, 0 never, 1 former, 2 current, 3 missing",6))) 
###merge with complete db

complete_db <- complete_db %>%
  left_join(rs_smoke, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])

# ALCOHOL INTAKE ---------------------------------------------------------
oh1_1 <- import("./Data/Alcohol/e1_FFQ_AlcoholGramPerday_inclGLAZEN.sav")
oh1_2 <- import("./Data/Alcohol/e2_intvw_Alcoholperday_25-10-2013.sav")
oh1_3 <- import("./Data/Alcohol/e3_intvw_Alcoholperday_24-10-2017.sav")
oh1_4 <- import("./Data/Alcohol/e4_intvw_Alcoholperday_22-11-2013.sav")
oh1_5 <- import("./Data/Alcohol/e5intvw_Alcoholperday_11-07-2014.sav")
oh1_6 <- import("./Data/Alcohol/e6_AlcoholGramsDay_FFQ_energy_(13-dec-2018).sav")
oh2_1 <- import("./Data/Alcohol/ep_intvw_Alcoholperday_22-11-2013.sav")
oh3_1 <- import("./Data/Alcohol/ej_intvw_ALCOHOLGRAMSPERDAY_(14072014).sav")

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
  select(ergoid, antalc, e2_Alc_Tot, e3_Alc_Tot, e4_Alc_Tot, e5_Alc_tot, e6_alc_item_sum)

rs2_oh <- rs2_oh %>% 
  select(ergoid, ep_Alc_Tot, e4_Alc_Tot, e5_Alc_tot,e6_alc_item_sum)

rs3_oh <- rs3_oh %>% 
  select(ergoid, ej_Alc_tot, e5_Alc_tot)

### Rename

setnames(rs1_oh, old = c("antalc", "e2_Alc_Tot", "e3_Alc_Tot", "e4_Alc_Tot", "e5_Alc_tot","e6_alc_item_sum"), new = c("oh1", "oh2", "oh3", "oh4", "oh5","oh6"))

setnames(rs2_oh, old = c("ep_Alc_Tot", "e4_Alc_Tot", "e5_Alc_tot","e6_alc_item_sum"), new = c("oh3", "oh4", "oh5","oh6"))

setnames(rs3_oh, old = c("ej_Alc_tot", "e5_Alc_tot"), new = c("oh4", "oh5"))

### bind together

rs_oh <- rs1_oh %>% 
  bind_rows(rs2_oh) %>% 
  bind_rows(rs3_oh)

label(rs_oh) <- as.list(c("Identification",rep("Total alcohol intake g/day",6))) 
###merge with complete db

complete_db <- complete_db %>%
  left_join(rs_oh, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])

# HYPERTENSION ------------------------------------------------------------
### download from the wiki: https://epi-wiki.erasmusmc.nl/wiki/ergowiki/index.php/Hypertension
ht <- import("./Data/Hypertension/HT2018_analysisfile_(15-may-2018).sav")

ht <- ht %>% 
  select (ergoid, rs_cohort, e1_systolicBP, e1_HT2018, e2_systolicBP, e2_HT2018, e3_systolicBP, e3_HT2018, e4_systolicBP, e4_HT2018, e5_systolicBP, e5_HT2018, ep_systolicBP, ep_HT2018, ej_systolicBP, ej_HT2018,e6_systolicBP, e6_HT2018)

ht1 <- ht %>% 
  filter(rs_cohort == 1) %>%
  select(ergoid, e1_systolicBP, e1_HT2018, e2_systolicBP, e2_HT2018, e3_systolicBP, e3_HT2018, e4_systolicBP, e4_HT2018, e5_systolicBP, e5_HT2018, e6_systolicBP, e6_HT2018) %>% 
  setnames(old = c("e1_systolicBP", "e1_HT2018", "e2_systolicBP", "e2_HT2018", "e3_systolicBP", "e3_HT2018", "e4_systolicBP", "e4_HT2018", "e5_systolicBP", "e5_HT2018","e6_systolicBP","e6_HT2018"), new = c("sbp1", "ht1", "sbp2", "ht2", "sbp3", "ht3", "sbp4", "ht4", "sbp5", "ht5", "sbp6", "ht6"))

ht2 <- ht %>% 
  filter(rs_cohort == 2) %>%
  select(ergoid, ep_systolicBP, ep_HT2018, e4_systolicBP, e4_HT2018, e5_systolicBP, e5_HT2018,e6_systolicBP, e6_HT2018 ) %>% 
  setnames(old = c("ep_systolicBP", "ep_HT2018", "e4_systolicBP", "e4_HT2018", "e5_systolicBP", "e5_HT2018","e6_systolicBP", "e6_HT2018"), new = c("sbp3", "ht3", "sbp4", "ht4", "sbp5", "ht5","sbp6","ht6"))

ht3 <- ht %>% 
  filter(rs_cohort == 3) %>%
  select(ergoid, ej_systolicBP, ej_HT2018, e5_systolicBP, e5_HT2018) %>% 
  setnames(old = c("ej_systolicBP", "ej_HT2018", "e5_systolicBP", "e5_HT2018"), new = c("sbp4", "ht4", "sbp5", "ht5"))

rs_ht <- ht1 %>% 
  bind_rows(ht2) %>% 
  bind_rows(ht3) %>%
  select(ergoid, sbp1, sbp2, sbp3, sbp4, sbp5, sbp6, ht1, ht2, ht3, ht4, ht5, ht6)

label(rs_ht)<- as.list(c("Identification",rep("Systolic Blood Pressure mmHg",6),rep("Hypertension, resting BP > 140/90 or taking BP lowering meds",6)))

#merge in complete dataset
complete_db <- complete_db %>%
  left_join(rs_ht, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])

# CHOLESTEROL and HDL -------------------------------------------------------------
chol1_1 <- import("./Cholesterol/e1_CHOLESTEROL_(10.03.2010).sav")
chol1_3 <- import("./Cholesterol/e3_(3)_LAB_(10-mar-2010).sav")
chol1_4 <- import("./Cholesterol/e4_(4)_LAB_(10-mar-2010)b.sav")
chol1_5 <- import("./Cholesterol/e5_(5)_LAB_(29-aug-2014)r.sav")
#chol1_6 <- import("./Cholesterol/e6_(6)_LAB_(10-jun-2016)r.sav") # No cholesterol data in that dataset yet, also not on the wiki yet
chol2_1 <- import("./Cholesterol/ep_(1)_LAB_(15-mar-2010).sav")
chol3_1 <- import("./Cholesterol/ej_(1)_LAB_(11-jun-2009)r.sav")

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

#chol1_6 <- chol1_6 %>% 
#  select(ergoid, rs_cohort, e6_3845, e6_4107 ) %>%
#  setnames(old = c("e6_3845", "e6_4107"), new=c("chol6", "hdl6"))

chol2_1 <- chol2_1 %>% 
  select(ergoid, ep_3845, ep_4107) %>%
  setnames(old = c("ep_3845", "ep_4107"), new=c("chol3", "hdl3"))

chol3_1 <- chol3_1 %>% 
  select(ergoid, ej_3845, ej_4107) %>%
  setnames(old = c("ej_3845", "ej_4107"), new=c("chol4", "hdl4"))

#Separate chol1_5 into rs1, rs2, rs3

chol3_2 <- chol1_5 %>%
  filter(rs_cohort == 3) %>%
  select(ergoid, chol5, hdl5)

chol2_3 <- chol1_5 %>%
  filter(rs_cohort == 2) %>% 
  select(ergoid, chol5, hdl5)

chol1_5 <- chol1_5 %>%
  filter(rs_cohort == 1) %>% 
  select(ergoid, chol5, hdl5)

#separate chol1_4 into rs1 and rs2
chol2_2 <- chol1_4 %>%
  filter(rs_cohort == 2) %>% 
  select(ergoid, chol4, hdl4)

chol1_4 <- chol1_4 %>%
  filter(rs_cohort == 1) %>% 
  select(ergoid, chol4, hdl4)

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

###merge with complete db

complete_db <- complete_db %>%
  left_join(rs_chol, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])

# Diabetes medication -----------------------------------------------------
db1_1 <- import("./Data/Farmaco/e1_MEDICATION_(08-jan-2010).sav")
db1_3 <- import("./Data/Farmaco/e3_MEDICATION_(27-jun-2012).sav")
db2_1 <- import("./Data/Farmaco/ep_MEDICATION_(12-jan-2010).sav")
db1_4 <- import("./Data/Farmaco/e4_MEDICATION_(20-jan-2010).sav")
db1_5 <- import("./Data/Farmaco/e5_MEDICATION_(12-MAR-2015).sav")
db1_6 <- import("./Data/Farmaco/e6_MEDICATION_(13-feb-2018).sav")
db3_1 <- import("./Data/Farmaco/ej_MEDICATION_(22-mar-2010).sav")

db1_1 <- db1_1 %>%
  select(ergoid, e1_a10)

db1_3 <- db1_3 %>%
  select(ergoid, e3_a10)

db2_1 <- db2_1 %>%
  select(ergoid, ep_a10,rs_cohort)

db3_1 <- db3_1 %>%
  select(ergoid, ej_a10,rs_cohort)

db1_6 <- db1_6 %>%
  select(ergoid, e6_a10,rs_cohort)

### Filter from e4 and e5 into cohort 1 and 2

#1

db2_2 <- db1_4 %>%
  filter(rs_cohort == 2) %>%
  select(ergoid, e4_a10) 

db1_4 <- db1_4 %>%
  filter(rs_cohort == 1) %>%
  select(ergoid, e4_a10) 

db3_2 <- db1_5 %>%
  filter(rs_cohort == 3) %>%
  select(ergoid, e5_a10) 

db2_3 <- db1_5 %>%
  filter(rs_cohort == 2) %>%
  select(ergoid, e5_a10) 

db1_5<- db1_5 %>%
  filter(rs_cohort == 1) %>%
  select(ergoid, e5_a10) 

db2_4 <- db1_6 %>%
  filter(rs_cohort == 2) %>%
  select(ergoid, e6_a10)

db1_6<- db1_6 %>%
  filter(rs_cohort == 1) %>%
  select(ergoid, e6_a10)


### Merge cohorts for rs1
db1 <- list(db1_1, db1_3, db1_4, db1_5, db1_6)

rs1_db <- reduce(db1, left_join, by = c("ergoid"))

setnames(rs1_db, old = c("e1_a10", "e3_a10", "e4_a10", "e5_a10","e6_a10"), new = c("db_med1", "db_med3", "db_med4","db_med5","db_med6"))

### Merge cohorts for rs2
db2 <- list(db2_1, db2_2, db2_3, db2_4)

rs2_db <- reduce(db2, left_join, by = c("ergoid"))

dim(rs2_db)

setnames(rs2_db, old = c("ep_a10", "e4_a10", "e5_a10", "e6_a10"), new = c("db_med1", "db_med2", "db_med3","db_med4"))

### Merge cohorts for rs3
db3 <- list(db3_1, db3_2)

rs3_db <- reduce(db3, left_join, by = c("ergoid"))

dim(rs3_db)

setnames(rs3_db, old = c("ej_a10", "e5_a10"), new = c("db_med1", "db_med2"))

### bind together

rs_db <- rs1_db %>% 
  bind_rows(rs2_db) %>% 
  bind_rows(rs3_db) 

### Review consistency

rs_db <- rs_db %>% 
  mutate(db_dx_1 = db_med1, 
         db_dx_2 = ifelse(db_dx_1 == 1, 1, db_med2),
         db_dx_3 = ifelse((db_dx_2 == 1 | db_med3 == 1), 1, db_med3),
         db_dx_4 = ifelse((db_dx_3 == 1 | db_med4 == 1), 1, db_med4),
         db_dx_5 = ifelse((db_dx_4 == 1 | db_med5 == 1), 1, db_med5), 
         db_dx_6 = ifelse((db_dx_5 == 1 | db_med6 == 1), 1, db_med6))
###merge with complete db

complete_db <- complete_db %>%
  left_join(rs_db, by = c("ergoid"))

rm(list=ls()[! ls() %in% c("complete_db")])



# Save wide format dataset ------------------------------------------------

export(complete_db, "./complete_db.RData")

# Convert complete_db in LONG ---------------------------------------------

tv_var <- c("e1", "e2", "e3", "e4", "e5", "mmse1", "mmse2", "mmse3", "mmse4", "mmse5", "bmi1", "bmi2", "bmi3", "bmi4", "bmi5", 
            "smoke1", "smoke2", "smoke3", "smoke4", "smoke5", "oh1", "oh2", "oh3", "oh4", "oh5",
            "sbp1", "sbp2", "sbp3", "sbp4", "sbp5", "ht1", "ht2", "ht3", "ht4", "ht5",
            "chol1", "chol2", "chol3", "chol4", "chol5", "hdl1", "hdl2", "hdl3", "hdl4", "hdl5")#, 
            #"db_med1", "db_med2", "db_med3", "db_med4", "db_med5")

db_long <- reshape( data = complete_db,
                    idvar = "ergoid",
                    varying = tv_var,
                    sep = "",
                    timevar = "Ergo",
                    times = c(1,2,3,4,5),
                    direction = "long")

colnames(db_long)[15] <- "interview_date"

# Export wide or long -------------------------------------------------------------
export(complete_db, "V:/Uitwissel/Paloma/Data/db/complete_db.RData")
export(db_long, "V:/Uitwissel/Paloma/Data/db/db_long.RData")

# Export as spss file:
# change dates to SPSS date format using the following function:
dt2spss <- function(x){
  as.numeric(x) - as.numeric(as.POSIXct("1582-10-14",tz="UTC"))
}

date_columns <- c(3:7,9:10,19:21)
for (i in date_columns){
complete_db[,i] <- dt2spss(complete_db[,i])
}

write.foreign(complete_db, "V:/Uitwissel/Paloma/Data/db/complete_db.txt", "V:/Uitwissel/Paloma/Data/db/complete_db.sps",   package="SPSS") 

date_columns <- c(3:7,9:10,19:21)
for (i in date_columns){
  complete_db[,i] <- dt2spss(complete_db[,i])
}

write.foreign(complete_db, "V:/Uitwissel/Paloma/Data/db/complete_db.txt", "V:/Uitwissel/Paloma/Data/db/complete_db.sps",   package="SPSS") 

