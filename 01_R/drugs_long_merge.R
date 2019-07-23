
# Directory and Library -----------------------------------------------------------------

setwd("~/")
setwd("V:/Uitwissel/Paloma/Data")
# library("tidyr")
# library("dplyr")
# library("ggplot2")
# library ("rio")
# library("data.table")
# library("purrr")
# library("lubridate")
# library("reshape2")
# library("zoo")
# library("Hmisc")

# Pharmacy data -----------------------------------------------------------
farmaco <- import("V:/Uitwissel/Paloma/Data/Farmaco/dementie_statins_HRT_calendar.sav")

farmaco <- farmaco %>% 
  rename(ergoid = ergonr, rs_cohort = cohort) %>% 
  select(-c(sex))

farmaco %>% 
  distinct(ergoid) %>%
  tally()

# Import db_long in long format ---------------------------------------

db_long <- import("V:/Uitwissel/Paloma/Data/db/db_long.RData")
db_long <- db_long %>% 
  mutate(yr = as.numeric(yr),
         mo = as.numeric(mo))

# Merge drug data with complete_db in long --------------------------------
db_long <- db_long %>% 
  mutate(yr = year(as.Date(e)),
         mo = month(as.Date(e)),#month in which the measurement was performed
         ergoid = as.numeric(ergoid),
         rs_cohort = as.numeric(rs_cohort),
         date_of_birth = as.Date(date_of_birth),
         startdat = as.Date(startdat),
         censor_date = as.Date(censor_date),
         dementia_date = as.Date(dementia_date),
         hd_date = as.Date(hd_date),
         stroke_date = as.Date(stroke_date)) 

class(db_long$censor_date)

merged <- farmaco %>%
  left_join(db_long, by = c("ergoid", "rs_cohort", "yr", "mo"))


class(farmaco$ergoid)
class(farmaco$rs_cohort)
class(farmaco$yr)
class(farmaco$mo)

class(db_long$ergoid)
class(db_long$rs_cohort)
class(db_long$yr)
class(db_long$mo)

# Carry forward last observation ------------------------------------------

merged_locf <- merged %>% 
  arrange(ergoid, start) %>% 
  group_by(ergoid) %>% 
  mutate(sex=na.locf(sex, na.rm=FALSE),
         date_of_birth=na.locf(date_of_birth, na.rm=FALSE),
         startdat=na.locf(startdat, na.rm=FALSE),
         age_0=na.locf(age_0, na.rm=FALSE),
         dementia_prev=na.locf(dementia_prev, na.rm=FALSE),
         dementia_inc=na.locf(dementia_inc, na.rm=FALSE),
         dementia_date=na.locf(dementia_date, na.rm=FALSE),
         mort_date=na.locf(mort_date, na.rm=FALSE),
         censor_date=na.locf(censor_date, na.rm=FALSE),     
         stroke_prev=na.locf(stroke_prev, na.rm=FALSE),
         stroke_inc=na.locf(stroke_inc, na.rm=FALSE),
         stroke_date=na.locf(stroke_date, na.rm=FALSE),
         apoe4=na.locf(apoe4, na.rm=FALSE),
         hd_prev=na.locf(hd_prev, na.rm=FALSE),
         hd_inc=na.locf(hd_inc, na.rm=FALSE),
         hd_date=na.locf(hd_date, na.rm=FALSE),
         visit=na.locf(visit, na.rm=FALSE),
         e=na.locf(e, na.rm=FALSE),
         mmse=na.locf(mmse, na.rm=FALSE),
         bmi=na.locf(bmi, na.rm=FALSE),
         smoke=na.locf(smoke, na.rm=FALSE),
         oh=na.locf(oh, na.rm=FALSE),
         sbp=na.locf(sbp, na.rm=FALSE),
         ht=na.locf(ht, na.rm=FALSE),
         chol=na.locf(chol, na.rm=FALSE),
         hdl=na.locf(hdl, na.rm=FALSE),
         db_med=na.locf(db_med, na.rm=FALSE))


# Delete empty rows -------------------------------------------------------

merged_locf_trimmed <- merged_locf %>% 
  group_by(ergoid) %>% 
  filter(!is.na(startdat)) %>% 
  filter(censor_date >= end)

### Create a time variable per ID
merged_locf_trimmed <- merged_locf_trimmed %>%
  arrange(ergoid, yr, mo) %>% 
  group_by(ergoid) %>%
  mutate(time = sequence(n()) - 1)

##modify age as it increases in time 
merged_locf_trimmed <- merged_locf_trimmed %>%
  mutate(age_1 = as.integer(start - date_of_birth)/365.25 ) %>% 
  mutate(age_1 = round(age_1, digits = 2))


# Creating incident events for covariates based on dates ------------------

merged_locf_trimmed <- merged_locf_trimmed %>%  
mutate(dementia_inc_long = ifelse((dementia_date >= start & dementia_date <= end & !is.na(dementia_date)), 1, NA),
      dementia_inc_long = na.locf(dementia_inc_long, na.rm=FALSE),
      dementia_inc_long = ifelse(is.na(dementia_inc_long), 0, dementia_inc_long),
      hd_inc_long = ifelse((hd_date >= start & hd_date <= end & !is.na(hd_date)), 1, NA),
      hd_inc_long = na.locf(hd_inc_long, na.rm=FALSE),
      hd_inc_long = ifelse(is.na(hd_inc_long), 0, hd_inc_long),
      stroke_inc_long = ifelse((stroke_date >= start & stroke_date <= end & !is.na(stroke_date)), 1, NA),
      stroke_inc_long = na.locf(stroke_inc_long, na.rm=FALSE),
      stroke_inc_long = ifelse(is.na(stroke_inc_long), 0, stroke_inc_long))


# Creating laggued variables ----------------------------------------------
create_lag_var <- function(data,group_factor,variables){
  for (i in variables){
    variable <- as.formula(paste("~lag(", i,",k=1)",sep=""))
    lag_var <- paste(i,"_l",sep="")
    data <- data %>%
      group_by_(.dots = group_factor) %>%
      mutate_(.dots=setNames(list(variable),lag_var))
  
  }
  data
}

variables <- colnames(merged_locf_trimmed)[25:38]
test <- create_lag_var(merged_locf_trimmed,"ergoid",variables)

# Checking excluded due to start and censore dates ------------------------
### how many observations are per id and how many get excluded because the were censored before the "start" date,
###of because ther "censor_date" was = to their "startdat"
a <- merged_locf_trimmed %>% 
  group_by(ergoid, rs_cohort) %>%
  summarise(n())

a1 <- a %>% 
  group_by(rs_cohort) %>% 
  distinct(ergoid) %>% 
  summarise(n())

b <- db_long %>% 
  mutate(ergoid = as.numeric(ergoid)) %>% 
  group_by(ergoid, rs_cohort) %>%
  summarise(n()) 

excluded <- b %>% 
  anti_join(a, by = c("ergoid")) ## this 417 equals to the ammount of excluded for date reasons

rm(list=ls()[! ls() %in% c("merged_locf_trimmed", "excluded")])

# ELEGIBILITY CRITERIA ----------------------------------------------------

in_21 <-  merged_locf_trimmed %>%
  group_by(ergoid) %>% 
  mutate(timemax = max(time)) %>% 
  filter(timemax >21)

in_212 <- in_21 %>% 
  group_by(ergoid) %>% 
  mutate(elegnew2 = rollsumr(hrt, k=timemax, align = "left"))

abc <- in_21 %>% 
  mutate(elegnew = as.integer(elegnew == 1 & 
                                rollsumr(hrt == 1, k = 20, align = "right", fill = 0) == 1)) ### it only works if the subjects at least had the same k


create_lag_var <- function(data,group_factor,variables){
  for (i in variables){
    variable <- as.formula(paste("~lag(", i,",k=1)",sep=""))
    lag_var <- paste(i,"_l",sep="")
    data <- data %>%
      group_by_(.dots = group_factor) %>%
      mutate_(.dots=setNames(list(variable),lag_var))
    
  }
  data
}

time_slots_ones <- NA
time_slots_zeros <- NA

for (i in ergoid_list){
hrt_vector <- in_21$hrt[which(in_21$ergoid == i)]
# find ways to find the positions of the ones, and then count the ones that are connecter to each

}
  
complete_start_time %>%
  group_by(eleg) %>%
  summarise(total = n())

 
###Age
complete_start_time <- complete_start_time %>%
  group_by(ergoid) %>%
  mutate(eleg_age = ifelse(age_fu <= 80, 1, 0))


complete_start_time <- complete_start_time %>%
  group_by(ergoid) %>%
  mutate(eleg_wo_yrs = ifelse((eleg == 1 & eleg_age == 1), 1, 0))

complete_start_time %>%
  group_by(eleg_wo_yrs) %>%
  summarise(n())

View(complete_start_time)

ex <- complete_start_time %>%
  filter(eleg_wo_yrs == 1) %>%
  arrange(desc(start)) 

View(ex)

example <- complete_start_time %>%
  filter(ergoid == 96)
View(example)


