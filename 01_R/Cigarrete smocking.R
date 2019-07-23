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
  select(ergoid, e1_ai7_20, e1_ai7_30, e2_b0cg, e2_b0ct, e2_b0pi, e3_cicg, e3_cipi, e3_cict, e3_cictps, e4_dicg, e4_dipi, e4_dict, e5_EILF6, e5_EILFE, e5_EILF4, e5_EILF5)

rs1_cig <- rs1_cig %>%
  mutate(smoke_cig1 = ifelse((e1_ai7_20 == 0 & e1_ai7_30 == 0), 0, NA),
         smoke_cig1 = ifelse((e1_ai7_20 == 0 & e1_ai7_30 == 1), 1, smoke_cig1),
         smoke_cig1 = ifelse(e1_ai7_20 == 1, 2, smoke_cig1)) 

rs1_cig <- rs1_cig %>%
  mutate(smoke_cig2 = ifelse(e2_b0ct == 3, 0, NA),
         smoke_cig2 = ifelse(e2_b0ct == 2, 1, smoke_cig2),
         smoke_cig2 = ifelse(e2_b0ct == 1, 2, smoke_cig2))

rs1_cig <- rs1_cig %>%
  mutate(smoke_cig3 = ifelse((e3_cict == 0 & e3_cictps == 0), 0, NA), 
         smoke_cig3 = ifelse((e3_cict == 0 & e3_cictps == 1), 1, smoke_cig3),
         smoke_cig3 = ifelse(e3_cict == 1, 2, smoke_cig3))


rs1_cig <- rs1_cig %>%
  mutate(smoke_cig4 = ifelse(e4_dict == 0, 0, NA),
         smoke_cig4 = ifelse((e4_dict == 2 | e4_dict == 3), 1, smoke_cig4),
         smoke_cig4 = ifelse(e4_dict == 1, 2, smoke_cig4))

rs1_cig <- rs1_cig %>%
  mutate(smoke_cig5 = ifelse(e5_EILF6 == 0 & e5_EILFE == 0, 0, NA),
         smoke_cig5 = ifelse((e5_EILF6 == 0 & e5_EILFE == 1), 1, smoke_cig5),
         smoke_cig5 = ifelse(e5_EILF6 == 1, 2, smoke_cig5))

rs1_cig <- rs1_cig %>%
  select(ergoid, smoke_cig1, smoke_cig2, smoke_cig3, smoke_cig4, smoke_cig5)

pattern_cig_rs1<- rs1_cig %>% 
  filter(!is.na(smoke_cig1)) %>% 
  mutate(smoke_cig2 = ifelse(is.na(smoke_cig2), smoke_cig1, smoke_cig2),
         smoke_cig3 = ifelse(is.na(smoke_cig3), smoke_cig2, smoke_cig3),
         smoke_cig4 = ifelse(is.na(smoke_cig4), smoke_cig3, smoke_cig4),
         smoke_cig5 = ifelse(is.na(smoke_cig5), smoke_cig4, smoke_cig5)) %>% 
  count(smoke_cig1, smoke_cig2, smoke_cig3, smoke_cig4, smoke_cig5)

rs1_cig %>% 
  select(starts_with("smoke_cig")) %>% 
  aggr(prop = TRUE, numbers = TRUE) 


######## rs2
rs2_cig <- rs2_cig %>%
  select(ergoid, ep_lf4, ep_lf5, ep_lf6, ep_lf6e, e4_dicg, e4_dipi, e4_dict, e5_EILF6, e5_EILFE, e5_EILF4, e5_EILF5)


rs2_cig <- rs2_cig %>%
  mutate(smoke_cig1 = ifelse((ep_lf6 == 0 & ep_lf6e == 0), 0, NA),
         smoke_cig1 = ifelse((ep_lf6 == 0 & ep_lf6e == 1), 1, smoke_cig1),
         smoke_cig1 = ifelse(ep_lf6 == 1, 2, smoke_cig1))

rs2_cig <- rs2_cig %>%
  mutate(smoke_cig2 = ifelse(e4_dict == 0, 0, NA),
         smoke_cig2 = ifelse((e4_dict == 2 | e4_dict == 3), 1, smoke_cig2),
         smoke_cig2 = ifelse(e4_dict == 1, 2, smoke_cig2))

rs2_cig <- rs2_cig %>%
  mutate(smoke_cig3 = ifelse(e5_EILF6 == 0 & e5_EILFE == 0, 0, NA),
         smoke_cig3 = ifelse((e5_EILF6 == 0 & e5_EILFE == 1), 1, smoke_cig3),
         smoke_cig3 = ifelse(e5_EILF6 == 1, 2, smoke_cig3))

rs2_cig <- rs2_cig %>%
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
         smoke_cig1 = ifelse(ej_yilf6 == 1, 2, smoke_cig1))

rs3_cig <- rs3_cig %>%
  mutate(smoke_cig2 = ifelse(e5_EILF6 == 0 & e5_EILFE == 0, 0, NA),
         smoke_cig2 = ifelse((e5_EILF6 == 0 & e5_EILFE == 1), 1, smoke_cig2),
         smoke_cig2 = ifelse(e5_EILF6 == 1, 2, smoke_cig2))

rs3_cig <- rs3_cig %>%
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
