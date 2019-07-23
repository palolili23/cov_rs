# Pharmacy data -----------------------------------------------------------
farmaco <- import("V:/Uitwissel/Paloma/Data/Farmaco/dementie_statins_HRT_calendar.sav")

farmaco <- farmaco %>% 
  rename(id = ergonr) %>% 
  select(-c(sex)) %>% 
  filter(cohort == 1)

# Merge and erase extra rows --------------------------------------------------------
dates_rs1 <- farmaco %>% 
  inner_join(dates_rs1, by = c("id", "cohort"))

dates_rs1 %>% 
  distinct(id) %>%
  tally()

#### set to NA if e1 > 1991
dates_rs1 <- dates_rs1 %>% 
  mutate(e1 = ifelse((e1 < "1991-01-01"), is.na(e1), e1))

## erase rows before e1
dates_rs1 <- dates_rs1 %>%
  group_by(id) %>% 
  filter(start >= e1) %>% 
  ungroup()

dates_rs1 %>% 
  distinct(id) %>%
  tally()

##create a month var
dates_rs1 <- dates_rs1 %>%
  group_by(id) %>% 
  mutate(months = row_number() - 1) %>% 
  ungroup()

