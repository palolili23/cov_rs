surv <-import("V:/Uitwissel/Paloma/HT trial/Gform/graphsurv.sas7bdat") %>% 
clean_names() %>% 
  filter(int == 3 & sample == 1)
# 
surv_risk<- surv %>%
  select(starts_with("risk"))

surv_long<- melt(surv_risk) %>% 
  mutate(time = 1:21,
         risk = "Dementia Risk")

comp_risk<- surv %>%
  select(starts_with("comprisk")) 

comprisk <- melt(comp_risk)%>% 
  mutate(time = 1:21,
         risk = "Death Risk")

surv_long <- surv_long %>% 
  bind_rows(comprisk) 

# esquisse::esquisser(data = surv_long)

ggplot(data = surv_long) +
  aes(x = time, y = value, color = risk) +
  geom_point() +
  geom_smooth(span = 0.5) +
  labs(title = "Cumulative incidence of dementia",
       x = "Years",
       y = "Probability of outcome") +
  theme_minimal()