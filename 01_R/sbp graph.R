surv <- import("V:/Uitwissel/Paloma/HT trial/Gform/checkvis2.sas7bdat")
# 
surv_risk<- surv %>%
  select(starts_with("ssbp")) %>% 
  mutate(id = 1:5247)

surv_long<- melt(surv_risk, id.vars = c("id"))
%>% 
  mutate(time = 1:21,
         Value = "Simulated")

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