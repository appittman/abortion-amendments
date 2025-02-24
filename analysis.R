###### VERY QUICK initial analysis stuff

library(tidyverse)
library(here)
library(did)

load(here("data", "hps.Rdata"))

### so, Ohio passed abortion protections in the 2023 election
### AZ, CO, MO, MT, NV all passed them in the 2024 election
# what if I compare Ohio to those
# treatment happens between t 12 and 13

hps <- hps |> 
  filter(state %in% c("OH", "AZ", "CO", "MO", "MT", "NV"))

### Analysis 1: comparing women in Ohio to women in the other states

hps_1 <- hps |> 
  filter(sex == "f") |> 
  mutate(first_treat = case_match(state, 
                                  "OH" ~ 13,
                                  .default = 0)) |> 
  group_by(state, state_code, t, first_treat) |> 
  summarize(phq4 = weighted.mean(phq4, w = pweight, na.rm = TRUE))


############### run DID
set.seed(623)
did1 <- att_gt(data = hps_1,
       yname = "phq4", 
       tname = "t", 
       idname = "state_code", 
       gname = "first_treat")

################# compute average treatment effect
set.seed(623)
aggte(did1, type = "dynamic") |> 
  ggdid()


############## analysis 2: compare men in OH to men in other states
hps_2 <- hps |> 
  filter(sex == "m") |> 
  mutate(first_treat = case_match(state, 
                                  "OH" ~ 13,
                                  .default = 0)) |> 
  group_by(state, state_code, t, first_treat) |> 
  summarize(phq4 = weighted.mean(phq4, w = pweight, na.rm = TRUE))
  
############### run DID
set.seed(623)
did2 <- att_gt(data = hps_2,
               yname = "phq4", 
               tname = "t", 
               idname = "state_code", 
               gname = "first_treat")

################# compute average treatment effect
set.seed(623)
aggte(did2)



###################### Analysis 3 (more descriptive): compare women in OH to men in OH

hps_3 <- hps |> 
  filter(state == "OH") |> 
  group_by(sex, t, startdate) |> 
  summarize(phq4 = weighted.mean(phq4, w = pweight, na.rm = T))


hps_3 |> 
  ggplot(aes(x = startdate, y = phq4, group = sex, color = sex)) +
  geom_line() +
  geom_vline(xintercept = mdy("11-05-2023"), linetype = "dashed") +
  theme_minimal() +
  labs(title = "Ohioans' Scores on the PHQ-4",
       y = "PHQ-4 Score Weighted Average",
       x = "Week")


hps |> 
  mutate(first_treat = case_match(state,
                                  "OH" ~ 13,
                                  .default = 0)) |> 
  group_by(state, first_treat, startdate, sex) |> 
  summarize(phq4 = weighted.mean(phq4, w = pweight, na.rm = T)) |> 
  ggplot(aes(x = startdate, y = phq4, group = state, color = as.factor(first_treat))) +
  geom_line() +
  facet_wrap("sex") +
  scale_color_manual(values = c("gray", "red"))
  


