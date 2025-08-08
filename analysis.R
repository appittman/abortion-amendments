library(tidyverse)
library(here)
library(did)

load(here("data", "hps.Rdata"))

wavedates <- hps |> 
  select(t, startdate, enddate) |> 
  unique()

# treatment happens between t 12 and 13


############## Analysis 1: compare women in Ohio to women in *all* other states
hps_1 <- hps |> 
  filter(sex == "f") |> 
  mutate(first_treat = case_match(state, 
                                  "OH" ~ 13,
                                  "MI" ~ NA_integer_, #michigan is already treated
                                  .default = 0),
         phq4 = phq4-4) |> 
  drop_na(phq4)
  
############### run DID
did1 <- att_gt(data = hps_1,
                 yname = "phq4", 
                 tname = "t",
                 clustervars = "state_code",
                 gname = "first_treat",
                 panel = FALSE,
                 pl = TRUE,
                 cores = 4)

did1

did1_w <- att_gt(data = hps_1,
               yname = "phq4", 
               tname = "t",
               clustervars = "state_code",
               gname = "first_treat",
               weightsname = "pweight",
               panel = FALSE,
               pl = TRUE,
               cores = 4)

did1_w

### save this for later
save(did1, file = here("data", "did1.Rdata"), compress = "bzip2")
save(did1_w, file = here("data", "did1_w.Rdata"), compress = "bzip2")

################# compute aggte
set.seed(623)
dynamicte1 <- aggte(did1, type = "dynamic")

set.seed(623)
dynamicte1_w <- aggte(did1_w, type = "dynamic")


dynamicte1_w |> ggdid()

#save this for later too
save(dynamicte1, file = here("data", "dynamicte1.Rdata"), compress = "bzip2")
save(dynamicte1_w, file = here("data", "dynamicte1_w.Rdata"), compress = "bzip2")




#################### Analysis 2: compare women in Ohio to women in not-yet-treated states

hps_2 <- hps |> 
  filter(sex == "f") |> 
  filter(state %in% c("OH", "AZ", "CO", "MT", "MO", "NV")) |> 
  mutate(first_treat = case_match(state, 
                                  "OH" ~ 13,
                                  "MI" ~ NA_integer_, #michigan is already treated
                                  .default = 0),
         phq4 = phq4-4) |> 
  drop_na(phq4)

############### run DID
did2 <- att_gt(data = hps_2,
               yname = "phq4", 
               tname = "t",
               clustervars = "state_code",
               gname = "first_treat",
               panel = FALSE,
               pl = TRUE,
               cores = 4)

save(did2, file = here("data", "did2.Rdata"), compress = "bzip2")

did2_w <- att_gt(data = hps_2,
               yname = "phq4", 
               tname = "t",
               clustervars = "state_code",
               gname = "first_treat",
               weightsname = "pweight",
               panel = FALSE,
               pl = TRUE,
               cores = 4)


did2_w

save(did2_w, file = here("data", "did2_w.Rdata"), compress = "bzip2")

################ compute aggte
set.seed(623)
dynamicte2 <- aggte(did2, type = "dynamic")

save(dynamicte2, file = here("data", "dynamicte2.Rdata"), compress = "bzip2")


set.seed(623)
dynamicte2_w <- aggte(did2_w, type = "dynamic")

dynamicte2_w |> ggdid()

save(dynamicte2_w, file = here("data", "dynamicte2_w.Rdata"), compress = "bzip2")


######################################### Analyses 3 + 4: Men
hps_3 <- hps |> 
  filter(sex == "m") |>
  mutate(first_treat = case_match(state, 
                                  "OH" ~ 13,
                                  "MI" ~ NA_integer_, #michigan is already treated
                                  .default = 0),
         phq4 = phq4-4) |> 
  drop_na(phq4)

hps_4 <- hps |> 
  filter(sex == "m") |> 
  filter(state %in% c("OH", "AZ", "CO", "MT", "MO", "NV")) |> 
  mutate(first_treat = case_match(state, 
                                  "OH" ~ 13,
                                  "MI" ~ NA_integer_, #michigan is already treated
                                  .default = 0),
         phq4 = phq4-4) |> 
  drop_na(phq4)

did3_w <- att_gt(data = hps_3,
                 yname = "phq4", 
                 tname = "t",
                 clustervars = "state_code",
                 gname = "first_treat",
                 weightsname = "pweight",
                 panel = FALSE,
                 pl = TRUE,
                 cores = 4)

did4_w <- att_gt(data = hps_4,
                 yname = "phq4", 
                 tname = "t",
                 clustervars = "state_code",
                 gname = "first_treat",
                 weightsname = "pweight",
                 panel = FALSE,
                 pl = TRUE,
                 cores = 4)

dynamicte3_w <- aggte(did3_w, type = "dynamic")
dynamicte4_w <- aggte(did4_w, type = "dynamic")

save(dynamicte3_w, dynamicte4_w, file = here("data", "suppanalyses.Rdata"))



# visualizations

load(here("data", "did1_w.Rdata"))
load(here("data", "did2_w.Rdata"))
load(here("data", "dynamicte1_w.Rdata"))
load(here("data", "dynamicte2_w.Rdata"))


did1_w
did2_w


dynamicte1_w
dynamicte2_w

ggdid(dynamicte1_w) +
  ggtitle("Model 1: ATT -.1937 (95% CI [-.2679, -.1196])")
ggdid(dynamicte2_w) +
  ggtitle("Model 2: ATT -.2598 (95% CI [-.4651, -.0545])")


hps_1 |> 
  group_by(state, startdate) |> 
  summarize(phq4 = weighted.mean(phq4, w = pweight)) |> 
  mutate(ohio = if_else(state == "OH", 1, 0)) |> 
  ggplot(aes(x = startdate, y = phq4)) +
  geom_line(aes(group = state, color = as.factor(ohio))) +
  scale_color_manual(values = c("grey", "blue")) +
  geom_vline(aes(xintercept = mdy("Nov 8 2023")), color = "red") +
  theme(legend.position = "none") +
  labs(x = "Date",
       y = "PHQ-4 Score (Weighted Average)")

hps_2 |> 
  group_by(state, startdate) |> 
  summarize(phq4 = weighted.mean(phq4, w = pweight)) |> 
  mutate(ohio = if_else(state == "OH", 1, 0)) |> 
  ggplot(aes(x = startdate, y = phq4)) +
  geom_line(aes(group = state, color = as.factor(ohio))) +
  scale_color_manual(values = c("grey", "blue")) +
  geom_vline(aes(xintercept = mdy("Nov 8 2023")), color = "red") +
  theme(legend.position = "none") +
  labs(x = "Date",
       y = "PHQ-4 Score (Weighted Average)")



