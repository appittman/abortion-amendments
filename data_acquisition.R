library(tidyverse)
library(here)
library(rvest)

#### setup
if (!dir.exists(here("zipfiles"))) {
  dir.create(here("zipfiles"))
} else {
  message("zipfiles directory exists")
}

if (!dir.exists(here("csvfiles"))) {
  dir.create(here("csvfiles"))
} else {
  message("csvfiles directory exists")
}

if (!dir.exists(here("data_dictionary"))) {
  dir.create(here("data_dictionary"))
} else {
  message("data_dictionary directory exists")
}

if (!dir.exists(here("data"))) {
  dir.create(here("data"))
} else {
  message("data directory exists")
}


####################### Step 1: get zipfiles

html24 <- read_html("https://www.census.gov/programs-surveys/household-pulse-survey/data/datasets.html")

html23 <- read_html("https://www.census.gov/programs-surveys/household-pulse-survey/data/datasets.2023.html#list-tab-1264157801")


urls23 <- html23 |> 
  html_elements("li") |> 
  html_children() |> 
  html_attr("href") |> 
  str_subset("CSV.zip")

urls24 <- html24 |> 
  html_elements("li") |> 
  html_children() |> 
  html_attr("href") |> 
  str_subset("CSV.zip")

urls <- c(urls24, urls23)

urls <- tibble(urls = urls) |> 
  mutate(files = basename(urls))


map2(.x = urls$urls, .y = urls$files, 
     ~curl::curl_download(.x, destfile = here("zipfiles", .y)))
# should take a few minutes



################### Step 2: get the CSV files

zips <- dir(here("zipfiles"))

contents <- zips |> 
  set_names() |> 
  map(~unzip(here("zipfiles", .x), list = TRUE))

csvs <- tibble(contents) |> 
  mutate(zip = names(contents)) |> 
  unnest(contents) |> 
  select(csv = Name, zip) |> 
  filter(str_detect(csv, ".csv")) |> 
  filter(!str_detect(csv, "repwgt"))

map2(.x = csvs$zip, .y = csvs$csv,
     ~unzip(here("zipfiles", .x), files = .y, 
            exdir = here("csvfiles")))


############# Step 3: read in the CSV files


datalist <- map(.x = here("csvfiles", dir(here("csvfiles"))), 
                .f = ~read_csv(.x,
                               col_select = any_of(c("SCRAM", #id
                                              "WEEK", #week
                                              "CYCLE", #cycle (for newer ones)
                                              "EST_ST", #state
                                              "PWEIGHT", #weight
                                              "TBIRTH_YEAR", #age
                                              "RRACE", #race
                                              "RHISPANIC", #ethnicity
                                              "EGENID_BIRTH",
                                              "AGENID_BIRTH",
                                              "ANXIOUS",
                                              "WORRY",
                                              "INTEREST",
                                              "DOWN"))))

#october and december 2024 files are preliminary; I exclude them here

datalist <- datalist |> 
  discard_at(c(10,11))

###### now need to combine all the elements of the list into one dataframe
# first need to harmonize the name of the time variable; it's the second column in every one, so...

datalist <- map(datalist, ~rename(.x, wave = 2))

df <- bind_rows(datalist)



################### Step 4: Cleaning
###### getting dates and creating a harmonized time variable, t

dates23 <- html23 |> #2023 contains waves 52-63
  html_elements("h4") |> 
  html_text() |> 
  str_split(pattern = "[[:punct:]]") |> 
  tibble() |> 
  unnest_wider(1, names_sep = "_") |> 
  select(startdate = 2, enddate = 3) |> 
  mutate(across(everything(), ~str_squish(.x)),
         year = c(rep(2023, 11), 2022),
         startdate = mdy(paste(startdate, year)),
         enddate = mdy(paste(enddate, year))) |> 
  select(-year) |> 
  arrange(startdate) |> 
  mutate(wave = 52:63)

dates24 <- html24 |> #2024 contains waves 1-9
  html_elements("h4") |> 
  html_text() |> 
  str_split("[[:punct:]]") |> 
  tibble() |> 
  unnest_wider(1, names_sep = "_") |> 
  select(startdate = 2, enddate = 3, year = 4) |> 
  tail(-2) |> 
  mutate(across(everything(), ~str_squish(.x)),
         startdate = mdy(paste(startdate, year)),
         enddate = mdy(paste(enddate, year))) |> 
  select(-year) |> 
  arrange(startdate) |> 
  mutate(wave = 1:9)


df_2 <- bind_rows(dates23, dates24) |> 
  right_join(df, by = "wave") |> 
  janitor::clean_names() |> 
  select(id = scram,
         wave,
         startdate,
         enddate,
         state_code = est_st,
         birth_year = tbirth_year,
         race = rrace,
         hispanic = rhispanic,
         genid_birth = egenid_birth,
         genid_imp = agenid_birth,
         anxious, worry, interest, down,
         pweight) |> 
  mutate(t = case_match(wave,
                        52 ~ 1, 53 ~ 2, 54 ~ 3,
                        55 ~ 4, 56 ~ 5, 57 ~ 6,
                        58 ~ 7, 59 ~ 8, 60 ~ 9,
                        61 ~ 10, 62 ~ 11, 63 ~ 12,
                        1 ~ 13, 2 ~ 14, 3 ~ 15,
                        4 ~ 16, 5 ~ 17, 6 ~ 18,
                        7 ~ 19, 8 ~ 20, 9 ~ 21
                        )) |> 
  relocate(t, .after = "wave")



# now, cleaning up other variables 
# need to get 1 data dictionary for each phase, to make sure stuff hasn't changed
unzip(here("zipfiles", "HPS_Phase4-1Cycle04_PUF_CSV.zip"),
      files = "HPS_data.dictionary_Phase 4.1 Cycle04_CSV.xlsx",
      exdir = here("data_dictionary"))
unzip(here("zipfiles", "HPS_Week63_PUF_CSV.zip"), 
      files = "pulse2023_data.dictionary_CSV_63.xlsx",
      exdir = here("data_dictionary"))


df_3 <- df_2 |> 
  mutate(state_code = as.integer(state_code),
         age = year(startdate) - birth_year,
         race_rc = case_when(hispanic == 2 ~ "hispanic",
                             hispanic == 1 & race == 1 ~ "nhw",
                             hispanic == 1 & race == 2 ~ "nhb",
                             is.na(race) ~ NA_character_,
                             is.na(hispanic) ~ NA_character_,
                             .default = "other/multi"),
         sex = case_match(genid_birth,
                          1 ~ "m",
                          2 ~ "f"),
         across(c(anxious, worry, interest, down), 
                       ~case_when(.x > 0 ~ .x,
                                  .x < 0 ~ NA_integer_,
                                  .default = NA_integer_)),
         phq4 = anxious + worry + interest + down) |> 
  select(-c(race, hispanic, birth_year, genid_birth, genid_imp,
            anxious, worry, interest, down)) |> 
  relocate(c(wave, pweight), .after = everything())


# getting state abbreviations instead of fips codes for ease

hps <- tibble(tidycensus::fips_codes) |> 
  select(state, state_code) |> 
  mutate(state_code = as.integer(state_code)) |> 
  unique() |> 
  right_join(df_3, by = "state_code")


#################################### last step: save it

save(hps, file = here("data", "hps.Rdata"), compress = "bzip2")
# takes a bit longer but bzip compression makes it a bit smaller