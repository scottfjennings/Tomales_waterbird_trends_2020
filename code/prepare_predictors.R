# prepare predictor variables 


library(tidyverse)
library(here)



# raptors from shorebird surveys ----
raptors <- readRDS("C:/Users/scott.jennings/Documents/Projects/shorebirds/shorebird_data_work/data_files/rds/raptors4analysis")

annual_mean_winter_raptors <- raptors %>% 
  group_by(season.year, date) %>% 
  summarise(bay.date.total = sum(count)) %>% 
  ungroup() %>%
  group_by(season.year) %>% 
  summarise(bay.annual.mean.raptors = mean(bay.date.total)) %>% 
  filter(grepl("winter", season.year)) %>% 
  mutate(season.year = gsub("winter_", "", season.year),
         season.year = as.numeric(season.year)) %>% 
  rename(study.year = season.year)
  


# moci ----

moci <- read.csv("C:/Users/scott.jennings/Documents/Projects/oceans/data/CaliforniaMOCI_JFM1991-AMJ2020.csv")

colnames(moci)[grepl("North", colnames(moci))] <- "north"
colnames(moci)[grepl("Central", colnames(moci))] <- "central"

# I think using the annual mean value of the North section MOCI values makes most sense because:
# 1. transport of nutrients is more likely to have a downcoast component than an upcoast component, thus ocean conditions fro Pt Reyes North are more likely to influece TB than from Pt Reyes south. 
# 2. we lack conclusive prior research suggesting MOCI effect would be strongest at a particular lag, so averaging the prior year's values takes a broad, exploratory view of this variable's importance
mean_north_moci <- moci %>% 
  rename_with(tolower) %>% 
  mutate(study.year = ifelse(season == "JFM", year - 1, year)) %>% 
  group_by(study.year) %>% 
  summarise(study.year.mean.moci = mean(north),
         study.year.sd.moci = sd(north))

# rainfall ----
tomales_watershed_month_mean_rains <- read.csv("C:/Users/scott.jennings/Documents/Projects/Rainfall/derived_data/tomales_watershed_mean_month_rain.csv")

rain <- tomales_watershed_month_mean_rains %>% 
  dplyr::select(-buffer_m) %>% 
  arrange(year, month) %>% 
  mutate(study.year = ifelse(month <= 2, year - 1, year),
         study.month = ifelse(month <= 2, month + 12, month))

fall_rain <- rain %>% 
  filter(study.month > 6) %>% 
  group_by(study.year) %>% 
  summarise(total.fall.rain = sum(mean.rain))

year_rain <- rain %>% 
  group_by(study.year) %>% 
  summarise(total.year.rain = sum(mean.rain))

# join predictors
predictors <- full_join(mean_north_moci, annual_mean_winter_raptors) %>% 
  full_join(., fall_rain) %>% 
  full_join(., year_rain) %>% 
  arrange(study.year)

saveRDS(predictors, here("data_files/predictors"))

