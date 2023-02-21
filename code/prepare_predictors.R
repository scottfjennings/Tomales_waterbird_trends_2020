# prepare predictor variables 


library(tidyverse)
library(lubridate)
library(here)



# raptors from shorebird surveys ----
# raptors currently excluded from trend analysis 12/12/2021

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
moci <- readRDS("C:/Users/scott.jennings/Documents/Projects/general_data_sources/oceans/data/moci_long")

# We use the annual mean value of the North section MOCI values for the AMJ, JAS, OND periods because:
# 1. transport of nutrients is more likely to have a downcoast component than an upcoast component, thus ocean conditions fro Pt Reyes North are more likely to influece TB than from Pt Reyes south. 
# 2. we lack conclusive prior research suggesting MOCI effect would be strongest at a particular lag, so averaging the prior year's values takes a broad, exploratory view of this variable's importance

# 3. Conditions during JFM may act on current year and next year. so to avoid complicated interpretation just exclude that quarter

mean_north_moci <- moci %>% 
  filter(season != "JFM", region2 == "Northern CA") %>% 
  group_by(year) %>% 
  summarise(mean.moci = mean(moci)) %>% 
  rename(study.year = year)
#
# rainfall ----
# Feb 2023 decided to use freshwater inflow instead of rainfall
tomales_watershed_month_mean_rains <- read.csv("C:/Users/scott.jennings/Documents/Projects/Rainfall/derived_data/tomales_watershed_mean_month_rain.csv")

rain <- tomales_watershed_month_mean_rains %>% 
  dplyr::select(-buffer_m) %>% 
  arrange(year, month) %>% 
  mutate(study.year = ifelse(month <= 2, year - 1, year),
         study.month = ifelse(month <= 2, month + 12, month))

fall_rain <- rain %>% 
  filter(study.month > 8) %>% 
  group_by(study.year) %>% 
  summarise(total.fall.rain = sum(mean.rain))

year_rain <- rain %>% 
  group_by(study.year) %>% 
  summarise(total.year.rain = sum(mean.rain))


# freshwater inflow ----
# Although Walker Creek flow is meaningful, the Lagunitas and Walker Creek flows are highly correlated (Pearson R = 0.95) but the Lagunitas Flow has higher spikes and better represents the combined freshwater flow into the bay. Using just the Lagunitas flow in this analysis to simplify   
lacr <- read.csv("C:/Users/scott.jennings/Documents/Projects/general_data_sources/rivers/data/flow_data/LagunitasDischarge.csv") %>% 
  mutate(date = mdy(datetime)) %>% 
  dplyr::select(date, mean.daily.cfs) %>% 
  mutate(study.year = ifelse(month(date) < 3, year(date) - 1, year(date))) %>% 
  filter(study.year > 1991) 


annual_lacr <- lacr %>% 
  mutate(flow.start = paste(study.year, "-10-01", sep = ""),
         flow.end = paste(study.year + 1, "-02-15", sep = "")) %>% 
  filter(date >= flow.start & date <= flow.end) %>% 
  group_by(study.year) %>% 
  summarise(annual.freshwater = mean(mean.daily.cfs))

# ggplot(annual_lacr) + geom_line(aes(x = study.year, y = annual.freshwater))
 


# join predictors ----
predictors <- annual_lacr %>%  
  full_join(., mean_north_moci) %>% 
  mutate(giac = ifelse(study.year < 2009, 0, 1)) %>% 
  arrange(study.year)

saveRDS(predictors, here("data_files/predictors"))

