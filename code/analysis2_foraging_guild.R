

library(tidyverse)
library(birdnames)


source("C:/Users/scott.jennings/Documents/Projects/water_birds/ACR_waterbird_data_management/code/utility/waterbird_utility_functions.r")


wbirds4analysis <- readRDS("C:/Users/scott.jennings/Documents/Projects/water_birds/ACR_waterbird_data_management/data_files/working_rds/wbirds4analysis") %>% 
  wbird_add_study_day()

num_years_detected <- readRDS("C:/Users/scott.jennings/Documents/Projects/water_birds/ACR_waterbird_data_management/data_files/working_rds/num_years_detected")


annual_percent_change <- wbirds4analysis %>% 
  filter(alpha.code %in% filter(num_years_detected, num.year.detected >=20)$alpha.code) %>% 
  group_by(study.year, alpha.code, section) %>% 
  summarise(annual.section.spp.mean = mean(section.final)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c("alpha.code", "section"), names_from = study.year, values_from = annual.section.spp.mean) %>% 
  pivot_longer(-c("alpha.code", "section"), names_to = "study.year", values_to = "annual.section.spp.mean") %>%
  arrange(alpha.code, section, study.year) %>%  
  group_by(alpha.code, section) %>% 
  mutate(year.section.percent.change = annual.section.spp.mean - (annual.section.spp.mean/lag(annual.section.spp.mean)))






# abund ~ B0 + year(B1) + year2(B2) + MOCI(B4) +   