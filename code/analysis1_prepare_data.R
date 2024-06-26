

library(tidyverse)
library(here)
library(birdnames)

source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/water_birds/ACR_waterbird_data_management/code/utils.r")
source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/water_birds/waterbird_data_work/code/utility/waterbird_utility_functions.R")


#custom_bird_list <- make_custom_bird_list("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_species.csv")
#' saveRDS(custom_bird_list, "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")

custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")


wbird_keep_taxa <- c("AMCOGRSCLESCBUFF", "AMCO", "COGA", "Anseriformes", "Alcidae", "Gaviidae", "Pelecanidae", "Podicipediformes", "Sterninae", "Suliformes")

options(scipen = 999)
# data prep ----
# double check days are entered in raw xlsx files match those in the previous database 
readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/water_birds/ACR_waterbird_data_management/data_files/working_rds/new_neg_machine_bay_total") %>% 
  distinct(date) %>% 
  mutate(new.data = T) %>% 
  full_join(read.csv("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/water_birds/ACR_waterbird_data_management/data_files/survey_dates_old_db.csv") %>% 
              mutate(date = as.Date(date))) %>% 
  arrange(date) %>% 
  view()


# several dates had data quirks either in the field or on data sheets and should be excluded from analysis
exclude_dates <- as.Date(c("2017-12-16", # Beaufort 3-4
                           "2011-12-17", # Bivalve count incomplete; ponds only
                   "2006-02-11", # WCD not surveyed due to fog
                   "2004-12-18", # inverness, millerton not surveyed
                   "1999-12-18", # survey done north to south
                   "1999-01-30", # strong wind, not entered in raw tallies but including to have a complete coded reference to excluded dates
                   #"1996-12-21", # no WCD precount, never many birds there so maybe ok to include. Also multiple data sheet ambiguities, entered raw tally data likely ok but difficult to be sure
                   "1997-12-20", # apparent large swell and strong wind in north bay
                   #"1993-02-06", # no CGP precount, never many birds there so maybe ok to include
                   #"1993-01-23", # no WCD precount, never many birds there so maybe ok to include.
                   "1992-01-11", # survey apparently done north to south
                   "1991-12-14", # substantial ambiguity in precount data sheets. only millerton, wcd entered
                   "1990-12-15", # unclear labeling of section/transect on data sheets and can't reverse engineer. Data not fully entered; date should be excluded from analysis
                   "1989-12-16" # funky protocol. No precounts. Should probably exclude from analysis 
                   ))

# add study year, combine groupd spp for analysis, and recalculate the total number of birds for each species or species group each day
spp_day_total <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/water_birds/ACR_waterbird_data_management/data_files/working_rds/new_neg_machine_bay_total") %>% 
  wbird_add_study_day() %>% # from waterbird_utility_functions.R
  filter(!date %in% exclude_dates, study.year > 1991) %>% 
  bird_taxa_filter(wbird_keep_taxa) %>% 
  mutate(alpha.code = ifelse(alpha.code %in% c("GRSC", "LESC"), "SCAUP", alpha.code),
         alpha.code = ifelse(alpha.code %in% c("WEGR", "CLGR"), "WCGR", alpha.code)) %>% 
  group_by(study.year, date, alpha.code) %>% 
  summarise(bay.total = sum(bay.total)) %>% 
  ungroup()


  
saveRDS(spp_day_total, here("data_files/spp_day_total"))

spp_day_total <-readRDS(here("data_files/spp_day_total"))

# same as above but without SCAUP and WCGR lumped
spp_day_total_ungrouped <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/water_birds/ACR_waterbird_data_management/data_files/working_rds/new_neg_machine_bay_total") %>% 
  wbird_add_study_day() %>% # from waterbird_utility_functions.R
  filter(!date %in% exclude_dates, study.year > 1991) %>% 
  bird_taxa_filter(wbird_keep_taxa) %>% 
  mutate(alpha.code = ifelse(alpha.code == "ACGO", "CACG", alpha.code)) %>% 
  group_by(study.year, date, alpha.code) %>% 
  summarise(bay.total = sum(bay.total)) %>% 
  ungroup()


  
saveRDS(spp_day_total_ungrouped, here("data_files/spp_day_total_ungrouped"))



guilds <- read.csv(here("data_files/Foraging guild table.csv")) %>% 
  mutate(alpha.code = translate_bird_names(common.name, "common.name", "alpha.code")) %>% 
  dplyr::select(alpha.code, guild) %>% 
  right_join(spp_day_total) %>% 
  filter(!is.na(guild)) %>% 
  dplyr::select(-alpha.code) %>% 
  rename("alpha.code" = guild) %>%
  group_by(study.year, date, alpha.code) %>% 
  summarise(bay.total = sum(bay.total))

# add up all species each year (includes non trend species), combine with by-species data, and calculate the 75th percentile of the individual day totals for each species/species group each year
spp_annual <- spp_day_total %>%
  group_by(study.year, date) %>% 
  summarise(bay.total = sum(bay.total)) %>% 
  mutate(alpha.code = "ALL") %>% 
  bind_rows(spp_day_total %>% dplyr::select(study.year, date, alpha.code, bay.total)) %>% 
  bind_rows(guilds) %>% 
  group_by(study.year, alpha.code) %>% 
  summarise(p75.abund = floor(quantile(bay.total, 0.75)))



# there aren't many HEGR left so not analyzing them separately
filter(spp_annual, alpha.code == "HEGR") %>% view()

# filter to just those species/species groups seen at least 20 years and with the median 75th percentile at least 5 (models don't fit well for very low abundance species)
trend_spp <- spp_annual %>% 
  group_by(alpha.code) %>% 
  summarise(num.years.detected = n(),
            median.p75 = median(p75.abund),
            min.p75 = min(p75.abund),
            max.p75 = max(p75.abund)) %>% 
  filter(num.years.detected >= 20 & median.p75 >= 5, alpha.code != "HEGR") %>% 
  mutate(tax.order = translate_bird_names(alpha.code, "alpha.code", "taxonomic.order"),
         tax.order = case_when(alpha.code == "ALL" ~ 0,
                               alpha.code == "SCAUP" ~ 80.1,
                               TRUE ~ as.numeric(tax.order)),
         tax.order = as.numeric(tax.order)) %>% 
  arrange(tax.order)


saveRDS(trend_spp, here("data_files/trend_spp"))


# fill in 0 for years when spp not detected
spp_annual_full <- spp_annual %>%
  filter(alpha.code %in% trend_spp$alpha.code) %>% 
  pivot_wider(id_cols = alpha.code, names_from = study.year, values_from = p75.abund) %>% 
  pivot_longer(-alpha.code, names_to = "study.year", values_to = "p75.abund") %>% 
  mutate(p75.abund = ifelse(is.na(p75.abund), 0, p75.abund),
         study.year = as.numeric(study.year))


# join bird data with predictors
spp_annual_full_preds <- spp_annual_full %>% 
  full_join(., readRDS(here("data_files/predictors")) %>% 
              rename(moci = mean.moci,
                     fresh = annual.freshwater) %>% 
              mutate(moci = scale(moci, scale = TRUE, center = TRUE)[,1],
                     fresh = scale(fresh, scale = TRUE, center = TRUE)[,1])) %>% 
  data.frame()

# check correlation of predictors
readRDS(here("data_files/predictors")) %>% 
  dplyr::select("fresh" = annual.freshwater, "moci" = mean.moci) %>% 
  cor()


saveRDS(spp_annual_full_preds, here("data_files/spp_annual_full_preds"))





