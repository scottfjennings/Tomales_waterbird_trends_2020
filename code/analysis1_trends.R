

library(tidyverse)
library(here)
library(MASS) # for glm.nb
library(AICcmodavg)
library(birdnames)

source("C:/Users/scott.jennings/Documents/Projects/water_birds/ACR_waterbird_data_management/code/utility/waterbird_utility_functions.r")


wbirds4analysis <- readRDS("C:/Users/scott.jennings/Documents/Projects/water_birds/ACR_waterbird_data_management/data_files/working_rds/wbirds4analysis") %>% 
  wbird_add_study_day()

survey_spp_total <- wbirds4analysis %>% 
  wbird_spp_survey_total() %>% 
  wbird_add_study_day()

num_years_detected <- wbirds4analysis %>% 
  wbird_num_years_detected() 

# first analysis step is to evaluate evidence for any long term trends 

# primary interest is in estimating support for and direction/magnitude of Year coefficient 

# initial data exploration suggested some auto correlation in abundance for some species:
sig_acf_spp <- readRDS(here("data_files/autocor_spp"))

# predictors 
predictors <- readRDS(here("data_files/predictors"))


# basic trend candidate set
spp_annual_mean <- wbirds4analysis %>% 
  wbird_spp_annual_mean() %>% 
  full_join(., predictors) %>% 
  rename(moci = study.year.mean.moci,
         rain = total.fall.rain) %>% 
  filter(study.year > 1990)



fit_nb_basic_candidates<- function(zspp) {
spp_annual_mean <- filter(spp_annual_mean, alpha.code == zspp)

year2 <- glm.nb(floor(spp.annual.mean) ~ poly(study.year, 2), data = spp_annual_mean, maxit = zmaxit)
year <- glm.nb(floor(spp.annual.mean) ~ study.year, data = spp_annual_mean, maxit = zmaxit)
rain <- glm.nb(floor(spp.annual.mean) ~ rain, data = spp_annual_mean, maxit = zmaxit)
moci <- glm.nb(floor(spp.annual.mean) ~ moci, data = spp_annual_mean, maxit = zmaxit)

year2_rain <- glm.nb(floor(spp.annual.mean) ~ poly(study.year, 2) + rain, data = spp_annual_mean, maxit = zmaxit)
year2_moci <- glm.nb(floor(spp.annual.mean) ~ poly(study.year, 2) + moci, data = spp_annual_mean, maxit = zmaxit)

year_rain <- glm.nb(floor(spp.annual.mean) ~ study.year + rain, data = spp_annual_mean, maxit = zmaxit)
year_moci <- glm.nb(floor(spp.annual.mean) ~ study.year + moci, data = spp_annual_mean, maxit = zmaxit)

intercept <- glm.nb(floor(spp.annual.mean) ~ 1, data = spp_annual_mean, maxit = zmaxit)

wbird_aic <- aictab(list(year2, year, rain, moci, year2_rain, year2_moci, year_rain, year_moci, intercept), 
                    c("year2", "year", "rain", "moci", "year2_rain", "year2_moci", "year_rain", "year_moci", "intercept")) %>% 
  mutate(species = zspp)
return(wbird_aic)
}

