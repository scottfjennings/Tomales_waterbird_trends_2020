


library(tidyverse)
library(here)
library(birdnames)
library(flextable)
library(officer)

options(scipen = 999)

custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")

source(here("code/analysis_utilities.R"))

trend_spp <- readRDS(here("data_files/trend_spp"))


# getting and plotting predicted estimates ----



all_best <- map_df(trend_spp$alpha.code, get_best_model) %>%
  bind_rows(data.frame(alpha.code = "BLSC", Modnames = "year")) %>%
  bind_rows(data.frame(alpha.code = "GWTE", Modnames = "intercept")) 

# best BLSC model (year2) gives estimates for the early years that seem unrealistically high, second best model (year; delta aicc = 0.3) seems to give more reasonable estimates relative to the raw data. adding the year model to all_best here to extract estimates for both models
# GWTE intercept only has dAICc = 2.152. 2 competitive models year and year_moci_giac have mostly uninformative parms (only year in year_moci_giac not uninformative). best model estimates seem kinda iffy so including intercept

# mod_predictions_link in analysis_utilities.R
all_best_preds <- map2_df(all_best$alpha.code, all_best$Modnames, mod_predictions_link)



all_best_preds_response <- all_best_preds %>%
  data.frame() %>% 
  mutate(lci = exp(fit - (1.96 * se.fit)),
         uci = exp(fit + (1.96 * se.fit)),
         predicted = exp(fit))

saveRDS(all_best_preds_response, here("data_files/all_best_preds_response"))
all_best_preds_response <- readRDS(here("data_files/all_best_preds_response"))


# predict across fresh and moci range ----
# need range of predictors, plus year of mean estimated abundance for each species with that predictor in the best model


predictors <- readRDS(here("data_files/predictors")) %>% 
              mutate(moci = scale(mean.moci, scale = TRUE, center = TRUE)[,1],
                     fresh = scale(annual.freshwater, scale = TRUE, center = TRUE)[,1])
# moci predictions
moci_spp <- map_df(trend_spp$alpha.code, get_best_model) %>% 
  filter(grepl("moci", Modnames))


check_moci_informative <- function(zspp, zmod.name) {
  zspp_mod_obj <- readRDS(here("fitted_models/final_models"))[[zspp]]

zspp_mods <- zspp_mod_obj[zmod.name]

parms_inf <- map2_df(zspp_mods, names(zspp_mods), parm_informative) %>% 
  filter(parm == "moci", informative95 == TRUE) %>% 
  select(parm, informative95, Modnames) %>% 
  mutate(alpha.code = zspp)
}

moci_inform <- map2_df(moci_spp$alpha.code, moci_spp$Modnames, check_moci_informative)


moci_years <- readRDS(here("data_files/all_best_preds_response")) %>% 
  right_join(moci_inform) %>% 
  dplyr::select(alpha.code, predicted, study.year) %>% 
  group_by(alpha.code) %>% 
  mutate(mean.pred = mean(predicted),
         mean.pred.diff = abs(mean.pred - predicted)) %>% 
  filter(mean.pred.diff == min(mean.pred.diff)) %>% 
  mutate(year.diff = abs(2007 - study.year)) %>% 
  filter(year.diff == min(year.diff)) %>% 
  ungroup() %>% 
  dplyr::select(alpha.code, study.year)


get_moci_predictions <- function(zspp) {
zspp_year <- filter(moci_years, alpha.code == zspp)
zspp_mod_name <- filter(moci_spp, alpha.code == zspp)$Modname

zspp_mod <- readRDS(here("fitted_models/final_models"))[[zspp]][[zspp_mod_name]]

moci_newdat <- data.frame(moci = seq(min(predictors$moci), max(predictors$moci), length.out = 10)) %>% 
  mutate(fresh = 0,
         giac = 0,
         study.year = zspp_year$study.year)

zpred <- predict(zspp_mod, moci_newdat, type = "link", se = TRUE) %>% 
  bind_cols(moci_newdat) %>% 
  mutate(alpha.code = zspp,
         Modnames = zspp_mod_name) %>% 
  select(alpha.code, Modnames, study.year, fit, se.fit, moci) 

}

moci_predictions <- map_df(moci_inform$alpha.code, get_moci_predictions) %>%
  data.frame() %>% 
  mutate(lci = exp(fit - (1.96 * se.fit)),
         uci = exp(fit + (1.96 * se.fit)),
         predicted = exp(fit))

saveRDS(moci_predictions, here("data_files/moci_predictions"))

# freshwater inflow predictions

fresh_spp <- map_df(trend_spp$alpha.code, get_best_model) %>% 
  filter(grepl("fresh", Modnames))


check_fresh_informative <- function(zspp, zmod.name) {
  zspp_mod_obj <- readRDS(here("fitted_models/final_models"))[[zspp]]
  
zspp_mods <- zspp_mod_obj[zmod.name]

parms_inf <- map2_df(zspp_mods, names(zspp_mods), parm_informative) %>% 
  filter(parm == "fresh", informative95 == TRUE) %>% 
  select(parm, informative95, Modnames) %>% 
  mutate(alpha.code = zspp)
}

fresh_inform <- map2_df(fresh_spp$alpha.code, fresh_spp$Modnames, check_fresh_informative)

fresh_years <- readRDS(here("data_files/all_best_preds_response")) %>% 
  right_join(fresh_inform) %>% 
  dplyr::select(alpha.code, predicted, study.year) %>% 
  group_by(alpha.code) %>% 
  mutate(mean.pred = mean(predicted),
         mean.pred.diff = abs(mean.pred - predicted)) %>% 
  filter(mean.pred.diff == min(mean.pred.diff)) %>% 
  mutate(year.diff = abs(2007 - study.year)) %>% 
  filter(year.diff == min(year.diff)) %>% 
  ungroup() %>% 
  dplyr::select(alpha.code, study.year)


get_fresh_predictions <- function(zspp) {
zspp_year <- filter(fresh_years, alpha.code == zspp)
zspp_mod_name <- filter(fresh_spp, alpha.code == zspp)$Modname

zspp_mod <- readRDS(here("fitted_models/final_models"))[[zspp]][[zspp_mod_name]]

fresh_newdat <- data.frame(fresh = seq(min(predictors$fresh), max(predictors$fresh), length.out = 10)) %>% 
  mutate(moci = 0,
         giac = 0,
         study.year = zspp_year$study.year)

zpred <- predict(zspp_mod, fresh_newdat, type = "link", se = TRUE) %>% 
  bind_cols(fresh_newdat) %>% 
  mutate(alpha.code = zspp,
         Modnames = zspp_mod_name) %>% 
  select(alpha.code, Modnames, study.year, fit, se.fit, fresh) 

}

fresh_predictions <- map_df(fresh_inform$alpha.code, get_fresh_predictions) %>%
  data.frame() %>% 
  mutate(lci = exp(fit - (1.96 * se.fit)),
         uci = exp(fit + (1.96 * se.fit)),
         predicted = exp(fit))

saveRDS(fresh_predictions, here("data_files/fresh_predictions"))

# best guild model predictions ----

guild_newdat <- expand.grid(study.year = seq(1992, 2022),
                            guild = c("Herbivore", "Diving benthivore", "Piscivore")) %>% 
  mutate(moci = 0,
         fresh = 0)


get_guild_prediction <- function(zmod) {
guild_pred = predict(readRDS(here("fitted_models/final_models"))[["guild"]][[zmod]], guild_newdat, type = "link", se = TRUE) %>% 
  bind_cols(guild_newdat) %>%
  data.frame() %>% 
  mutate(lci = exp(fit - (1.96 * se.fit)),
         uci = exp(fit + (1.96 * se.fit)),
         predicted = exp(fit),
         Modnames = zmod)

}

guild_pred <- map_df(c("year2.guild_fresh_moci", "year2_guild_fresh_moci", "year2_guild.fresh_moci", "year2_fresh_guild.moci"), get_guild_prediction)


saveRDS(guild_pred, here("data_files/guild_preds_response"))

