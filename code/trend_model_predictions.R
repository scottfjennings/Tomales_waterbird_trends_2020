


library(tidyverse)
library(here)
library(birdnames)
library(flextable)
library(officer)

options(scipen = 999)

custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")

source(here("code/analysis_utilities.R"))

trend_spp <- readRDS(here("data_files/trend_spp"))


# getting and plotting predicted estimates ----


mod_predictions_link <- function(zspp, zmod) {
  
zspp_mod <- readRDS(here("fitted_models/all_spp_mods"))[zspp][[1]][[1]][[zmod]]

znewdat <- data.frame(study.year = seq(1992, 2022),
                      moci = 0,
                      fresh = 0)

zpred <- predict(zspp_mod, znewdat, type = "link", se = TRUE) %>% 
  bind_cols(znewdat) %>% 
  mutate(alpha.code = zspp,
         Modnames = zmod) %>% 
  select(alpha.code, Modnames, study.year, fit, se.fit) 
}

all_best <- map_df(trend_spp$alpha.code, get_best_model) %>%
  bind_rows(data.frame(alpha.code = "BLSC", Modnames = "year")) # best BLSC model (year2) gives estimates for the early years that seem unrealistically high, second best model (year; delta aicc = 0.3) seems to give more reasonable estimates relative to the raw data. adding the year model to all_best here to extract estimates for both models

all_best_preds <- map2_df(all_best$alpha.code, all_best$Modnames, mod_predictions_link)

all_best_preds_response <- all_best_preds %>%
  data.frame() %>% 
  mutate(lci = exp(fit - (1.96 * se.fit)),
         uci = exp(fit + (1.96 * se.fit)),
         predicted = exp(fit))

saveRDS(all_best_preds_response, here("data_files/all_best_preds_response"))


