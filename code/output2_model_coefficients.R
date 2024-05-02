
# get estimated coefficients from best or competitive models for each species

library(tidyverse)
library(here)
library(birdnames)
library(flextable)
library(officer)

options(scipen = 999)

custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")

source(here("code/analysis_utilities.R"))

trend_spp <- readRDS(here("data_files/trend_spp"))

# get_competitive_models() and mod_coefs() are from analysis_utilities.R

all_competitive <- map_df(trend_spp$alpha.code, get_competitive_models)

all_comp_coefs <- map2_df(all_competitive$alpha.code, all_competitive$Modnames, mod_coefs)

saveRDS(all_comp_coefs, here("data_files/all_competitive_coefs"))

all_comp_dev_expl <- map2_df(all_competitive$alpha.code, all_competitive$Modnames, mod_dev_explained)

all_aic <- map_df(trend_spp$alpha.code, get_aic)

inform_mod_names <- all_comp_coefs %>% 
  filter(variable != "(Intercept)") %>% 
  mutate(#informative = ifelse((lci < 0 & uci < 0) | (lci > 0 & uci > 0), TRUE, FALSE),
         informative = ifelse(sign(lci) == sign(uci), TRUE, FALSE),
         variable = ifelse(informative == FALSE & !variable %in% c("poly(study.year, 2)1"), sprintf(paste(variable, "^\u2020^", sep = "")), variable),
         variable = fix_varb_names(variable)) %>%
  group_by(alpha.code, Modnames) %>% 
  summarise(mod.name.out = paste(variable, collapse = " + "))

all_comp_coefs_wide <- all_comp_coefs %>% 
  mutate(across(c(coefficient, lci, uci, Delta_AICc), ~round(., 3)),
         informative = ifelse(sign(lci) == sign(uci), "", "*"),
         variable = fix_varb_names(variable),
         coef.ci = paste(coefficient, " (", lci, ", ", uci, ")", informative, sep = "")) %>% 
  pivot_wider(id_cols = c(alpha.code, Modnames), names_from = variable, values_from = coef.ci)

coefs_wide <- full_join(all_comp_coefs_wide, inform_mod_names) %>%  
  full_join(all_comp_dev_expl) %>% 
  left_join(all_aic %>% select(alpha.code = species, Modnames, Delta_AICc)) %>% 
  mutate(mod.name.out = gsub("Year \\+ Year\\^2\\^", "Year^2^", mod.name.out),
         Delta_AICc = round(Delta_AICc, 2),
         mod.name.out = replace_na(mod.name.out, "Intercept only"),
         common.name = translate_bird_names(alpha.code, "alpha.code", "common.name"),
         common.name = ifelse(common.name == "all", "All species combined", common.name)) %>%
  select(common.name, mod.name.out, Year, "Year^2^", "Freshwater inflow", MOCI, Restoration, dev.expl, Delta_AICc, alpha.code, Modnames)



saveRDS(coefs_wide, here("data_files/coefs_wide"))


coefs_wide <- readRDS(here("data_files/coefs_wide"))


# guild model

guild_competitive <- get_competitive_models("guild")


guild_comp_coefs <- map2_df(guild_competitive$alpha.code, guild_competitive$Modnames, mod_coefs)


saveRDS(guild_comp_coefs, here("data_files/guild_competitive_coefs"))

guild_comp_dev_expl <- map2_df(guild_competitive$alpha.code, guild_competitive$Modnames, mod_dev_explained)

guild_aic <- get_aic("guild")

guild_inform_mod_names <- guild_comp_coefs %>% 
  filter(variable != "(Intercept)") %>% 
  mutate(informative = ifelse((lci < 0 & uci < 0) | (lci > 0 & uci > 0), TRUE, FALSE),
         variable = ifelse(informative == FALSE & !variable %in% c("poly(study.year, 2)1"), sprintf(paste(variable, "^\u2020^", sep = "")), variable),
         variable = fix_varb_names(variable)) %>%
  group_by(alpha.code, Modnames) %>% 
  summarise(mod.name.out = paste(variable, collapse = " + "))

guild_comp_coefs_wide <- guild_comp_coefs %>% 
  mutate(across(c(coefficient, lci, uci, Delta_AICc), ~round(., 3)),
         variable = fix_varb_names(variable),
         coef.ci = paste(coefficient, " (", lci, ", ", uci, ")", sep = "")) %>% 
  pivot_wider(id_cols = c(alpha.code, Modnames), names_from = variable, values_from = coef.ci)

coefs_wide <- full_join(all_comp_coefs_wide, inform_mod_names) %>%  
  full_join(all_comp_dev_expl) %>% 
  left_join(all_aic %>% select(alpha.code = species, Modnames, Delta_AICc)) %>% 
  mutate(mod.name.out = gsub("Year \\+ Year\\^2\\^", "Year^2^", mod.name.out),
         Delta_AICc = round(Delta_AICc, 2),
         mod.name.out = replace_na(mod.name.out, "Intercept only"),
         common.name = translate_bird_names(alpha.code, "alpha.code", "common.name"),
         common.name = ifelse(common.name == "all", "All species combined", common.name)) %>%
  select(common.name, mod.name.out, Year, "Year^2^", "Freshwater inflow", MOCI, Restoration, dev.expl, Delta_AICc, alpha.code, Modnames)



saveRDS(coefs_wide, here("data_files/coefs_wide"))

