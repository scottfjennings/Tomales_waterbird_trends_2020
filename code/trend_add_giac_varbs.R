

library(tidyverse)
library(here)
library(MASS) # for glm.nb
library(AICcmodavg)




giac_spp <- c("AMWI", "BUFF", "CANG", "GADW", "GWTE", "MALL", "NOPI", "PBGR")

giac_varbs <- data.frame(study.year = seq(1992, 2022)) %>% 
  mutate(giac = ifelse(study.year < 2009, 0, 1))

add_giac_mods <- function(zspp) {
  
  
zspp_annual <- readRDS(here("data_files/spp_annual_full_preds")) %>% 
  filter(alpha.code == zspp) %>% 
  full_join(giac_varbs)
  
  zspp_mods <- readRDS(here("fitted_models/all_spp_mods"))[zspp][[1]][[1]] 
  
  zspp_mods[["aic_tab"]] <- NULL
  
  competitive <- readRDS(here("fitted_models/all_spp_mods"))[zspp][[1]][[1]][["aic_tab"]] %>% 
    filter(Delta_AICc <= 2, Modnames != "intercept") %>% 
    dplyr::select(Modnames)
  
  update_mod <- function(zmod_name) {
    newmod <- update(zspp_mods[[zmod_name]], ~ . + giac)
  }
  
zmods <- map(competitive$Modnames, update_mod)  
names(zmods) <- paste(competitive$Modnames, "_giac", sep = "")
  
zspp_mods_new = append(zspp_mods, zmods) 

wbird_aic <- aictab(zspp_mods_new, names(zspp_mods_new)) %>% 
  data.frame() %>% 
  mutate(species = zspp)

zspp_mods_new$aic_tab <- wbird_aic

zspp_mods_new$aic_tab_no_giac <- readRDS(here("fitted_models/all_spp_mods"))[zspp][[1]][[1]][["aic_tab"]]

return(zspp_mods_new)

}


giac_spp_mods <- map(giac_spp, quietly(add_giac_mods))

names(giac_spp_mods) <- giac_spp


saveRDS(giac_spp_mods, here("fitted_models/giac_spp_mods"))

#giac_spp_mods <- readRDS(here("fitted_models/giac_spp_mods"))

non_giac_spp <- readRDS(here("data_files/trend_spp")) %>% 
  filter(!alpha.code %in% giac_spp)

final_models <- c(readRDS(here("fitted_models/all_spp_mods"))[non_giac_spp$alpha.code], giac_spp_mods)



saveRDS(final_models, here("fitted_models/final_models"))
