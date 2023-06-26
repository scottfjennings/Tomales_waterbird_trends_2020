


# check diagnostic plots for best model and full model for each species


library(tidyverse)
library(here)

trend_spp <- readRDS(here("data_files/trend_spp"))

best_model_diagnostics <- function(zspp, which_mod = "best") {
  
zspp_mods <- readRDS(here("fitted_models/all_spp_mods"))[zspp][[1]][[1]]

if(which_mod == "best") {
best_mod_name <- zspp_mods$aic_tab %>% 
  filter(Delta_AICc == 0)

best_mod <- zspp_mods[[best_mod_name$Modnames]]

zmain = paste(zspp, "\n", best_mod_name$Modnames)

op <- par(mfrow = c(2, 2))
plot(best_mod, main = zmain)
par(op)

} else {

plot_mod <- zspp_mods[[which_mod]]

zmain = paste(zspp, "\n", which_mod)

op <- par(mfrow = c(2, 2))
plot(plot_mod, main = zmain)
par(op)
}

}

# good > ok > fair > bad

best_model_diagnostics("ALL")
# ok
best_model_diagnostics("ALL", which_mod = "year2_fresh_moci")
# fair
#-
best_model_diagnostics("AMWI")
# ok
best_model_diagnostics("AMWI", which_mod = "year2_fresh_moci")
# fair
#-
best_model_diagnostics("BLSC")
# ok
best_model_diagnostics("BLSC", which_mod = "year2_fresh_moci")
# ok
best_model_diagnostics("BRAC")
# fair
best_model_diagnostics("BRAC", which_mod = "year2_fresh_moci")
# fair
best_model_diagnostics("BRAN")
# fair
best_model_diagnostics("BRAN", which_mod = "year2_fresh_moci")
# fair
best_model_diagnostics("BRPE")
# fair
best_model_diagnostics("BRPE", which_mod = "year2_fresh_moci")
# fair
best_model_diagnostics("BUFF")
# ok
best_model_diagnostics("BUFF", which_mod = "year2_fresh_moci")
# ok
best_model_diagnostics("CANG")
# ok
best_model_diagnostics("CANG", which_mod = "year2_fresh_moci")
# ok
best_model_diagnostics("CLGR")
# good
best_model_diagnostics("CLGR", which_mod = "year2_fresh_moci")
# fair
best_model_diagnostics("COGO")
# good
best_model_diagnostics("COGO", which_mod = "year2_fresh_moci")
# good
best_model_diagnostics("COLO")
# bad, but intercept is best
readRDS(here("fitted_models/all_spp_mods"))["COLO"][[1]][[1]][["aic_tab"]]
best_model_diagnostics("COLO", which_mod = "year")
# ok
best_model_diagnostics("COLO", which_mod = "year2_fresh_moci")
# fair
best_model_diagnostics("COME")
# ok
best_model_diagnostics("COME", which_mod = "year2_fresh_moci")
# ok
best_model_diagnostics("DCCO")
# fair
best_model_diagnostics("DCCO", which_mod = "year2_fresh_moci")
# fair
best_model_diagnostics("EAGR")
# good
best_model_diagnostics("EAGR", which_mod = "year2_fresh_moci")
# good
best_model_diagnostics("FOTE")
# bad, but intercept is best
readRDS(here("fitted_models/all_spp_mods"))["FOTE"][[1]][[1]][["aic_tab"]]
best_model_diagnostics("FOTE", which_mod = "moci") # delta aicc = 0.7
# fair
best_model_diagnostics("FOTE", which_mod = "year") # delta aicc = 1.5
# good
best_model_diagnostics("FOTE", which_mod = "year2_fresh_moci")
# fair
best_model_diagnostics("GADW")
# ok
best_model_diagnostics("GADW", which_mod = "year2_fresh_moci")
# ok
best_model_diagnostics("GWTE")
# good
best_model_diagnostics("GWTE", which_mod = "year2_fresh_moci")
# ok
best_model_diagnostics("HOGR")
# ok
best_model_diagnostics("HOGR", which_mod = "year2_fresh_moci")
# fair
best_model_diagnostics("MALL")
# ok
best_model_diagnostics("MALL", which_mod = "year2_fresh_moci")
# ok
best_model_diagnostics("NOPI")
# ok
best_model_diagnostics("NOPI", which_mod = "year2_fresh_moci")
# ok
best_model_diagnostics("PALO")
# good
best_model_diagnostics("PALO", which_mod = "year2_fresh_moci")
# ok
best_model_diagnostics("PBGR")
# ok
best_model_diagnostics("PBGR", which_mod = "year2_fresh_moci")
# ok
best_model_diagnostics("PECO")
# good
best_model_diagnostics("PECO", which_mod = "year2_fresh_moci")
# ok
best_model_diagnostics("RBME")
# fair
best_model_diagnostics("RBME", which_mod = "year2_fresh_moci")
# ok
best_model_diagnostics("RNGR")
# bad, but intercept is best
readRDS(here("fitted_models/all_spp_mods"))["RNGR"][[1]][[1]][["aic_tab"]]
best_model_diagnostics("RNGR", which_mod = "year2") # delta aicc = 0.9
# ok
best_model_diagnostics("RNGR", which_mod = "year") # delta aicc = 1.4
# ok
best_model_diagnostics("RNGR", which_mod = "year2_fresh_moci")
# fair
best_model_diagnostics("RTLO")
# fair
best_model_diagnostics("RTLO", which_mod = "year2_fresh_moci")
# ok
best_model_diagnostics("RUDU")
# fair
best_model_diagnostics("RUDU", which_mod = "year2_fresh_moci")
# fair
best_model_diagnostics("SCAUP")
# ok
best_model_diagnostics("SCAUP", which_mod = "year2_fresh_moci")
# ok
best_model_diagnostics("SUSC")
# good
best_model_diagnostics("SUSC", which_mod = "year2_fresh_moci")
# ok
best_model_diagnostics("WEGR")
# ok
best_model_diagnostics("WEGR", which_mod = "year2_fresh_moci")
# ok

