

library(tidyverse)
library(here)
library(MASS) # for glm.nb
library(AICcmodavg)

trend_spp <- readRDS(here("data_files/trend_spp"))

# fit models to each species ----

fit_wbird_mods <- function(zspp) {
zspp_annual <- readRDS(here("data_files/spp_annual_full_preds")) %>% 
  filter(alpha.code == zspp)
# trend and both environmental
year2_fresh_moci <- glm.nb(p75.abund ~ poly(study.year, 2) + fresh + moci, data = zspp_annual)
year_fresh_moci <- glm.nb(p75.abund ~ study.year + fresh + moci, data = zspp_annual)

# trend and single environmental
year2_moci <- glm.nb(p75.abund ~ poly(study.year, 2) + moci, data = zspp_annual)
year_moci <- glm.nb(p75.abund ~ study.year + moci, data = zspp_annual)
year2_fresh <- glm.nb(p75.abund ~ poly(study.year, 2) + fresh, data = zspp_annual)
year_fresh <- glm.nb(p75.abund ~ study.year + fresh, data = zspp_annual)

# trend only
year2 <- glm.nb(p75.abund ~ poly(study.year, 2), data = zspp_annual)
year <- glm.nb(p75.abund ~ study.year, data = zspp_annual)

# environmental only
fresh_moci <- glm.nb(p75.abund ~ fresh + moci, data = zspp_annual)
fresh <- glm.nb(p75.abund ~ fresh, data = zspp_annual)
moci <- glm.nb(p75.abund ~ moci, data = zspp_annual)

#intercept only
intercept <- glm.nb(p75.abund ~ 1, data = zspp_annual)

zspp_mods <- list(year2_fresh_moci, year_fresh_moci, year2_moci, year_moci, year2_fresh, year_fresh, year2, year, fresh_moci, fresh, moci, intercept)
names(zspp_mods) <- c("year2_fresh_moci", "year_fresh_moci", "year2_moci", "year_moci", "year2_fresh", "year_fresh", "year2", "year", "fresh_moci", "fresh", "moci", "intercept")

wbird_aic <- aictab(list(year2_fresh_moci, year_fresh_moci, year2_moci, year_moci, year2_fresh, year_fresh, year2, year, fresh_moci, fresh, moci, intercept), 
                    c("year2_fresh_moci", "year_fresh_moci", "year2_moci", "year_moci", "year2_fresh", "year_fresh", "year2", "year", "fresh_moci", "fresh", "moci", "intercept")) %>% 
  data.frame() %>% 
  mutate(species = zspp)

zspp_mods$aic_tab = wbird_aic

return(zspp_mods)

}

# running inside safely() will capture errors instead of failing
all_spp_mods<- map(trend_spp$alpha.code, quietly(fit_wbird_mods))



names(all_spp_mods) <- trend_spp$alpha.code

all_spp_mods %>% 
  map("warnings") %>% 
  compact() %>% names()
all_spp_mods %>% 
  map("errors") %>% 
  compact()


all_spp_mods<- map(trend_spp$alpha.code, fit_wbird_mods)

names(all_spp_mods) <- trend_spp$alpha.code



saveRDS(all_spp_mods, here("fitted_models/all_spp_mods"))



