


library(tidyverse)
library(here)
library(birdnames)
library(flextable)
library(officer)

options(scipen = 999)

custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")

source(here("code/analysis_utilities.R"))

trend_spp <- readRDS(here("data_files/trend_spp"))

year_breaks <- seq(1990, 2025, by = 5)

# plot model predictions for each species ----

all_best_preds_response <- readRDS(here("data_files/all_best_preds_response")) %>% 
# all_best_preds_response <- all_best_preds_response %>% 
  mutate(common.name = translate_bird_names(alpha.code, "alpha.code", "common.name"),
         common.name = ifelse(common.name == "all", "All species combined", common.name)) %>% 
  full_join(readRDS(here("data_files/spp_annual_full_preds")) %>% select(alpha.code, study.year, p75.abund))



#' spp_plotter
#' 
#' Plot estimates and raw data for each model in all_best_preds_response for each species, but only does year2 model for BLSC
#'
#' @param zspp 
#'
#' @return saves plot to figures_output/
#' @export
#'
#' @examples
spp_plotter <- function(zspp) {
  zmain = ifelse(zspp == "ALL", "All species combined", translate_bird_names(zspp, "alpha.code", "common.name"))

  zdat <- all_best_preds_response %>% 
    filter(alpha.code == zspp)
  
  if(zspp == "BLSC") {
    zdat = filter(zdat, Modnames == "year2")
  }
  
  zymax = (ceiling(max(zdat$uci * 1.1)/100) * 100)
  
zplot <- zdat %>% 
  ggplot() +
  geom_point(aes(x = study.year, y = p75.abund)) +
  geom_line(aes(x = study.year, y = predicted)) +
    geom_ribbon(aes(x = study.year, ymin = lci, ymax = uci), alpha = 0.5) +
  scale_x_continuous(breaks = year_breaks, labels = year_breaks) +
    theme_bw() +
  ylim(0, zymax) +
    labs(title = zmain,
         x = "Year",
         y = "Estimated abundance")
  
  ggsave(here(paste("figures_output/", zspp, ".png", sep = "")), zplot, height = 6, width = 6)
  
}


spp_plotter("BLSC")

map(trend_spp$alpha.code, spp_plotter)

# stick plots for all species in same figure ----
all_best_preds_response %>% 
  filter(!is.na(alpha.code)) %>% 
  ggplot() +
  geom_point(aes(x = study.year, y = p75.abund)) +
  geom_line(aes(x = study.year, y = predicted)) +
    geom_ribbon(aes(x = study.year, ymin = lci, ymax = uci), alpha = 0.5) +
    theme_bw() +
    labs(x = "Year",
         y = "Estimated abundance") +
  facet_wrap(~alpha.code, scales = "free_y")



##
# plot year2 and year model estimates together for BLSC ----

blsc_2mods <- all_best_preds_response %>% 
    filter(alpha.code == "BLSC") %>% 
  mutate(Modnames = factor(Modnames))


blsc_2mods_plot <- blsc_2mods %>% 
  ggplot() +
  geom_point(aes(x = study.year, y = p75.abund)) +
  geom_line(aes(x = study.year, y = predicted, linetype = Modnames)) +
    geom_ribbon(aes(x = study.year, ymin = lci, ymax = uci, linetype = Modnames), alpha = 0.5) +
    theme_bw() +
  theme(legend.position = c(.8, .8),
        legend.text.align = 0) +
    labs(title = "Black Scoter",
         x = "Year",
         y = "Estimated abundance",
         linetype = "Model") +    
  scale_linetype_discrete(breaks=levels(blsc_2mods$Modnames),
                          labels = expression(Year, Year^2))

blsc_2mods_plot

  ggsave(here("figures_output/BLSC_2mods.png"), blsc_2mods_plot, height = 6, width = 6)

# plot predictor variables ----


predictor_plot <- readRDS(here("data_files/predictors")) %>%
  select(-giac) %>% 
  pivot_longer(cols = c(annual.freshwater, mean.moci), names_to = "predictor", values_to = "predictor.value") %>% 
  mutate(predictor = ifelse(predictor == "mean.moci", "MOCI", "freshwater inflow")) %>% 
  ggplot() +
    geom_line(aes(x = study.year, y = predictor.value)) +
  scale_x_continuous(breaks = year_breaks, labels = year_breaks) +
    facet_wrap(~predictor, scales = "free_y", ncol = 1) +
    theme_bw() +
    labs(x = "Year",
         y = "Predictor value")

  ggsave(here("figures_output/predictor_plot.png"), predictor_plot, height = 6, width = 6)
  
