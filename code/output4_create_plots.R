


library(tidyverse)
library(here)
library(birdnames)

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

# annual percent change

annual_per_change <- all_best_preds_response %>% 
  filter(grepl("year", Modnames)) %>% 
  arrange(common.name, Modnames, study.year) %>% 
  group_by(common.name, Modnames) %>% 
  mutate(annual.per.change = 100 * ((predicted/lag(predicted))-1),
         iucn.threat = ifelse(annual.per.change <= -2, TRUE, FALSE)) %>% 
  filter(iucn.threat == TRUE) %>% 
  mutate(decline.dur = n()) %>% 
  filter(decline.dur >= 10)


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

    
  iucn_threat <- annual_per_change %>% 
    filter(alpha.code == zspp, iucn.threat == TRUE) %>% 
    inner_join(zdat)
  
  zymax = (ceiling(max(zdat$uci * 1.1)/100) * 100)
  
zplot <- zdat %>% 
  ggplot() +
  geom_point(aes(x = study.year, y = p75.abund)) +
  geom_line(aes(x = study.year, y = predicted)) +
  geom_ribbon(aes(x = study.year, ymin = lci, ymax = uci), alpha = 0.5) +
  geom_line(data = iucn_threat, aes(x = study.year, y = predicted), color = "red") +
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
  geom_line(aes(x = study.year, y = predicted, linetype = Modnames)) +
    geom_ribbon(aes(x = study.year, ymin = lci, ymax = uci, linetype = Modnames), alpha = 0.5) +
    theme_bw() +
  theme(legend.position = c(.8, .8),
        legend.text.align = 0) +
    labs(x = "Year",
         y = "Estimated abundance") +
  facet_wrap(~alpha.code, scales = "free_y")



# plot GWTE with split in red line for restoration ----
  gwte <- all_best_preds_response %>% 
    filter(alpha.code == "GWTE") %>% 
  mutate(Modnames = factor(Modnames, levels = c("year_giac", "intercept")))


  gwte_iucn_threat <- annual_per_change %>% 
    filter(iucn.threat == TRUE) %>% 
    inner_join(gwte)
  
  zymax = (ceiling(max(gwte$uci * 1.1)/100) * 100)
  
gwte_plot <- gwte %>% 
  ggplot() +
  geom_point(aes(x = study.year, y = p75.abund)) +
  geom_line(aes(x = study.year, y = predicted, linetype = Modnames)) +
  geom_ribbon(aes(x = study.year, ymin = lci, ymax = uci, linetype = Modnames), alpha = 0.5) +
  geom_line(data = filter(gwte_iucn_threat, study.year < 2009), aes(x = study.year, y = predicted, linetype = Modnames), color = "red", show_guide = FALSE) +
  geom_line(data = filter(gwte_iucn_threat, study.year > 2009), aes(x = study.year, y = predicted, linetype = Modnames), color = "red", show_guide = FALSE) +
  scale_x_continuous(breaks = year_breaks, labels = year_breaks) +
    theme_bw() +
  theme(legend.position = c(.8, .8),
        legend.text.align = 0) +
    labs(title = "Green-winged Teal",
         x = "Year",
         y = "Estimated abundance",
         linetype = "Model") +    
  scale_linetype_discrete(breaks=levels(gwte$Modnames),
                          labels = expression("Year + Restoration", "Intercept only"))
  
gwte_plot

  ggsave(here("figures_output/GWTE_2mods.png"), gwte_plot, height = 6, width = 6)
# plot year2 and year model estimates together for BLSC ----

blsc_2mods <- all_best_preds_response %>% 
    filter(alpha.code == "BLSC") %>% 
  mutate(Modnames = factor(Modnames, levels = c("year2", "year")))


  iucn_threat <- annual_per_change %>% 
    filter(iucn.threat == TRUE) %>% 
    inner_join(blsc_2mods)

blsc_2mods_plot <- blsc_2mods %>% 
  ggplot() +
  geom_point(aes(x = study.year, y = p75.abund)) +
  geom_line(aes(x = study.year, y = predicted, linetype = Modnames)) +
    geom_ribbon(aes(x = study.year, ymin = lci, ymax = uci, linetype = Modnames), alpha = 0.5) +
  geom_line(data = iucn_threat, aes(x = study.year, y = predicted, linetype = Modnames), color = "red", show_guide = FALSE) +
    theme_bw() +
  theme(legend.position = c(.8, .8),
        legend.text.align = 0) +
    labs(title = "Black Scoter",
         x = "Year",
         y = "Estimated abundance",
         linetype = "Model") +    
  scale_linetype_discrete(breaks=levels(blsc_2mods$Modnames),
                          labels = expression(Year^2, Year))

blsc_2mods_plot

  ggsave(here("figures_output/BLSC_2mods.png"), blsc_2mods_plot, height = 6, width = 6)

# plot predictor variables ----


predictor_plot <- readRDS(here("data_files/predictors")) %>%
  dplyr::select(-giac) %>% 
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
  
  
  
# plots by foraging guild ----
  
guilds <- read.csv(here("data_files/Foraging guild table.csv")) %>% 
  mutate(alpha.code = translate_bird_names(common.name, "common.name", "alpha.code")) %>% 
  dplyr::select(alpha.code, guild)
  
  # percent change
  
percent_changes <- readRDS(here("data_files/percent_changes")) %>% 
    filter(!is.na(percent.change))

per_change_guilds <- inner_join(percent_changes, guilds)


per_change_guilds %>% 
  filter(phase == "overall") %>%
  filter(alpha.code != "CANG") %>% 
ggplot() +
  geom_hline(aes(yintercept = 0), color = "red") +
  geom_boxplot(aes(y = percent.change, x = guild)) +
  #geom_point(aes(y = percent.change, x = guild)) +
  labs(y = "% change",
       x = "Foraging guild") +
  theme_bw() 

ggsave(here("figures_output/percent_change_guild.png"), width = 7.5)
  

# scaled predicted abundance
scaled_predictions <- readRDS(here("data_files/all_best_preds_response")) %>%
  mutate(common.name = translate_bird_names(alpha.code, "alpha.code", "common.name")) %>% 
  filter(alpha.code != "ALL",
         !(alpha.code == "GWTE" & Modnames == "year_giac"),
         !(alpha.code == "BLSC" & Modnames == "year2")) %>%  
  group_by(common.name) %>% 
  mutate(mean.pred = mean(predicted),
         sd.pred = sd(predicted),
         scale.pred = (predicted - mean.pred)/sd.pred) %>% 
  ungroup() %>% 
  mutate(scale.pred = ifelse(is.nan(scale.pred), 0, scale.pred)) %>% 
  inner_join(guilds)




guild_plotter <- function(zguild) {
guild_plot <- scaled_predictions %>% 
  filter(guild == zguild) %>% 
  ggplot() +
  geom_line(aes(x = study.year, y = scale.pred, color = common.name)) +
  facet_wrap(~guild) +
  labs(color = "",
       y = "",
       x = "") +
  theme_bw() +
  scale_x_continuous(breaks = year_breaks, labels = year_breaks)
}


herb <- guild_plotter("Herbivore")

div_benth <- guild_plotter("Diving benthivore")

omni <- guild_plotter("Omnivore")

pisc <- guild_plotter("Piscivore")

# plot as grid in 1 columns
cowplot::plot_grid(herb, div_benth, pisc, ncol = 1,
                   align = 'v')

ggsave(here("figures_output/scaled_predictions_guild.png"), width = 7.5, height = 10)


# moci, freshwater coefs by guild --

all_comp_coefs <- readRDS(here("data_files/all_competitive_coefs")) %>% 
  filter(variable %in% c("fresh", "moci"), Delta_AICc == 0) %>% 
  inner_join(guilds) %>% 
  #group_by(guild, variable) %>% 
  #mutate(mean.coef = mean(coefficient),
  #          sd.coef = sd(coefficient),
  #          num.spp = n()) %>% 
  #ungroup()  %>%
  mutate(variable = ifelse(variable == "moci", "MOCI", "Freshwater inflow"),
         guild = gsub("Diving ", "Diving\n", guild),
         common.name = translate_bird_names(alpha.code, "alpha.code", "common.name"))


ggplot(data = all_comp_coefs, aes(x = guild, y = coefficient, color = common.name)) +
    geom_point(size = 4, position=position_dodge(width=0.5)) +
    geom_errorbar(
        aes(ymin = lci, ymax = uci),
        width = 0.1,
        position=position_dodge(width=0.5)) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") + 
    theme_bw() +
  facet_wrap(~variable) +
  labs(x = "Foraging guild", 
       y = "Mean coefficient value",
       color = "") +
  scale_y_continuous(breaks = seq(-0.8, 0.9, by = 0.2), labels =  seq(-0.8, 0.9, by = 0.2))


ggsave(here("figures_output/moci_fresh_mean_coefs.png"), width = 7.5, height = 7)




