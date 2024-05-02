


# generate model averaged estimates of change in abundance


library(tidyverse)
library(here)
library(MASS) # for glm.nb
library(AICcmodavg)
library(birdnames)

options(scipen = 999)

custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")

source(here("code/analysis_utilities.R"))


trend_spp <- readRDS(here("data_files/trend_spp")) %>% 
  filter(!alpha.code %in% c("Omnivore", "Diving benthivore", "Herbivore", "Piscivore"))


znewdat <- data.frame(study.year = seq(1992, 2022),
                      moci = 0,
                      fresh = 0)%>% 
  mutate(giac = ifelse(study.year < 2009, 0, 1))


zspp = "ALL"

competitive_mods <- map_df(trend_spp$alpha.code, get_competitive_models) 

trend_evidence <- competitive_mods %>% 
  group_by(alpha.code) %>% 
  summarise(Modnames = paste(Modnames, collapse = ", ")) %>% 
  ungroup() %>% 
  mutate(year.comp = grepl("year", Modnames),
         int.comp = grepl("intercept", Modnames),
         trend.evidence = year.comp == TRUE & int.comp != TRUE)

no_change_spp <- trend_evidence %>% 
  filter(trend.evidence == FALSE) %>% 
  dplyr::select(alpha.code)



#' spp_mod_ave_plotter
#' 
#' Plot model averaged estimates and raw data for each zspp. This function calculates estimates on the fly to allow plotting estimates from multiple models without having to separately go generate predictions for more than just the best model.
#'
#' @param zspp which species
#' @param zmod character string with the names of the models you want to plot
#'
#' @return returns the plot. does not save to disk
#' @export
#'
#' @examples
spp_mod_ave_plotter <- function(zspp, save.plot = TRUE) {
  
  zmain = ifelse(zspp == "ALL", "All species combined", translate_bird_names(zspp, "alpha.code", "common.name"))
  
  zspp_mods <- readRDS(here("fitted_models/final_models"))[[zspp]]
  zspp_mods[["aic_tab"]] <- NULL
  zspp_mods[["aic_tab_no_giac"]] <- NULL
  
  znewdat <- data.frame(study.year = seq(1992, 2022),
                        moci = 0,
                        fresh = 0)%>% 
    mutate(giac = ifelse(study.year < 2009, 0, 1))
  
  zspp_pred <- modavgPred(zspp_mods, modnames = names(zspp_mods), newdata = znewdat)$matrix.output %>% 
    data.frame() %>% 
    bind_cols(znewdat)  %>%
    mutate(alpha.code = zspp) %>% 
    left_join(readRDS(here("data_files/spp_annual_full_preds")) %>% dplyr::select(alpha.code, study.year, p75.abund)) 
  
  
  iucn_threat <- zspp_pred %>% 
    mutate(annual.per.change = 100 * ((mod.avg.pred/lag(mod.avg.pred))-1),
           iucn.threat = ifelse(annual.per.change <= -2, TRUE, FALSE)) %>% 
    filter(iucn.threat == TRUE) %>% 
    mutate(decline.dur = n()) %>% 
    filter(decline.dur >= 10)
  
  
  y.max = ifelse(max(zspp_pred$p75.abund, na.rm = TRUE) > max(zspp_pred$upper.CL), 
                 max(zspp_pred$p75.abund, na.rm = TRUE),
                 max(zspp_pred$upper.CL))
  
  y.rounder = case_when(y.max <= 100 ~ 10,
                        between(y.max, 100, 500) ~ 50,
                        between(y.max, 500, 1000) ~ 100,
                        between(y.max, 1000, 5000) ~ 500,
                        y.max > 5000 ~ 1000)
  
  
  y.splitter = case_when(y.max < 70 ~ 10,
                         (y.max >= 70 & y.max <  150) ~ 20,
                         (y.max >= 150 & y.max < 300) ~ 50,
                         (y.max >=300 & y.max < 700) ~ 100,
                         (y.max >= 700 & y.max < 1500) ~ 200,
                         (y.max >= 1500 & y.max < 3000) ~ 500,
                         (y.max >= 3000 & y.max < 7000) ~ 1000,
                         (y.max >= 7000 & y.max < 30000) ~ 2000,
                         y.max > 30000 ~ 5000)
  
  
  y.top = ceiling(((y.max)/y.splitter)) * y.splitter
  
  
  y.scale = seq(0, y.top, by = y.splitter)
  y.scale.minor = seq(0, y.top, by = y.splitter/5)
  
  zplot <- ggplot(zspp_pred) +
    geom_ribbon(aes(x = study.year, ymin = lower.CL, ymax = upper.CL), alpha = 0.5) + 
    geom_point(aes(x = study.year, y = p75.abund), size = 3) +
    scale_x_continuous(breaks = seq(1990, 2025, by = 5), labels = seq(1990, 2025, by = 5), minor_breaks = seq(1992, 2022, by = 1)) +
    scale_y_continuous(breaks = y.scale, labels = y.scale, minor_breaks = y.scale.minor, limits = c(0, y.top)) + 
    guides(color = "none") +
    theme_bw() +
    labs(title = zmain,
         x = "Year",
         y = "Model averaged estimated abundance")   
  
  
if(zspp %in% no_change_spp$alpha.code) {
  
  zplot = zplot +
    geom_line(aes(x = study.year, y = mod.avg.pred), linetype = "dashed")
    
}  else {
  if(zspp %in% c("AMWI", "CANG", "GADW", "GWTE", "MALL", "NOPI", "AMCO")) {
    
    zplot <- zplot +
      geom_line(aes(x = study.year, y = mod.avg.pred))  + 
      geom_line(data = filter(iucn_threat, study.year < 2009), aes(x = study.year, y = mod.avg.pred), linewidth = 1, color = "red", show.legend = FALSE) +
      geom_line(data = filter(iucn_threat, study.year >= 2009), aes(x = study.year, y = mod.avg.pred), linewidth = 1, color = "red", show.legend = FALSE)
  } else {
    
    zplot <- zplot +
      geom_line(aes(x = study.year, y = mod.avg.pred))   + 
      geom_line(data = iucn_threat, aes(x = study.year, y = mod.avg.pred), linewidth = 1, color = "red", show.legend = FALSE)
  }
  #zplot
  
}
  if(save.plot == TRUE) {
    ggsave(here(paste("figures_output/mod_avg/mod_avg_", zspp, ".png", sep = "")), zplot, height = 6, width = 6)
  } else {
    return(zplot)
  }
}
  

#spp_mod_ave_plotter("BRPE", save.plot = FALSE)


map(trend_spp$alpha.code, spp_mod_ave_plotter)

trend_spp <- trend_spp %>% 
  mutate(save.plot = FALSE)

zall_plots <- map2(trend_spp$alpha.code, trend_spp$save.plot, spp_mod_ave_plotter)

saveRDS(zall_plots, here("figures_output/mod_avg/all_plots_mod_avg"))  

