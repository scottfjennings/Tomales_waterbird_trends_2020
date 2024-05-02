


library(tidyverse)
library(cowplot)
library(here)
library(birdnames)

custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list") 

source(here("code/analysis_utilities.R"))

trend_spp <- readRDS(here("data_files/trend_spp"))

year_breaks <- seq(1990, 2025, by = 5)

# plot model predictions for best model or multiple models for each species ----

# spp_mod_plotter lives in utilities
# these species only need best model plotted and don't need legend adjustments
map(c("AMCO", "BRAC", "BRPE", "COGO", "DCCO", "FOTE", "GADW", "HOGR", "MALL", "PALO", "RTLO", "SCAUP", "SUSC", "WCGR"), spp_mod_plotter)


# these species only need best model plotted but need legend position changed ----
all <- spp_mod_plotter("ALL", save.plot = FALSE) +
  theme(legend.position = c(.95, .05), 
        legend.justification=c("right", "bottom"))  +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/ALL.png"), all, height = 6, width = 6)

buff <- spp_mod_plotter("BUFF", save.plot = FALSE) +
  theme(legend.position = c(.95, .05), 
        legend.justification=c("right", "bottom"))  +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/BUFF.png"), buff, height = 6, width = 6)
 
cang <- spp_mod_plotter("CANG", save.plot = FALSE) +
  theme(legend.position = c(.05, .95), 
        legend.justification=c("left", "top"))  +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/CANG.png"), cang, height = 6, width = 6)
 
colo <- spp_mod_plotter("COLO", save.plot = FALSE) +
  theme(legend.position = c(.05, .95), 
        legend.justification=c("left", "top"))  +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/COLO.png"), colo, height = 6, width = 6)
 
come <- spp_mod_plotter("COME", save.plot = FALSE) +
  theme(legend.position = c(.05, .95), 
        legend.justification=c("left", "top")) +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/COME.png"), come, height = 6, width = 6)
  
eagr <- spp_mod_plotter("EAGR", save.plot = FALSE) +
  theme(legend.position = c(.02, .98), 
        legend.justification=c("left", "top")) +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/EAGR.png"), eagr, height = 6, width = 6)
 
pbgr <- spp_mod_plotter("PBGR", save.plot = FALSE) +
  theme(legend.position = c(.05, .95), 
        legend.justification=c("left", "top")) +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/PBGR.png"), pbgr, height = 6, width = 6)
 
rngr <- spp_mod_plotter("RNGR", save.plot = FALSE) +
  theme(legend.position = c(.02, .98), 
        legend.justification=c("left", "top")) +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/RNGR.png"), rngr, height = 6, width = 6)
  

 
  
# These species need multiple models plotted. want to save with different file name so even when don't need legend adjustments, use save.plot = FALSE and save manually ----
amwi <- spp_mod_plotter("AMWI", c("fresh_moci_giac", "year_fresh_moci"), save.plot = FALSE) +
  theme(legend.position = c(.05, .98), 
        legend.justification=c("left", "top")) +
  labs(x = "",
       y = "") +
  ylim(0, 2200)
# ggsave(here("figures_output/AMWI_2mods.png"), amwi, height = 6, width = 6)

blsc <- spp_mod_plotter("BLSC", c("year2", "year"), save.plot = FALSE) +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/BLSC_2mods.png"), blsc, height = 6, width = 6)

bran <- spp_mod_plotter("BRAN", c("year2", "intercept"), save.plot = FALSE) +
  theme(legend.position = c(.05, .95), 
        legend.justification=c("left", "top")) +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/BRAN_2mods.png"), bran, height = 6, width = 6)

cogo <- spp_mod_plotter("COGO", c("year_fresh_moci", "year_moci", "year_fresh"), save.plot = FALSE) +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/COGO_3mods.png"), cogo, height = 6, width = 6)

come <- spp_mod_plotter("COME", c("year_moci", "year"), save.plot = FALSE) +
  theme(legend.position = c(.05, .95), 
        legend.justification=c("left", "top")) +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/COME_2mods.png"), come, height = 6, width = 6)

nopi <- spp_mod_plotter("NOPI", c("year2", "intercept"), save.plot = FALSE) +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/NOPI_2mods.png"), nopi, height = 6, width = 6)

peco <- spp_mod_plotter("PECO", c("year", "intercept"), save.plot = FALSE) +
  theme(legend.position = c(.05, .95), 
        legend.justification=c("left", "top")) +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/PECO_2mods.png"), peco, height = 6, width = 6)

rbme <- spp_mod_plotter("RBME", c("year2", "year"), save.plot = FALSE) +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/RBME_2mods.png"), rbme, height = 6, width = 6)
 
rudu <- spp_mod_plotter("RUDU", c("year2_fresh", "year"), save.plot = FALSE) +
  labs(x = "",
       y = "")
# ggsave(here("figures_output/RUDU_2mods.png"), rudu, height = 6, width = 6)
  
#
# plot GWTE with split in red line for restoration ----

    mod.ranks <- get_aic("GWTE") %>%
    filter(Modnames %in% c("year_giac", "intercept")) %>%
    distinct(Modnames, Delta_AICc) %>% 
    arrange(Delta_AICc) %>%
    mutate(mod.name.out = ifelse(Modnames == "intercept", "Intercept only", Modnames)) %>% 
    fix_mod_name_out()


  gwte_dat <- map2_df("GWTE", c("year_giac", "intercept"), mod_predictions_link) %>%
  data.frame() %>% 
  mutate(lci = exp(fit - (1.96 * se.fit)),
         uci = exp(fit + (1.96 * se.fit)),
         predicted = exp(fit)) %>%
  left_join(readRDS(here("data_files/spp_annual_full_preds")) %>% select(alpha.code, study.year, p75.abund)) %>% 
  mutate(mod.name.out = ifelse(Modnames == "intercept", "Intercept only", Modnames)) %>% 
    fix_mod_name_out() %>% 
    left_join(get_aic("GWTE") %>% select(Modnames, Delta_AICc)) %>% 
    arrange(Delta_AICc, study.year) %>% 
    mutate(mod.name.out = factor(mod.name.out, levels = mod.ranks$mod.name.out))
  
  
  gwte_iucn_threat <- gwte_dat %>% 
    mutate(common.name = translate_bird_names(alpha.code, "alpha.code", "common.name")) %>%
    is_iucn_threatened_trend()
  
  
   
  y.max = ifelse(max(gwte_dat$p75.abund, na.rm = TRUE) > max(gwte$uci), 
                 max(gwte_dat$p75.abund, na.rm = TRUE),
                 max(gwte_dat$uci))
  
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
  
  
gwte <- gwte_dat %>% 
  ggplot() +
  geom_line(aes(x = study.year, y = predicted, linetype = mod.name.out)) +
  geom_ribbon(aes(x = study.year, ymin = lci, ymax = uci, linetype = mod.name.out), alpha = 0.5) +
  geom_line(data = filter(gwte_iucn_threat, study.year < 2009), aes(x = study.year, y = predicted, linetype = mod.name.out), linewidth = 1, color = "red", show.legend = FALSE) +
  geom_line(data = filter(gwte_iucn_threat, study.year > 2009), aes(x = study.year, y = predicted, linetype = mod.name.out), linewidth = 1, color = "red", show.legend = FALSE)  +
  geom_point(aes(x = study.year, y = p75.abund), size = 3) +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5), labels = seq(1990, 2025, by = 5), minor_breaks = seq(1992, 2022, by = 1)) +
  scale_y_continuous(breaks = y.scale, labels = y.scale, minor_breaks = y.scale.minor, limits = c(0, y.top)) +
    theme_bw() +
  theme(legend.position = c(.95, .98),
        legend.justification = c("right", "top"),
        legend.text.align = 0,
        legend.title=element_blank()) +
    labs(title = "Green-winged Teal",
         x = "",
         y = "",
         linetype = "") 
  
gwte

  ggsave(here("figures_output/GWTE_2mods.png"), gwte, height = 6, width = 6)
  
# combine all species. need different settings for many species, so faceting doesn't work well. In stead, make each species plot separately then combine with cowplot ----
  
amco <- spp_mod_plotter("AMCO", save.plot = FALSE) +
  labs(x = "",
       y = "")
brac <- spp_mod_plotter("BRAC", save.plot = FALSE) +
  labs(x = "",
       y = "")
brpe <- spp_mod_plotter("BRPE", save.plot = FALSE) +
  labs(x = "",
       y = "")
cogo <- spp_mod_plotter("COGO", save.plot = FALSE) +
  labs(x = "",
       y = "")
dcco <- spp_mod_plotter("DCCO", save.plot = FALSE) +
  labs(x = "",
       y = "")
fote <- spp_mod_plotter("FOTE", save.plot = FALSE) +
  labs(x = "",
       y = "")
gadw <- spp_mod_plotter("GADW", save.plot = FALSE) +
  labs(x = "",
       y = "")
hogr <- spp_mod_plotter("HOGR", save.plot = FALSE) +
  labs(x = "",
       y = "")
mall <- spp_mod_plotter("MALL", save.plot = FALSE) +
  labs(x = "",
       y = "")+
  theme(legend.position = c(.95, .98), 
        legend.justification=c("right", "top")) +
  ylim(0, 150)
palo <- spp_mod_plotter("PALO", save.plot = FALSE) +
  labs(x = "",
       y = "")
rtlo <- spp_mod_plotter("RTLO", save.plot = FALSE) +
  labs(x = "",
       y = "")
scaup <- spp_mod_plotter("SCAUP", save.plot = FALSE) +
  labs(x = "",
       y = "")
susc <- spp_mod_plotter("SUSC", save.plot = FALSE) +
  labs(x = "",
       y = "")
wcgr <- spp_mod_plotter("WCGR", save.plot = FALSE) +
  labs(x = "",
       y = "")
  

all_plots <- list("ALL" = all, "BRAN" = bran, "CANG" = cang, "GADW" = gadw, "AMWI" = amwi, "MALL" = mall, "NOPI" = nopi, "GWTE" = gwte, "SCAUP" = scaup, "SUSC" = susc, "BLSC" = blsc, "BUFF" = buff, "COGO" = cogo, "COME" = come, "RBME" = rbme, "RUDU" = rudu, "PBGR" = pbgr, "HOGR" = hogr, "RNGR" = rngr, "EAGR" = eagr, "WCGR" = wcgr, "AMCO" = amco, "FOTE" = fote, "RTLO" = rtlo, "PALO" = palo, "COLO" = colo, "BRAC" = brac, "PECO" = peco, "DCCO" = dcco, "BRPE" = brpe)

saveRDS(all_plots, here("figures_output/best_mods/all_plots"))  
  
# plot predictor variables ----


predictor_plot <- readRDS(here("data_files/predictors")) %>%
  dplyr::select(-giac) %>% 
  pivot_longer(cols = c(annual.freshwater, mean.moci), names_to = "predictor", values_to = "predictor.value") %>% 
  mutate(predictor = ifelse(predictor == "mean.moci", "MOCI", "Freshwater inflow (mean daily CFS)")) %>% 
  ggplot() +
    geom_line(aes(x = study.year, y = predictor.value)) +
  stat_smooth(aes(x = study.year, y = predictor.value), method = "lm", se = FALSE) +
  scale_x_continuous(breaks = year_breaks, labels = year_breaks) +
    facet_wrap(~predictor, scales = "free_y", ncol = 1) +
    theme_bw() +
    labs(x = "Year",
         y = "Predictor value")

predictor_plot  
  
  ggsave(here("figures_output/Fig2.png"), predictor_plot, height = 6, width = 6, dpi = 600)
  
  
  
# plots by foraging guild NO RUN ----
  
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


# predicted abundance across ranges of fresh and moci ----

# get mean and SD for fresh and moci to backtransform the scaled/centered estimates
pred_backtrans <- readRDS(here("data_files/predictors")) %>% 
  pivot_longer(cols = c("annual.freshwater", "mean.moci"), names_to = "predictor") %>% 
  group_by(predictor) %>% 
  summarise(mean.val = mean(value),
            sd.val = sd(value)) 


moci_fresh_predictions <- bind_rows(readRDS(here("data_files/moci_predictions")) %>% rename("predictor.val" = moci) %>% mutate(predictor = "mean.moci"),
                                    readRDS(here("data_files/fresh_predictions")) %>% rename("predictor.val" = fresh) %>% mutate(predictor = "annual.freshwater")) %>% 
  full_join(pred_backtrans) %>% 
  mutate(predictor.native = (predictor.val*sd.val) + mean.val,
         abund.group = case_when(alpha.code %in% c("ALL", "SCAUP", "BUFF") ~ "high",
                                 alpha.code %in% c("MALL", "AMWI", "EAGR", "RUDU") ~ "medium",
                                 alpha.code %in% c("COME", "PBGR", "GADW") ~ "low"),
         common.name = translate_bird_names(alpha.code, "alpha.code", "common.name"),
         common.name = ifelse(common.name == "all", "All species combined", common.name),
         predictor = ifelse(predictor == "annual.freshwater", "Freshwater inflow (mean daily CFS)", "MOCI"))
  
make_moci_fresh_plot <- function(zabund.group) {
moci_fresh_predictions %>% 
  filter(abund.group == zabund.group) %>% 
  ggplot() +
  geom_line(aes(x = predictor.native, y = predicted, color = common.name)) +
  geom_ribbon(aes(x = predictor.native, ymin = lci, ymax = uci, fill = common.name), alpha = 0.25) +
    labs(x = "",
         y = "",
         color = "",
         fill = "") +
    theme_bw() +
  facet_grid(~predictor, scales = "free") 
}


high <- make_moci_fresh_plot("high") + 
  scale_y_continuous(breaks = seq(5000, 35000, by = 5000), labels = seq(5000, 35000, by = 5000), minor_breaks = seq(3000, 35000, by = 1000)) + 
  scale_colour_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  labs(title = "High abundance species")


medium_colors <- RColorBrewer::brewer.pal(8, "Dark2")[1:4]
medium <- make_moci_fresh_plot("medium") + 
  labs(y = "Estimated bird abundance") +
  scale_y_continuous(minor_breaks = seq(0, 1800, by = 100)) + 
  scale_colour_manual(values = medium_colors) + 
  scale_fill_manual(values = medium_colors) +
  labs(title = "Medium abundance species")

low_colors <- RColorBrewer::brewer.pal(8, "Dark2")[5:8]
low <- make_moci_fresh_plot("low") + labs(x = "Predictor variable value") +
  scale_y_continuous(minor_breaks = seq(0, 300, by = 20)) + 
  scale_colour_manual(values = low_colors) + 
  scale_fill_manual(values = low_colors) +
  labs(title = "Low abundance species")

cowplot::plot_grid(high, medium, low, ncol = 1, align = "v")

ggsave(here("figures_output/Fig4.png"), width = 7.5, height = 7.5, dpi = 600)

# moci, freshwater coefs by guild NO RUN ----

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



# guild plots ----

abund_guilds <- readRDS(here("data_files/abund_guilds"))

guild_n_spp <- read.csv(here("data_files/Foraging guild table.csv")) %>% 
  mutate(alpha.code = translate_bird_names(common.name, "common.name", "alpha.code")) %>% 
  dplyr::select(alpha.code, guild) %>% 
  inner_join(readRDS(here("data_files/spp_annual_full_preds"))) %>%
  distinct(guild, alpha.code) %>% 
  group_by(guild) %>% 
  summarise(n.spp = n()) %>% 
  filter(guild != "Omnivore")
  
guild_mod_names <- get_best_model("guild") %>% 
  mutate(mod.name.out = fix_varb_names(Modnames)) %>% 
  fix_mod_name_out() %>% 
  mutate(mod.name.out = fct_inorder(mod.name.out))

readRDS(here("data_files/guild_preds_response")) %>% 
  left_join(abund_guilds %>% dplyr::select(study.year, guild, guild.total)) %>% 
  full_join(guild_n_spp) %>% 
  right_join(guild_mod_names) %>%  
  mutate(guild = paste(guild, " (", n.spp, " species)", sep = "")) %>% 
  ggplot() +  
  geom_point(aes(x = study.year, y = guild.total)) +
  geom_line(aes(x = study.year, y = predicted, color = mod.name.out)) +
  geom_ribbon(aes(x = study.year, ymin = lci, ymax = uci, fill = mod.name.out), alpha = 0.1) +
  scale_x_continuous(breaks = year_breaks, labels = year_breaks) +
    theme_bw() +
    labs(x = "Year",
         y = "Estimated abundance",
         color = "",
         fill = "") +
  facet_wrap(~guild, scales = "free_y", ncol = 2)

ggsave(here("figures_output/guild_trends_facet.png"), width = 7.5)


readRDS(here("data_files/guild_preds_response")) %>%
  left_join(abund_guilds %>% dplyr::select(study.year, guild, guild.total)) %>% 
  full_join(guild_n_spp) %>% 
  right_join(guild_mod_names) %>%  
  mutate(guild = paste(guild, " (", n.spp, " species)", sep = "")) %>% 
  ggplot() +  
  geom_point(aes(x = study.year, y = guild.total, color = guild)) +
  geom_line(aes(x = study.year, y = predicted, color = guild)) +
  geom_ribbon(aes(x = study.year, ymin = lci, ymax = uci, fill = guild), alpha = 0.5) +
  scale_x_continuous(breaks = year_breaks, labels = year_breaks) +
    theme_bw() +
    labs(x = "Year",
         y = "Estimated abundance",
         color = "",
         fill = "") 
  #theme(legend.position = c(.8, .9),
  #      legend.text.align = 0) +
  #facet_wrap(~mod.name.out)


ggsave(here("figures_output/guild_trends.png"), width = 7.5)

