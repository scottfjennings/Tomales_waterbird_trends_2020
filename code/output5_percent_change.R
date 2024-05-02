


library(tidyverse)
library(here)
library(birdnames)

custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")

source(here("code/analysis_utilities.R"))

options(scipen = 999)

# read model predictions

# this has predictions from the best model for each species, 
# and for BLSC it also has the 2nd best model, year. Only want to use year for BLSC, don't use year2 model
all_best_preds_response <- readRDS(here("data_files/all_best_preds_response")) %>% 
  mutate(common.name = translate_bird_names(alpha.code, "alpha.code", "common.name"),
         common.name = ifelse(common.name == "all", "All species combined", common.name)) %>% 
  filter(!(alpha.code == "BLSC" & Modnames == "year2"),
         !(alpha.code == "GWTE"  & Modnames == "year_giac"))

#%>% 
#  bind_rows(readRDS(here("data_files/guild_preds_response")) %>% 
#              rename(common.name = guild) %>% 
#              dplyr::select(-residual.scale, -moci, -fresh) %>% 
#              mutate(Modnames = "year2_guild_fresh_moci"))


# calculate percent change from top model ----

  
 # vertex of quadratic curves

quad_vertex <- all_best_preds_response  %>% 
  filter(grepl("year2", Modnames)) %>% 
  arrange(common.name, study.year) %>% 
  group_by(common.name) %>% 
  mutate(annual.percent.change.sign = ifelse(1 - predicted/lag(predicted) < 0, "negative", "positive")) %>%
  mutate(quad.vertex = ifelse(annual.percent.change.sign == lead(annual.percent.change.sign), FALSE, TRUE))



quad_vertex %>%  
  ggplot() +
  geom_line(aes(x = study.year, y = predicted)) +
  geom_point(aes(x = study.year, y = predicted, color = quad.vertex)) +
  facet_wrap(~common.name, scales = "free_y")



change_quad_phase1 <- quad_vertex %>% 
  filter(quad.vertex == TRUE) %>% 
  dplyr::select(common.name, quad.vertex.year = study.year) %>% 
  left_join(all_best_preds_response) %>% 
  group_by(alpha.code) %>% 
  filter(study.year == min(study.year)| study.year == quad.vertex.year) %>% 
  mutate(phase = "1")
         



change_quad_phase2 <- quad_vertex %>% 
  filter(quad.vertex == TRUE) %>% 
  dplyr::select(common.name, quad.vertex.year = study.year) %>% 
  left_join(all_best_preds_response) %>% 
  group_by(alpha.code) %>% 
  filter(study.year == max(study.year) | study.year == quad.vertex.year) %>% 
  mutate(phase = "2")


change_overall <- all_best_preds_response %>% 
  group_by(common.name) %>% 
  filter(study.year == min(study.year) | study.year == max(study.year)) %>% 
  mutate(phase = "overall")



percent_changes <- bind_rows(change_quad_phase1, change_quad_phase2, change_overall) %>% 
  mutate(phase = factor(phase, levels = c("overall", "1", "2"))) %>% 
  arrange(common.name, phase, study.year) %>% 
  group_by(common.name, phase) %>% 
  mutate(percent.change = 100 * ((predicted/lag(predicted))-1),
         which.year = ifelse(study.year == min(study.year), "start", "end")) %>% 
  ungroup() %>% 
  mutate(across(c(predicted, lci, uci), ~floor(.)),
         pred.out = paste(predicted, " (", lci, ", ", uci, ")", sep = ""),
         percent.change = round(percent.change, 1))

saveRDS(percent_changes, here("data_files/percent_changes"))

percent_changes <- readRDS(here("data_files/percent_changes"))

filter(percent_changes, phase == "overall") %>% 
  view()

  
# calculate percent change from model averaged estimates ----  
  # not separating for quadratic curves
  

mod_avg_preds <- readRDS(here("data_files/mod_avg_preds"))
  

mod_avg_per_change <- mod_avg_preds %>% 
  mutate(common.name = translate_bird_names(alpha.code, "alpha.code", "common.name")) %>% 
  group_by(common.name) %>% 
  filter(study.year == min(study.year) | study.year == max(study.year)) %>% 
  arrange(common.name, study.year) %>% 
  group_by(common.name) %>% 
  mutate(percent.change = 100 * ((mod.avg.pred/lag(mod.avg.pred))-1),
         which.year = ifelse(study.year == min(study.year), "start", "end")) %>% 
  ungroup() %>% 
  mutate(across(c(mod.avg.pred, lower.CL, upper.CL), ~floor(.)),
         pred.out = paste(mod.avg.pred, " (", lower.CL, ", ", upper.CL, ")", sep = ""),
         percent.change = round(percent.change, 1))




saveRDS(mod_avg_per_change, here("data_files/mod_avg_per_change"))

  
