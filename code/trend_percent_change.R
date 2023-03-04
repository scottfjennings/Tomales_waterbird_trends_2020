


library(tidyverse)
library(here)
library(birdnames)

custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")

source(here("code/analysis_utilities.R"))

options(scipen = 999)

# read model predictions

# this has predictions from the best model for each species, 
# and for BLSC it also has the 2nd best model, year. Only want to use year for BLSC, don't use year2 model
all_best_preds_response <- readRDS(here("data_files/all_best_preds_response")) %>% 
  mutate(common.name = translate_bird_names(alpha.code, "alpha.code", "common.name"),
         common.name = ifelse(common.name == "all", "All species combined", common.name)) %>% 
  filter(!(alpha.code == "BLSC" & Modnames == "year2"))


# calculate percent change ----

  
 # vertex of quadratic curves

quad_vertex <- all_best_preds_response  %>% 
  #filter(grepl("year2", Modnames)) %>% 
  arrange(alpha.code, study.year) %>% 
  group_by(alpha.code) %>% 
  mutate(annual.percent.change.sign = ifelse(1 - predicted/lag(predicted) < 0, "negative", "positive")) %>%
  mutate(quad.vertex = ifelse(annual.percent.change.sign == lead(annual.percent.change.sign), FALSE, TRUE))



quad_vertex %>%  
  ggplot() +
  geom_line(aes(x = study.year, y = predicted)) +
  geom_point(aes(x = study.year, y = predicted, color = quad.vertex)) +
  facet_wrap(~alpha.code, scales = "free_y")



percent_change <- quad_vertex %>% 
  filter(quad.vertex == TRUE) %>% 
  dplyr::select(alpha.code, quad.vertex.year = study.year) %>% 
  full_join(all_best_preds_response) %>% 
  group_by(alpha.code) %>% 
  filter(study.year == min(study.year) | study.year == max(study.year) | study.year == quad.vertex.year) %>% 
  arrange(alpha.code, study.year) %>% 
  mutate(percent.change = 100 * ((predicted/lag(predicted))-1)) %>% 
  ungroup() %>% 
  mutate(across(c(predicted, lci, uci), ~floor(.)),
         pred.out = paste(predicted, " (", lci, ", ", uci, ")", sep = ""))
         
         




before_vertex <- full_join(all_predicted_year2, quad_vertex) %>% 
  filter(study.year <= quad.vertex.year) %>%
  group_by(alpha.code) %>% 
  mutate(trend.segment.bound = case_when(study.year == min(study.year) ~ "start",
                                         study.year == max(study.year) ~ "end")) %>% 
  ungroup() %>% 
  dplyr::select(alpha.code, study.year, contains("resp"), trend.segment.bound) %>% 
  filter(!is.na(trend.segment.bound)) %>% 
  mutate(trend.segment = "before.vertex")


after_vertex <- full_join(all_predicted_year2, quad_vertex) %>% 
  filter(study.year >= quad.vertex.year) %>%
  group_by(alpha.code) %>% 
  mutate(trend.segment.bound = case_when(study.year == min(study.year) ~ "start",
                                         study.year == max(study.year) ~ "end")) %>% 
  ungroup() %>% 
  dplyr::select(alpha.code, study.year, contains("resp"), trend.segment.bound) %>% 
  filter(!is.na(trend.segment.bound)) %>% 
  mutate(trend.segment = "after.vertex")


percent_change_wide_quad <- rbind(before_vertex, after_vertex) %>% 
  pivot_longer(cols = c(study.year, contains("resp"))) %>% 
  mutate(trend.segment.bound.varb = paste(trend.segment.bound, name, sep = ".")) %>% 
  pivot_wider(id_cols = c("alpha.code", "trend.segment"), values_from = value, names_from = trend.segment.bound.varb) %>% 
  mutate(trend.segment.percent.change = -100 * (1 - end.predicted.resp/start.predicted.resp),
         year.effect = ifelse(trend.segment == "linear", "linear", "quadratic")) %>% 
  arrange(desc(year.effect), alpha.code, desc(trend.segment))
