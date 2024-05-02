
library(tidyverse)
library(lubridate)
library(here)
library(birdnames)

custom_bird_list <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/my_R_general/birdnames_support/data/custom_bird_list")


source(here("code/analysis_utilities.R"))

trend_spp <- readRDS(here("data_files/trend_spp"))


spp_day_total <- readRDS(here("data_files/spp_day_total"))

# mean and sd  surveys per winter ----
spp_day_total %>% 
  distinct(study.year, date) %>% 
  group_by(study.year) %>% 
  summarise(num.studies = n()) %>% 
  ungroup() %>% 
  summarise(total.studies = sum(num.studies),
            mean.studies = mean(num.studies),
            sd.studies = sd(num.studies)) %>% 
  ungroup() %>% 
  mutate(across(c(mean.studies, sd.studies), ~round(., 2)))


spp_day_total %>% 
  distinct(study.year, date) %>% 
  mutate(survey.per = case_when(month(date) == 12 ~ "Dec",
                                month(date) == 1 & day(date) < 16 ~ "Early Jan",
                                month(date) == 1 & day(date) >= 16 ~ "Late Jan",
                                month(date) == 2 ~ "Feb",
                                TRUE ~ NA),
         survey.per = factor(survey.per, levels = c("Dec", "Early Jan", "Late Jan", "Feb"))) %>% 
  group_by(study.year, survey.per) %>% 
  summarise(num.surveys = n()) %>%
  ungroup() %>% 
  group_by(study.year) %>% 
  mutate(num.surveys.year = sum(num.surveys)) %>% 
  ggplot() +
  geom_tile(aes(y = study.year, x = survey.per, fill = as.factor(num.surveys)), color = "black", alpha = 0.3) +
  scale_y_continuous(breaks = seq(1992, 2022), labels = seq(1992, 2022)) +
  theme_bw() +
  geom_label(aes(y = study.year, x = 5, label = num.surveys.year), size = 3 , label.size = NA) +
  labs(y = "Year",
       x = "Survey period",
       fill = "Number of\nsurveys per\nperiod",
       title = "Number of waterbird surveys in each of 4 \'survey periods\'\nvalues along right are total per year") +
  scale_x_discrete(expand = expansion(add = 1.2))

ggsave(here("figures_output/number of surveys per year.png"))


# mean and sd birds per survey ----
spp_day_total %>% 
  group_by(study.year, date) %>% 
  summarise(tot.birds.day = sum(bay.total)) %>% 
  ungroup() %>% 
  summarise(mean.birds = mean(tot.birds.day),
            sd.birds = sd(tot.birds.day)) %>% 
  mutate(across(c("mean.birds", "sd.birds"), ~round(., 1)))


spp_day_total %>% 
  mutate(survey.per = case_when(month(date) == 12 ~ "Dec",
                                month(date) == 1 & day(date) < 16 ~ "Early Jan",
                                month(date) == 1 & day(date) >= 16 ~ "Late Jan",
                                month(date) == 2 ~ "Feb",
                                TRUE ~ NA),
         survey.per = factor(survey.per, levels = c("Dec", "Early Jan", "Late Jan", "Feb"))) %>% 
  group_by(study.year, alpha.code, survey.per) %>%
  mutate(survey.per.num = row_number()) %>% 
  ungroup() %>% 
  group_by(study.year, alpha.code) %>% 
  mutate(mean.bay.total = mean(bay.total),
         p75.bay.total = quantile(bay.total, p = 0.75),
         resid.bay.total = bay.total - mean.bay.total,
         p75.resid = bay.total - p75.bay.total,
         sd.bay.total = sd(bay.total)) %>% 
  filter(alpha.code %in% c("SUSC", "SCAUP", "BUFF", "DCCO", "BRAC")) %>% 
  ggplot() +
  geom_point(aes(x = study.year, y = sd.bay.total)) +
    geom_hline(yintercept = 0) +
  facet_wrap(~alpha.code)
  

# mean and sd birds allocated from groups to species each survey ----

allocated <- read.csv("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/ACR_waterbird_data_management/data_files/entered_raw_combined/allocation_report.csv")

allocated_per_survey <- allocated %>% 
  filter(!pooled.alpha.code %in% c("SCAUP", "WCGR")) %>% 
  group_by(date) %>% 
  summarise(tot.pooled = sum(tally),
            tot.allocated = sum(tot.allocated)) %>%
  ungroup() %>% 
  mutate(date = ymd(date)) %>% 
  left_join(distinct(spp_day_total, date, study.year)) %>% 
  filter(!is.na(study.year)) %>% 
  summarise(mean.tot.pooled = mean(tot.pooled),
            sd.tot.pooled = sd(tot.pooled),
            mean.tot.allocated = mean(tot.allocated),
            sd.tot.allocated = sd(tot.allocated)) %>% 
  mutate_all(~round(., 1))

main_pooled_spp <- allocated %>% 
  group_by(date, pooled.alpha.code) %>% 
  summarise(tot.pooled = sum(tally)) %>% 
  ungroup() %>% 
  group_by(pooled.alpha.code) %>% 
  summarise(mean.tot.pooled = mean(tot.pooled),
            sd.tot.pooled = sd(tot.pooled)) %>% 
  mutate(across(c("mean.tot.pooled", "sd.tot.pooled"), ~round(., 1)))

# mean and sd of predictors on their native scale ----
readRDS(here("data_files/predictors")) %>% 
  dplyr::select("fresh" = annual.freshwater, "moci" = mean.moci) %>% 
  pivot_longer(cols = c("fresh", "moci")) %>% 
  group_by(name) %>% 
  summarise(min.val = min(value),
            max.val = max(value)) %>% 
  mutate(across(contains("val"), ~ round(., 1)))
# total species and number of species per day ----  
  
readRDS(here("data_files/spp_day_total_ungrouped")) %>% 
  mutate(group.spp = translate_bird_names(alpha.code, "alpha.code", "group.spp")) %>% 
  filter(is.na(group.spp), !alpha.code %in% c("MURRELET", "GOOSE", "DUCK", "SWAN", "UNTE", "TUDUSCAUP")) %>%
  distinct(date, alpha.code) %>% view()
  group_by(date) %>% 
  summarise(tot.spp = n()) %>% 
  ungroup() %>% 
  summarise(mean.tot.spp = mean(tot.spp),
            sd.tot.spp = sd(tot.spp)) %>% 
  mutate(across(c("mean.tot.spp", "sd.tot.spp"), ~round(., 1)))

spp_day_total %>% 
  mutate(group.spp = translate_bird_names(alpha.code, "alpha.code", "group.spp")) %>% 
  filter(is.na(group.spp), !alpha.code %in% c("MURRELET", "GOOSE", "DUCK", "SWAN", "UNTE", "TUDUSCAUP")) %>% 
  distinct(alpha.code) %>% 
  nrow()


# summarise negatives ----
neg_machine <- readRDS(here("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/ACR_waterbird_data_management/data_files/working_rds/new_neg_machine_all"))

negatives <- neg_machine %>% 
  filter(transect == "section.sum") %>% 
  select(date, alpha.code, contains("field.tally")) %>% 
  pivot_longer(cols = contains("field.tally"), names_to = "section", values_to = "tally") %>% 
  filter(!is.na(tally), tally < 0) %>% 
  group_by(date, alpha.code) %>% 
  summarise(tot.neg = sum(tally)) %>% 
  ungroup()

mean_negatives <- negatives %>% 
  group_by(date) %>% 
  summarise(tot.neg = sum(tot.neg)) %>% 
  ungroup() %>% 
  summarise(mean.neg = mean(tot.neg),
            sd.neg = sd(tot.neg))  %>% 
  mutate(across(c("mean.neg", "sd.neg"), ~round(., 1)))


departed <- neg_machine %>%  
  select(date, alpha.code, contains("departed")) %>% 
  filter(departed.allocated.by.transect_4 > 0) %>% 
  group_by(date, alpha.code) %>% 
  summarise(tot.added.back = sum(departed.allocated.by.transect_4)) %>% 
  ungroup() %>% 
  summarise(mean.added.back = mean(tot.added.back),
            sd.added.back = sd(tot.added.back))  %>% 
  mutate(across(c("mean.added.back", "sd.added.back"), ~round(., 1)))


# models summary ----

trend_spp <- readRDS(here("data_files/trend_spp"))

best_mods <- map_df(trend_spp$alpha.code, get_best_model) 

filter(best_mods, grepl("giac", Modnames)) %>% nrow()

qual_trends <- read.csv(here("documents/result_summary.csv"))

filter(qual_trends, qualitative.trend == "no change") %>% 
  mutate(common.name = translate_bird_names(alpha.code, "alpha.code", "common.name")) %>% 
  summarise(species = paste(common.name, collapse = ", "))


filter(best_mods, grepl("giac", Modnames)) %>% 
  mutate(common.name = translate_bird_names(alpha.code, "alpha.code", "common.name")) %>% 
  summarise(species = paste(common.name, collapse = ", "))


# number of species with intercept model competitive ----

int_comp <- map_df(trend_spp$alpha.code, get_competitive_models) %>% 
#  filter(Modnames == "intercept") %>% 
  left_join(readRDS(here("data_files/percent_changes")) %>% select(alpha.code, phase, percent.change) %>% filter(!is.na(percent.change)))

# number of species with non-year model competitive ----

non_year_comp <- map_df(trend_spp$alpha.code, get_competitive_models) %>% 
  filter(!grepl("year", Modnames))



# 

meaningful_changes <- readRDS(here("data_files/percent_changes")) %>% 
  select(common.name, phase, which.year, predicted, lci, uci) %>%
  pivot_longer(cols = c(predicted, lci, uci)) %>% 
  mutate(name = paste(which.year, name, sep = ".")) %>% 
  pivot_wider(id_cols = c("common.name", "phase"), values_from = value, names_from = name) %>% 
  mutate(meaningful.change = ifelse(start.predicted == end.predicted |
                                      (start.predicted > end.predicted & start.lci < end.uci) |
                                      (start.predicted < end.predicted & start.lci > end.uci), FALSE, TRUE)) %>% 
  filter(phase == "overall") %>% 
  left_join(., percent_changes %>% filter(which.year == "end") %>% select(common.name, phase, Modnames, percent.change))


write.csv(meaningful_changes, here("documents/meaningful_changes.csv"), row.names = FALSE)

# hogr percent decline ----

hogr <- readRDS(here("data_files/all_best_preds_response")) %>% 
  filter(alpha.code == "HOGR") %>% 
  mutate(percent.change = 100 * ((predicted/lag(predicted))-1))

#  percent decline explore ----

preds <- readRDS(here("data_files/percent_changes"))
filter(preds, phase != "overall") %>% arrange(percent.change) %>% view()