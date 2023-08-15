


library(tidyverse)
library(here)

options(scipen = 999)

all_best_preds_response <- readRDS(here("data_files/all_best_preds_response")) %>% 
  select(alpha.code, Modnames, study.year, predicted)

all_best_preds_response_new <- readRDS("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/water_birds/waterbird_analyses/Tomales_waterbird_trends_2020 - Copy/data_files/all_best_preds_response") %>% 
  select(alpha.code, Modnames, study.year, predicted.new = predicted)


combined <- full_join(all_best_preds_response, all_best_preds_response_new) %>% 
  mutate(zdiff = predicted.new - predicted,
         zdiff.per = zdiff/predicted)

combined_long <- combined %>% 
  pivot_longer(cols = contains("predicted"))


combined_long %>% 
  ggplot() +
  geom_line(aes(x = study.year, y = value, color = name))+
  facet_wrap(~alpha.code, scales = "free_y")



combined_long %>% 
  filter(alpha.code == "GWTE") %>% 
  ggplot() +
  geom_line(aes(x = study.year, y = value, color = name, linetype = Modnames))

