


library(tidyverse)
library(here)

spp_day_total <- readRDS(here("data_files/spp_day_total"))


amco_test <- bind_rows(spp_day_total %>%
                         group_by(study.year, date) %>%
                         summarise(bay.total = sum(bay.total)) %>%
                         mutate(alpha.code = "ALL"),
                       spp_day_total %>%
                         filter(alpha.code != "AMCO") %>% 
                         group_by(study.year, date) %>%
                         summarise(bay.total = sum(bay.total)) %>%
                         mutate(alpha.code = "ALLnoAMCO")) %>% 
  group_by(study.year, alpha.code) %>% 
  mutate(p75.abund = floor(quantile(bay.total, 0.75)))

ggplot(amco_test, aes(x = study.year, y = p75.abund, color = alpha.code)) +
  geom_point(aes(y = bay.total)) +
  geom_line() +
  stat_smooth(method = "lm", formula = "y ~ poly(x, 2)")


ggsave(here("figures_output/check_amco_effect.png"), width = 8)
