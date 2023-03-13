

library(tidyverse)
library(here)
library(MASS) # for glm.nb
library(AICcmodavg)
library(lmtest) # for lrtest
library(birdnames)

custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")

trend_spp <- readRDS(here("data_files/trend_spp"))

# fit models to each species ----

p75_annual <- readRDS(here("data_files/spp_annual_full_preds"))


guilds <- read.csv(here("data_files/Foraging guild table.csv")) %>% 
  mutate(alpha.code = translate_bird_names(common.name, "common.name", "alpha.code")) %>% 
  dplyr::select(alpha.code, guild)


abund_guilds <- inner_join(p75_annual, guilds) %>% 
  group_by(guild, study.year, moci, fresh) %>% 
  summarise(guild.total = sum(p75.abund)) %>% 
  ungroup() %>% 
  filter(guild != "Omnivore") %>% 
  droplevels()

guild_n_spp <- inner_join(p75_annual, guilds) %>%
  distinct(guild, alpha.code) %>% 
  group_by(guild) %>% 
  summarise(n.spp = n()) %>% 
  filter(guild != "Omnivore")
  


year2_fresh_moci <- glm.nb(guild.total ~ poly(study.year, 2) + fresh + moci, data = abund_guilds)

year2.guild_fresh_moci <- glm.nb(guild.total ~ poly(study.year, 2) * guild + fresh + moci, data = abund_guilds)
year2_guild_fresh_moci <- glm.nb(guild.total ~ poly(study.year, 2) + guild + fresh + moci, data = abund_guilds)

guild_aic <- aictab(list(year2_fresh_moci, year2.guild_fresh_moci, year2_guild_fresh_moci), 
                    c("year2_fresh_moci", "year2.guild_fresh_moci", "year2_guild_fresh_moci")) %>% 
  data.frame()


lrtest(year2.guild_fresh_moci, year2_guild_fresh_moci)
lrtest(year2_guild_fresh_moci, year2_fresh_moci)


summary(year2.guild_fresh_moci)


guild_newdat <- expand.grid(study.year = distinct(abund_guilds, study.year)$study.year,
                            guild = distinct(abund_guilds, guild)$guild) %>% 
  mutate(moci = 0,
         fresh = 0)

guild_pred = predict(year2_guild_fresh_moci, guild_newdat, type = "link", se = TRUE) %>% 
  bind_cols(guild_newdat) %>%
  data.frame() %>% 
  mutate(lci = exp(fit - (1.96 * se.fit)),
         uci = exp(fit + (1.96 * se.fit)),
         predicted = exp(fit))

saveRDS(guild_pred, here("data_files/guild_preds_response"))

year_breaks <- seq(1990, 2025, by = 5)

guild_pred %>% 
  left_join(abund_guilds %>% dplyr::select(study.year, guild, guild.total)) %>% 
  full_join(guild_n_spp) %>% 
  mutate(guild = paste(guild, " (", n.spp, " species)", sep = "")) %>% 
  ggplot() +  
  geom_point(aes(x = study.year, y = guild.total)) +
  geom_line(aes(x = study.year, y = predicted)) +
  geom_ribbon(aes(x = study.year, ymin = lci, ymax = uci), alpha = 0.5) +
  scale_x_continuous(breaks = year_breaks, labels = year_breaks) +
    theme_bw() +
    labs(x = "Year",
         y = "Estimated abundance") +
  facet_wrap(~guild, scales = "free_y", ncol = 2)

ggsave(here("figures_output/guild_trends_facet.png"), width = 7.5)


guild_pred %>% 
  left_join(abund_guilds %>% dplyr::select(study.year, guild, guild.total)) %>% 
  full_join(guild_n_spp) %>% 
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
         fill = "") +
  theme(legend.position = c(.8, .9),
        legend.text.align = 0) 


ggsave(here("figures_output/guild_trends.png"), width = 7.5)
