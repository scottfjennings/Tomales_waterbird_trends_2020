

library(tidyverse)
library(here)
library(MASS) # for glm.nb
library(AICcmodavg)
library(birdnames)

custom_bird_list <- readRDS("C:/Users/scott.jennings/Documents/Projects/my_R_general/birdnames_support/data/custom_bird_list")


# fit models to each species ----


herring_spp <- read.csv(here("data_files/Foraging guild table.csv")) %>% 
  mutate(alpha.code = translate_bird_names(common.name, "common.name", "alpha.code")) %>% 
  dplyr::select(alpha.code, guild) %>% 
  mutate(herring_spp = ifelse(alpha.code %in% c("SUSC", "SCAUP", "BRAN", "BUFF", "RUDU", "AMCO", "AMWI", "NOPI", "CAGO", "COGO", "RBME", "BLSC", "MALL", "WWSC", "CANV", "COME", "REDH", "RNDU", "LTDU", "BAGO", "CITE", "COMU", "HOME", "HADU", "ROGO") | guild == "Piscivore", TRUE, FALSE))


p75_annual <- readRDS(here("data_files/spp_annual_full_preds"))



abund_guilds <- inner_join(p75_annual, guilds) %>% 
  group_by(guild, study.year, moci, fresh) %>% 
  summarise(guild.total = sum(p75.abund)) %>% 
  ungroup() %>% 
  filter(guild != "Omnivore") %>% 
  droplevels()

saveRDS(abund_guilds, here("data_files/abund_guilds"))



year2_fresh_moci <- glm.nb(guild.total ~ poly(study.year, 2) + fresh + moci, data = abund_guilds)

year2.guild_fresh_moci <- glm.nb(guild.total ~ poly(study.year, 2) * guild + fresh + moci, data = abund_guilds)
year2_guild_fresh_moci <- glm.nb(guild.total ~ poly(study.year, 2) + guild + fresh + moci, data = abund_guilds)
year2_guild.fresh_moci <- glm.nb(guild.total ~ poly(study.year, 2) + guild * fresh + moci, data = abund_guilds)
year2_fresh_guild.moci <- glm.nb(guild.total ~ poly(study.year, 2) + fresh + guild * moci, data = abund_guilds)

guild_mods <- list(year2_fresh_moci, year2.guild_fresh_moci, year2_guild_fresh_moci, year2_guild.fresh_moci, year2_fresh_guild.moci)

names(guild_mods) <- c("year2_fresh_moci", "year2.guild_fresh_moci", "year2_guild_fresh_moci", "year2_guild.fresh_moci", "year2_fresh_guild.moci")

guild_aic <- aictab(list(year2_fresh_moci, year2.guild_fresh_moci, year2_guild_fresh_moci, year2_guild.fresh_moci, year2_fresh_guild.moci), 
                    c("year2_fresh_moci", "year2.guild_fresh_moci", "year2_guild_fresh_moci", "year2_guild.fresh_moci", "year2_fresh_guild.moci")) %>% 
  data.frame() %>% 
  mutate(species = "guild")

guild_mods$aic_tab = guild_aic


final_models <- readRDS(here("fitted_models/final_models"))

final_models$guild = guild_mods

saveRDS(final_models, here("fitted_models/final_models"))


summary(year2_guild_fresh_moci)
