


# basic raw data visualization

# packages, source ----
library(tidyverse)
library(lubridate)
options(scipen = 999)

source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.r")
source("C:/Users/scott.jennings/Documents/Projects/water_birds/code/utility/waterbird_utility_functions.r")
# data ----
# waterbird foraging guild membership
# recieved in xlsx via email from NW on 2020/08/26
wbird_guilds <- read.csv("data_files/Foraging guild table.csv") %>% 
  bird_taxa_filter(join_taxa = c("common.name", "common.name"))



wbird_guilds_longer <- wbird_guilds %>% 
  select(alpha.code, guild, guild2) %>% 
  pivot_longer(cols = contains("guild"), names_to = "which.guild", values_to = "guild.name") %>% 
  mutate_all(na_if,"") %>% 
  filter(!is.na(guild.name))


# allocated, negative tallied waterbird data


wbirds4analysis <- readRDS("C:/Users/scott.jennings/Documents/Projects/water_birds/data_files/working_rds/wbirds4analysis") %>% 
  wbird_add_study_day() 

wbird_by_guild <- wbirds4analysis %>% 
  full_join(., filter(wbird_guilds_longer, which.guild == "guild"))



# some summaries created by water_birds/code/data_summary_visualize/visualize_species_detections_proportions
ave_proportion_each_year <- readRDS("C:/Users/scott.jennings/Documents/Projects/water_birds/data_files/working_rds/ave_proportion_each_year")
ave_proportion_all_years <- readRDS("C:/Users/scott.jennings/Documents/Projects/water_birds/data_files/working_rds/ave_proportion_all_years")
num_years_detected <- readRDS("C:/Users/scott.jennings/Documents/Projects/water_birds/data_files/working_rds/num_years_detected")

#
# by species abundance summaries ----

wbird_spp_summ <- wbirds4analysis %>%
  mutate(alpha.code = ifelse(alpha.code %in% c("GRSC", "LESC"), "SCAUP", alpha.code)) %>% 
  filter(!alpha.code %in% useful_groupies, !alpha.code %in% useless_groupies) %>% 
  group_by(alpha.code, date) %>% 
  summarise(spp.survey.tot = sum(section.final)) %>% 
  ungroup() %>% 
  group_by(alpha.code) %>% 
  summarise(spp.min = min(spp.survey.tot),
            spp.mean = mean(spp.survey.tot),
            spp.med = median(spp.survey.tot),
            spp.max = max(spp.survey.tot),
            num.days.obs = n()) %>% 
  arrange(spp.max)



wbird_spp_summ %>% 
ggplot() +
  geom_histogram(aes(x = spp.max))


wbirds4analysis %>% 
  group_by(alpha.code, date) %>% 
  summarise(spp.survey.tot = sum(section.final)) %>% 
  ungroup() %>% 
  filter(alpha.code %in% filter(wbird_spp_summ, num.days.obs > 10)$alpha.code & alpha.code %in% filter(wbird_spp_summ, spp.max > 10)$alpha.code) %>% 
  ggplot() +
  geom_histogram(aes(x = spp.survey.tot)) +
  #geom_line(aes(x = date, y = spp.survey.tot)) +
  facet_wrap(~alpha.code, scales = "free")

wbirds4analysis %>% 
  group_by(alpha.code, date) %>% 
  summarise(spp.survey.tot = sum(section.final)) %>% 
  ungroup() %>% 
  arrange(alpha.code, date) %>% 
  filter(alpha.code == "GRSC") %>% 
  select(spp.survey.tot) %>% 
  acf()

#
# viewing survey dates ----


survey_intervals <- wbirds4analysis %>% 
  distinct(date, study.day, study.year) %>% 
  group_by(study.year) %>% 
  mutate(survey.interval = study.day - lag(study.day))
  

survey_intervals %>% 
  filter(!is.na(survey.interval)) %>% 
ggplot() +
  geom_histogram(aes(x = survey.interval), binwidth = 1)+
  xlim(0, 70)


#
# by guild sum and mean ----



wbird_by_guild_summ <- wbird_by_guild %>% 
  filter(!is.na(guild.name), !is.na(date)) %>% 
  mutate(year = year(date)) %>% 
  group_by(year, guild.name, date) %>% 
  summarize(guild.tot.survey = sum(section.final)) %>% 
  ungroup()

wbird_by_guild_summ %>% 
  filter(!guild.name %in% c("Omnivore", "Planktivore")) %>% 
  group_by(guild.name) %>% 
  summarise(min.count = min(guild.tot.survey),
            mean.count = mean(guild.tot.survey),
            med.count = median(guild.tot.survey),
            max.count = max(guild.tot.survey))

wbird_by_guild_summ %>% 
  filter(!guild.name %in% c("Planktivore")) %>% 
ggplot() +
  geom_histogram(aes(x = guild.tot.survey)) +
  facet_wrap(~guild.name, scales = "free")



wbird_by_guild_surv_mean <- wbird_by_guild_summ %>% 
  mutate(survey.num)
  group_by(year, guild.name) %>% 
  mutate(guild.mean = mean(guild.tot.survey)) %>% 
  ungroup() %>% 
  arrange(year, guild.name)

# by guild trends ----

wbird_by_guild_surv_mean %>% 
  filter(guild.name != "Planktivore") %>%  
ggplot() +
  geom_line(aes(x = year, y = guild.tot.survey, color = survey.number)) +
  geom_line(aes(x = year, y = guild.mean)) +
  ylab("Abundance") +
  ggtitle("Mean (black) is a good representation\nof counts from each survey") +
  facet_wrap(~guild.name, scales = "free") +
  theme_classic() +
  guides(color=guide_legend(title="Survey\nnumber"))
        
wbird_by_guild_surv_mean %>% 
  filter(guild.name != "Planktivore") %>% 
  mutate(survey.number = as.factor(survey.num)) %>% 
ggplot() +
  geom_point(aes(x = year, y = guild.mean)) +
  stat_smooth(aes(x = year, y = guild.mean)) +
  ylab("Abundance") +
  ggtitle("Mean (across surveys) waterbird abundance") +
  facet_wrap(~guild.name, scales = "free")


wbird_by_guild_surv_mean %>% 
  filter(guild.name != "Planktivore") %>% 
ggplot() +
  geom_line(aes(x = year, y = guild.tot.survey, color = guild.name)) +
  facet_wrap(~survey.num)    


# by guild ACF ----

zzz <- wbird_by_guild_surv_mean %>% 
  #filter(guild.name == "Diving benthivore") %>% 
  group_by(guild.name) %>% 
  arrange(date)

acf(zzz$guild.tot.survey)

#
# check how much BLBR drive herbivore patterns ----


blbr_mean <- wbird_by_guild %>% 
  filter(alpha.code == "BLBR") %>% 
  group_by(year, survey.num) %>% 
  summarise(tot.herb = sum(section.final)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  summarise(mean.abund = mean(tot.herb)) %>% 
  ungroup() %>% 
  mutate(with.blbr = "BLBR only")

herb_w_blbr <- wbird_by_guild %>% 
  filter(guild.name == "Herbivore") %>% 
  group_by(year, survey.num) %>% 
  summarise(tot.herb = sum(section.final)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  summarise(mean.abund = mean(tot.herb)) %>% 
  ungroup() %>% 
  mutate(with.blbr = "Herbivores w/ BLBR")

herb_w_out_blbr <- wbird_by_guild %>% 
  filter(guild.name == "Herbivore", alpha.code != "BLBR") %>% 
  group_by(year, survey.num) %>% 
  summarise(tot.herb = sum(section.final)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  summarise(mean.abund = mean(tot.herb)) %>% 
  ungroup() %>% 
  mutate(with.blbr = "Herbivores w/out BLBR")


blbr_comp <- rbind(blbr_mean, herb_w_blbr, herb_w_out_blbr) %>% 
  filter(!is.na(year))


ggplot(blbr_comp) +
  geom_line(aes(x = year, y = mean.abund, color = with.blbr))+
  stat_smooth(aes(x = year, y = mean.abund, color = with.blbr), se = F) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  ylab("Mean abundance") +
  xlab("Year")
ggsave("figures_output/blbr_herbivores.png", height = 6, width = 8)


