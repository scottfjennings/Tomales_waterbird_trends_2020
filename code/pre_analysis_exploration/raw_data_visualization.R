


# basic raw data visualization

# packages, source ----
library(tidyverse)
library(lubridate)
library(here)
library(birdnames)
options(scipen = 999)

source("C:/Users/scott.jennings/Documents/Projects/water_birds/ACR_waterbird_data_management/code/utility/waterbird_utility_functions.r")

# data ----
# allocated, negative tallied waterbird data


wbirds4analysis <- readRDS("C:/Users/scott.jennings/Documents/Projects/water_birds/ACR_waterbird_data_management/data_files/working_rds/wbirds4analysis") %>% 
  wbird_add_study_day() 

# waterbird foraging guild membership
# recieved in xlsx via email from NW on 2020/08/26
wbird_guilds <- read.csv("data_files/Foraging guild table.csv") %>% 
  mutate(alpha.code = translate_bird_names(common.name, "common.name", "alpha.code"))



wbird_guilds_longer <- wbird_guilds %>% 
  select(alpha.code, guild, guild2) %>% 
  pivot_longer(cols = contains("guild"), names_to = "which.guild", values_to = "guild.name") %>% 
  mutate_all(na_if,"") %>% 
  filter(!is.na(guild.name))


wbird_by_guild <- wbirds4analysis %>% 
  full_join(., filter(wbird_guilds_longer, which.guild == "guild"))


# foraging/diet guilds from Vilchis et al

vilchis_guilds <- readRDS(here("data_files/rds/vilchis_guilds"))
vilchis_spp <- readRDS(here("data_files/rds/vilchis_spp"))

vilchis_foraging <- vilchis_guilds %>% 
  select(common.name, diving, `surface seizing`, dabbling, scavenging) %>% 
  pivot_longer(cols = -common.name, names_to = "foraging.behave", values_to = "done") %>% 
  filter(done == "yes")

vilchis_diet <- vilchis_guilds %>% 
  select(common.name, `forage fish`, `demersal fish`, `fish roe`, `mammals &/or birds`, snails, mussels, crustaceans, plants) %>% 
  pivot_longer(cols = -common.name, names_to = "diet.type", values_to = "done") %>% 
  filter(done == "yes")
  
vilchis_warnock_diet <- vilchis_guilds %>% 
  select(common.name, `forage fish`, `demersal fish`, `fish roe`, `mammals &/or birds`, snails, mussels, crustaceans, plants) %>% 
  full_join(wbird_guilds)

# some summaries created by water_birds/code/data_summary_visualize/visualize_species_detections_proportions
ave_proportion_each_year <- readRDS("C:/Users/scott.jennings/Documents/Projects/water_birds/ACR_waterbird_data_management/data_files/working_rds/ave_proportion_each_year")
ave_proportion_all_years <- readRDS("C:/Users/scott.jennings/Documents/Projects/water_birds/ACR_waterbird_data_management/data_files/working_rds/ave_proportion_all_years")
num_years_detected <- readRDS("C:/Users/scott.jennings/Documents/Projects/water_birds/ACR_waterbird_data_management/data_files/working_rds/num_years_detected")

# following Vilchis, which species make up at least 0.05% of total waterbirds?
overall_spp_percent <- ave_proportion_each_year %>%
  pivot_wider(id_cols = alpha.code, values_from = mean.proportion, names_from = study.year) %>% 
  pivot_longer(cols = !alpha.code, names_to = "study.year", values_to = "mean.proportion") %>% 
  mutate(mean.proportion = ifelse(is.na(mean.proportion), 0, mean.proportion)) %>% 
  group_by(alpha.code) %>% 
  summarise(overall.mean.percent = 100 * mean(mean.proportion)) %>% 
  arrange(-overall.mean.percent)

# by species abundance summaries ----

wbird_spp_summ <- wbirds4analysis %>%
  mutate(alpha.code = ifelse(alpha.code %in% c("GRSC", "LESC"), "SCAUP", alpha.code)) %>% 
  #filter(!alpha.code %in% useful_groupies, !alpha.code %in% useless_groupies) %>% 
  group_by(alpha.code, date) %>% 
  summarise(spp.survey.tot = sum(section.final)) %>% 
  ungroup() %>% 
  group_by(alpha.code) %>% 
  summarise(spp.min = min(spp.survey.tot),
            spp.mean = mean(spp.survey.tot),
            spp.med = median(spp.survey.tot),
            spp.sd = sd(spp.survey.tot),
            spp.max = max(spp.survey.tot),
            num.days.obs = n()) %>% 
  ungroup() %>% 
  left_join(., distinct(wbirds4analysis, alpha.code, study.year)%>%
              mutate(alpha.code = ifelse(alpha.code %in% c("GRSC", "LESC"), "SCAUP", alpha.code)) %>% 
              distinct(alpha.code, study.year) %>% 
              group_by(alpha.code) %>% 
              summarise(num.years.obs = n())) %>% 
  mutate(mean.sd.ratio = spp.mean/spp.sd)



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

# species missing guild classification from vilchis
tomales_missing_vilchis_guild <- full_join(vilchis_guilds %>%  
                                              rename(common.name.orig = common.name) %>% 
                                              full_join(vilchis_spp) %>% 
                                             mutate(alpha.code = translate_bird_names(common.name, "common.name", "alpha.code"),
                                                    alpha.code = ifelse(alpha.code == "GWTE", "AGWT", alpha.code),
                                                    vilchis = TRUE),
                                           wbird_spp_summ %>% 
                                             filter(num.years.obs >= 10) %>% 
                                             #mutate(alpha.code = case_when(alpha.code %in% c("GRSC", "LESC") ~ "SCAUP",
                                              #                             alpha.code %in% c("COME", "RBME") ~ "COMERBME",
                                              #                             alpha.code %in% c("WWSC", "SUSC", "BLSC") ~ "SCOTER",
                                              #                             TRUE ~ alpha.code)) %>% 
                                             distinct(alpha.code) %>% 
                                             mutate(tomales = TRUE)) %>% 
  select(alpha.code, vilchis, tomales, everything())


translate_bird_names("Brandt’s Cormorant", "common.name", "alpha.code")
# which species show some autocorrelation in their abundance? ----

spp_acf <- function(zspp) {
wbirds4analysis %>% 
  group_by(alpha.code, study.year, date) %>% 
  summarise(spp.survey.tot = sum(section.final)) %>% 
  ungroup() %>% 
    group_by(alpha.code, study.year) %>% 
    summarise(annual.tot = sum(spp.survey.tot)) %>% 
    ungroup() %>% 
  arrange(alpha.code, study.year) %>% 
  filter(alpha.code == zspp) %>% 
  select(annual.tot) %>% 
  acf()
}

spp_acf("AMCO")

spp10years <- wbirds4analysis %>% 
  wbird_num_years_detected() %>% 
  filter(num.years.detected >= 10)

all_spp_acf <- map(spp10years$alpha.code, spp_acf)

names(all_spp_acf) <- spp10years$alpha.code

all_spp_acf_df <- unlist(all_spp_acf) %>% 
  data.frame() %>% 
  rename(value = 1) %>% 
  rownames_to_column("spp.varb") %>% 
  separate(spp.varb, c("spp", "varb"), sep = "\\.", extra = "merge") 

# CI calculation from https://stackoverflow.com/questions/29996910/significance-level-of-acf-and-pacf-in-r

all_spp_acf_wide <- all_spp_acf_df %>% 
  pivot_wider(id_cols = spp, values_from = value, names_from = varb) %>% 
  select(spp, contains("acf"), n.used) %>% 
  mutate(n.used = as.numeric(n.used)) %>% 
# CI calculation from https://stackoverflow.com/questions/29996910/significance-level-of-acf-and-pacf-in-r
  mutate(ci95 = qnorm((1 + 0.95)/2)/sqrt(n.used))

all_spp_acf_long <- all_spp_acf_wide %>% 
  pivot_longer(cols = contains("acf")) %>% 
  mutate(value = as.numeric(value),
         ci95 = as.numeric(ci95)) %>% 
  mutate(sig.acf = ifelse(name != "acf1", abs(value) > ci95, NA))

sig_acf_spp <- all_spp_acf_long %>% 
  filter(sig.acf == TRUE)

saveRDS(sig_acf_spp, here("data_files/autocor_spp"))

#
# are any species generally only seen in certain sections? ----

spp_section_prop <- wbirds4analysis %>% 
  group_by(alpha.code, study.year, date) %>% 
  mutate(spp.survey.tot = sum(section.final)) %>% 
  ungroup() %>% 
  mutate(spp.survey.section.prop = section.final/spp.survey.tot)

spp_section_prop %>%
  filter(alpha.code %in% spp10years$alpha.code) %>% 
  ggplot() +
  geom_violin(aes(x = section, y = spp.survey.section.prop)) +
  geom_point(aes(x = section, y = spp.survey.section.prop)) +
  facet_wrap(~alpha.code)


# all species trend plot ----
# baywide annual abundance
wbirds4analysis %>% 
  mutate(alpha.code = ifelse(alpha.code %in% c("GRSC", "LESC"), "SCAUP", alpha.code)) %>%
  group_by(alpha.code, study.year, date) %>% 
  summarise(date.spp.total = sum(section.final)) %>% 
  ungroup() %>% 
  group_by(alpha.code, study.year) %>% 
  summarise(annual.spp.mean = mean(date.spp.total)) %>% 
  ungroup() %>% 
  filter(alpha.code %in% filter(wbird_spp_summ, num.years.obs > 20)$alpha.code & alpha.code %in% filter(wbird_spp_summ, spp.max > 10)$alpha.code) %>% 
  ggplot() +
  geom_line(aes(x = study.year, y = annual.spp.mean)) +
  facet_wrap(~alpha.code, scales = "free")


# annual mean abundance by section
wbirds4analysis %>% 
  mutate(alpha.code = ifelse(alpha.code %in% c("GRSC", "LESC"), "SCAUP", alpha.code)) %>%
  group_by(alpha.code, study.year, section) %>% 
  summarise(spp.annual.mean = mean(section.final)) %>% 
  ungroup() %>% 
  filter(alpha.code %in% filter(wbird_spp_summ, num.years.obs > 20)$alpha.code & alpha.code %in% filter(wbird_spp_summ, spp.max > 10)$alpha.code) %>% 
  ggplot() +
  geom_line(aes(x = study.year, y = spp.annual.mean, color = section)) +
  facet_wrap(~alpha.code, scales = "free")

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


