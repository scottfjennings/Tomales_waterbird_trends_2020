

## THIS NOT YET FULLY FUNCTIONAL. 
## SUCCESSFULLY GENERATES DATA FOR EACH SPECIES X YEAR, BUT DOESN'T YET GENERATE DATA FOR PARTICULAR DAYS WITHIN YEARS

## CONTAINS SOME CODE THAT IS NOT NEEDED, BUT DIFFICULT TO FOGURE OUT WHICH IS GOOD VS BAD

## ULTIMATELY MAY NOT NEED THIS SIMULATED DATA

# basic raw data visualization

# packages, source ----
library(tidyverse)
library(lubridate)
library(MASS)
library(AICcmodavg)

source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.r")
source("C:/Users/scott.jennings/Documents/Projects/water_birds/code/utility/waterbird_utility_functions.r")

options(scipen = 999)
# data ----
# waterbird foraging guild membership
# received in xlsx via email from NW on 2020/08/26
wbird_guilds <- read.csv("data_files/Foraging guild table.csv") %>% 
  bird_taxa_filter(join_taxa = c("common.name", "common.name"))



wbird_guilds_longer <- wbird_guilds %>% 
  dplyr::select(alpha.code, guild, guild2) %>% 
  pivot_longer(cols = contains("guild"), names_to = "which.guild", values_to = "guild.name") %>% 
  mutate_all(na_if,"") %>% 
  filter(!is.na(guild.name))


# allocated, negative tallied waterbird data ----


wbirds4analysis <- readRDS("C:/Users/scott.jennings/Documents/Projects/water_birds/data_files/working_rds/wbirds4analysis") %>% 
  wbird_add_study_day() 

wbird_by_guild <- wbirds4analysis %>% 
  full_join(., filter(wbird_guilds_longer, which.guild == "guild"))



spp_surv_total <- wbirds4analysis %>% 
  group_by(alpha.code, date, study.year, study.day) %>% 
  summarise(spp.survey.tot = sum(section.final)) %>% 
  ungroup()

#
# all spp X surveys ----

spp_year <- expand.grid(alpha.code = distinct(wbirds4analysis, alpha.code)$alpha.code,
                        study.year = distinct(wbirds4analysis, study.year)$study.year) %>% 
  full_join(., distinct(wbirds4analysis, date, study.year, study.day)) %>% 
  arrange(alpha.code, study.year, study.day)


wbirds_surveys_zeros <- spp_surv_total %>% 
  full_join(spp_year, by = c("alpha.code", "date", "study.year", "study.day")) %>% 
  replace_na(list(spp.survey.tot = 0)) %>% 
  arrange(alpha.code, study.year, study.day)


#
# very basic model fitting to check assumptions - no predictors of interest!!! ----

basic_mods <- wbirds_surveys_zeros %>% 
  nest(-alpha.code) %>% 
  mutate(fit = map(data, ~ glm.nb(spp.survey.tot ~ study.year + study.day, data = .))) 


spp_mod_looker <- function(zspp) {
 mod <- basic_mods$fit[basic_mods$alpha.code == zspp][[1]]
}
spp_dat_looker <- function(zspp) {
 mod <- basic_mods$data[basic_mods$alpha.code == zspp][[1]]
}


spp_mod_looker("GRSC") %>% summary()
spp_mod_looker("GRSC") %>% plot()

spp_dat_looker("GRSC") %>% 
  ggplot(group = as.factor(study.year)) +
  geom_line(aes(x = study.day, y = spp.survey.tot, color = as.factor(study.year))) 

spp_dat_looker("GRSC") %>% 
  ggplot(group = as.factor(study.year)) +
  geom_boxplot(aes(x = as.factor(study.year), y = spp.survey.tot)) +
  geom_point(aes(x = as.factor(study.year), y = spp.survey.tot))


## generate some fake data ----


varbs <- c("study.year", "study.day", "moci", "forage.fish",	"demersal.fish",	"fish.roe",	"mammals.birds", "snails",	"mussels.clams", "crustaceans", "plants")


coefs <- rnorm(length(varbs), 0, 1)

mod_mat <- c(2010, 60, rnorm(1, 0, 1), rbinom(8, 1, 0.5))

varbs_coefs <- cbind(varbs, coefs, mod_mat) %>%
  data.frame() %>% 
  mutate(coefs = as.numeric(coefs),
         mod_mat = as.numeric(mod_mat)) %>% 
  mutate(val = coefs * mod_mat,
         val = ifelse(varbs == "study.year", val + 6000, val))


 spp <- paste("spp", 1:10, sep = "")

   #regression coefficients ----
 beta0 <- runif(10, 2, 10)
 beta.year <- runif(10, -1, 1)
 beta.day <- runif(10, -0.1, 0.1)
 beta.moci <- rnorm(10, 0, 3.9)
 beta.forage.fish	<- rnorm(10, 0, 1)
 beta.demersal.fish	<- rnorm(10, 0, 1)
 beta.fish.roe	<- rnorm(10, 0, 1)	
 beta.snails	<- rnorm(10, 0, 1)	
 beta.mussels.clams		<- rnorm(10, 0, 1)
 beta.crustaceans		<- rnorm(10, 0, 1)
 beta.plants	<- rnorm(10, 0, 1)

 all_coefs <- cbind(spp, beta0, beta.year, beta.day, beta.moci, beta.forage.fish, beta.demersal.fish, beta.fish.roe, beta.snails, beta.mussels.clams, beta.crustaceans,  beta.plants) %>% 
   data.frame()
 
   #generate covariate values ----
 
 
 make_covariates <- function() {
 spp <- paste("spp", 1:10, sep = "")
 forage.fish	<- rbinom(10, 1, 0.5)
 demersal.fish	<- rbinom(10, 1, 0.5)
 fish.roe	<- rbinom(10, 1, 0.5)	
 snails	<- rbinom(10, 1, 0.5)	
 mussels.clams		<- rbinom(10, 1, 0.5)
 crustaceans		<- rbinom(10, 1, 0.5)
 plants	<- rbinom(10, 1, 0.5)
 
covariates <- cbind(spp, forage.fish, demersal.fish, fish.roe, snails, mussels.clams, crustaceans,  plants) %>% 
  data.frame()
 }
 
 covariates <- make_covariates()

 
 x_day <- distinct(wbirds4analysis, study.year, study.day)

# generate an overall mean abundance for each species
spp_pop_seed <- data.frame(spp = covariates$spp,
                           spp.grand.mean = seq(5, 5000, length.out = 10))

# ----

make_spp_abund <- function(zspp) {
  zspp_pop_seed <- spp_pop_seed %>% 
    filter(spp == zspp)
  
  cos_start <- rnorm(1, 0, 3)
  cos_end <- cos_start + rnorm(1, 31, 3)
spp_abund <- data.frame(spp = zspp,
                        study.year = 1989:2019,
                        spp.abund = rpois(31, zspp_pop_seed$spp.grand.mean * exp(cos(seq(cos_start, cos_end, length.out = 31)))))
return(spp_abund)
}

make_spp_abund("spp10")

all_spp_abund <- map_df(spp, make_spp_abund)


ggplot(all_spp_abund) + 
  geom_line(aes(x = study.year, y = spp.abund, color = spp))

 year_covariate <- expand.grid(spp = spp, study.year = 1989:2019) %>% 
   arrange(spp, study.year) %>% 
   full_join(., spp_pop_seed) %>% 
   full_join(x_day)
  

 
 all_covariates <- full_join(year_covariate, covariates) %>% 
   arrange(spp, study.year, study.day) %>% 
   ungroup()%>%
   mutate(rand.day = rnorm(length(spp), 1, 0.1))
 
 
 truf <- full_join(all_covariates, all_coefs) %>% 
   mutate_at(vars(-spp), as.numeric) %>% 
   mutate(log.mu = beta0 + 
                     beta.year * study.year + 
                     beta.day * study.day +  
                     beta.moci * x.moci +
                     beta.forage.fish * x.forage.fish + 
                     beta.demersal.fish * x.demersal.fish +
                     beta.fish.roe * x.fish.roe +
                     beta.snails * x.snails +
                     beta.mussels.clams * x.mussels.clams +
                     beta.crustaceans * x.crustaceans +
                     beta.plants * x.plants,
          mu = exp(log.mu))
 
   #compute mu's
 mu <- exp(beta0 + beta1 * x)
   #generate Y-values
 y <- rpois(n=n, lambda=mu)
   #data set
 data <- data.frame(y=y, x=x)
 data

data.frame(ind = seq(1, 10, by = 0.01)) %>% 
  mutate(sin1.10 = sin(2 * pi * ind),
         cos1.10 = cos(2 * pi * ind),
         #sin.x.cos = sin1.10 * cos1.10,
         sin.p.cos = sin1.10 + cos1.10) %>% 
  pivot_longer(cols = contains(c("sin", "cos")), names_to = "zcurve", values_to = "zval") %>% 
  ggplot() +
  geom_line(aes(x = ind, y = zval, color = zcurve))
                  
spp_seed <- c(rnbinom(3, mu = 50, size = 3), rnbinom(4, mu = 500, size = 10), rnbinom(3, mu = 2000, size = 1))
true_abund <- rpois(10, spp_seed) %>% map(~ rnbinom(31, mu = .x, size = 100)) %>% unlist()
spp <- paste("spp", rep(1:10, each = 31), sep = "")

spp_env1_effect <- data.frame(spp = paste("spp", 1:10, sep = ""),
                              env1.effect = rnorm(10, 1, 0.4))

years <- rep(seq(1989, 2019), 10)

env1 <- data.frame(years = 1989:2019,
                       env1 = as.numeric(1 + (cos(1:31) * 0.2)))


truf <- cbind(spp, years, true_abund) %>% 
  data.frame()%>% 
   mutate_at(vars(-spp), as.numeric) %>% 
  full_join(env1) %>% 
  full_join(spp_env1_effect) %>% 
  mutate(stoch.abund = round(true_abund * (env1.effect * env1), 0)) %>% 
  full_join(., covariates)

ggplot(truf) +
  geom_line(aes(x = years, y = stoch.abund, color = spp))



#
# fit some models ----
zmod.nb <- glm.nb(stoch.abund ~ years + env1 + forage.fish + demersal.fish + fish.roe + snails + mussels.clams + crustaceans + plants, data = truf)
zmod.pois <- glm(stoch.abund ~ years + env1 + forage.fish + demersal.fish + fish.roe + snails + mussels.clams + crustaceans + plants, data = truf, family = poisson(link = "log"))
zmod.qpois <- glm(stoch.abund ~ years + env1 + forage.fish + demersal.fish + fish.roe + snails + mussels.clams + crustaceans + plants, data = truf, family = "quasipoisson")

pchisq(summary(zmod.qpois)$dispersion * zmod.pois$df.residual, zmod.pois$df.residual, lower = F) 

aictab(list(zmod.glm, zmod.nb), c("glm", "nb"))

zmod.forage.fish <- glm.nb(stoch.abund ~ years + env1 + forage.fish, data = truf)
zmod.demersal.fish <- glm.nb(stoch.abund ~ years + env1 + demersal.fish, data = truf)
zmod.fish.roe <- glm.nb(stoch.abund ~ years + env1 + fish.roe, data = truf)
zmod.snails <- glm.nb(stoch.abund ~ years + env1 + snails, data = truf)
zmod.mussels.clams <- glm.nb(stoch.abund ~ years + env1 + mussels.clams, data = truf)
zmod.crustaceans <- glm.nb(stoch.abund ~ years + env1 + crustaceans, data = truf)
zmod.plants <- glm.nb(stoch.abund ~ years + env1 + plants, data = truf)


aictab(list(zmod.forage.fish,
            zmod.demersal.fish,
            zmod.fish.roe,
            zmod.snails,
            zmod.mussels.clams,
            zmod.crustaceans,
            zmod.plants), 
       c("zmod.forage.fish",
            "zmod.demersal.fish",
            "zmod.fish.roe",
            "zmod.snails",
            "zmod.mussels.clams",
            "zmod.crustaceans",
            "zmod.plants"))






