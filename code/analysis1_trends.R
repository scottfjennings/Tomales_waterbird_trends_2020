

library(tidyverse)
library(here)
library(MASS) # for glm.nb
library(lmtest) # for lrtest 
library(AICcmodavg)
library(birdnames)

source("C:/Users/scott.jennings/Documents/Projects/water_birds/ACR_waterbird_data_management/code/utility/waterbird_utility_functions.r")

options(scipen = 999)

# data prep ----
wbirds4analysis <- readRDS("C:/Users/scott.jennings/Documents/Projects/water_birds/ACR_waterbird_data_management/data_files/working_rds/wbirds4analysis") %>% 
  wbird_add_study_day()

spp_annual_mean <- wbirds4analysis %>% 
  mutate(alpha.code = ifelse(alpha.code %in% c("GRSC", "LESC"), "SCAUP", alpha.code)) %>% 
  wbird_spp_annual_mean()


trend_spp <- spp_annual_mean %>% 
  group_by(alpha.code) %>% 
  summarise(num.years.detected = n()) %>% 
  filter(num.years.detected >= 20, !alpha.code %in% c("EUWI", "WCGR", "AWPE", "LOON", "PCLO", "HEGR"))
# consistently low abundance of EUWI, AWPE and WCGR so models don't converge, low numbers of other groups so not including even though models converge


# first analysis step is to evaluate evidence for any long term trends 

# primary interest is in estimating support for and direction/magnitude of Year coefficient 


readRDS(here("data_files/predictors")) %>% 
  dplyr::select(fall.mean.moci, total.fall.rain) %>% 
  filter(!is.na(fall.mean.moci)) %>% 
  cor()



# fill in 0 for years spp not detected
spp_annual_mean <- spp_annual_mean %>%
  filter(alpha.code %in% trend_spp$alpha.code) %>% 
  pivot_wider(id_cols = alpha.code, names_from = study.year, values_from = spp.annual.mean) %>% 
  pivot_longer(-alpha.code, names_to = "study.year", values_to = "spp.annual.mean") %>% 
  mutate(spp.annual.mean = ifelse(is.na(spp.annual.mean), 0, spp.annual.mean),
         study.year = as.numeric(study.year))


# join bird data with predictors
spp_annual_mean <- spp_annual_mean %>% 
  full_join(., readRDS(here("data_files/predictors"))) %>% # predictor variables
  rename(moci = fall.mean.moci,
         rain = total.fall.rain) %>% 
  filter(study.year > 1990) %>% 
  #mutate(spp.annual.mean = floor(spp.annual.mean),
  #       rain = scale(rain)) %>% 
  data.frame()

saveRDS(spp_annual_mean, here("data_files/spp_annual_mean"))

# first round of LRT ----

nb_lrt1<- function(zspp) {
spp_annual_mean <- filter(spp_annual_mean, alpha.code == zspp)

year2_rain_moci <- glm.nb(floor(spp.annual.mean) ~ poly(study.year, 2) + rain + moci, data = spp_annual_mean)
year_rain_moci <- glm.nb(floor(spp.annual.mean) ~ study.year + rain + moci, data = spp_annual_mean)

lrt <- lrtest(year2_rain_moci, year_rain_moci)

out_result <- list(year2_rain_moci = year2_rain_moci,
                   year_rain_moci = year_rain_moci,
                   lrt = lrt,
                   alpha.code = toString(zspp))

}

# running inside safely() will capture errors instead of failing
all_spp_lrt1 <- map(trend_spp$alpha.code, quietly(nb_lrt1))

names(all_spp_lrt1) <- trend_spp$alpha.code

all_spp_lrt1 %>% 
  map("warnings") %>% 
  compact()
all_spp_lrt1 %>% 
  map("errors") %>% 
  compact()


saveRDS(all_spp_lrt1, here("fitted_models/all_spp_lrt1"))

# str(all_spp_lrt1, max.level = 2)


# check for LRT significance at 0.05 level
get_pval <- function(spp_lrt) {
pval <- spp_lrt$result$lrt$`Pr(>Chisq)` %>% 
  data.frame() %>% 
  rename(pval = 1) %>% 
  filter(!is.na(pval)) %>% 
  mutate(alpha.code = spp_lrt$result$alpha.code)
}

lrt1_pvals <- map_df(all_spp_lrt1, get_pval) %>% 
  rename(alpha.code = 2) %>% 
  mutate(sig.year2 = pval <= 0.05)



#
# second round of LRT ----
# for all species where year2 was not better supported than year, do LRT comparing model with year + rain + moci to model with rain + moci
nb_lrt2 <- function(zspp) {
spp_annual_mean <- filter(spp_annual_mean, alpha.code == zspp)

year_rain_moci <- glm.nb(floor(spp.annual.mean) ~ study.year + rain + moci, data = spp_annual_mean)
rain_moci <- glm.nb(floor(spp.annual.mean) ~ rain + moci, data = spp_annual_mean)

lrt <- lrtest(year_rain_moci, rain_moci)

out_result <- list(year_rain_moci = year_rain_moci,
                   rain_moci = rain_moci,
                   lrt = lrt,
                   alpha.code = toString(zspp))

}


lrt2_spp <- filter(lrt1_pvals, pval > 0.05)

all_spp_lrt2 <- map(lrt2_spp$alpha.code, quietly(nb_lrt2))

names(all_spp_lrt2) <- lrt2_spp$alpha.code

all_spp_lrt2 %>% 
  map("warnings") %>% 
  compact()
all_spp_lrt2 %>% 
  map("errors") %>% 
  compact()



# str(all_spp_lrt2, max.level = 2)


saveRDS(all_spp_lrt2, here("fitted_models/all_spp_lrt2"))


lrt2_pvals <- map_df(all_spp_lrt2, get_pval) %>% 
  rename(alpha.code = 2) %>% 
  mutate(sig.year = pval <= 0.05)



# get year coefficients and ci ----
# from from the species with support for quadratic year effect
get_year2_coefs <- function(spp_lrt) {
mod <- spp_lrt$result$year2_rain_moci

coefs <- mod$coefficients %>% 
  data.frame() %>% 
  rownames_to_column("variable") %>% 
  rename(coefficient = 2)

ci <- confint(mod) %>% 
  data.frame() %>% 
  rownames_to_column("variable") %>% 
  rename(lci = 2, uci = 3)

dev_explained <- 1 - (mod$deviance/mod$null.deviance)

coef_ci <- full_join(coefs, ci)

year_coef_ci <- coef_ci %>% 
  filter(grepl("study.year", variable)) %>% 
  mutate(alpha.code = spp_lrt$result$alpha.code,
         variable = ifelse(grepl("2)1", variable), "study.year", variable),
         variable = ifelse(grepl("2)2", variable), "study.year2", variable))

}


sig_year2_lrt1 <- all_spp_lrt1[filter(lrt1_pvals, pval <= 0.05)$alpha.code]


year2_spp_coef_ci <- map_df(sig_year2_lrt1, get_year2_coefs)

# then for species with support for only linear year effect
get_year_coefs <- function(spp_lrt) {
mod <- spp_lrt$result$year_rain_moci

coefs <- mod$coefficients %>% 
  data.frame() %>% 
  rownames_to_column("variable") %>% 
  rename(coefficient = 2)

ci <- confint(mod) %>% 
  data.frame() %>% 
  rownames_to_column("variable") %>% 
  rename(lci = 2, uci = 3)

coef_ci <- full_join(coefs, ci)

year_coef_ci <- coef_ci %>% 
  filter(grepl("study.year", variable)) %>% 
  mutate(alpha.code = spp_lrt$result$alpha.code)

}

sig_year_lrt2 <- all_spp_lrt2[filter(lrt2_pvals, pval <= 0.05)$alpha.code]


year_spp_coef_ci <- map_df(sig_year_lrt2, get_year_coefs)


# combine lrt significance and coefs/ci from significant models ----

lrt1_result_summary <- year2_spp_coef_ci %>% 
  pivot_wider(id_cols = alpha.code, names_from = variable, values_from = c("coefficient", "lci", "uci")) %>% 
  left_join(., lrt1_pvals) 


lrt2_result_summary <- year_spp_coef_ci %>% 
  pivot_wider(id_cols = alpha.code, names_from = variable, values_from = c("coefficient", "lci", "uci")) %>% 
  full_join(., lrt2_pvals)



trend_result_summary <- full_join(lrt1_result_summary, lrt2_result_summary)



saveRDS(trend_result_summary, here("fitted_models/trend_result_summary"))




# model objects saved, can load packages, source, then start here ----
# getting and plotting predicted estimates ----

all_spp_lrt1 <- readRDS(here("fitted_models/all_spp_lrt1"))
all_spp_lrt2 <- readRDS(here("fitted_models/all_spp_lrt2"))

trend_result_summary <- readRDS(here("fitted_models/trend_result_summary"))



wbird_newdat = data.frame(moci = 0,  # mean value
                          rain = 653, # mean value 
                          study.year = seq(1991, 2019))

#
# for single species at a time ----
buff <- all_spp_lrt2$BUFF$result$rain_moci

predict(buff, wbird_newdat, type = "link", se.fit = TRUE) %>% 
  data.frame()


fitted(all_spp_lrt1$BRAC$result$year2_rain_moci, type = "response")

predict(all_spp_lrt1$BRAC$result$year2_rain_moci, wbird_newdat, type = "response") %>% 
  cbind(fitted(all_spp_lrt1$BRAC$result$year2_rain_moci, type = "response")) %>% 
  cbind(., wbird_newdat %>% dplyr::select(study.year)) %>% 
  data.frame() %>% 
  rename(pred.abund = 1,
         fitted.abund = 2) %>%
  mutate(alpha.code = "BRAC") %>% 
  full_join(., filter(spp_annual_mean, alpha.code == "BRAC"), by = c("alpha.code", "study.year")) %>% 
  dplyr::select(-total.year.rain) %>% 
  pivot_longer(cols = c("pred.abund", "fitted.abund", "spp.annual.mean", "moci", "rain")) %>% 
  mutate(zgroup = ifelse(grepl(c("abund|mean"), name), "bird", name)) %>% 
  ggplot() +
  geom_line(aes(x = study.year, y = value, color = name)) +
    facet_wrap(~zgroup, ncol = 1, scales = "free_y")
    
  


# functions to map across all species ----

extract_predicted_year2 <- function(lrt.obj) {
  critval <- 1.96 ## approx 95% CI
  
  best.mod <- lrt.obj$result$year2_rain_moci
  
predict(best.mod, wbird_newdat, type = "link", se.fit = TRUE) %>%  
  data.frame() %>% 
  rename(predicted = fit, se = se.fit) %>% 
  cbind(fitted(best.mod, type = "response") %>% 
          data.frame() %>% 
          rename(fitted = 1)) %>% 
  cbind(., wbird_newdat %>% dplyr::select(study.year)) %>%
  mutate(uci = predicted + (critval * se),
         lci = predicted - (critval * se),
         predicted.resp = best.mod$family$linkinv(predicted),
         uci.resp = best.mod$family$linkinv(uci),
         lci.resp = best.mod$family$linkinv(lci),
         alpha.code = lrt.obj$result$alpha.code)
}


sig_year2_lrt1 <- all_spp_lrt1[filter(trend_result_summary, sig.year2 == TRUE)$alpha.code]


all_predicted_year2 <- map_df(sig_year2_lrt1, extract_predicted_year2)

saveRDS(all_predicted_year2, here("fitted_models/all_predicted_year2"))
#--
extract_predicted_year <- function(lrt.obj) {
  critval <- 1.96 ## approx 95% CI
  
  best.mod <- lrt.obj$result$year_rain_moci
  
predict(best.mod, wbird_newdat, type = "link", se.fit = TRUE) %>%  
  data.frame() %>% 
  rename(predicted = fit, se = se.fit) %>% 
  cbind(fitted(best.mod, type = "response") %>% 
          data.frame() %>% 
          rename(fitted = 1)) %>% 
  cbind(., wbird_newdat %>% dplyr::select(study.year)) %>%
  mutate(uci = predicted + (critval * se),
         lci = predicted - (critval * se),
         predicted.resp = best.mod$family$linkinv(predicted),
         uci.resp = best.mod$family$linkinv(uci),
         lci.resp = best.mod$family$linkinv(lci),
         alpha.code = lrt.obj$result$alpha.code)
}

sig_year_lrt2 <- all_spp_lrt2[filter(trend_result_summary, sig.year == TRUE)$alpha.code]

all_predicted_year <- map_df(sig_year_lrt2, extract_predicted_year)

saveRDS(all_predicted_year, here("fitted_models/all_predicted_year"))

#--
extract_predicted_noyear <- function(lrt.obj) {
  critval <- 1.96 ## approx 95% CI
  
  best.mod <- lrt.obj$result$rain_moci
  
predict(best.mod, wbird_newdat, type = "link", se.fit = TRUE) %>%  
  data.frame() %>% 
  rename(predicted = fit, se = se.fit) %>% 
  cbind(fitted(best.mod, type = "response") %>% 
          data.frame() %>% 
          rename(fitted = 1)) %>% 
  cbind(., wbird_newdat %>% dplyr::select(study.year)) %>%
  mutate(uci = predicted + (critval * se),
         lci = predicted - (critval * se),
         predicted.resp = best.mod$family$linkinv(predicted),
         uci.resp = best.mod$family$linkinv(uci),
         lci.resp = best.mod$family$linkinv(lci),
         alpha.code = lrt.obj$result$alpha.code)
}

nosig_year_lrt2 <- all_spp_lrt2[filter(trend_result_summary, sig.year == FALSE)$alpha.code]


all_predicted_noyear <- map_df(nosig_year_lrt2, extract_predicted_noyear)

saveRDS(all_predicted_noyear, here("fitted_models/all_predicted_noyear"))

# combine
all_predicted <- rbind(all_predicted_year2, all_predicted_year, all_predicted_noyear)

saveRDS(all_predicted, here("fitted_models/all_trend_predicted"))


# calculate percent change ----

  
 # vertex of quadratic curves

quad_vertex <- all_predicted_year2  %>% 
  arrange(alpha.code, study.year) %>% 
  group_by(alpha.code) %>% 
  mutate(annual.percent.change = -100 * (1 - predicted.resp/lag(predicted.resp))) %>%
  filter(!is.na(annual.percent.change)) %>% 
  group_by(alpha.code) %>% 
  mutate(quad.vertex = ifelse(abs(annual.percent.change) == min(abs(annual.percent.change)), TRUE, FALSE)) %>% 
  filter(quad.vertex == TRUE) %>% 
  dplyr::select(alpha.code, quad.vertex.year = study.year, quad.vertex.per.change = annual.percent.change)

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

saveRDS(percent_change_wide_quad, here("data_files/percent_change_wide_quad"))

# --

linear_trends <- all_predicted_year %>%
  group_by(alpha.code) %>% 
  mutate(trend.segment.bound = case_when(study.year == min(study.year) ~ "start",
                                         study.year == max(study.year) ~ "end")) %>% 
  ungroup() %>% 
  dplyr::select(alpha.code, study.year, contains("resp"), trend.segment.bound) %>% 
  filter(!is.na(trend.segment.bound)) %>% 
  mutate(trend.segment = "linear")

percent_change_wide_lin <- linear_trends %>% 
  pivot_longer(cols = c(study.year, contains("resp"))) %>% 
  mutate(trend.segment.bound.varb = paste(trend.segment.bound, name, sep = ".")) %>% 
  pivot_wider(id_cols = c("alpha.code", "trend.segment"), values_from = value, names_from = trend.segment.bound.varb) %>% 
  mutate(trend.segment.percent.change = -100 * (1 - end.predicted.resp/start.predicted.resp),
         year.effect = ifelse(trend.segment == "linear", "linear", "quadratic")) %>% 
  arrange(desc(year.effect), alpha.code, desc(trend.segment))
  
saveRDS(percent_change_wide_lin, here("data_files/percent_change_wide_lin"))



