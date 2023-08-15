


dabbler_goose_swan_genera <- c("Dendrocygna", "Anser", "Branta", "Cygnus", "Aix", "Spatula", "Mareca", "Anas")
diver_seaduck_genera <- c("Aythya", "Somateria", "Melanitta", "Clangula", "Bucephala", "Oxyura")
merg_loon_grebe_taxa <- c("Lophodytes", "Mergus", "Podicipediformes", "Gaviiformes")
corm_peli_taxa <- c("Suliformes", "Pelecaniformes")

fish_eater_taxa <- c("Lophodytes", "Mergus", "Podicipediformes", "Gaviiformes", "Suliformes", "Pelecaniformes")


#' filter_observed_x_years
#' 
#' Filter waterbird data to species seen in at least x_years years. 
#'
#' @param df data frame with at least alpha.code and study.year
#' @param x_years number of years observed for cutoff
#'
#' @return
#' @export
#'
#' @examples
filter_observed_x_years <- function(df, x_years) {
  df2 <- df %>% 
  distinct(study.year, alpha.code) %>% 
  count(alpha.code) %>% 
  filter(n >= x_years) %>% 
  select(alpha.code) %>% 
  left_join(df)
return(df2)
}


#' get_best_model
#' 
#' get the best model for a particular species
#'
#' @param zspp 
#'
#' @return
#' @export
#'
#' @examples brac_best <- get_best_model("BRAC")
#' all_best <- map_df(trend_spp$alpha.code, get_best_model)
get_best_model <- function(zspp) {
best_mod_name <- readRDS(here("fitted_models/final_models"))[[zspp]][["aic_tab"]] %>% 
  filter(Delta_AICc == 0) %>% 
  dplyr::select(Modnames) %>% 
  mutate(alpha.code = zspp)

}


#' get_competitive_models
#' 
#' get model names for the competitive models (Delta_AICc <= 2) for a particular species
#'
#' @param zspp 
#'
#' @return data frame with columns alpha.code and Modnames
#' @export
#'
#' @examples brac_competitive <- get_competitive_models("BRAC")
#' 
#' all_cometitive <- map_df(trend_spp$alpha.code, get_competitive_models)
get_competitive_models <- function(zspp) {
comp_mods <- readRDS(here("fitted_models/final_models"))[[zspp]][["aic_tab"]] %>% 
  filter(Delta_AICc <= 2) %>% 
  select(Modnames) %>% 
  mutate(alpha.code = zspp)
}


#' mod_coefs
#'
#' get coefficients and 95% ci for a particular model for a particular species
#'
#' @param zspp which species 
#' @param zmod which model do you want to extract coefficients and CI for
#'
#' @return data frame with row for each variable in zmod. has columns variable, coeficient, lci, uci, Modnames, alpha.code, Delta_AICc
#' @export
#'
#' @examples brac_best_coefs <- mod_coefs("BRAC", "year2")
#' 
#' brac_comp_coefs <- map2_df(brac_competitive$alpha.code, brac_competitive$Modnames, mod_coefs)
#' all_comp_coefs <- map2_df(all_competitive$alpha.code, all_competitive$Modnames, mod_coefs)
mod_coefs <- function(zspp, zmod) {
  
  
#zmod = "year2_fresh_moci"
#zspp = "ALL"
  
zspp_mod <- readRDS(here("fitted_models/final_models"))[[zspp]][[zmod]]


coefs <- zspp_mod$coefficients %>% 
  data.frame() %>% 
  rownames_to_column("variable") %>% 
  rename(coefficient = 2)

ci <- confint(zspp_mod) %>% 
  data.frame() 

if(zmod == "intercept") {
  ci <- ci %>%
  rownames_to_column("zci") %>% 
    rename(value = 2) %>% 
    pivot_wider(names_from = zci) %>% 
  rename(lci = 1, uci = 2) %>% 
    mutate(variable = "(Intercept)")
 } else { 
ci <- ci %>% 
  rownames_to_column("variable") %>% 
  rename(lci = 2, uci = 3)
 }

coef_ci <- full_join(coefs, ci) %>% 
  mutate(Modnames = zmod,
         alpha.code = zspp) %>% 
  left_join(readRDS(here("fitted_models/final_models"))[[zspp]][["aic_tab"]][c("Modnames", "Delta_AICc")])

return(coef_ci)
}


#' mod_dev_explained
#' 
#' get deviance explained from a particular model for a particular species
#'
#' @param zspp 
#' @param zmod 
#'
#' @return
#' @export
#'
#' @examples  brac_year_dev <- mod_dev_explained("BRAC", "year")
mod_dev_explained <- function(zspp, zmod) {
zspp_mod <- readRDS(here("fitted_models/final_models"))[[zspp]][[zmod]]

dev_explained <- data.frame(dev.expl = 1 - (zspp_mod$deviance/zspp_mod$null.deviance),
                            alpha.code = zspp,
                            Modnames = zmod) %>% 
  mutate(dev.expl = round(dev.expl, 2))
return(dev_explained)
}


#' spp_aic
#' 
#' get aic table for a particular species
#'
#' @param zspp 
#'
#' @return
#' @export
#'
#' @examples  brac_aic<- get_aic("BRAC")
get_aic <- function(zspp) {
zspp_aic <- readRDS(here("fitted_models/final_models"))[[zspp]][["aic_tab"]]
return(zspp_aic)
}


#' parm_informative
#' 
#' Check for uninformative parameters in a model, based on 95% Confidence Intervals.
#'
#' @param zmod 
#'
#' @return
#' @export
#'
#' @examples
parm_informative <- function(zmod, zmod.name) {

  #zmod <- zspp_mods[[zmod.name]]
  #zmodel.name <- zmod %>% names()
  #zmodel <- zmod[[1]]

  zmodel_ci <- confint(zmod) %>% 
                data.frame() %>% 
                rownames_to_column("parm") %>% 
    rename(lcl95 = X2.5..,
           ucl95 = X97.5..)
  
  
parms_informative <- coef(summary(zmod)) %>% 
  data.frame() %>% 
  rownames_to_column("parm") %>% 
  filter(!grepl("ntercept", parm)) %>% 
    dplyr::select(parm, Estimate) %>% 
    left_join(., zmodel_ci) %>% 
    mutate(informative95 = ifelse((lcl95 < 0 & ucl95 < 0) | (lcl95 > 0 & ucl95 > 0), TRUE, FALSE),
           Modnames = zmod.name)

return(parms_informative)
}

# linear model version
#' spp_parm_informative
#'  
#'  wrapper for parm_informative to loop through all models for a particular species.
#' 
#' @param zmodel 
#' @param zmod.name 
#'
#' @return
#' @export
#'
#' @examples brac_inf <- lm_parm_informative("BRAC")
spp_parm_informative <- function(zspp, mod_obj) {

# mod_obj = "final_models"  
  
zspp_mod_obj <- readRDS(here(paste("fitted_models/", mod_obj, sep = "")))[[zspp]]

znames <- names(zspp_mod_obj)[!grepl("aic_tab|intercept", names(zspp_mod_obj))]

zspp_mods <- zspp_mod_obj[znames]


parms_inf <- map2_df(zspp_mods, names(zspp_mods), parm_informative) %>% 
  mutate(alpha.code = zspp)

return(parms_inf)
}



#' fix_varb_names
#' 
#' Make variable names pretty for output. 
#'
#' @param zvarb column with variable names output by modeling function
#'
#' @return column with nicer looking variable names
#' @export
#'
#' @examples
fix_varb_names <- function(zvarb) {
          zvarb = gsub("intercept|\\(Intercept\\)", "Intercept", zvarb)
          zvarb = gsub("moci", "MOCI", zvarb)
         zvarb = gsub("fresh", "Freshwater inflow", zvarb)
         zvarb = gsub("poly\\(study.year, 2\\)1", "Year", zvarb)
         zvarb = gsub("poly\\(study.year, 2\\)2", "Year^2^", zvarb)
         zvarb = gsub("study.year", "Year", zvarb)
         zvarb = gsub("year", "Year", zvarb)
         zvarb = gsub("giac", "Restoration", zvarb)
}



make_informative_names <- function(zspp_aic) {
model_names <- zspp_aic %>% 
  mutate(parm = ifelse(informative95 == FALSE & parm != "poly(study.year, 2)1", sprintf(paste(parm, "^\u2020^", sep = "")), parm)) %>% 
  group_by(Modnames) %>% 
  summarise(mod.name.inf = paste(parm, collapse = "_")) %>% 
  ungroup() %>% 
  mutate(mod.name.out = ifelse(Modnames == "intercept", "Intercept only", mod.name.inf))
}

fix_mod_name_out <- function(model_names) {
model_names <- model_names %>% 
  mutate(mod.name.out = fix_varb_names(mod.name.out),
         mod.name.out = gsub("_", " + ", mod.name.out),
         mod.name.out = gsub(" \\+ \\^2\\^", "^2^", mod.name.out),
         mod.name.out = gsub("Year \\+ Year\\^2\\^", "Year^2^", mod.name.out),
         mod.name.out = gsub("giac", "Restoration", mod.name.out),
         mod.name.out = gsub("\\.", " * ", mod.name.out))
} 



#' is_iucn_threatened_trend
#'
#' identify whether predicted changes in abundance meet IUCN criteria for listing as threatened or higher (>= 2% decline for >= 10 years)
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
is_iucn_threatened_trend <- function(df) {
iucn_threatened_trend <- df %>% 
  filter(grepl("year", Modnames)) %>% 
  arrange(common.name, Modnames, study.year) %>% 
  group_by(common.name, Modnames) %>% 
  mutate(annual.per.change = 100 * ((predicted/lag(predicted))-1),
         iucn.threat = ifelse(annual.per.change <= -2, TRUE, FALSE)) %>% 
  filter(iucn.threat == TRUE) %>% 
  mutate(decline.dur = n()) %>% 
  filter(decline.dur >= 10)
}

#' mod_predictions_link
#' 
#' calculate model estimates (predictions) for zspp species and zmod model on the scale of the link function
#'
#' @param zspp 
#' @param zmod 
#'
#' @return data frame
#' @export
#'
#' @examples
mod_predictions_link <- function(zspp, zmod) {
  
zspp_mod <- readRDS(here("fitted_models/final_models"))[[zspp]][[zmod]]

znewdat <- data.frame(study.year = seq(1992, 2022),
                      moci = 0,
                      fresh = 0)%>% 
  mutate(giac = ifelse(study.year < 2009, 0, 1))

zpred_new <- predict(zspp_mod, znewdat, type = "link", se = TRUE) %>% 
  bind_cols(znewdat) %>% 
  mutate(alpha.code = zspp,
         Modnames = zmod) %>% 
  dplyr::select(alpha.code, Modnames, study.year, fit, se.fit) 

}


#' spp_mod_plotter
#' 
#' Plot estimates and raw data for each zspp and each model in zmod. This function calculates estimates on the fly to allow plotting estimates from multiple models without having to separately go generate predictions for more than just the best model.
#'
#' @param zspp which species
#' @param zmod character string with the names of the models you want to plot
#'
#' @return returns the plot. does not save to disk
#' @export
#'
#' @examples
spp_mod_plotter <- function(zspp, zmod = NA, save.plot = TRUE) {
  
 # zspp = "ALL"
#  zmod = c("fresh_moci_giac", "year_fresh_moci")
  
  if(is.na(zmod)) {
    zmod = get_best_model(zspp)$Modnames
  }
  
  zmain = ifelse(zspp == "ALL", "All species combined", translate_bird_names(zspp, "alpha.code", "common.name"))

    mod.ranks <- get_aic(zspp) %>%
    filter(Modnames %in% zmod) %>%
    distinct(Modnames, Delta_AICc) %>% 
    arrange(Delta_AICc) %>%
    mutate(mod.name.out = ifelse(Modnames == "intercept", "Intercept only", Modnames)) %>% 
    fix_mod_name_out() %>% 
      mutate(mod.name.out2 = gsub("Year2", "Year^2^", mod.name.out))


  zdat <- map2_df(zspp, zmod, mod_predictions_link) %>%
  data.frame() %>% 
  mutate(lci = exp(fit - (1.96 * se.fit)),
         uci = exp(fit + (1.96 * se.fit)),
         predicted = exp(fit)) %>%
  left_join(readRDS(here("data_files/spp_annual_full_preds")) %>% select(alpha.code, study.year, p75.abund)) %>% 
  mutate(mod.name.out = ifelse(Modnames == "intercept", "Intercept only", Modnames)) %>% 
    fix_mod_name_out() %>% 
    left_join(get_aic(zspp) %>% select(Modnames, Delta_AICc)) %>% 
    arrange(Delta_AICc, study.year) %>% 
    mutate(mod.name.out = factor(mod.name.out, levels = mod.ranks$mod.name.out),
           common.name = translate_bird_names(alpha.code, "alpha.code", "common.name"))
  
  
  iucn_threat <- zdat %>% 
    is_iucn_threatened_trend()
  
zdat <- full_join(zdat, iucn_threat) %>% 
  mutate(iucn.threat = as.numeric(iucn.threat),
         iucn.threat = replace_na(iucn.threat, 0),
         iucn.threat = as.character(iucn.threat))
  
  
  y.max = ifelse(max(zdat$p75.abund, na.rm = TRUE) > max(zdat$uci), 
                 max(zdat$p75.abund, na.rm = TRUE),
                 max(zdat$uci))
  
  y.rounder = case_when(y.max <= 100 ~ 10,
                         between(y.max, 100, 500) ~ 50,
                         between(y.max, 500, 1000) ~ 100,
                         between(y.max, 1000, 5000) ~ 500,
                         y.max > 5000 ~ 1000)
  
  
  y.splitter = case_when(y.max < 70 ~ 10,
                         (y.max >= 70 & y.max <  150) ~ 20,
                         (y.max >= 150 & y.max < 300) ~ 50,
                         (y.max >=300 & y.max < 700) ~ 100,
                         (y.max >= 700 & y.max < 1500) ~ 200,
                         (y.max >= 1500 & y.max < 3000) ~ 500,
                         (y.max >= 3000 & y.max < 7000) ~ 1000,
                         (y.max >= 7000 & y.max < 30000) ~ 2000,
                         y.max > 30000 ~ 5000)
  
  
  y.top = ceiling(((y.max)/y.splitter)) * y.splitter
  
  
  y.scale = seq(0, y.top, by = y.splitter)
  y.scale.minor = seq(0, y.top, by = y.splitter/5)
  
  
  line.vals = mod.ranks$mod.name.out
  line.labs = mod.ranks$mod.name.out2
  
  
zplot <- zdat %>% 
  ggplot() +  
  geom_line(aes(x = study.year, y = predicted, linetype = mod.name.out)) +
    geom_ribbon(aes(x = study.year, ymin = lci, ymax = uci, linetype = mod.name.out), alpha = 0.5) +
  geom_line(data = iucn_threat, aes(x = study.year, y = predicted, linetype = mod.name.out), linewidth = 1, color = "red", show.legend = FALSE) +
  geom_point(aes(x = study.year, y = p75.abund), size = 3) +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5), labels = seq(1990, 2025, by = 5), minor_breaks = seq(1992, 2022, by = 1)) +
  scale_y_continuous(breaks = y.scale, labels = y.scale, minor_breaks = y.scale.minor, limits = c(0, y.top)) + 
  guides(color = "none") +
    theme_bw() +
  theme(legend.position = c(.95, .98),
        legend.justification = c("right", "top"),
        legend.text.align = 0,
        legend.title=element_blank()) +
    labs(title = zmain,
         x = "Year",
         y = "Estimated abundance",
         linetype = "") 

zplot
  
if(save.plot == TRUE) {
    ggsave(here(paste("figures_output/", zspp, ".png", sep = "")), zplot, height = 6, width = 6)
} else {
  return(zplot)
}
}

