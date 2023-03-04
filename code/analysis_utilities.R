


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
best_mod_name <- readRDS(here("fitted_models/all_spp_mods"))[zspp][[1]][[1]][["aic_tab"]] %>% 
  filter(Delta_AICc == 0) %>% 
  select(Modnames) %>% 
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
comp_mods <- readRDS(here("fitted_models/all_spp_mods"))[zspp][[1]][[1]][["aic_tab"]] %>% 
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
zspp_mod <- readRDS(here("fitted_models/all_spp_mods"))[zspp][[1]][[1]][[zmod]]


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
  left_join(readRDS(here("fitted_models/all_spp_mods"))[zspp][[1]][[1]][["aic_tab"]][c("Modnames", "Delta_AICc")])

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
zspp_mod <- readRDS(here("fitted_models/all_spp_mods"))[zspp][[1]][[1]][[zmod]]

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
zspp_aic <- readRDS(here("fitted_models/all_spp_mods"))[zspp][[1]][[1]][["aic_tab"]]
return(zspp_aic)
}

# 
# 

# linear model version
#' lm_parm_informative
#' 
#' Check for uninformative parameters in a model, based on both 95% and 85% Confidence Intervals. Arnold 2010 recommends removing uninformative parameters if 85% CI overlap 0. 85% is more consistent with AIC selection (95% CI is too broad and we end up excluding variables based on different standards). Calculates 85% CI based on multiplying se by 1.44 (from Z table for 85% CI). To help ensure that multiplying se by some Z value provides appropriate CI, I also include a test for whether 95% CI calculated by this method are equal to 95%.
#' 
#' @param zmodel 
#' @param zmod.name 
#'
#' @return
#' @export
#'
#' @examples brac_inf <- lm_parm_informative("BRAC")
lm_parm_informative <- function(zspp) {

zspp_mods <- readRDS(here("fitted_models/all_spp_mods"))[zspp][[1]][[1]][1:12]


parm_inf <- function(zmod) {

  zmod <- zspp_mods[zmod]
  zmodel.name <- zmod %>% names()
  zmodel <- zmod[[1]]

  zmodel_ci95 <- confint(zmodel)%>% 
                data.frame() %>% 
                rownames_to_column("parm") %>% 
    rename(lcl95 = X2.5..,
           ucl95 = X97.5..)
  
  zmodel_ci85 <- confint(zmodel, level = 0.85)%>% 
                data.frame() %>% 
                rownames_to_column("parm") %>% 
    rename(lcl85 = X7.5..,
           ucl85 = X92.5..)
  
  zmodel_ci <- full_join(zmodel_ci95, zmodel_ci85)
  
parms_informative <- coef(summary(zmodel)) %>% 
  data.frame() %>% 
  rownames_to_column("parm") %>% 
  filter(!grepl("ntercept", parm)) %>% 
    select(parm, Estimate) %>% 
    left_join(., zmodel_ci) %>% 
    mutate(informative95 = ifelse((lcl95 < 0 & ucl95 < 0) | (lcl95 > 0 & ucl95 > 0), TRUE, FALSE),
           informative85 = ifelse((lcl85 < 0 & ucl85 < 0) | (lcl85 > 0 & ucl85 > 0), TRUE, FALSE),
           Modnames = zmodel.name)

return(parms_informative)
}

parms_inf <- map_df(list("year2_fresh_moci", "year_fresh_moci", "year2_moci", "year_moci", "year2_fresh", "year_fresh", "year2", "year", "fresh_moci", "fresh", "moci"), parm_inf) %>% 
  mutate(alpha.code = zspp)

  
  return(parms_inf)
}



#' fix_modnames
#' 
#' Make model names pretty for output. 
#'
#' @param df with at least the column Modnames
#'
#' @return df with columns Modnames and mod.name.out
#' @export
#'
#' @examples
fix_varb_names <- function(zvarb) {
          zvarb = gsub("intercept|\\(Intercept\\)", "Intercept", zvarb)
          zvarb = gsub("moci", "MOCI", zvarb)
         zvarb = gsub("fresh", "freshwater inflow", zvarb)
         zvarb = gsub("poly\\(study.year, 2\\)1", "Year", zvarb)
         zvarb = gsub("poly\\(study.year, 2\\)2", "Year^2^", zvarb)
         zvarb = gsub("study.year", "Year", zvarb)
}
