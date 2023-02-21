


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




#' best_mod_coefs
#'
#' get best model coefficients and 95% ci for a particular species
#'
#' @param zspp which species do you want to extract coefficients and CI for
#'
#' @return data frame
#' @export
#'
#' @examples
best_mod_coefs <- function(zspp) {
zspp_mods <- readRDS(here("fitted_models/all_spp_mods"))[zspp][[1]][[1]]

best_mod_name <- zspp_mods$aic_tab %>% 
  filter(Delta_AICc == 0)

best_mod <- zspp_mods[[best_mod_name$Modnames]]

coefs <- best_mod$coefficients %>% 
  data.frame() %>% 
  rownames_to_column("variable") %>% 
  rename(coefficient = 2)

ci <- confint(best_mod) %>% 
  data.frame() %>% 
  rownames_to_column("variable") %>% 
  rename(lci = 2, uci = 3)

coef_ci <- full_join(coefs, ci) %>% 
  mutate(alpha.code = zspp)

return(coef_ci)
}


#' best_mod_dev_explained
#' 
#' get deviance explained from the best model for each species
#'
#' @param zspp 
#'
#' @return
#' @export
#'
#' @examples
best_mod_dev_explained <- function(zspp) {
zspp_mods <- readRDS(here("fitted_models/all_spp_mods"))[zspp][[1]][[1]]

best_mod_name <- zspp_mods$aic_tab %>% 
  filter(Delta_AICc == 0)

best_mod <- zspp_mods[[best_mod_name$Modnames]]

dev_explained <- data.frame(best.mod.dev.expl = 1 - (best_mod$deviance/best_mod$null.deviance),
                            alpha.code = zspp)
return(dev_explained)
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

parms_inf <- map_df(list("year2_fresh_moci", "year_fresh_moci", "year2_moci", "year_moci", "year2_fresh", "year_fresh", "year2", "year", "fresh_moci", "fresh", "moci"), parm_inf)

parms_inf_named <- parms_inf %>%
  mutate(parm = ifelse(informative95 == FALSE, sprintf(paste(parm, "^\u2020^", sep = "")), parm)) %>% 
  group_by(Modnames) %>% 
  summarise(mod.name.inf = paste(parm, collapse = "_")) %>% 
  ungroup() %>% 
  mutate(alpha.code = zspp)
  
  return(parms_inf_named)
}




