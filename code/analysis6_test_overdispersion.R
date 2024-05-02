

# reviewer comment on second round of reviews suggested Poisson might give a better fit.
# testing overdispersion here 

# process: fit full model with poisson dist. then test overdispersion




library(tidyverse)
library(here)
library(AER)

options(scipen = 999)

trend_spp <- readRDS(here("data_files/trend_spp"))

# fit models to each species ----

#' disp_test
#' 
#' function to test overdispersion of the fullest model in our waterbirds analysis for a given species
#'
#' @param zspp 
#'
#' @return
#' @export
#' 
#' @details
#' file that gets read in in the first line of the function should be your raw data file. This file must include all variables in the fitted model
#' 
#'
#' @examples
disp_test <- function(zspp) {
  zspp_annual <- readRDS(here("data_files/spp_annual_full_preds")) %>% 
    filter(alpha.code == zspp)
  # trend and both environmental
  year2_fresh_moci <- glm(p75.abund ~ poly(study.year, 2) + fresh + moci, data = zspp_annual,  family = poisson())
  
zz <-  data.frame(p.value = dispersiontest(year2_fresh_moci)$p.value) %>% 
  mutate(alpha.code = zspp,
         model = "full")
}



disp_test_results <- map_df(trend_spp$alpha.code, disp_test)

