


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
