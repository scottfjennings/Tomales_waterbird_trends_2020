


library(tidyverse)
library(birdnames)
library(tabulizer)
library(pdftools)
library(here)



ethier_app1 <- extract_tables("https://www.ace-eco.org/vol15/iss2/art20/appendix1.pdf")


ethier_spp_class <- data.frame(common.name = c(ethier_app1[[1]][,1], ethier_app1[[2]][,1]),
                               mig.dist = c(ethier_app1[[1]][,10], ethier_app1[[2]][,8]),
                               diet = c(ethier_app1[[1]][,11], ethier_app1[[2]][,9])) %>% 
  mutate(common.name = ifelse(grepl("Bonapart", common.name), "Bonapart's Gull", common.name)) %>% 
  bird_taxa_filter(keep_taxa = c("Anseriformes", "Podicipediformes", "Gaviiformes", "Alcidae", "Gruiformes", "Pelecanidae", "Suliformes")) %>% 
  distinct()

write.csv(ethier_spp_class, here("data_files/ethier_etal_spp_classification.csv"), row.names = FALSE)



