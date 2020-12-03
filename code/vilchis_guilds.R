
# top chunk extracts guild and diet classifications from .docx appendix of 
# Vilchis, L. I., Johnson, C. K., Evenson, J. R., Pearson, S. F., Barry, K. L., Davidson, P., … Gaydos, J. K. (2014). Assessing ecological correlates of marine bird declines to inform marine conservation. Conservation Biology, 29(1), 154–163. https://doi.org/10.1111/cobi.12378

# once extracted, only need to run from second chunk onward

# chunk 1, extract ----
library(tidyverse)
library(docxtractr)
library(janitor)

vilchis_appendix2 <- read_docx("C:/Users/scott.jennings/Documents/Papers/Vilchis et al 2014-sup-0002-appendixS2.DOCX")

app2_table <- docx_extract_all_tbls(vilchis_appendix2)

fix_yes <- function(x) {
  x = ifelse(grepl("yes", x), "yes", x)
}


app2_table_df <- data.frame(app2_table[[1]]) %>%
  row_to_names(row_number = 1) %>% 
  rename(order = 1, family = 2, species = 3, common.name = 4, foo1 = 5, foo2 = 11, foo3 = 20) %>% 
  select(-contains("foo"), -order, -family, -species, -contains("includes")) %>% 
  mutate(common.name = as.factor(common.name),
         across(where(is.character), fix_yes),
         common.name = gsub("Br&t", "Brandt", common.name)) %>% 
  mutate_all(na_if,"") %>% 
  filter(!is.na(common.name))

saveRDS(app2_table_df, "data_files/rds/vilchis_guilds")


# everything else ----
# once extracted, run from here down
library(tidyverse)
source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.r")



vilchis_guilds <- readRDS("data_files/rds/vilchis_guilds") %>% 
  bird_taxa_filter(join_taxa = c("common.name", "common.name"), drop_cols = c("species.number", "order", "family", "subfamily", "genus", "species"))

# vilchis_spp <- vilchis_guilds %>% 
#  mutate(common.name.orig = common.name) %>% 
#  select(common.name.orig, common.name, alpha.code) %>% 
#  edit()

# saveRDS(vilchis_spp, "data_files/rds/vilchis_spp")

vilchis_spp <- readRDS("data_files/rds/vilchis_spp") %>% 
  select(alpha.code) %>% 
  mutate(vilchis = T)

tomales_spp <- wbird_guilds %>% 
  select(common.name, alpha.code, guild, guild2) %>% 
  bird_taxa_filter(join_taxa = c("alpha.code", "alpha.code"), keep_taxa = c("Podicipediformes", "Gaviiformes", "Suliformes", "Pelecanidae", "Alcidae", "Sterninae", "Ralidae", "Anseriformes", "AMCO", "AMCOGRSCLESCBUFF")) %>% 
  select(common.name, alpha.code, guild, guild2) %>% 
  mutate(tomales = T)

missing_spp <- left_join(tomales_spp, vilchis_spp) %>% 
  full_join(., vilchis_guilds)

write.csv(missing_spp, "data_files/spp_foraging_diet_classification.csv", row.names = F)


guild_categories <- names(vilchis_guilds) %>% 
  data.frame() %>% 
  rename(guilds = 1) %>% 
  mutate(guilds = gsub("&/or ", "", guilds),
         guilds = gsub(" ", ".", guilds)) %>% 
  filter(!guilds %in% c("common.name", "alpha.code")) %>% 
  mutate(guild.type = c(rep("foraging.strategy", 5), rep("diet.preferences", 8)))


foraging <- filter(guild_categories, guild.type == "foraging.strategy") %>% select(guilds)
diet <- filter(guild_categories, guild.type == "diet.preferences") %>% select(guilds)



vilchis_foraging_strategy <- vilchis_guilds %>% 
  select(common.name, matches(foraging$guilds))

overall_strategy <- apply(vilchis_foraging_strategy, 1, function(r) paste0(names(r)[r == 'yes'], collapse = ', '))

foraging_stragies <- cbind(vilchis_foraging_strategy, overall_strategy)

vilchis_diet_pref <- vilchis_guilds %>% 
  select(common.name, matches(diet$guilds))

overall_diet <- apply(vilchis_diet_pref, 1, function(r) paste0(names(r)[r == 'yes'], collapse = ', '))

diet_preferences <- cbind(vilchis_diet_pref, overall_diet)



