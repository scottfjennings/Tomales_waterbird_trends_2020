


library(tidyverse)
library(here)
library(cowplot)
library(birdnames)


mod_avg_per_change <- readRDS(here("data_files/mod_avg_per_change")) %>% 
  filter(!is.na(percent.change), !common.name %in% c("piscivore", "omnivore", "herbivore", "diving benthivore")) %>% 
  mutate(common.name = ifelse(common.name == "all", "All waterbirds", common.name))

spp_order <- mod_avg_per_change  %>% 
  filter(!common.name %in% c("All waterbirds")) %>%
  arrange(percent.change) 
spp.order = c("All waterbirds", spp_order$common.name)


mod_avg_per_change <- mod_avg_per_change %>% 
  mutate(common.name = factor(common.name, levels = spp.order))

per_change_plotter <- function(per_change_df) {
  zplot <-  ggplot(data = per_change_df, aes(x = percent.change, y = common.name)) +
                     geom_segment(aes(x = -100, y = common.name, xend = max(percent.change), yend = common.name), color = 'gray85') +
                     geom_point(aes(color = percent.change, size = 3)) +
                     geom_segment(aes(x = percent.change, y = common.name, xend = 0, yend = common.name, color = percent.change)) +
                     geom_vline(xintercept = 0)  +
                     theme_classic() +
                     theme(legend.position = "none") +
    labs(x = "",
         y = "") +
    scale_y_discrete(limits = rev(levels(per_change_df$common.name)))
                   
                   return(zplot)
}


# zback_color = "gray50" # color to match slide background fro docent training slides
zback_color = "white"
all_others <- mod_avg_per_change  %>% 
  filter(!common.name %in% c("Canada Goose", "Common Merganser", "Brandt's Cormorant")) %>% 
  droplevels() %>% 
  per_change_plotter() +
  scale_x_continuous(breaks = seq(-100, 150, by = 50)) +
  scale_color_gradient2(low = "red", mid = "gray75", high = "blue")+
  theme(plot.margin = unit(c(.5, 0.5, 0, 0), "cm"))  +
  theme(text = element_text(size=20, face= "bold", colour= "black")) +
  theme(panel.background = element_rect(fill = zback_color, colour = NA),  
        plot.background = element_rect(fill = zback_color, colour = NA))

cang <- mod_avg_per_change %>%
  filter(common.name %in% c("Canada Goose")) %>% 
  droplevels() %>% 
per_change_plotter() +
  scale_x_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000), limits = c(0, 6000)) +
  theme(plot.margin = unit(c(0, 0.5, 0, 0), "cm")) +
  theme(text = element_text(size=20, face= "bold", colour= "black")) +
  theme(panel.background = element_rect(fill = zback_color, colour = NA),  
        plot.background = element_rect(fill = zback_color, colour = NA)) 


come_brac <- mod_avg_per_change %>%
  filter(common.name %in% c("Common Merganser", "Brandt's Cormorant")) %>% 
  droplevels() %>% 
  per_change_plotter() +
  scale_x_continuous(breaks = c(0, 250, 500, 750, 1000), limits = c(0, 1000)) +
  scale_color_gradient2(low = "blue1", high = "blue3")+
  theme(plot.margin = unit(c(0, 0.5, 0, 0), "cm")) +
  theme(text = element_text(size=20, face= "bold", colour= "black")) +
  theme(panel.background = element_rect(fill = zback_color, colour = NA),  
        plot.background = element_rect(fill = zback_color, colour = NA)) 


title <- ggdraw() + 
  draw_label(
    "Percent change in wintering waterbird abundance, \n Tomales Bay, CA, 1992 to 2022",
    fontface = 'bold',
    size = 16)

foot <- ggdraw() + 
  draw_label(
    "*note different scales for subplots",
    size = 10,
    x = 0.25,
    vjust = -1)

per_cow_head_foot <- plot_grid(title, all_others, come_brac, cang, foot, align = "v", nrow = 5, rel_heights = c(.4, 4, 0.6, 0.5, 0.1)) +
  theme(plot.background = element_rect(fill = "white"))

ggsave(here("figures_output/overall_per_change.png"), height = 12)

