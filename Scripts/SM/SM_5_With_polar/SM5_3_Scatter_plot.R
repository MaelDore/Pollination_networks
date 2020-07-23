
##### Script SM5_3: Generate Figure S5.1 #####

# Scatter plot highlighting polar networks as outliers

# Inputs:

# Data for aggregated webs

# Outputs:

# Figure S5.1: Scatter plot highlighting polar networks as outliers

#####


##################################### Summary #########################################

# 1/ Load data

# 2/ Generate plot

#######################################################################################


### Clear memory
rm(list = ls())
gc() # Force R to release memory it is no longer using


### 1/ Load data for aggregated webs ####

aggreg.webs <- readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_full_rich_with_polar.RData"))

### 2/ Generate plot

library(tidyverse)
library(scales)
library(ggthemes)

limits_df <- tibble(x = c(0, 100), y = c(-20, 30))
polar_networks <- aggreg.webs[aggreg.webs$Latitude_dec > 70, ]
Polar_names <- polar_networks$Name_paper
All_coord <- paste0(aggreg.webs$Latitude_dec, "°N, ", str_sub(aggreg.webs$Longitude_dec, 2, 5), "°W")


g1 <- ggplot(aggreg.webs, aes(x = HF, y = Mean_T_IPCC)) +
  geom_point(data = polar_networks, col = "red", size = 5) +
  geom_point(size = 2) +
  # scale_x_log10() +
  scale_x_continuous(trans = log1p_trans(),
                     breaks = c(0, 1, 2, 5, 10, 20, 50, 100)) +
  labs(x = "HII [%]", y = "Tmean [°C]") +
  geom_rangeframe(data = limits_df, aes(x = x, y = y), size = 1.4) +
  ggrepel::geom_label_repel(data = . %>% 
                              mutate(Name_paper = as.character(Name_paper)) %>% 
                              mutate(label = ifelse(Name_paper %in% Polar_names,
                                                    All_coord, "")),
                            aes(label = label),
                            label.size = 0.8, # Width of the label box line
                            box.padding = 1.5, # Space between boxes. Use to tune position of labels
                            point.padding = 0.5, # Space between point and arrow tip
                            label.padding	= 0.35, # Space between box border and text 
                            alpha = 1,     # Transparency of the label and the text
                            fill = "white",  # Background label color
                            color = "red",           # Color is applied to the text and the label border
                            segment.color = "red",  # Color of the line joining the point and the label. If NA, no segment is drawn.
                            segment.size = 1,    # Width of the line joining the point and the label
                            segment.alpha = 1) + # Transparency of the line joining the point and the label
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_line(colour = "black", size = 1.2),
        axis.title.x = element_text(size = 18, vjust = -1.4, color = 'black'),  # Titre x-axis
        axis.text.x = element_text(size = 15, vjust = 0, color = 'black'),      # Labels x-axis 
        axis.title.y = element_text(size = 18, vjust = 2, color = 'black'),     # Titre y-axis
        axis.text.y = element_text(size = 15, vjust = 0.3, color = 'black'),    # Labels y-axis
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))     

?geom_label_repel




pdf(file = paste0("./SM_Final_figures/Figure_S5.1.pdf"), width = 12, height = 8)

print(g1)

dev.off()
  