# Author: Maël DORÉ
# Contact: mael.dore@gmail.com
# License: MIT

##### Graphical Abstract #####

### Generate base plot for Graphical abstract

aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_full.RData")) # Retrieve dataset associated with web coverage

library(tidyverse)
library(ggthemes)
library(scales)

limits_df <- tibble(x = c(20, 1000), y = c(0.01, 1))

Graph_abs_plot <-  ggplot(aggreg.webs, aes(x = sptot, y = Connectance)) +
  geom_point(alpha = 0.15) +
  scale_y_log10(breaks = c(0.01, 0.03, 0.10, 0.30, 1)) +
  scale_x_log10(breaks = c(20, 50, 100, 200, 500, 1000)) +
  labs(x = "Species richness") +
  geom_rangeframe(data = limits_df, aes(x = x, y = y), size = 1.4) +
  theme_classic() +
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, color = NA),
        axis.line = element_line(color = NA),
        axis.ticks = element_line(size = 1.2),
        axis.ticks.length = unit(5, "pt"),
        axis.text = element_text(size = 12, face = "bold", color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10, b = 5)),
        axis.title.y = element_text(margin = margin(l = 5, r = 10)))


pdf(file = paste0("./Final_figures/Graphical_Abstract_background.pdf"), width = 8, height = 6)

print(Graph_abs_plot)

dev.off()

