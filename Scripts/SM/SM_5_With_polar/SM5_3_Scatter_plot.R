
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
library(cowplot)

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

### 3D plot with climate as 2 PCA-axes ####

library(ade4) # Package utile pour l'ACP qui contient des jeux de données quantis

aggreg.webs <- readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_full_rich_with_polar.RData"))

polar_indices <- which(aggreg.webs$Latitude_dec > 70)

# Extract climate variables

climate_var <- aggreg.webs[, c("Mean_T_IPCC", "T_Seasonality_IPCC", "Tot_Rainfall_IPCC", "Rainfall_Seasonality_IPCC")]

PCA_climate <- dudi.pca(df = climate_var, scannf = F, nf = 2) # Generate PCA and keep only 2 axes

round(PCA_climate$eig/sum(PCA_climate$eig)*100, 2) # % Explained variance per axis
cumsum(round(PCA_climate$eig/sum(PCA_climate$eig)*100, 2)) # % Cumulative explained variance per axis

s.corcircle(PCA_climate$co, xax = 1, yax = 2)

plot(PCA_climate$li, col = "black", pch = 16, main = "Climate ordination", xlab = paste0("PC1 (", round(PCA_climate$eig/sum(PCA_climate$eig)*100,2)[1] ," %)"), ylab = paste0("PC2 (", round(PCA_climate$eig/sum(PCA_climate$eig)*100,2)[2] ," %)"))
points(PCA_climate$li[polar_indices, ], pch = 16, cex = 1.5, col = "red")
# Ajouter les flèches des variables # Attention, la version correcte serait de récupérér les coordonnées transformées du biplot
arrows(x0 = rep(0, nrow(PCA_climate$co)), y0 = rep(0, nrow(PCA_climate$co)), x1 = PCA_climate$co[,1]*3, y1 =  PCA_climate$co[,2]*3, length = 0.2, code = 2, lwd = 1)
text(PCA_climate$co*3.3, labels = c("Tmean", "Tvar", "Rtot", "Rvar"))

library(rgl)

ThreeD_axis <- cbind(aggreg.webs$ln_HF, PCA_climate$li)

col_pts <- rep("black", nrow(aggreg.webs)) ; col_pts[polar_indices] <- "red"
labels_pts <- rep("", nrow(aggreg.webs)) ; labels_pts[polar_indices] <- All_coord[polar_indices]

# Open 3D window with define orientation
open3d(zoom = 1, 
       userMatrix = matrix(data = c(0.639, 0.769, 0.004, 0, -0.165, 0.133, 0.978, 0, 0.752, -0.625, 0.211, 0, 0, 0, 0, 1) , nrow = 4, ncol = 4, byrow = T),
       windowRect = c(0, 23, 1920, 1040),
       viewport = c(0, 0, 1811, 1017))
plot3d(x = ThreeD_axis, type = "n", xlab = "HII", ylab = "Climate PC1", zlab = "Climate PC2") # To generate the axis
spheres3d(x = ThreeD_axis, radius = 0.05, col = "black") # To add points in the desired color in the 3D plot
spheres3d(x = ThreeD_axis[polar_indices, ], radius = 0.1, col = "red")
# text3d(x = ThreeD_axis[polar_indices, ], texts = All_coord[polar_indices], col = "red") # To add the labels in the desired color in the 3D plot

# Get current 3D window parameters
zoom <- par3d()$zoom ; userMatrix <- par3d()$userMatrix ; windowRect <- par3d()$windowRect ; viewport <- par3d()$viewport



