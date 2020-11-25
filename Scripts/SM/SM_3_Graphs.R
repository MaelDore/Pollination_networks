# Author: Maël DORÉ
# Contact: mael.dore@gmail.com
# License: MIT

##### Script SM_3: Generate graphs for the SM_3 #####

# Inputs:

  # Table with all networks variables (aggreg.webs)
  # Results of calibrated models from Script 1
  # Summary table with information on graph execution from Script 2 (all_single_plots_summary_all_VE)
  # Summary table with information on multiple graph execution from Script 2 (all_multiple_plots_summary_all_VE)

# Outputs:

# Figure S3.1.1: Effects of anthropogenic pressures on insect richness (A) and insect link density (B)
# Figure S3.1.2: Effects of anthropogenic pressures on plant richness (A) and plant link density (B)
# Figure S3.2.1: Effects of climate on insect richness (A) and insect link density (B)
# Figure S3.2.2: Effects of climate on plant richness (A) and plant link density (B)
# Figure S3.3.1: Effects of sampling characteristics on insect richness (A & C) and insect link density (B & D)
# Figure S3.3.2: Effects of sampling characteristics on plant richness (A & C) and plant link density (B & D)

#####



### Clear memory
rm(list = ls())
gc() # Force R to release memory it is no longer using


##### Figure S3.1.1: Effects of anthropogenic pressures on insect richness (A) and insect link density (B) #####

# A/ Effect of human influence on insect richness
# B/ Effect of human influence on insect link density


# Cut panels for figure
decoupe <- layout(mat = matrix(data = c(1,0,2), nrow = 1, ncol = 3, byrow = TRUE), # Numbers = order of plots ;
                  widths=c(7,0.2,7),  # Widths of columns
                  heights=c(6),       # Heights of lines
                  respect=TRUE)

layout.show(decoupe) # Visualiser la d?coupe de la zone de dessin

## Load files

load(file = "./Data/all_single_plots_summary_all_VE.RData")
load(file = "./Data/all_multiple_plots_summary_all_VE.RData")


# To associate the right legend and the right variable name in the dataset with the right response variable (VAE)
VAE_list <- c("C", "Li", "Lp", "S", "I", "P" )
VAE_legend_list <- c("Connectance", "Insect link density  [inter/sp.]", "Plant link density  [inter/sp.]", "Total richness  [sp.]", "Insect richness  [sp.]", "Plant richness  [sp.]")
VAE_dataset_name_list <- c("Connectance", "Li", "Lp", "sptot", "full_insects", "full_plants" )


# To associate the right legend and the right variable name in the dataset with the right explanatory variable (VE)
VE_short_name_list <-  c("S", "P", "I", "SE", "Time", "ATS", "HF", "Ptot", "Tmean", "Pvar", "Tvar", "Forest", "Taxo")
VE_legend_list <- c("Network size  [sp.]", "Partner pool  [plant sp.] ", "Partner pool  [insect sp.]", "Standardized sampling effort  [h/inter]", "Sampling effort  [h]", "Annual time span of sampling  [days]", "Human Influence Index  [%]", "Annual total precipitation  [mm]", "Mean temperature  [°C]", "Precipitation seasonality  [CV]", "Temperature seasonality  [°C]", "Forest cover  [%]", "Taxonomic resolution  [%]")
VE_axis_name_list <- c("sptot", "full_plants", "full_insects", "Sampling_effort", "Sampling_time", "Annual_time_span", "HF", "Tot_Rainfall_IPCC", "Mean_T_IPCC", "Rainfall_Seasonality_IPCC", "T_Seasonality_IPCC", "Forest", "taxo_full_sp_perc")


# Chose web coverage to plot on a single Multiple plot
web_coverage_list <- c("full", "dipt", "hymeno")
web_coverage_to_plot <- c("full", "dipt", "hymeno")
web_coverage_legend <- c("Full", "Diptera", "Hymenoptera")
web_coverage_legend_custom <- paste0(web_coverage_legend,":") # Add the double point for plot convenience

# Chose panel A: effect of Human Influence on Insect richness
panel_A <- which((all_multiple_plots_summary_all_VE$VAE == "I") & (all_multiple_plots_summary_all_VE$VE_short_name == "HF"))

# Chose panel B: effect of Insect link density
panel_B <- which((all_multiple_plots_summary_all_VE$VAE == "Li") & (all_multiple_plots_summary_all_VE$VE_short_name == "HF"))


# Save plot index corresponding to each panel
index_plot_to_plot <- c(panel_A, panel_B)

# Define panel letters
panel_letters <- c("(a)", "(b)")

# Open plot
pdf(file = paste0("./SM_Final_figures/Figure_S3.1.1.pdf"), width = 14, height = 6)

# Cut panels
layout(mat = matrix(data = c(1,0,2), nrow = 1, ncol = 3, byrow = TRUE), # Numbers = order of plots ;
       widths=c(7,0.2,7),  # Widths of columns
       heights=c(6),       # Heights of lines
       respect=TRUE)

# Set margins
mar_old_settings <- par()$mar
par(mar = c(6.6, 5.1, 2.1, 2.1))

# Set outer margins
oma_old_settings <- par()$oma
par(oma = c(0, 1, 0, 0))

# Set position of tick labels
mgp_old_settings <- par()$mgp
par(mgp = c(3, 1.5, 0))

# Boucle
k <- 1 ; par(xpd = F)
for (j in index_plot_to_plot) {
  
  web_coverage_available <- unlist(strsplit(x = all_multiple_plots_summary_all_VE$web_coverage[j], split = "-"))
  
  # Chack if all web_coverage are available for this multiple plot
  if (!all(web_coverage_to_plot %in% web_coverage_available)) { # If not, warn with a message
    
    cat(paste("Not all web coverages are available to do a Multliple plot for", all_multiple_plots_summary_all_VE$VAE[j], "~", all_multiple_plots_summary_all_VE$VE_short_name[j] , "in model", all_multiple_plots_summary_all_VE$model_type[j], all_multiple_plots_summary_all_VE$polar[j], all_multiple_plots_summary_all_VE$taxo[j], all_multiple_plots_summary_all_VE$forest[j], "= Multiple plot n°", j, "\n"))
    
  } else {  # If available, plot !
    
    # Choix de la VAE
    VAE <- all_multiple_plots_summary_all_VE$VAE[j]
    VAE_legend <- VAE_legend_list[which(VAE_list == VAE)]
    
    # Choix de la VE
    VE <- VE_axis_name <- all_single_plots_summary_all_VE$VE[j]
    VE_short_name <- all_single_plots_summary_all_VE$VE_short_name[j]
    VE_legend <- VE_legend_list[which(VE_short_name_list == VE_short_name)]
    ln_transfo <- "no"
    
    # Special case of VE plotted with log transformation
    if (VE_short_name %in% c("S", "P", "I", "SE", "Time", "ATS")) { 
      VE_axis_name <- VE_axis_name_list[which(VE_short_name_list == VE_short_name)]
      ln_transfo <- "log"
    }
    
    # Special case of VE plotted with log(X+1) transformation
    if (VE_short_name == "HF") { 
      VE_axis_name <- VE_axis_name_list[which(VE_short_name_list == VE_short_name)]
      ln_transfo <- "log1p"
    }
    
    # Extract information on model 
    model_type <- all_multiple_plots_summary_all_VE$model_type[j]
    polar <- all_multiple_plots_summary_all_VE$polar[j]
    
    # Load models for each web coverage and save dataset_list
    dataset_list <- NA
    for (i in 1:length(web_coverage_to_plot)) {
      model_name <- paste0(VAE, "_", web_coverage_to_plot[i], "_", model_type, "_", polar)
      
      eval(call("<-", as.name(paste0("predict_table_",i)), readRDS(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Predicts_for_Graphs/predict_table_",model_name,"_for_",VE_short_name,".RData"))))
      eval(call("<-", as.name(paste0("model_",i)), readRDS(file = paste0("./Models_outputs/",VAE,"/",web_coverage_to_plot[i],"/",model_name,"/",model_name,"_bestmod_with_", VE_short_name, ".RData"))))
      
      dataset_list[i] <- paste0(web_coverage_to_plot[i], "_", model_type, "_", polar)
    }  
    
    # Use real limits of X and Y, all web_coverage confounded, to plot axis
    VAE_all_coverage <- as.numeric()
    for (i in 1:length(dataset_list)) { 
      aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve VAE (Y-axis) for all web_coverage
      VAE_all_coverage <- c(VAE_all_coverage, eval(parse(text = paste0("aggreg.webs$", VAE_dataset_name_list[which(VAE_list == VAE)]))))
    }
    
    VE_all_coverage <- as.numeric()
    for (i in 1:length(dataset_list)) { 
      aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve VE (X-axis) for all_coverage
      VE_all_coverage <- c(VE_all_coverage, eval(parse(text = paste0("aggreg.webs$",VE_axis_name))))
    }
    
    # Chose colors to associate with each model/web coverage
    col_list <- c("black", "dodgerblue3", "red") 
    
    # Extract significativit? of the slope from p-value of the single models stored in all_single_plots_summary_all_VE
    web_coverage_index <- NA
    for (i in 1:length(web_coverage_to_plot)) {
      web_coverage_index[i] <- which(web_coverage_to_plot[i] == web_coverage_list) # Retrieve index of plotted web coverages among all possible web coverages
    }
    ID_plot_list <- unlist(strsplit(x = all_multiple_plots_summary_all_VE$ID_plot[j], split = "-")) # Retrieve index of single plots
    ID_plot_used <- as.numeric(ID_plot_list[web_coverage_index])
    
    beta_value_list <- round(all_single_plots_summary_all_VE$Beta_coef[ID_plot_used], 3) # Extract beta-coefs
    beta_value_list_custom <- paste0(beta_value_list,",") # Add the comma to help to plot the legend
    
    p_value_list <- round(all_single_plots_summary_all_VE$p_value[ID_plot_used], 3) # Extract p-values
    
    signif_list <- as.numeric(p_value_list < 0.05)+1 # Code significance of slopes. 2 = significant, 1 = non-significant
    
    # Create label for p-value
    p_value_custom_list <- paste("p = ", p_value_list)
    p_value_custom_list[which(p_value_list < 0.001)] <- "p < 0.001"
    
    # # Create an output folder if needed
    # if (!dir.exists(paths = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar))) { 
    #   dir.create(path = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar), recursive = T) 
    # }   
    
    # Plot without log on X-axis
    # pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Multiple_plot_",VAE,"_",model_type,"_",polar,"_for_",VE_short_name,"_in_",paste(web_coverage_to_plot, collapse = "_"),".pdf"), width = 7, height = 6)
    
    # Plot axis and title
    # par(mfcol=c(1,1))
    
    if (ln_transfo == "no") {
      
      plot(y = VAE_all_coverage, x = VE_all_coverage, log = "y", xlab = "", ylab = "",
           main = "",
           cex.axis=1.9, cex.lab=1.9, type="n")
      
      # Custom position for y-label
      # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
      mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
      # Custom position for x-label
      mtext(text = VE_legend, side = 1, line = 3.6, cex = 1.4)
      
      for (i in 1:length(dataset_list)) {  # Plot each web_coverge iteratively
        
        aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve dataset associated with web coverage
        model <- eval(parse(text = paste0("model_",i))) # Retrieve model
        
        points(x = model$model[,grep(VE, names(model$model))], # No retro-transformation of the VE
               y = exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255),
               cex = 1.3)
      }
      
      for (i in 1:length(dataset_list)) { # Add predict lines
        predict_table <- eval(parse(text = paste0("predict_table_",i))) # Load predict for model i
        
        lines(x = predict_table$var_abs, # No retro-transformation of the VE
              y = exp(predict_table$predict), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
              col = col_list[i], lwd = 3, lty = c(2,1)[signif_list][i]) # Significant = full ligne. Non-significant = dashed line
      }
      
      par(xpd = T)
      
      # Add legend line by line, with beta letter as an expression
      for (i in 1:length(web_coverage_legend_custom)) {
        legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
               col = col_list[i], text.col = "black", text.font = 2,
               x = "topleft", inset=c(0, -0.4 + i*0.045), 
               bty ="n", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
      }
      
      par(xpd = F)
      
    }
    
    
    
    # dev.off()  
    
    # Plot with log on the X-axis
    
    if (ln_transfo %in% c("log", "log1p")) { 
      
      # pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Multiple_plot_",VAE,"_",model_type,"_",polar,"_for_",VE_short_name,"_in_",paste(web_coverage_to_plot, collapse = "_"),"_log.pdf"), width = 7, height = 6)
      
      # par(mfcol=c(1,1))
      
      if (ln_transfo == "log1p") { # Need to specify not to plot the X-axis (xaxt = "n") that will be customed for ln(HF+1)
        
        plot(y = VAE_all_coverage, x = VE_all_coverage + 1, log = "xy", # Need to add +1 to the original value because model with log(X+1)
             xlab = "", ylab = "",
             main = "",
             xaxt = "n", cex.axis=1.9, cex.lab=1.9, type="n")
        
        # Custom position for y-label
        # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
        mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
        # Custom position for x-label
        # mtext(text = paste(VE_legend, "(log scale)"), side = 1, line = 3.6, cex = 1.4) # with (log scale)
        mtext(text = paste(VE_legend), side = 1, line = 3.6, cex = 1.4)
        
        # Custom axis to display real values for low values of HF
        axis(side = 1, at = c(1,2,5,10,20,50,100), labels = c(0,1,5,10,20,50,100), cex.axis = 1.9, cex.lab = 1.9) 
        
      } else { # Otherwise, plot title and all axis
        
        plot(y = VAE_all_coverage, x = VE_all_coverage, log = "xy", 
             xlab = "", ylab = "",
             main = "",
             cex.axis=1.9, cex.lab=1.9, type="n")
        
        # Custom position for y-label
        # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
        mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
        # Custom position for x-label
        # mtext(text = paste(VE_legend, "(log scale)"), side = 1, line = 3.6, cex = 1.4) # with (log scale)
        mtext(text = paste(VE_legend), side = 1, line = 3.6, cex = 1.4)
      }
      
      # Plot points iteratively for each web_coverage
      for (i in 1:length(dataset_list)) { 
        aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve dataset associated with web coverage
        model <- eval(parse(text = paste0("model_",i))) # Retrieve the model
        points(exp(model$model[,grep(VE, names(model$model))]), # Retro-transformation of the VE tel such as Exp(X) - 1 = X' <=> X = ln(X' + 1)
               exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255),
               cex = 1.3)
      }
      
      # Plot predict iteratively for each web_coverage
      for (i in 1:length(dataset_list)) { # Add predict lines
        predict_table <- eval(parse(text = paste0("predict_table_",i))) # Load predict for model i
        lines(x = exp(predict_table$var_abs), # Retro-transformation of the VE tel such as Exp(X) - 1 = X' <=> X = ln(X' + 1)
              y = exp(predict_table$predict), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
              col = col_list[i], lwd = 3, lty = c(2,1)[signif_list][i]) # Significant = full ligne. Non-significant = dashed line
      }
      
      par(xpd = T)
      
      # Add legend line by line, with beta letter as an expression
      for (i in 1:length(web_coverage_legend_custom)) {
        legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
               col = col_list[i], text.col = "black", text.font = 2,
               x = "topleft", inset=c(0, -0.04 + i*0.045), 
               bty ="n", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
      }
      
      par(xpd = F)
      
      # dev.off()
    }    
    
  }
  
  legend(legend = panel_letters[k], x = "bottomleft", bty = "n",
         text.font = 2, cex = 2.2, inset=c(-0.03, 0))
  
  cat(paste("\n Plot n°", k, "on", length(index_plot_to_plot), "-", Sys.time(), "\n"))
  
  k <- k + 1 # Index to keep track when plotting a reduced number of plots
  
}

dev.off()

# Reset graphic parameters
par(mar = mar_old_settings)
par(mgp = mgp_old_settings)
par(oma = oma_old_settings)




##### Figure S3.1.2: Effects of anthropogenic pressures on plant richness (A) and plant link density (B) #####

# A/ Effect of human influence on plant richness
# B/ Effect of human influence on plant link density

# Cut panels for figure
decoupe <- layout(mat = matrix(data = c(1,0,2), nrow = 1, ncol = 3, byrow = TRUE), # Numbers = order of plots ;
                  widths=c(7,0.2,7),  # Widths of columns
                  heights=c(6),       # Heights of lines
                  respect=TRUE)

layout.show(decoupe) # Show layout organization

## Load files

load(file = "./Data/all_single_plots_summary_all_VE.RData")
load(file = "./Data/all_multiple_plots_summary_all_VE.RData")


# To associate the right legend and the right variable name in the dataset with the right response variable (VAE)
VAE_list <- c("C", "Li", "Lp", "S", "I", "P" )
VAE_legend_list <- c("Connectance", "Insect link density  [inter/sp.]", "Plant link density  [inter/sp.]", "Total richness  [sp.]", "Insect richness  [sp.]", "Plant richness  [sp.]")
VAE_dataset_name_list <- c("Connectance", "Li", "Lp", "sptot", "full_insects", "full_plants" )


# To associate the right legend and the right variable name in the dataset with the right explanatory variable (VE)
VE_short_name_list <-  c("S", "P", "I", "SE", "Time", "ATS", "HF", "Ptot", "Tmean", "Pvar", "Tvar", "Forest", "Taxo")
VE_legend_list <- c("Network size  [sp.]", "Partner pool  [plant sp.] ", "Partner pool  [insect sp.]", "Standardized sampling effort  [h/inter]", "Sampling effort  [h]", "Annual time span of sampling  [days]", "Human Influence Index  [%]", "Annual total precipitation  [mm]", "Mean temperature  [°C]", "Precipitation seasonality  [CV]", "Temperature seasonality  [°C]", "Forest cover  [%]", "Taxonomic resolution  [%]")
VE_axis_name_list <- c("sptot", "full_plants", "full_insects", "Sampling_effort", "Sampling_time", "Annual_time_span", "HF", "Tot_Rainfall_IPCC", "Mean_T_IPCC", "Rainfall_Seasonality_IPCC", "T_Seasonality_IPCC", "Forest", "taxo_full_sp_perc")


# Chose web coverage to plot on a single Multiple plot
web_coverage_list <- c("full", "dipt", "hymeno")
web_coverage_to_plot <- c("full", "dipt", "hymeno")
web_coverage_legend <- c("Full", "Diptera", "Hymenoptera")
web_coverage_legend_custom <- paste0(web_coverage_legend,":") # Add the double point for plot convenience

# Chose panel A: effect of Human Influence on Plant richness
panel_A <- which((all_multiple_plots_summary_all_VE$VAE == "P") & (all_multiple_plots_summary_all_VE$VE_short_name == "HF"))

# Chose panel B: effect of Plant link density
panel_B <- which((all_multiple_plots_summary_all_VE$VAE == "Lp") & (all_multiple_plots_summary_all_VE$VE_short_name == "HF"))


# Save plot index corresponding to each panel
index_plot_to_plot <- c(panel_A, panel_B)

# Define panel letters
panel_letters <- c("(a)", "(b)")

# Open plot
pdf(file = paste0("./SM_Final_figures/Figure_S3.1.2.pdf"), width = 14, height = 6)

# Cut panels
layout(mat = matrix(data = c(1,0,2), nrow = 1, ncol = 3, byrow = TRUE), # Numbers = order of plots ;
       widths=c(7,0.2,7),  # Widths of columns
       heights=c(6),       # Heights of lines
       respect=TRUE)

# Set margins
mar_old_settings <- par()$mar
par(mar = c(6.6, 5.1, 2.1, 2.1))

# Set outer margins
oma_old_settings <- par()$oma
par(oma = c(0, 1, 0, 0))

# Set position of tick labels
mgp_old_settings <- par()$mgp
par(mgp = c(3, 1.5, 0))

# Boucle
k <- 1 ; par(xpd = F)
for (j in index_plot_to_plot) {
  
  web_coverage_available <- unlist(strsplit(x = all_multiple_plots_summary_all_VE$web_coverage[j], split = "-"))
  
  # Chack if all web_coverage are available for this multiple plot
  if (!all(web_coverage_to_plot %in% web_coverage_available)) { # If not, warn with a message
    
    cat(paste("Not all web coverages are available to do a Multliple plot for", all_multiple_plots_summary_all_VE$VAE[j], "~", all_multiple_plots_summary_all_VE$VE_short_name[j] , "in model", all_multiple_plots_summary_all_VE$model_type[j], all_multiple_plots_summary_all_VE$polar[j], all_multiple_plots_summary_all_VE$taxo[j], all_multiple_plots_summary_all_VE$forest[j], "= Multiple plot n°", j, "\n"))
    
  } else {  # If available, plot !
    
    # Choix de la VAE
    VAE <- all_multiple_plots_summary_all_VE$VAE[j]
    VAE_legend <- VAE_legend_list[which(VAE_list == VAE)]
    
    # Choix de la VE
    VE <- VE_axis_name <- all_single_plots_summary_all_VE$VE[j]
    VE_short_name <- all_single_plots_summary_all_VE$VE_short_name[j]
    VE_legend <- VE_legend_list[which(VE_short_name_list == VE_short_name)]
    ln_transfo <- "no"
    
    # Special case of VE plotted with log transformation
    if (VE_short_name %in% c("S", "P", "I", "SE", "Time", "ATS")) { 
      VE_axis_name <- VE_axis_name_list[which(VE_short_name_list == VE_short_name)]
      ln_transfo <- "log"
    }
    
    # Special case of VE plotted with log(X+1) transformation
    if (VE_short_name == "HF") { 
      VE_axis_name <- VE_axis_name_list[which(VE_short_name_list == VE_short_name)]
      ln_transfo <- "log1p"
    }
    
    # Extract information on model 
    model_type <- all_multiple_plots_summary_all_VE$model_type[j]
    polar <- all_multiple_plots_summary_all_VE$polar[j]
    
    # Load models for each web coverage and save dataset_list
    dataset_list <- NA
    for (i in 1:length(web_coverage_to_plot)) {
      model_name <- paste0(VAE, "_", web_coverage_to_plot[i], "_", model_type, "_", polar)
      
      eval(call("<-", as.name(paste0("predict_table_",i)), readRDS(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Predicts_for_Graphs/predict_table_",model_name,"_for_",VE_short_name,".RData"))))
      eval(call("<-", as.name(paste0("model_",i)), readRDS(file = paste0("./Models_outputs/",VAE,"/",web_coverage_to_plot[i],"/",model_name,"/",model_name,"_bestmod_with_", VE_short_name, ".RData"))))
      
      dataset_list[i] <- paste0(web_coverage_to_plot[i], "_", model_type, "_", polar)
    }  
    
    # Use real limits of X and Y, all web_coverage confounded, to plot axis
    VAE_all_coverage <- as.numeric()
    for (i in 1:length(dataset_list)) { 
      aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve VAE (Y-axis) for all web_coverage
      VAE_all_coverage <- c(VAE_all_coverage, eval(parse(text = paste0("aggreg.webs$", VAE_dataset_name_list[which(VAE_list == VAE)]))))
    }
    
    VE_all_coverage <- as.numeric()
    for (i in 1:length(dataset_list)) { 
      aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve VE (X-axis) for all_coverage
      VE_all_coverage <- c(VE_all_coverage, eval(parse(text = paste0("aggreg.webs$",VE_axis_name))))
    }
    
    # Chose colors to associate with each model/web coverage
    col_list <- c("black", "dodgerblue3", "red") 
    
    # Extract significativit? of the slope from p-value of the single models stored in all_single_plots_summary_all_VE
    web_coverage_index <- NA
    for (i in 1:length(web_coverage_to_plot)) {
      web_coverage_index[i] <- which(web_coverage_to_plot[i] == web_coverage_list) # Retrieve index of plotted web coverages among all possible web coverages
    }
    ID_plot_list <- unlist(strsplit(x = all_multiple_plots_summary_all_VE$ID_plot[j], split = "-")) # Retrieve index of single plots
    ID_plot_used <- as.numeric(ID_plot_list[web_coverage_index])
    
    beta_value_list <- round(all_single_plots_summary_all_VE$Beta_coef[ID_plot_used], 3) # Extract beta-coefs
    beta_value_list_custom <- paste0(beta_value_list,",") # Add the comma to help to plot the legend
    
    p_value_list <- round(all_single_plots_summary_all_VE$p_value[ID_plot_used], 3) # Extract p-values
    
    signif_list <- as.numeric(p_value_list < 0.05)+1 # Code significance of slopes. 2 = significant, 1 = non-significant
    
    # Create label for p-value
    p_value_custom_list <- paste("p = ", p_value_list)
    p_value_custom_list[which(p_value_list < 0.001)] <- "p < 0.001"
    
    # # Create an output folder if needed
    # if (!dir.exists(paths = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar))) { 
    #   dir.create(path = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar), recursive = T) 
    # }   
    
    # Plot without log on X-axis
    # pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Multiple_plot_",VAE,"_",model_type,"_",polar,"_for_",VE_short_name,"_in_",paste(web_coverage_to_plot, collapse = "_"),".pdf"), width = 7, height = 6)
    
    # Plot axis and title
    # par(mfcol=c(1,1))
    
    if (ln_transfo == "no") {
      
      plot(y = VAE_all_coverage, x = VE_all_coverage, log = "y", xlab = "", ylab = "",
           main = "",
           cex.axis=1.9, cex.lab=1.9, type="n")
      
      # Custom position for y-label
      # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
      mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
      # Custom position for x-label
      mtext(text = VE_legend, side = 1, line = 3.6, cex = 1.4)
      
      for (i in 1:length(dataset_list)) {  # Plot each web_coverge iteratively
        
        aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve dataset associated with web coverage
        model <- eval(parse(text = paste0("model_",i))) # Retrieve model
        
        points(x = model$model[,grep(VE, names(model$model))], # No retro-transformation of the VE
               y = exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255),
               cex = 1.3)
      }
      
      for (i in 1:length(dataset_list)) { # Add predict lines
        predict_table <- eval(parse(text = paste0("predict_table_",i))) # Load predict for model i
        
        lines(x = predict_table$var_abs, # No retro-transformation of the VE
              y = exp(predict_table$predict), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
              col = col_list[i], lwd = 3, lty = c(2,1)[signif_list][i]) # Significant = full ligne. Non-significant = dashed line
      }
      
      par(xpd = T)
      
      # Add legend line by line, with beta letter as an expression
      for (i in 1:length(web_coverage_legend_custom)) {
        legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
               col = col_list[i], text.col = "black", text.font = 2,
               x = "topleft", inset=c(0, -0.4 + i*0.045), 
               bty ="n", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
      }
      
      par(xpd = F)
      
    }
    
    
    
    # dev.off()  
    
    # Plot with log on the X-axis
    
    if (ln_transfo %in% c("log", "log1p")) { 
      
      # pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Multiple_plot_",VAE,"_",model_type,"_",polar,"_for_",VE_short_name,"_in_",paste(web_coverage_to_plot, collapse = "_"),"_log.pdf"), width = 7, height = 6)
      
      # par(mfcol=c(1,1))
      
      if (ln_transfo == "log1p") { # Need to specify not to plot the X-axis (xaxt = "n") that will be customed for ln(HF+1)
        
        plot(y = VAE_all_coverage, x = VE_all_coverage + 1, log = "xy", # Need to add +1 to the original value because model with log(X+1)
             xlab = "", ylab = "",
             main = "",
             xaxt = "n", cex.axis=1.9, cex.lab=1.9, type="n")
        
        # Custom position for y-label
        # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
        mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
        # Custom position for x-label
        # mtext(text = paste(VE_legend, "(log scale)"), side = 1, line = 3.6, cex = 1.4) # with (log scale)
        mtext(text = paste(VE_legend), side = 1, line = 3.6, cex = 1.4)
        
        # Custom axis to display real values for low values of HF
        axis(side = 1, at = c(1,2,5,10,20,50,100), labels = c(0,1,5,10,20,50,100), cex.axis = 1.9, cex.lab = 1.9) 
        
      } else { # Otherwise, plot title and all axis
        
        plot(y = VAE_all_coverage, x = VE_all_coverage, log = "xy", 
             xlab = "", ylab = "",
             main = "",
             cex.axis=1.9, cex.lab=1.9, type="n")
        
        # Custom position for y-label
        # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
        mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
        # Custom position for x-label
        # mtext(text = paste(VE_legend, "(log scale)"), side = 1, line = 3.6, cex = 1.4) # with (log scale)
        mtext(text = paste(VE_legend), side = 1, line = 3.6, cex = 1.4)
      }
      
      # Plot points iteratively for each web_coverage
      for (i in 1:length(dataset_list)) { 
        aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve dataset associated with web coverage
        model <- eval(parse(text = paste0("model_",i))) # Retrieve the model
        points(exp(model$model[,grep(VE, names(model$model))]), # Retro-transformation of the VE tel such as Exp(X) - 1 = X' <=> X = ln(X' + 1)
               exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255),
               cex = 1.3)
      }
      
      # Plot predict iteratively for each web_coverage
      for (i in 1:length(dataset_list)) { # Add predict lines
        predict_table <- eval(parse(text = paste0("predict_table_",i))) # Load predict for model i
        lines(x = exp(predict_table$var_abs), # Retro-transformation of the VE tel such as Exp(X) - 1 = X' <=> X = ln(X' + 1)
              y = exp(predict_table$predict), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
              col = col_list[i], lwd = 3, lty = c(2,1)[signif_list][i]) # Significant = full ligne. Non-significant = dashed line
      }
      
      par(xpd = T)
      
      # Add legend line by line, with beta letter as an expression
      for (i in 1:length(web_coverage_legend_custom)) {
        legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
               col = col_list[i], text.col = "black", text.font = 2,
               x = "topleft", inset=c(0, -0.04 + i*0.045), 
               bty ="n", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
      }
      
      par(xpd = F)
      
      # dev.off()
    }    
    
  }
  
  legend(legend = panel_letters[k], x = "bottomleft", bty = "n",
         text.font = 2, cex = 2.2, inset=c(-0.03, 0))
  
  cat(paste("\n Plot n°", k, "on", length(index_plot_to_plot), "-", Sys.time(), "\n"))
  
  k <- k + 1 # Index to keep track when plotting a reduced number of plots
  
}

dev.off()

# Reset graphic parameters
par(mar = mar_old_settings)
par(mgp = mgp_old_settings)
par(oma = oma_old_settings)



##### Figure S3.2.1: Effects of climate on insect richness (A) and insect link density (B) #####

# A/ Effect of mean temperature on insect richness
# B/ Effect of mean temperature on insect link density


### Clear memory
rm(list = ls())
gc() # Force R to release memory it is no longer using

# Cut panels for figure
decoupe <- layout(mat = matrix(data = c(1,0,2), nrow = 1, ncol = 3, byrow = TRUE), # Numbers = order of plots ;
                  widths=c(7,0.2,7),  # Widths of columns
                  heights=c(6),       # Heights of lines
                  respect=TRUE)

layout.show(decoupe) # Visualiser la d?coupe de la zone de dessin

## Load files

load(file = "./Data/all_single_plots_summary_all_VE.RData")
load(file = "./Data/all_multiple_plots_summary_all_VE.RData")


# To associate the right legend and the right variable name in the dataset with the right response variable (VAE)
VAE_list <- c("C", "Li", "Lp", "S", "I", "P" )
VAE_legend_list <- c("Connectance", "Insect link density  [inter/sp.]", "Plant link density  [inter/sp.]", "Total richness  [sp.]", "Insect richness  [sp.]", "Plant richness  [sp.]")
VAE_dataset_name_list <- c("Connectance", "Li", "Lp", "sptot", "full_insects", "full_plants" )


# To associate the right legend and the right variable name in the dataset with the right explanatory variable (VE)
VE_short_name_list <-  c("S", "P", "I", "SE", "Time", "ATS", "HF", "Ptot", "Tmean", "Pvar", "Tvar", "Forest", "Taxo")
VE_legend_list <- c("Network size  [sp.]", "Partner pool  [plant sp.] ", "Partner pool  [insect sp.]", "Standardized sampling effort  [h/inter]", "Sampling effort  [h]", "Annual time span of sampling  [days]", "Human Influence Index  [%]", "Annual total precipitation  [mm]", "Mean temperature  [°C]", "Precipitation seasonality  [CV]", "Temperature seasonality  [°C]", "Forest cover  [%]", "Taxonomic resolution  [%]")
VE_axis_name_list <- c("sptot", "full_plants", "full_insects", "Sampling_effort", "Sampling_time", "Annual_time_span", "HF", "Tot_Rainfall_IPCC", "Mean_T_IPCC", "Rainfall_Seasonality_IPCC", "T_Seasonality_IPCC", "Forest", "taxo_full_sp_perc")


# Chose web coverage to plot on a single Multiple plot
web_coverage_list <- c("full", "dipt", "hymeno")
web_coverage_to_plot <- c("full", "dipt", "hymeno")
web_coverage_legend <- c("Full", "Diptera", "Hymenoptera")
web_coverage_legend_custom <- paste0(web_coverage_legend,":") # Add the double point for plot convenience

# Chose panel A: Effect of mean temperature on Insect richness 
panel_A <- which((all_multiple_plots_summary_all_VE$VAE == "I") & (all_multiple_plots_summary_all_VE$VE_short_name == "Tmean"))

# Chose panel B: Effect of mean temperature on Insect link density
panel_B <- which((all_multiple_plots_summary_all_VE$VAE == "Li") & (all_multiple_plots_summary_all_VE$VE_short_name == "Tmean"))


# Save plot index corresponding to each panel
index_plot_to_plot <- c(panel_A, panel_B)

# Define panel letters
panel_letters <- c("(a)", "(b)")

# Open plot
pdf(file = paste0("./SM_Final_figures/Figure_S3.2.1.pdf"), width = 14, height = 6)

# Cut panels
layout(mat = matrix(data = c(1,0,2), nrow = 1, ncol = 3, byrow = TRUE), # Numbers = order of plots ;
       widths=c(7,0.2,7),  # Widths of columns
       heights=c(6),       # Heights of lines
       respect=TRUE)

# Set margins
mar_old_settings <- par()$mar
par(mar = c(6.6, 5.1, 2.1, 2.1))

# Set outer margins
oma_old_settings <- par()$oma
par(oma = c(0, 1, 0, 0))

# Set position of tick labels
mgp_old_settings <- par()$mgp
par(mgp = c(3, 1.5, 0))

# Boucle
k <- 1 ; par(xpd = F)
for (j in index_plot_to_plot) {
  
  web_coverage_available <- unlist(strsplit(x = all_multiple_plots_summary_all_VE$web_coverage[j], split = "-"))
  
  # Chack if all web_coverage are available for this multiple plot
  if (!all(web_coverage_to_plot %in% web_coverage_available)) { # If not, warn with a message
    
    cat(paste("Not all web coverages are available to do a Multliple plot for", all_multiple_plots_summary_all_VE$VAE[j], "~", all_multiple_plots_summary_all_VE$VE_short_name[j] , "in model", all_multiple_plots_summary_all_VE$model_type[j], all_multiple_plots_summary_all_VE$polar[j], all_multiple_plots_summary_all_VE$taxo[j], all_multiple_plots_summary_all_VE$forest[j], "= Multiple plot n°", j, "\n"))
    
  } else {  # If available, plot !
    
    # Choix de la VAE
    VAE <- all_multiple_plots_summary_all_VE$VAE[j]
    VAE_legend <- VAE_legend_list[which(VAE_list == VAE)]
    
    # Choix de la VE
    VE <- VE_axis_name <- all_single_plots_summary_all_VE$VE[j]
    VE_short_name <- all_single_plots_summary_all_VE$VE_short_name[j]
    VE_legend <- VE_legend_list[which(VE_short_name_list == VE_short_name)]
    ln_transfo <- "no"
    
    # Special case of VE plotted with log transformation
    if (VE_short_name %in% c("S", "P", "I", "SE", "Time", "ATS")) { 
      VE_axis_name <- VE_axis_name_list[which(VE_short_name_list == VE_short_name)]
      ln_transfo <- "log"
    }
    
    # Special case of VE plotted with log(X+1) transformation
    if (VE_short_name == "HF") { 
      VE_axis_name <- VE_axis_name_list[which(VE_short_name_list == VE_short_name)]
      ln_transfo <- "log1p"
    }
    
    # Extract information on model 
    model_type <- all_multiple_plots_summary_all_VE$model_type[j]
    polar <- all_multiple_plots_summary_all_VE$polar[j]
    
    # Load models for each web coverage and save dataset_list
    dataset_list <- NA
    for (i in 1:length(web_coverage_to_plot)) {
      model_name <- paste0(VAE, "_", web_coverage_to_plot[i], "_", model_type, "_", polar)
      
      eval(call("<-", as.name(paste0("predict_table_",i)), readRDS(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Predicts_for_Graphs/predict_table_",model_name,"_for_",VE_short_name,".RData"))))
      eval(call("<-", as.name(paste0("model_",i)), readRDS(file = paste0("./Models_outputs/",VAE,"/",web_coverage_to_plot[i],"/",model_name,"/",model_name,"_bestmod_with_", VE_short_name, ".RData"))))
      
      dataset_list[i] <- paste0(web_coverage_to_plot[i], "_", model_type, "_", polar)
    }  
    
    # Use real limits of X and Y, all web_coverage confounded, to plot axis
    VAE_all_coverage <- as.numeric()
    for (i in 1:length(dataset_list)) { 
      aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve VAE (Y-axis) for all web_coverage
      VAE_all_coverage <- c(VAE_all_coverage, eval(parse(text = paste0("aggreg.webs$", VAE_dataset_name_list[which(VAE_list == VAE)]))))
    }
    
    VE_all_coverage <- as.numeric()
    for (i in 1:length(dataset_list)) { 
      aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve VE (X-axis) for all_coverage
      VE_all_coverage <- c(VE_all_coverage, eval(parse(text = paste0("aggreg.webs$",VE_axis_name))))
    }
    
    # Chose colors to associate with each model/web coverage
    col_list <- c("black", "dodgerblue3", "red") 
    
    # Extract significativit? of the slope from p-value of the single models stored in all_single_plots_summary_all_VE
    web_coverage_index <- NA
    for (i in 1:length(web_coverage_to_plot)) {
      web_coverage_index[i] <- which(web_coverage_to_plot[i] == web_coverage_list) # Retrieve index of plotted web coverages among all possible web coverages
    }
    ID_plot_list <- unlist(strsplit(x = all_multiple_plots_summary_all_VE$ID_plot[j], split = "-")) # Retrieve index of single plots
    ID_plot_used <- as.numeric(ID_plot_list[web_coverage_index])
    
    beta_value_list <- round(all_single_plots_summary_all_VE$Beta_coef[ID_plot_used], 3) # Extract beta-coefs
    beta_value_list_custom <- paste0(beta_value_list,",") # Add the comma to help to plot the legend
    
    p_value_list <- round(all_single_plots_summary_all_VE$p_value[ID_plot_used], 3) # Extract p-values
    
    signif_list <- as.numeric(p_value_list < 0.05)+1 # Code significance of slopes. 2 = significant, 1 = non-significant
    
    # Create label for p-value
    p_value_custom_list <- paste("p = ", p_value_list)
    p_value_custom_list[which(p_value_list < 0.001)] <- "p < 0.001"
    
    # # Create an output folder if needed
    # if (!dir.exists(paths = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar))) { 
    #   dir.create(path = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar), recursive = T) 
    # }   
    
    # Plot without log on X-axis
    # pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Multiple_plot_",VAE,"_",model_type,"_",polar,"_for_",VE_short_name,"_in_",paste(web_coverage_to_plot, collapse = "_"),".pdf"), width = 7, height = 6)
    
    # Plot axis and title
    # par(mfcol=c(1,1))
    
    if (ln_transfo == "no") {
      
      plot(y = VAE_all_coverage, x = VE_all_coverage, log = "y", xlab = "", ylab = "",
           main = "",
           cex.axis=1.9, cex.lab=1.9, type="n")
      
      # Custom position for y-label
      # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
      mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
      # Custom position for x-label
      mtext(text = VE_legend, side = 1, line = 3.6, cex = 1.4)
      
      for (i in 1:length(dataset_list)) {  # Plot each web_coverge iteratively
        
        aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve dataset associated with web coverage
        model <- eval(parse(text = paste0("model_",i))) # Retrieve model
        
        points(x = model$model[,grep(VE, names(model$model))], # No retro-transformation of the VE
               y = exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255),
               cex = 1.3)
      }
      
      for (i in 1:length(dataset_list)) { # Add predict lines
        predict_table <- eval(parse(text = paste0("predict_table_",i))) # Load predict for model i
        
        lines(x = predict_table$var_abs, # No retro-transformation of the VE
              y = exp(predict_table$predict), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
              col = col_list[i], lwd = 3, lty = c(2,1)[signif_list][i]) # Significant = full ligne. Non-significant = dashed line
      }
      
      # Add legend line by line, with beta letter as an expression
      for (i in 1:length(web_coverage_legend_custom)) {
        legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
               col = col_list[i], text.col = "black", text.font = 2,
               x = "topleft", inset=c(0.01, -0.033 + i*0.045), 
               bty ="o", box.col = "white", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
      }
      
    }
    
    
    
    # dev.off()  
    
    # Plot with log on the X-axis
    
    if (ln_transfo %in% c("log", "log1p")) { 
      
      # pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Multiple_plot_",VAE,"_",model_type,"_",polar,"_for_",VE_short_name,"_in_",paste(web_coverage_to_plot, collapse = "_"),"_log.pdf"), width = 7, height = 6)
      
      # par(mfcol=c(1,1))
      
      if (ln_transfo == "log1p") { # Need to specify not to plot the X-axis (xaxt = "n") that will be customed for ln(HF+1)
        
        plot(y = VAE_all_coverage, x = VE_all_coverage + 1, log = "xy", # Need to add +1 to the original value because model with log(X+1)
             xlab = "", ylab = "",
             main = "",
             xaxt = "n", cex.axis=1.9, cex.lab=1.9, type="n")
        
        # Custom position for y-label
        # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
        mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
        # Custom position for x-label
        # mtext(text = paste(VE_legend, "(log scale)"), side = 1, line = 3.6, cex = 1.4) # with (log scale)
        mtext(text = paste(VE_legend), side = 1, line = 3.6, cex = 1.4)
        
        # Custom axis to display real values for low values of HF
        axis(side = 1, at = c(1,2,5,10,20,50,100), labels = c(0,1,5,10,20,50,100), cex.axis = 1.9, cex.lab = 1.9) 
        
      } else { # Otherwise, plot title and all axis
        
        plot(y = VAE_all_coverage, x = VE_all_coverage, log = "xy", 
             xlab = "", ylab = "",
             main = "",
             cex.axis=1.9, cex.lab=1.9, type="n")
        
        # Custom position for y-label
        # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
        mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
        # Custom position for x-label
        # mtext(text = paste(VE_legend, "(log scale)"), side = 1, line = 3.6, cex = 1.4) # with (log scale)
        mtext(text = paste(VE_legend), side = 1, line = 3.6, cex = 1.4)
      }
      
      # Plot points iteratively for each web_coverage
      for (i in 1:length(dataset_list)) { 
        aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve dataset associated with web coverage
        model <- eval(parse(text = paste0("model_",i))) # Retrieve the model
        points(exp(model$model[,grep(VE, names(model$model))]), # Retro-transformation of the VE tel such as Exp(X) - 1 = X' <=> X = ln(X' + 1)
               exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255),
               cex = 1.3)
      }
      
      # Plot predict iteratively for each web_coverage
      for (i in 1:length(dataset_list)) { # Add predict lines
        predict_table <- eval(parse(text = paste0("predict_table_",i))) # Load predict for model i
        lines(x = exp(predict_table$var_abs), # Retro-transformation of the VE tel such as Exp(X) - 1 = X' <=> X = ln(X' + 1)
              y = exp(predict_table$predict), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
              col = col_list[i], lwd = 3, lty = c(2,1)[signif_list][i]) # Significant = full ligne. Non-significant = dashed line
      }
      
      # Add legend line by line, with beta letter as an expression
      for (i in 1:length(web_coverage_legend_custom)) {
        legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
               col = col_list[i], text.col = "black", text.font = 2,
               x = "topleft", inset=c(0.01, -0.033 + i*0.045), 
               bty ="o", box.col = "white", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
      }
      
      # dev.off()
    }    
    
  }
  
  legend(legend = panel_letters[k], x = "bottomleft", bty = "n",
         text.font = 2, cex = 2.2, inset=c(-0.03, 0))
  
  cat(paste("\n Plot n°", k, "on", length(index_plot_to_plot), "-", Sys.time(), "\n"))
  
  k <- k + 1 # Index to keep track when plotting a reduced number of plots
  
}

dev.off()

# Reset graphic parameters
par(mar = mar_old_settings)
par(mgp = mgp_old_settings)
par(oma = oma_old_settings)



##### Figure S3.2.2: Effects of climate on plant richness (A) and plant link density (B) #####

# A/ Effect of mean temperature on plant richness
# B/ Effect of mean temperature on plant link density


### Clear memory
rm(list = ls())
gc() # Force R to release memory it is no longer using

# Cut panels for figure
decoupe <- layout(mat = matrix(data = c(1,0,2), nrow = 1, ncol = 3, byrow = TRUE), # Numbers = order of plots ;
                  widths=c(7,0.2,7),  # Widths of columns
                  heights=c(6),       # Heights of lines
                  respect=TRUE)

layout.show(decoupe) # Visualiser la d?coupe de la zone de dessin

## Load files

load(file = "./Data/all_single_plots_summary_all_VE.RData")
load(file = "./Data/all_multiple_plots_summary_all_VE.RData")


# To associate the right legend and the right variable name in the dataset with the right response variable (VAE)
VAE_list <- c("C", "Li", "Lp", "S", "I", "P" )
VAE_legend_list <- c("Connectance", "Insect link density  [inter/sp.]", "Plant link density  [inter/sp.]", "Total richness  [sp.]", "Insect richness  [sp.]", "Plant richness  [sp.]")
VAE_dataset_name_list <- c("Connectance", "Li", "Lp", "sptot", "full_insects", "full_plants" )


# To associate the right legend and the right variable name in the dataset with the right explanatory variable (VE)
VE_short_name_list <-  c("S", "P", "I", "SE", "Time", "ATS", "HF", "Ptot", "Tmean", "Pvar", "Tvar", "Forest", "Taxo")
VE_legend_list <- c("Network size  [sp.]", "Partner pool  [plant sp.] ", "Partner pool  [insect sp.]", "Standardized sampling effort  [h/inter]", "Sampling effort  [h]", "Annual time span of sampling  [days]", "Human Influence Index  [%]", "Annual total precipitation  [mm]", "Mean temperature  [°C]", "Precipitation seasonality  [CV]", "Temperature seasonality  [°C]", "Forest cover  [%]", "Taxonomic resolution  [%]")
VE_axis_name_list <- c("sptot", "full_plants", "full_insects", "Sampling_effort", "Sampling_time", "Annual_time_span", "HF", "Tot_Rainfall_IPCC", "Mean_T_IPCC", "Rainfall_Seasonality_IPCC", "T_Seasonality_IPCC", "Forest", "taxo_full_sp_perc")


# Chose web coverage to plot on a single Multiple plot
web_coverage_list <- c("full", "dipt", "hymeno")
web_coverage_to_plot <- c("full", "dipt", "hymeno")
web_coverage_legend <- c("Full", "Diptera", "Hymenoptera")
web_coverage_legend_custom <- paste0(web_coverage_legend,":") # Add the double point for plot convenience

# Chose panel A: Effect of mean temperature on Plant richness 
panel_A <- which((all_multiple_plots_summary_all_VE$VAE == "P") & (all_multiple_plots_summary_all_VE$VE_short_name == "Tmean"))

# Chose panel B: Effect of mean temperature on Plant link density
panel_B <- which((all_multiple_plots_summary_all_VE$VAE == "Lp") & (all_multiple_plots_summary_all_VE$VE_short_name == "Tmean"))


# Save plot index corresponding to each panel
index_plot_to_plot <- c(panel_A, panel_B)

# Define panel letters
panel_letters <- c("(a)", "(b)")

# Open plot
pdf(file = paste0("./SM_Final_figures/Figure_S3.2.2.pdf"), width = 14, height = 6)

# Cut panels
layout(mat = matrix(data = c(1,0,2), nrow = 1, ncol = 3, byrow = TRUE), # Numbers = order of plots ;
       widths=c(7,0.2,7),  # Widths of columns
       heights=c(6),       # Heights of lines
       respect=TRUE)

# Set margins
mar_old_settings <- par()$mar
par(mar = c(6.6, 5.1, 2.1, 2.1))

# Set outer margins
oma_old_settings <- par()$oma
par(oma = c(0, 1, 0, 0))

# Set position of tick labels
mgp_old_settings <- par()$mgp
par(mgp = c(3, 1.5, 0))

# Boucle
k <- 1 ; par(xpd = F)
for (j in index_plot_to_plot) {
  
  web_coverage_available <- unlist(strsplit(x = all_multiple_plots_summary_all_VE$web_coverage[j], split = "-"))
  
  # Chack if all web_coverage are available for this multiple plot
  if (!all(web_coverage_to_plot %in% web_coverage_available)) { # If not, warn with a message
    
    cat(paste("Not all web coverages are available to do a Multliple plot for", all_multiple_plots_summary_all_VE$VAE[j], "~", all_multiple_plots_summary_all_VE$VE_short_name[j] , "in model", all_multiple_plots_summary_all_VE$model_type[j], all_multiple_plots_summary_all_VE$polar[j], all_multiple_plots_summary_all_VE$taxo[j], all_multiple_plots_summary_all_VE$forest[j], "= Multiple plot n°", j, "\n"))
    
  } else {  # If available, plot !
    
    # Choix de la VAE
    VAE <- all_multiple_plots_summary_all_VE$VAE[j]
    VAE_legend <- VAE_legend_list[which(VAE_list == VAE)]
    
    # Choix de la VE
    VE <- VE_axis_name <- all_single_plots_summary_all_VE$VE[j]
    VE_short_name <- all_single_plots_summary_all_VE$VE_short_name[j]
    VE_legend <- VE_legend_list[which(VE_short_name_list == VE_short_name)]
    ln_transfo <- "no"
    
    # Special case of VE plotted with log transformation
    if (VE_short_name %in% c("S", "P", "I", "SE", "Time", "ATS")) { 
      VE_axis_name <- VE_axis_name_list[which(VE_short_name_list == VE_short_name)]
      ln_transfo <- "log"
    }
    
    # Special case of VE plotted with log(X+1) transformation
    if (VE_short_name == "HF") { 
      VE_axis_name <- VE_axis_name_list[which(VE_short_name_list == VE_short_name)]
      ln_transfo <- "log1p"
    }
    
    # Extract information on model 
    model_type <- all_multiple_plots_summary_all_VE$model_type[j]
    polar <- all_multiple_plots_summary_all_VE$polar[j]
    
    # Load models for each web coverage and save dataset_list
    dataset_list <- NA
    for (i in 1:length(web_coverage_to_plot)) {
      model_name <- paste0(VAE, "_", web_coverage_to_plot[i], "_", model_type, "_", polar)
      
      eval(call("<-", as.name(paste0("predict_table_",i)), readRDS(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Predicts_for_Graphs/predict_table_",model_name,"_for_",VE_short_name,".RData"))))
      eval(call("<-", as.name(paste0("model_",i)), readRDS(file = paste0("./Models_outputs/",VAE,"/",web_coverage_to_plot[i],"/",model_name,"/",model_name,"_bestmod_with_", VE_short_name, ".RData"))))
      
      dataset_list[i] <- paste0(web_coverage_to_plot[i], "_", model_type, "_", polar)
    }  
    
    # Use real limits of X and Y, all web_coverage confounded, to plot axis
    VAE_all_coverage <- as.numeric()
    for (i in 1:length(dataset_list)) { 
      aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve VAE (Y-axis) for all web_coverage
      VAE_all_coverage <- c(VAE_all_coverage, eval(parse(text = paste0("aggreg.webs$", VAE_dataset_name_list[which(VAE_list == VAE)]))))
    }
    
    VE_all_coverage <- as.numeric()
    for (i in 1:length(dataset_list)) { 
      aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve VE (X-axis) for all_coverage
      VE_all_coverage <- c(VE_all_coverage, eval(parse(text = paste0("aggreg.webs$",VE_axis_name))))
    }
    
    # Chose colors to associate with each model/web coverage
    col_list <- c("black", "dodgerblue3", "red") 
    
    # Extract significativit? of the slope from p-value of the single models stored in all_single_plots_summary_all_VE
    web_coverage_index <- NA
    for (i in 1:length(web_coverage_to_plot)) {
      web_coverage_index[i] <- which(web_coverage_to_plot[i] == web_coverage_list) # Retrieve index of plotted web coverages among all possible web coverages
    }
    ID_plot_list <- unlist(strsplit(x = all_multiple_plots_summary_all_VE$ID_plot[j], split = "-")) # Retrieve index of single plots
    ID_plot_used <- as.numeric(ID_plot_list[web_coverage_index])
    
    beta_value_list <- round(all_single_plots_summary_all_VE$Beta_coef[ID_plot_used], 3) # Extract beta-coefs
    beta_value_list_custom <- paste0(beta_value_list,",") # Add the comma to help to plot the legend
    
    p_value_list <- round(all_single_plots_summary_all_VE$p_value[ID_plot_used], 3) # Extract p-values
    
    signif_list <- as.numeric(p_value_list < 0.05)+1 # Code significance of slopes. 2 = significant, 1 = non-significant
    
    # Create label for p-value
    p_value_custom_list <- paste("p = ", p_value_list)
    p_value_custom_list[which(p_value_list < 0.001)] <- "p < 0.001"
    
    # # Create an output folder if needed
    # if (!dir.exists(paths = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar))) { 
    #   dir.create(path = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar), recursive = T) 
    # }   
    
    # Plot without log on X-axis
    # pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Multiple_plot_",VAE,"_",model_type,"_",polar,"_for_",VE_short_name,"_in_",paste(web_coverage_to_plot, collapse = "_"),".pdf"), width = 7, height = 6)
    
    # Plot axis and title
    # par(mfcol=c(1,1))
    
    if (ln_transfo == "no") {
      
      plot(y = VAE_all_coverage, x = VE_all_coverage, log = "y", xlab = "", ylab = "",
           main = "",
           cex.axis=1.9, cex.lab=1.9, type="n")
      
      # Custom position for y-label
      # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
      mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
      # Custom position for x-label
      mtext(text = VE_legend, side = 1, line = 3.6, cex = 1.4)
      
      for (i in 1:length(dataset_list)) {  # Plot each web_coverge iteratively
        
        aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve dataset associated with web coverage
        model <- eval(parse(text = paste0("model_",i))) # Retrieve model
        
        points(x = model$model[,grep(VE, names(model$model))], # No retro-transformation of the VE
               y = exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255),
               cex = 1.3)
      }
      
      for (i in 1:length(dataset_list)) { # Add predict lines
        predict_table <- eval(parse(text = paste0("predict_table_",i))) # Load predict for model i
        
        lines(x = predict_table$var_abs, # No retro-transformation of the VE
              y = exp(predict_table$predict), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
              col = col_list[i], lwd = 3, lty = c(2,1)[signif_list][i]) # Significant = full ligne. Non-significant = dashed line
      }
      
      # Add legend line by line, with beta letter as an expression
      for (i in 1:length(web_coverage_legend_custom)) {
        legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
               col = col_list[i], text.col = "black", text.font = 2,
               x = "topleft", inset=c(0.01, -0.033 + i*0.045), 
               bty ="o", box.col = "white", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
      }
      
    }
    
    
    
    # dev.off()  
    
    # Plot with log on the X-axis
    
    if (ln_transfo %in% c("log", "log1p")) { 
      
      # pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Multiple_plot_",VAE,"_",model_type,"_",polar,"_for_",VE_short_name,"_in_",paste(web_coverage_to_plot, collapse = "_"),"_log.pdf"), width = 7, height = 6)
      
      # par(mfcol=c(1,1))
      
      if (ln_transfo == "log1p") { # Need to specify not to plot the X-axis (xaxt = "n") that will be customed for ln(HF+1)
        
        plot(y = VAE_all_coverage, x = VE_all_coverage + 1, log = "xy", # Need to add +1 to the original value because model with log(X+1)
             xlab = "", ylab = "",
             main = "",
             xaxt = "n", cex.axis=1.9, cex.lab=1.9, type="n")
        
        # Custom position for y-label
        # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
        mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
        # Custom position for x-label
        # mtext(text = paste(VE_legend, "(log scale)"), side = 1, line = 3.6, cex = 1.4) # with (log scale)
        mtext(text = paste(VE_legend), side = 1, line = 3.6, cex = 1.4)
        
        # Custom axis to display real values for low values of HF
        axis(side = 1, at = c(1,2,5,10,20,50,100), labels = c(0,1,5,10,20,50,100), cex.axis = 1.9, cex.lab = 1.9) 
        
      } else { # Otherwise, plot title and all axis
        
        plot(y = VAE_all_coverage, x = VE_all_coverage, log = "xy", 
             xlab = "", ylab = "",
             main = "",
             cex.axis=1.9, cex.lab=1.9, type="n")
        
        # Custom position for y-label
        # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
        mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
        # Custom position for x-label
        # mtext(text = paste(VE_legend, "(log scale)"), side = 1, line = 3.6, cex = 1.4) # with (log scale)
        mtext(text = paste(VE_legend), side = 1, line = 3.6, cex = 1.4)
      }
      
      # Plot points iteratively for each web_coverage
      for (i in 1:length(dataset_list)) { 
        aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve dataset associated with web coverage
        model <- eval(parse(text = paste0("model_",i))) # Retrieve the model
        points(exp(model$model[,grep(VE, names(model$model))]), # Retro-transformation of the VE tel such as Exp(X) - 1 = X' <=> X = ln(X' + 1)
               exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255),
               cex = 1.3)
      }
      
      # Plot predict iteratively for each web_coverage
      for (i in 1:length(dataset_list)) { # Add predict lines
        predict_table <- eval(parse(text = paste0("predict_table_",i))) # Load predict for model i
        lines(x = exp(predict_table$var_abs), # Retro-transformation of the VE tel such as Exp(X) - 1 = X' <=> X = ln(X' + 1)
              y = exp(predict_table$predict), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
              col = col_list[i], lwd = 3, lty = c(2,1)[signif_list][i]) # Significant = full ligne. Non-significant = dashed line
      }
      
      # Add legend line by line, with beta letter as an expression
      for (i in 1:length(web_coverage_legend_custom)) {
        legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
               col = col_list[i], text.col = "black", text.font = 2,
               x = "topleft", inset=c(0.01, -0.033 + i*0.045), 
               bty ="o", box.col = "white", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
      }
      
      # dev.off()
    }    
    
  }
  
  legend(legend = panel_letters[k], x = "bottomleft", bty = "n",
         text.font = 2, cex = 2.2, inset=c(-0.03, 0))
  
  cat(paste("\n Plot n°", k, "on", length(index_plot_to_plot), "-", Sys.time(), "\n"))
  
  k <- k + 1 # Index to keep track when plotting a reduced number of plots
  
}

dev.off()

# Reset graphic parameters
par(mar = mar_old_settings)
par(mgp = mgp_old_settings)
par(oma = oma_old_settings)



##### Figure S3.3.1: Effects of sampling characteristics on insect richness (A & C) and insect link density (B & D) #####

# A/ Effect of sampling effort on insect richness
# B/ Effect of standardized sampling effort on insect link density
# C/ Effect of taxonomic resolution on insect richness
# D/ Effect of annual time span of sampling on insect link density


### Clear memory
rm(list = ls())
gc() # Force R to release memory it is no longer using

# Cut panels for figure
decoupe <- layout(mat = matrix(data = c(1,0,2,3,0,4), nrow = 2, ncol = 3, byrow = TRUE), # Numbers = order of plots ;
                  widths=c(7,0.2,7),  # Widths of columns
                  heights=c(6, 6),       # Heights of lines
                  respect=TRUE)

layout.show(decoupe) # Visualiser la d?coupe de la zone de dessin

## Load files

load(file = "./Data/all_single_plots_summary_all_VE.RData")
load(file = "./Data/all_multiple_plots_summary_all_VE.RData")


# To associate the right legend and the right variable name in the dataset with the right response variable (VAE)
VAE_list <- c("C", "Li", "Lp", "S", "I", "P" )
VAE_legend_list <- c("Connectance", "Insect link density  [inter/sp.]", "Plant link density  [inter/sp.]", "Total richness  [sp.]", "Insect richness  [sp.]", "Plant richness  [sp.]")
VAE_dataset_name_list <- c("Connectance", "Li", "Lp", "sptot", "full_insects", "full_plants" )


# To associate the right legend and the right variable name in the dataset with the right explanatory variable (VE)
VE_short_name_list <-  c("S", "P", "I", "SE", "Time", "ATS", "HF", "Ptot", "Tmean", "Pvar", "Tvar", "Forest", "Taxo")
VE_legend_list <- c("Network size  [sp.]", "Partner pool  [plant sp.] ", "Partner pool  [insect sp.]", "Standardized sampling effort  [h/inter]", "Sampling effort  [h]", "Annual time span of sampling  [days]", "Human Influence Index  [%]", "Annual total precipitation  [mm]", "Mean temperature  [°C]", "Precipitation seasonality  [CV]", "Temperature seasonality  [°C]", "Forest cover  [%]", "Taxonomic resolution  [%]")
VE_axis_name_list <- c("sptot", "full_plants", "full_insects", "Sampling_effort", "Sampling_time", "Annual_time_span", "HF", "Tot_Rainfall_IPCC", "Mean_T_IPCC", "Rainfall_Seasonality_IPCC", "T_Seasonality_IPCC", "Forest", "taxo_full_sp_perc")


# Chose web coverage to plot on a single Multiple plot
web_coverage_list <- c("full", "dipt", "hymeno")
web_coverage_to_plot <- c("full", "dipt", "hymeno")
web_coverage_legend <- c("Full", "Diptera", "Hymenoptera")
web_coverage_legend_custom <- paste0(web_coverage_legend,":") # Add the double point for plot convenience

# Chose panel A: Effect of sampling effort on insect richness
panel_A <- which((all_multiple_plots_summary_all_VE$VAE == "I") & (all_multiple_plots_summary_all_VE$VE_short_name == "Time"))

# Chose panel B: Effect of standardized sampling effort on insect link density
panel_B <- which((all_multiple_plots_summary_all_VE$VAE == "Li") & (all_multiple_plots_summary_all_VE$VE_short_name == "SE"))

# Chose panel C: Effect of taxonomic resolution on insect richness
panel_C <- which((all_multiple_plots_summary_all_VE$VAE == "I") & (all_multiple_plots_summary_all_VE$VE_short_name == "Taxo"))

# Chose panel D: Effect of annual time span of sampling on insect link density
panel_D <- which((all_multiple_plots_summary_all_VE$VAE == "Li") & (all_multiple_plots_summary_all_VE$VE_short_name == "ATS"))


# Save plot index corresponding to each panel
index_plot_to_plot <- c(panel_A, panel_B, panel_C, panel_D)

# Define panel letters
panel_letters <- c("(a)","(b)","(c)","(d)")

# Open plot
pdf(file = paste0("./SM_Final_figures/Figure_S3.3.1.pdf"), width = 14, height = 12)

# Cut panels
layout(mat = matrix(data = c(1,0,2,3,0,4), nrow = 2, ncol = 3, byrow = TRUE), # Numbers = order of plots ;
       widths=c(7,0.2,7),     # Widths of columns
       heights=c(6, 6),       # Heights of lines
       respect=TRUE)

# Set margins
mar_old_settings <- par()$mar
par(mar = c(5.1, 5.1, 2.1, 2.1))

# Set outer margins
oma_old_settings <- par()$oma
par(oma = c(0, 1, 0, 0))

# Set position of tick labels
mgp_old_settings <- par()$mgp
par(mgp = c(3, 1.5, 0))

# Boucle
k <- 1 ; par(xpd = F)
for (j in index_plot_to_plot) {
  
  web_coverage_available <- unlist(strsplit(x = all_multiple_plots_summary_all_VE$web_coverage[j], split = "-"))
  
  # Chack if all web_coverage are available for this multiple plot
  if (!all(web_coverage_to_plot %in% web_coverage_available)) { # If not, warn with a message
    
    cat(paste("Not all web coverages are available to do a Multliple plot for", all_multiple_plots_summary_all_VE$VAE[j], "~", all_multiple_plots_summary_all_VE$VE_short_name[j] , "in model", all_multiple_plots_summary_all_VE$model_type[j], all_multiple_plots_summary_all_VE$polar[j], all_multiple_plots_summary_all_VE$taxo[j], all_multiple_plots_summary_all_VE$forest[j], "= Multiple plot n°", j, "\n"))
    
  } else {  # If available, plot !
    
    # Choix de la VAE
    VAE <- all_multiple_plots_summary_all_VE$VAE[j]
    VAE_legend <- VAE_legend_list[which(VAE_list == VAE)]
    
    # Choix de la VE
    VE <- VE_axis_name <- all_single_plots_summary_all_VE$VE[j]
    VE_short_name <- all_single_plots_summary_all_VE$VE_short_name[j]
    VE_legend <- VE_legend_list[which(VE_short_name_list == VE_short_name)]
    ln_transfo <- "no"
    
    # Special case of VE plotted with log transformation
    if (VE_short_name %in% c("S", "P", "I", "SE", "Time", "ATS")) { 
      VE_axis_name <- VE_axis_name_list[which(VE_short_name_list == VE_short_name)]
      ln_transfo <- "log"
    }
    
    # Special case of VE plotted with log(X+1) transformation
    if (VE_short_name == "HF") { 
      VE_axis_name <- VE_axis_name_list[which(VE_short_name_list == VE_short_name)]
      ln_transfo <- "log1p"
    }
    
    # Extract information on model 
    model_type <- all_multiple_plots_summary_all_VE$model_type[j]
    polar <- all_multiple_plots_summary_all_VE$polar[j]
    
    # Load models for each web coverage and save dataset_list
    dataset_list <- NA
    for (i in 1:length(web_coverage_to_plot)) {
      model_name <- paste0(VAE, "_", web_coverage_to_plot[i], "_", model_type, "_", polar)
      
      eval(call("<-", as.name(paste0("predict_table_",i)), readRDS(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Predicts_for_Graphs/predict_table_",model_name,"_for_",VE_short_name,".RData"))))
      eval(call("<-", as.name(paste0("model_",i)), readRDS(file = paste0("./Models_outputs/",VAE,"/",web_coverage_to_plot[i],"/",model_name,"/",model_name,"_bestmod_with_", VE_short_name, ".RData"))))
      
      dataset_list[i] <- paste0(web_coverage_to_plot[i], "_", model_type, "_", polar)
    }  
    
    # Use real limits of X and Y, all web_coverage confounded, to plot axis
    VAE_all_coverage <- as.numeric()
    for (i in 1:length(dataset_list)) { 
      aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve VAE (Y-axis) for all web_coverage
      VAE_all_coverage <- c(VAE_all_coverage, eval(parse(text = paste0("aggreg.webs$", VAE_dataset_name_list[which(VAE_list == VAE)]))))
    }
    
    VE_all_coverage <- as.numeric()
    for (i in 1:length(dataset_list)) { 
      aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve VE (X-axis) for all_coverage
      VE_all_coverage <- c(VE_all_coverage, eval(parse(text = paste0("aggreg.webs$",VE_axis_name))))
    }
    
    # Chose colors to associate with each model/web coverage
    col_list <- c("black", "dodgerblue3", "red") 
    
    # Extract significativit? of the slope from p-value of the single models stored in all_single_plots_summary_all_VE
    web_coverage_index <- NA
    for (i in 1:length(web_coverage_to_plot)) {
      web_coverage_index[i] <- which(web_coverage_to_plot[i] == web_coverage_list) # Retrieve index of plotted web coverages among all possible web coverages
    }
    ID_plot_list <- unlist(strsplit(x = all_multiple_plots_summary_all_VE$ID_plot[j], split = "-")) # Retrieve index of single plots
    ID_plot_used <- as.numeric(ID_plot_list[web_coverage_index])
    
    beta_value_list <- round(all_single_plots_summary_all_VE$Beta_coef[ID_plot_used], 3) # Extract beta-coefs
    beta_value_list_custom <- paste0(beta_value_list,",") # Add the comma to help to plot the legend
    
    p_value_list <- round(all_single_plots_summary_all_VE$p_value[ID_plot_used], 3) # Extract p-values
    
    signif_list <- as.numeric(p_value_list < 0.05)+1 # Code significance of slopes. 2 = significant, 1 = non-significant
    
    # Create label for p-value
    p_value_custom_list <- paste("p = ", p_value_list)
    p_value_custom_list[which(p_value_list < 0.001)] <- "p < 0.001"
    
    # # Create an output folder if needed
    # if (!dir.exists(paths = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar))) { 
    #   dir.create(path = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar), recursive = T) 
    # }   
    
    # Plot without log on X-axis
    # pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Multiple_plot_",VAE,"_",model_type,"_",polar,"_for_",VE_short_name,"_in_",paste(web_coverage_to_plot, collapse = "_"),".pdf"), width = 7, height = 6)
    
    # Plot axis and title
    # par(mfcol=c(1,1))
    
    if (ln_transfo == "no") {
      
      plot(y = VAE_all_coverage, x = VE_all_coverage, log = "y", xlab = "", ylab = "",
           main = "",
           cex.axis=1.9, cex.lab=1.9, type="n")
      
      # Custom position for y-label
      # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
      mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
      # Custom position for x-label
      mtext(text = VE_legend, side = 1, line = 3.6, cex = 1.4)
      
      for (i in 1:length(dataset_list)) {  # Plot each web_coverge iteratively
        
        aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve dataset associated with web coverage
        model <- eval(parse(text = paste0("model_",i))) # Retrieve model
        
        points(x = model$model[,grep(VE, names(model$model))], # No retro-transformation of the VE
               y = exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255),
               cex = 1.3)
      }
      
      for (i in 1:length(dataset_list)) { # Add predict lines
        predict_table <- eval(parse(text = paste0("predict_table_",i))) # Load predict for model i
        
        lines(x = predict_table$var_abs, # No retro-transformation of the VE
              y = exp(predict_table$predict), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
              col = col_list[i], lwd = 3, lty = c(2,1)[signif_list][i]) # Significant = full ligne. Non-significant = dashed line
      }
      
      # Add legend line by line, with beta letter as an expression
      if(k %in% c(1, 2)) { # For A and B
        
        for (i in 1:length(web_coverage_legend_custom)) {
          legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
                 col = col_list[i], text.col = "black", text.font = 2,
                 x = "topleft", inset=c(0.01, -0.04 + i*0.045), 
                 bty ="n", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
        }
        
      } else {             # For C and D
        
        for (i in 1:length(web_coverage_legend_custom)) {
          legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
                 col = col_list[i], text.col = "black", text.font = 2,
                 x = "bottomleft", inset=c(0.01, 0.14 - i*0.045), 
                 bty = "o", box.col = "white", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
        }
        
      }
      
    }
    
    
    
    # dev.off()  
    
    # Plot with log on the X-axis
    
    if (ln_transfo %in% c("log", "log1p")) { 
      
      # pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Multiple_plot_",VAE,"_",model_type,"_",polar,"_for_",VE_short_name,"_in_",paste(web_coverage_to_plot, collapse = "_"),"_log.pdf"), width = 7, height = 6)
      
      # par(mfcol=c(1,1))
      
      if (ln_transfo == "log1p") { # Need to specify not to plot the X-axis (xaxt = "n") that will be customed for ln(HF+1)
        
        plot(y = VAE_all_coverage, x = VE_all_coverage + 1, log = "xy", # Need to add +1 to the original value because model with log(X+1)
             xlab = "", ylab = "",
             main = "",
             xaxt = "n", cex.axis=1.9, cex.lab=1.9, type="n")
        
        # Custom position for y-label
        # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
        mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
        # Custom position for x-label
        # mtext(text = paste(VE_legend, "(log scale)"), side = 1, line = 3.6, cex = 1.4) # with (log scale)
        mtext(text = paste(VE_legend), side = 1, line = 3.6, cex = 1.4)
        
        # Custom axis to display real values for low values of HF
        axis(side = 1, at = c(1,2,5,10,20,50,100), labels = c(0,1,5,10,20,50,100), cex.axis = 1.9, cex.lab = 1.9) 
        
      } else { # Otherwise, plot title and all axis
        
        plot(y = VAE_all_coverage, x = VE_all_coverage, log = "xy", 
             xlab = "", ylab = "",
             main = "",
             cex.axis=1.9, cex.lab=1.9, type="n")
        
        # Custom position for y-label
        # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
        mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
        # Custom position for x-label
        # mtext(text = paste(VE_legend, "(log scale)"), side = 1, line = 3.6, cex = 1.4) # with (log scale)
        mtext(text = paste(VE_legend), side = 1, line = 3.6, cex = 1.4)
      }
      
      # Plot points iteratively for each web_coverage
      for (i in 1:length(dataset_list)) { 
        aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve dataset associated with web coverage
        model <- eval(parse(text = paste0("model_",i))) # Retrieve the model
        points(exp(model$model[,grep(VE, names(model$model))]), # Retro-transformation of the VE tel such as Exp(X) - 1 = X' <=> X = ln(X' + 1)
               exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255),
               cex = 1.3)
      }
      
      # Plot predict iteratively for each web_coverage
      for (i in 1:length(dataset_list)) { # Add predict lines
        predict_table <- eval(parse(text = paste0("predict_table_",i))) # Load predict for model i
        lines(x = exp(predict_table$var_abs), # Retro-transformation of the VE tel such as Exp(X) - 1 = X' <=> X = ln(X' + 1)
              y = exp(predict_table$predict), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
              col = col_list[i], lwd = 3, lty = c(2,1)[signif_list][i]) # Significant = full ligne. Non-significant = dashed line
      }
      
      # Add legend line by line, with beta letter as an expression
      if(k %in% c(1, 2)) { # For A and B
        
        for (i in 1:length(web_coverage_legend_custom)) {
          legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
                 col = col_list[i], text.col = "black", text.font = 2,
                 x = "topleft", inset=c(0.01, -0.04 + i*0.045), 
                 bty ="n", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
        }
        
      } else {             # For C and D
        
        for (i in 1:length(web_coverage_legend_custom)) {
          legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
                 col = col_list[i], text.col = "black", text.font = 2,
                 x = "bottomleft", inset=c(0.01, 0.14 - i*0.045), 
                 bty = "o", box.col = "white", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
        }
        
      }
      
      # dev.off()
    }    
    
  }
  
  if(k %in% c(1, 2)) { # For A and B
    legend(legend = panel_letters[k], x = "bottomleft", bty = "n",
           text.font = 2, cex = 2.2, inset=c(-0.03, 0))
  } else {             # For C and D
    legend(legend = panel_letters[k], x = "topleft", bty = "n",
           text.font = 2, cex = 2.2, inset=c(-0.03, 0))
  }
  
  cat(paste("\n Plot n°", k, "on", length(index_plot_to_plot), "-", Sys.time(), "\n"))
  
  k <- k + 1 # Index to keep track when plotting a reduced number of plots
  
}

dev.off()

# Reset graphic parameters
par(mar = mar_old_settings)
par(mgp = mgp_old_settings)
par(oma = oma_old_settings)



##### Figure S3.3.2: Effects of sampling characteristics on plant richness (A & C) and plant link density (B & D) #####

# A/ Effect of sampling effort on plant richness
# B/ Effect of standardized sampling effort on plant link density
# C/ Effect of taxonomic resolution on plant richness
# D/ Effect of annual time span of sampling on plant link density


### Clear memory
rm(list = ls())
gc() # Force R to release memory it is no longer using

# Cut panels for figure
decoupe <- layout(mat = matrix(data = c(1,0,2,3,0,4), nrow = 2, ncol = 3, byrow = TRUE), # Numbers = order of plots ;
                  widths=c(7,0.2,7),  # Widths of columns
                  heights=c(6, 6),       # Heights of lines
                  respect=TRUE)

layout.show(decoupe) # Visualiser la d?coupe de la zone de dessin

## Load files

load(file = "./Data/all_single_plots_summary_all_VE.RData")
load(file = "./Data/all_multiple_plots_summary_all_VE.RData")


# To associate the right legend and the right variable name in the dataset with the right response variable (VAE)
VAE_list <- c("C", "Li", "Lp", "S", "I", "P" )
VAE_legend_list <- c("Connectance", "Insect link density  [inter/sp.]", "Plant link density  [inter/sp.]", "Total richness  [sp.]", "Insect richness  [sp.]", "Plant richness  [sp.]")
VAE_dataset_name_list <- c("Connectance", "Li", "Lp", "sptot", "full_insects", "full_plants" )


# To associate the right legend and the right variable name in the dataset with the right explanatory variable (VE)
VE_short_name_list <-  c("S", "P", "I", "SE", "Time", "ATS", "HF", "Ptot", "Tmean", "Pvar", "Tvar", "Forest", "Taxo")
VE_legend_list <- c("Network size  [sp.]", "Partner pool  [plant sp.] ", "Partner pool  [insect sp.]", "Standardized sampling effort  [h/inter]", "Sampling effort  [h]", "Annual time span of sampling  [days]", "Human Influence Index  [%]", "Annual total precipitation  [mm]", "Mean temperature  [°C]", "Precipitation seasonality  [CV]", "Temperature seasonality  [°C]", "Forest cover  [%]", "Taxonomic resolution  [%]")
VE_axis_name_list <- c("sptot", "full_plants", "full_insects", "Sampling_effort", "Sampling_time", "Annual_time_span", "HF", "Tot_Rainfall_IPCC", "Mean_T_IPCC", "Rainfall_Seasonality_IPCC", "T_Seasonality_IPCC", "Forest", "taxo_full_sp_perc")


# Chose web coverage to plot on a single Multiple plot
web_coverage_list <- c("full", "dipt", "hymeno")
web_coverage_to_plot <- c("full", "dipt", "hymeno")
web_coverage_legend <- c("Full", "Diptera", "Hymenoptera")
web_coverage_legend_custom <- paste0(web_coverage_legend,":") # Add the double point for plot convenience

# Chose panel A: Effect of sampling effort on plant richness
panel_A <- which((all_multiple_plots_summary_all_VE$VAE == "P") & (all_multiple_plots_summary_all_VE$VE_short_name == "Time"))

# Chose panel B: Effect of standardized sampling effort on plant link density
panel_B <- which((all_multiple_plots_summary_all_VE$VAE == "Lp") & (all_multiple_plots_summary_all_VE$VE_short_name == "SE"))

# Chose panel C: Effect of taxonomic resolution on plant richness
panel_C <- which((all_multiple_plots_summary_all_VE$VAE == "P") & (all_multiple_plots_summary_all_VE$VE_short_name == "Taxo"))

# Chose panel D: Effect of annual time span of sampling on plant link density
panel_D <- which((all_multiple_plots_summary_all_VE$VAE == "Lp") & (all_multiple_plots_summary_all_VE$VE_short_name == "ATS"))


# Save plot index corresponding to each panel
index_plot_to_plot <- c(panel_A, panel_B, panel_C, panel_D)

# Define panel letters
panel_letters <- c("(a)", "(b)","(c)","(d)")

# Open plot
pdf(file = paste0("./SM_Final_figures/Figure_S3.3.2.pdf"), width = 14, height = 12)

# Cut panels
layout(mat = matrix(data = c(1,0,2,3,0,4), nrow = 2, ncol = 3, byrow = TRUE), # Numbers = order of plots ;
       widths=c(7,0.2,7),     # Widths of columns
       heights=c(6, 6),       # Heights of lines
       respect=TRUE)

# Set margins
mar_old_settings <- par()$mar
par(mar = c(5.1, 5.1, 2.1, 2.1))

# Set outer margins
oma_old_settings <- par()$oma
par(oma = c(0, 1, 0, 0))

# Set position of tick labels
mgp_old_settings <- par()$mgp
par(mgp = c(3, 1.5, 0))

# Boucle
k <- 1 ; par(xpd = F)
for (j in index_plot_to_plot) {
  
  web_coverage_available <- unlist(strsplit(x = all_multiple_plots_summary_all_VE$web_coverage[j], split = "-"))
  
  # Chack if all web_coverage are available for this multiple plot
  if (!all(web_coverage_to_plot %in% web_coverage_available)) { # If not, warn with a message
    
    cat(paste("Not all web coverages are available to do a Multliple plot for", all_multiple_plots_summary_all_VE$VAE[j], "~", all_multiple_plots_summary_all_VE$VE_short_name[j] , "in model", all_multiple_plots_summary_all_VE$model_type[j], all_multiple_plots_summary_all_VE$polar[j], all_multiple_plots_summary_all_VE$taxo[j], all_multiple_plots_summary_all_VE$forest[j], "= Multiple plot n°", j, "\n"))
    
  } else {  # If available, plot !
    
    # Choix de la VAE
    VAE <- all_multiple_plots_summary_all_VE$VAE[j]
    VAE_legend <- VAE_legend_list[which(VAE_list == VAE)]
    
    # Choix de la VE
    VE <- VE_axis_name <- all_single_plots_summary_all_VE$VE[j]
    VE_short_name <- all_single_plots_summary_all_VE$VE_short_name[j]
    VE_legend <- VE_legend_list[which(VE_short_name_list == VE_short_name)]
    ln_transfo <- "no"
    
    # Special case of VE plotted with log transformation
    if (VE_short_name %in% c("S", "P", "I", "SE", "Time", "ATS")) { 
      VE_axis_name <- VE_axis_name_list[which(VE_short_name_list == VE_short_name)]
      ln_transfo <- "log"
    }
    
    # Special case of VE plotted with log(X+1) transformation
    if (VE_short_name == "HF") { 
      VE_axis_name <- VE_axis_name_list[which(VE_short_name_list == VE_short_name)]
      ln_transfo <- "log1p"
    }
    
    # Extract information on model 
    model_type <- all_multiple_plots_summary_all_VE$model_type[j]
    polar <- all_multiple_plots_summary_all_VE$polar[j]
    
    # Load models for each web coverage and save dataset_list
    dataset_list <- NA
    for (i in 1:length(web_coverage_to_plot)) {
      model_name <- paste0(VAE, "_", web_coverage_to_plot[i], "_", model_type, "_", polar)
      
      eval(call("<-", as.name(paste0("predict_table_",i)), readRDS(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Predicts_for_Graphs/predict_table_",model_name,"_for_",VE_short_name,".RData"))))
      eval(call("<-", as.name(paste0("model_",i)), readRDS(file = paste0("./Models_outputs/",VAE,"/",web_coverage_to_plot[i],"/",model_name,"/",model_name,"_bestmod_with_", VE_short_name, ".RData"))))
      
      dataset_list[i] <- paste0(web_coverage_to_plot[i], "_", model_type, "_", polar)
    }  
    
    # Use real limits of X and Y, all web_coverage confounded, to plot axis
    VAE_all_coverage <- as.numeric()
    for (i in 1:length(dataset_list)) { 
      aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve VAE (Y-axis) for all web_coverage
      VAE_all_coverage <- c(VAE_all_coverage, eval(parse(text = paste0("aggreg.webs$", VAE_dataset_name_list[which(VAE_list == VAE)]))))
    }
    
    VE_all_coverage <- as.numeric()
    for (i in 1:length(dataset_list)) { 
      aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve VE (X-axis) for all_coverage
      VE_all_coverage <- c(VE_all_coverage, eval(parse(text = paste0("aggreg.webs$",VE_axis_name))))
    }
    
    # Chose colors to associate with each model/web coverage
    col_list <- c("black", "dodgerblue3", "red") 
    
    # Extract significativit? of the slope from p-value of the single models stored in all_single_plots_summary_all_VE
    web_coverage_index <- NA
    for (i in 1:length(web_coverage_to_plot)) {
      web_coverage_index[i] <- which(web_coverage_to_plot[i] == web_coverage_list) # Retrieve index of plotted web coverages among all possible web coverages
    }
    ID_plot_list <- unlist(strsplit(x = all_multiple_plots_summary_all_VE$ID_plot[j], split = "-")) # Retrieve index of single plots
    ID_plot_used <- as.numeric(ID_plot_list[web_coverage_index])
    
    beta_value_list <- round(all_single_plots_summary_all_VE$Beta_coef[ID_plot_used], 3) # Extract beta-coefs
    beta_value_list_custom <- paste0(beta_value_list,",") # Add the comma to help to plot the legend
    
    p_value_list <- round(all_single_plots_summary_all_VE$p_value[ID_plot_used], 3) # Extract p-values
    
    signif_list <- as.numeric(p_value_list < 0.05)+1 # Code significance of slopes. 2 = significant, 1 = non-significant
    
    # Create label for p-value
    p_value_custom_list <- paste("p = ", p_value_list)
    p_value_custom_list[which(p_value_list < 0.001)] <- "p < 0.001"
    
    # # Create an output folder if needed
    # if (!dir.exists(paths = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar))) { 
    #   dir.create(path = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar), recursive = T) 
    # }   
    
    # Plot without log on X-axis
    # pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Multiple_plot_",VAE,"_",model_type,"_",polar,"_for_",VE_short_name,"_in_",paste(web_coverage_to_plot, collapse = "_"),".pdf"), width = 7, height = 6)
    
    # Plot axis and title
    # par(mfcol=c(1,1))
    
    if (ln_transfo == "no") {
      
      plot(y = VAE_all_coverage, x = VE_all_coverage, log = "y", xlab = "", ylab = "",
           main = "",
           cex.axis=1.9, cex.lab=1.9, type="n")
      
      # Custom position for y-label
      # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
      mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
      # Custom position for x-label
      mtext(text = VE_legend, side = 1, line = 3.6, cex = 1.4)
      
      for (i in 1:length(dataset_list)) {  # Plot each web_coverge iteratively
        
        aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve dataset associated with web coverage
        model <- eval(parse(text = paste0("model_",i))) # Retrieve model
        
        points(x = model$model[,grep(VE, names(model$model))], # No retro-transformation of the VE
               y = exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255),
               cex = 1.3)
      }
      
      for (i in 1:length(dataset_list)) { # Add predict lines
        predict_table <- eval(parse(text = paste0("predict_table_",i))) # Load predict for model i
        
        lines(x = predict_table$var_abs, # No retro-transformation of the VE
              y = exp(predict_table$predict), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
              col = col_list[i], lwd = 3, lty = c(2,1)[signif_list][i]) # Significant = full ligne. Non-significant = dashed line
      }
      
      # Add legend line by line, with beta letter as an expression
      if(k %in% c(1, 2)) { # For A and B
        
        for (i in 1:length(web_coverage_legend_custom)) {
          legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
                 col = col_list[i], text.col = "black", text.font = 2,
                 x = "topleft", inset=c(0.01, -0.04 + i*0.045), 
                 bty ="n", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
        }
        
      } else {             # For C and D
        
        for (i in 1:length(web_coverage_legend_custom)) {
          legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
                 col = col_list[i], text.col = "black", text.font = 2,
                 x = "bottomleft", inset=c(0.01, 0.14 - i*0.045), 
                 bty = "o", box.col = "white", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
        }
        
      }
      
    }
    
    
    
    # dev.off()  
    
    # Plot with log on the X-axis
    
    if (ln_transfo %in% c("log", "log1p")) { 
      
      # pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Multiple_plot_",VAE,"_",model_type,"_",polar,"_for_",VE_short_name,"_in_",paste(web_coverage_to_plot, collapse = "_"),"_log.pdf"), width = 7, height = 6)
      
      # par(mfcol=c(1,1))
      
      if (ln_transfo == "log1p") { # Need to specify not to plot the X-axis (xaxt = "n") that will be customed for ln(HF+1)
        
        plot(y = VAE_all_coverage, x = VE_all_coverage + 1, log = "xy", # Need to add +1 to the original value because model with log(X+1)
             xlab = "", ylab = "",
             main = "",
             xaxt = "n", cex.axis=1.9, cex.lab=1.9, type="n")
        
        # Custom position for y-label
        # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
        mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
        # Custom position for x-label
        # mtext(text = paste(VE_legend, "(log scale)"), side = 1, line = 3.6, cex = 1.4) # with (log scale)
        mtext(text = paste(VE_legend), side = 1, line = 3.6, cex = 1.4)
        
        # Custom axis to display real values for low values of HF
        axis(side = 1, at = c(1,2,5,10,20,50,100), labels = c(0,1,5,10,20,50,100), cex.axis = 1.9, cex.lab = 1.9) 
        
      } else { # Otherwise, plot title and all axis
        
        plot(y = VAE_all_coverage, x = VE_all_coverage, log = "xy", 
             xlab = "", ylab = "",
             main = "",
             cex.axis=1.9, cex.lab=1.9, type="n")
        
        # Custom position for y-label
        # mtext(text = paste(VAE_legend,"(log scale)"), side = 2, line = 4.2, cex = 1.4) # With (log scale)
        mtext(text = paste(VAE_legend), side = 2, line = 4.2, cex = 1.4)
        # Custom position for x-label
        # mtext(text = paste(VE_legend, "(log scale)"), side = 1, line = 3.6, cex = 1.4) # with (log scale)
        mtext(text = paste(VE_legend), side = 1, line = 3.6, cex = 1.4)
      }
      
      # Plot points iteratively for each web_coverage
      for (i in 1:length(dataset_list)) { 
        aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve dataset associated with web coverage
        model <- eval(parse(text = paste0("model_",i))) # Retrieve the model
        points(exp(model$model[,grep(VE, names(model$model))]), # Retro-transformation of the VE tel such as Exp(X) - 1 = X' <=> X = ln(X' + 1)
               exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255),
               cex = 1.3)
      }
      
      # Plot predict iteratively for each web_coverage
      for (i in 1:length(dataset_list)) { # Add predict lines
        predict_table <- eval(parse(text = paste0("predict_table_",i))) # Load predict for model i
        lines(x = exp(predict_table$var_abs), # Retro-transformation of the VE tel such as Exp(X) - 1 = X' <=> X = ln(X' + 1)
              y = exp(predict_table$predict), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
              col = col_list[i], lwd = 3, lty = c(2,1)[signif_list][i]) # Significant = full ligne. Non-significant = dashed line
      }
      
      # Add legend line by line, with beta letter as an expression
      if(k %in% c(1, 2)) { # For A and B
        
        for (i in 1:length(web_coverage_legend_custom)) {
          legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
                 col = col_list[i], text.col = "black", text.font = 2,
                 x = "topleft", inset=c(0.01, -0.04 + i*0.045), 
                 bty ="o", box.col = "white", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
        }
        
      } else {             # For C and D
        
        for (i in 1:length(web_coverage_legend_custom)) {
          legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
                 col = col_list[i], text.col = "black", text.font = 2,
                 x = "bottomleft", inset=c(0.01, 0.14 - i*0.045), 
                 bty = "o", box.col = "white", lty=1, lwd = 2, cex = 1.2, bg="white", horiz = F)
        }
        
      }
      
      # dev.off()
    }    
    
  }
  
  if(k %in% c(1, 2)) { # For A and B
    legend(legend = panel_letters[k], x = "bottomleft", bty = "n",
           text.font = 2, cex = 2.2, inset=c(-0.03, 0))
  } else {             # For C and D
    legend(legend = panel_letters[k], x = "topleft", bty = "n",
           text.font = 2, cex = 2.2, inset=c(-0.03, 0))
  }
  
  cat(paste("\n Plot n°", k, "on", length(index_plot_to_plot), "-", Sys.time(), "\n"))
  
  k <- k + 1 # Index to keep track when plotting a reduced number of plots
  
}

dev.off()

# Reset graphic parameters
par(mar = mar_old_settings)
par(mgp = mgp_old_settings)
par(oma = oma_old_settings)
