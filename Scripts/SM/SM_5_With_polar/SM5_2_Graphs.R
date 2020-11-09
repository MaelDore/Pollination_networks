
##### Script SM5_2: Generate Figure S5.2 #####

# Comparison of beta-coefficients obtain for full coverage, between models with and without polar networks included

# Inputs:

# Results of calibrated models from Script SM5_1 (models, and summary tables)
# Summary table of model parameters for model without polar networks (all_models_summary)

# Outputs:

# Summary array with all results for best models and MAM models (beta-coefficients, p-value, variable importance) from model with and without polar networks (Infos_all_models_polar)
# Summary table of model parameters for models with and without polar netwroks (all_models_summary_polar)
# Figure S5.2: Summary plots of beta-coefficients of best models including (blue) or excluding (red) polar networks

#####


##################################### Summary #########################################

# 1/ Generate array to store results from all models

# 2/ Load summary tables

# Used to keep track of model parameters

# 3/ Loop to extract results into the array

# Get beta-coefficients, standard error, p-value, and variable importance for each best model and MAM models

# 4/ Convert names of explanatory variables

# 5/ Plot all possible graphs

# 6/ Plot the final Figure

# Plot each panel of the final plots individually, with custom layout corrections
# Plot the final layout

#######################################################################################



### Clear memory
rm(list = ls())
gc() # Force R to release memory it is no longer using


### 1/ Generate array to store results from all models ####

# Declare dimensions

web_coverage_list <- c("full")
model_type_list <- c("str", "rich")
polar_list <- c("no_polar", "with_polar")
VAE_list <- c("C", "Li", "Lp", "S", "I", "P" )
VE_list <- c("ln_HF", "Tot_Rainfall_IPCC", "Mean_T_IPCC", "Rainfall_Seasonality_IPCC", "T_Seasonality_IPCC", "ln_sptot", "ln_pl", "ln_ins", "ln_SE", "ln_time", "ln_ATS", "Sampling_type", "taxo_full_sp_perc")
output_type_list <- c("Best", "MAM")
info_list <- c("Beta_coef", "SD", "p-value", "Signif_lvl", "Importance")


# Store results in array to be able to use easily with ggplot later

Infos_all_models_polar <- array(dim = c(length(VAE_list),           # Nb of VAE
                                        length(web_coverage_list),  # Nb of web coverage
                                        length(model_type_list),    # Str or Rich
                                        length(polar_list),         # With or without polar networks
                                        length(VE_list),            # Nb of VE in total
                                        length(output_type_list),   # Best models or MAM
                                        length(info_list)),         # Nb of infos retrieved
                                dimnames = list(VAE = VAE_list,                    # VAE names
                                                web_coverage = web_coverage_list,  # web coverage names
                                                model_type = model_type_list,      # Str or Rich
                                                polar = polar_list,                # With or without polar networks
                                                VE = VE_list,                      # All VE names
                                                output_type = output_type_list,    # Best model or MAMs
                                                infos = info_list)             # Beta-coef, sd, p-value, signifiance level, and importance of variable
)             

# save(Infos_all_models_polar, file = "./Data/Infos_all_models_polar.RData")

### 2/ Load summary tables ####

# load(file = "./Data/Infos_all_models_polar.RData")

# Merge summary table from results with polar networks, and without

load(file = "./Data/all_models_summary.RData")
load(file = "./Data/all_models_summary_no_polar.RData")

all_models_summary <- all_models_summary[all_models_summary$web_coverage == "full",] # Only for full coverage

all_models_summary_polar <- rbind(all_models_summary, all_models_summary_no_polar)

# save(all_models_summary_polar, file = "./Data/all_models_summary_polar.RData")


### 3/ Loop to extract results into the array ####

for (j in 1:nrow(all_models_summary_polar)) {
  
  # j <- 1
  
  web_coverage <- all_models_summary_polar$web_coverage[j] 
  model_type <- all_models_summary_polar$model_type[j] 
  polar <- all_models_summary_polar$polar[j] 
  VAE <- all_models_summary_polar$VAE[j]
  
  model_name <- all_models_summary_polar$model_name[j]
  
  if (polar == "no_polar") {
    
    # load summaries
    load(file = paste0("./Models_outputs/",VAE,"/",web_coverage,"/",model_name,"/",model_name,"_summaries.RData"))
    # Load Best model
    bestmod_AICc <- readRDS(file = paste0("./Models_outputs/",VAE,"/",web_coverage,"/",model_name,"/",model_name,"_bestmod_AICc.RData")) 
    # Load MAM models
    MAM_AICc <- readRDS(file = paste0("./Models_outputs/",VAE,"/",web_coverage,"/",model_name,"/",model_name,"_MAM_AICc.RData")) 
    
    
  } else { # Model with polar
    
    # Load summaries
    load(file = paste0("./Models_outputs_with_polar/",VAE,"/",model_name,"/",model_name,"_summaries.RData"))
    # Load Best model
    bestmod_AICc <- readRDS(file = paste0("./Models_outputs_with_polar/",VAE,"/",model_name,"/",model_name,"_bestmod_AICc.RData")) 
    # Load MAMs models
    MAM_AICc <- readRDS(file = paste0("./Models_outputs_with_polar/",VAE,"/",model_name,"/",model_name,"_MAM_AICc.RData")) 
    
  }
  
  for (VE in VE_list) {
    
    # Retrieve infos for best model
    
    if (length(grep(VE, row.names(summary_Bestmod_coefs_std))) != 0) { # Try to retrieve infos only if the VE is present in the best model
      
      # Retrieve beta-coef
      Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "Best", "Beta_coef"] <- 
        round(summary_Bestmod_coefs_std                                      # Matrix of Best model results
              [grep(VE, row.names(summary_Bestmod_coefs_std)),1], 4)         # Extract beta-coef estimate                                      
      
      # Retrieve sd
      Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "Best", "SD"] <- 
        round(summary_Bestmod_coefs_std                                      # Matrix of Best model results
              [grep(VE, row.names(summary_Bestmod_coefs_std)),2], 4)         # Extract sd 
      
      # Retrieve p-value
      Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "Best", "p-value"] <- 
        round(summary_Bestmod_coefs_std                                      # Matrix of Best model results
              [grep(VE, row.names(summary_Bestmod_coefs_std)),5], 4)         # Extract p-value 
      
      # Classify significance level regarding the p-value
      Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "Best", "Signif_lvl"] <- 0
      if (Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "Best", "p-value"] < 0.05) {
        Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "Best", "Signif_lvl"] <- 1
        
        if (Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "Best", "p-value"] < 0.01) {
          Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "Best", "Signif_lvl"] <- 2
          
          if (Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "Best", "p-value"] < 0.001) {
            Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "Best", "Signif_lvl"] <- 3
            
          }
        }
      }
      
      if (length(MAM_AICc) == 1) { # Case with only one MAM = best model, just need to copy results from Best model
        
        Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "MAM", ] <- Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "Best", ]
        Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "MAM", "Importance"] <- 1
        
      }
      
    }
    
    
    if (length(MAM_AICc) != 1) { # Case with several MAMs
      
      if (length(grep(VE, row.names(summary_MAM.avg.coefs_std$coefmat.full))) != 0) { # Try to retrieve infos only if the VE is present in one of the MAMs
        
        # Retrieve beta-coef
        Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "MAM", "Beta_coef"] <- 
          round(summary_MAM.avg.coefs_std$coefmat.full                         # Matrix of MAMs results
                [grep(VE, row.names(summary_MAM.avg.coefs_std$coefmat.full)),  # Extract results for the current VE
                  "Estimate"], 4)                                              # Extract beta-coef
        
        # Retrieve sd
        Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "MAM", "SD"] <- 
          round(summary_MAM.avg.coefs_std$coefmat.full                         # Matrix of MAMs results
                [grep(VE, row.names(summary_MAM.avg.coefs_std$coefmat.full)),  # Extract results for the current VE
                  "Adjusted SE"], 4)                                           # Extract SD
        
        # Retrieve p-value
        Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "MAM", "p-value"] <- 
          round(summary_MAM.avg.coefs_std$coefmat.full                         # Matrix of MAMs results
                [grep(VE, row.names(summary_MAM.avg.coefs_std$coefmat.full)),  # Extract results for the current VE
                  "Pr(>|z|)"], 4)                                              # Extract p-value
        
        # Classify significance level regarding the p-value
        Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "MAM", "Signif_lvl"] <- 0
        if (Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "MAM", "p-value"] < 0.05) {
          Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "MAM", "Signif_lvl"] <- 1
          
          if (Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "MAM", "p-value"] < 0.01) {
            Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "MAM", "Signif_lvl"] <- 2
            
            if (Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "MAM", "p-value"] < 0.001) {
              Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "MAM", "Signif_lvl"] <- 3
              
            }
          }
        }
        
        # Extract variable importance
        imp_var <- MuMIn::importance(MAM_AICc)
        Infos_all_models_polar[VAE, web_coverage, model_type, polar, VE, "MAM", "Importance"] <- imp_var[grep(VE, names(imp_var))]
        
      }
      
    }
    
  }
  
  # Check running
  cat(paste("\n Model n°", j, "on", nrow(all_models_summary_polar), "\n"))
  
}

# save(Infos_all_models_polar, file = "./Data/Infos_all_models_polar.RData")


### 4/ Generate a version with simplified name for VE (explanatory variables) ####

load(file = "./Data/Infos_all_models_polar.RData")

VE_list <- c("ln_HF", "Tot_Rainfall_IPCC", "Mean_T_IPCC", "Rainfall_Seasonality_IPCC", "T_Seasonality_IPCC", "ln_sptot", "ln_pl", "ln_ins", "ln_SE", "ln_time", "ln_ATS", "Sampling_type", "taxo_full_sp_perc")
VE_short_name_list <-  c("HII", "Ptot", "Tmean", "Pvar", "Tvar", "S", "P", "I", "stdSE", "SE", "ATS", "Method", "Taxo")

Infos_all_models_polar_VE_renamed <- Infos_all_models_polar

dimnames(Infos_all_models_polar_VE_renamed)$VE <- VE_short_name_list

# save(Infos_all_models_polar_VE_renamed, file = "./Data/Infos_all_models_polar_VE_renamed.RData")


##### 5/ Loop to plot graphs for each VAE, for best models and MAM models ####


# Load results from array
load(file = "./Data/Infos_all_models_polar.RData")

VAE_list <- c("C", "Li", "Lp", "S", "I", "P" )
VAE_full_name_list <- c("Connectance", "Insect link density", "Plant link density", "Species richness", "Insect richness", "Plant richness")
web_coverage_list <- c("full")

# Convert VE name to proper short names for plots
VE_list <- c("ln_HF", "Tot_Rainfall_IPCC", "Mean_T_IPCC", "Rainfall_Seasonality_IPCC", "T_Seasonality_IPCC", "ln_sptot", "ln_pl", "ln_ins", "ln_SE", "ln_time", "ln_ATS", "Sampling_type", "taxo_full_sp_perc")
VE_short_name_list <-  c("HII", "Ptot", "Tmean", "Pvar", "Tvar", "S", "P", "I", "stdSE", "SE", "ATS", "Method", "Taxo")

library(ggplot2)

# Set margin parameters: Bottom, left, top, right
mar_old_settings <- par()$mar
par(mar = c(7.1, 4.1, 4.1, 2.1))

i <- 1
for (VAE in VAE_list) {
  
  ### Choose manually
  # VAE <- "C"
  # VAE <- "Li"
  # VAE <- "Lp"
  # VAE <- "S"
  # VAE <- "I"
  # VAE <- "P"
  ###
  
  for (output_type in output_type_list) {
    
    # Extract only infos for this VAE
    Infos_sub_models <- Infos_all_models_polar[VAE, , , , , output_type, ] 
    
    # Convert VE name to proper short names for plots
    dimnames(Infos_sub_models)$VE <- VE_short_name_list
    
    # dimnames(Infos_sub_models)
    
    # Extract infos and format into df
    Estimate_sub_models <- Infos_sub_models[,,,"Beta_coef"]     # Extract beta-coefs 
    Estimate_sub_models_df <- reshape2::melt(Estimate_sub_models, value.name = "Beta_coef") # Generate a df for ggplot
    
    SD_sub_models <- Infos_sub_models[,,,"SD"]                             # Extract sd
    SD_sub_models_df <- reshape2::melt(SD_sub_models, value.name = "SD")   # Generate a df for ggplot
    
    Signif_sub_models <- Infos_sub_models[,,,"Signif_lvl"]                                 # Extract signifiance levels
    Signif_sub_models_df <- reshape2::melt(Signif_sub_models, value.name = "Signif_lvl")   # Generate a df for ggplot
    
    # Remove ligns with NA, useless for plots
    Estimate_sub_models_df <- Estimate_sub_models_df[!is.na(Estimate_sub_models_df$Beta_coef),]
    SD_sub_models_df <- SD_sub_models_df[!is.na(SD_sub_models_df$SD),]
    Signif_sub_models_df <- Signif_sub_models_df[!is.na(Signif_sub_models_df$Signif_lvl),]
    
    
    pdf(file = paste0("./SM_Figures/SM_5_Summary_no_polar/",output_type,"/Beta_coeffs_summary_",VAE,"_",output_type,".pdf"), width = 8, height = 6)
    
    # Declare plot
    g <- ggplot(Estimate_sub_models_df, aes(x = VE, y = Beta_coef, 
                                            ymin = Estimate_sub_models_df$Beta_coef-SD_sub_models_df$SD, 
                                            ymax = Estimate_sub_models_df$Beta_coef+SD_sub_models_df$SD,
                                            # color = Estimate_sub_models_df$polar, 
                                            # fill = Estimate_sub_models_df$polar,
                                            color = as.factor(c("With", "Without"))[-as.numeric(Estimate_sub_models_df$polar)+3],
                                            # shape = Estimate_sub_models_df$polar,
                                            size = as.factor(c("p > 0.05", "p < 0.05", "p < 0.01", "p < 0.001"))[Signif_sub_models_df$Signif_lvl+1]
    ))
    
    # Add points
    g <- g + geom_pointrange(# y = Estimate_sub_models_df$Beta_coef,
      # ymin = Estimate_sub_models_df$Beta_coef-SD_sub_models_df$SD,
      # ymax = Estimate_sub_models_df$Beta_coef+SD_sub_models_df$SD,
      # color = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")[Estimate_sub_models_df$model_legend], 
      # fill = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")[Estimate_sub_models_df$model_legend],
      # size = 0.8,
      # shape = c(16,17)[Estimate_sub_models_df$polar],
      position = position_dodge(width = 0.5),
      show.legend = T) 
    
    # Manage aesthetics
    g <- g + theme_bw() + ggtitle(paste("Models for", VAE_full_name_list[which(VAE_list == VAE)])) +  
      ylab(expression(paste(beta, "-coefficients"))) + 
      xlab("Explanatory variables") +
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.8, size = 16, face = "bold"),  # Titre
            axis.title.x = element_text(size=15, vjust=-0.8, color = 'black'),  # Titre x-axis
            axis.text.x = element_text(size=10, vjust=0, color = 'black'),      # Labels x-axis
            axis.title.y = element_text(size=15, vjust=2, color = 'black'),     # Titre y-axis
            axis.text.y = element_text(size=10, vjust=0, color = 'black'),      # Labels y-axis
            legend.title = element_text(face="bold"),
            legend.position = "right"
            
      ) +
      # Manual modification for point colors
      # scale_fill_manual(values = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")) +
      # guides(colour = guide_legend("my awesome title")) +
      # scale_colour_discrete("Continents") +
      scale_color_manual(values = c("dodgerblue3", "red")) +
      # Manual modification for point size
      scale_size_manual(values = c(1.4, 1.1, 0.8, 0.4), drop = F) +
      # Manual modification for y-axis breaks and limits
      scale_y_continuous(limits = c(min(Estimate_sub_models_df$Beta_coef-SD_sub_models_df$SD), max(Estimate_sub_models_df$Beta_coef+SD_sub_models_df$SD)),
                         # scale_y_continuous(limits = c(-0.15, 0.15),
                         breaks = round(seq(from = (floor(min(Estimate_sub_models_df$Beta_coef-SD_sub_models_df$SD)*10))/10, to = (ceiling(max(Estimate_sub_models_df$Beta_coef+SD_sub_models_df$SD)*10))/10, by = 0.1),1)) +
      guides(col=guide_legend("Polar networks", order = 1),
             size=guide_legend("Significance levels", order = 2)) +
      
      # Line for y = 0
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
    
    # Store plot with index
    eval(call("<-", as.name(paste0("g", i)), g))
    
    print(g)
    
    dev.off() 
    
  }

  cat(paste("\n", Sys.time(), "-", VAE, "- Done \n")) 
  
  i <- i + 1
  
}

# Reload previous graph parameter settings
par(mar = mar_old_settings)      



##### 6/ Final figure with all summary plot for MAM models for each VAE in one figure ####

# Need to generate each plot individually with its specificity, then organize them all together on one figure

rm(list = ls())
gc() # Force R to release memory it is no longer using

# Load packages
library("ggplot2")
library("grid")

# Load results from array
load(file = "./Data/Infos_all_models_polar.RData")

VAE_list <- c("C", "Li", "Lp", "S", "I", "P" )
VAE_full_name_list <- c("Connectance", "Insect link density", "Plant link density", "Species richness", "Insect richness", "Plant richness")
web_coverage_list <- c("full")

# Convert VE name to proper short names for plots
VE_list <- c("ln_HF", "Tot_Rainfall_IPCC", "Mean_T_IPCC", "Rainfall_Seasonality_IPCC", "T_Seasonality_IPCC", "ln_sptot", "ln_pl", "ln_ins", "ln_SE", "ln_time", "ln_ATS", "Sampling_type", "taxo_full_sp_perc")
VE_short_name_list <-  c("HII", "Ptot", "Tmean", "Pvar", "Tvar", "S", "P", "I", "stdSE", "SE", "ATS", "Method", "Taxo")

library(ggplot2)

# Set margin parameters: Bottom, left, top, right
mar_old_settings <- par()$mar
par(mar = c(7.1, 4.1, 4.1, 2.1))


### 6.1/ Total richness ####

i <- 1 ; VAE <- "S"
for (VAE in VAE) {
  
  ### Choose manually
  # VAE <- "C"
  # VAE <- "Li"
  # VAE <- "Lp"
  # VAE <- "S"
  # VAE <- "I"
  # VAE <- "P"
  ###
  
  # Extract only infos for this VAE and MAM models
  Infos_sub_models <- Infos_all_models_polar[VAE, , , , , "MAM", ] 
  
  # Convert VE name to proper short names for plots
  dimnames(Infos_sub_models)$VE <- VE_short_name_list
  
  # dimnames(Infos_sub_models)
  
  # Extract infos and format into df
  Estimate_sub_models <- Infos_sub_models[,,,"Beta_coef"]     # Extract beta-coefs 
  Estimate_sub_models_df <- reshape2::melt(Estimate_sub_models, value.name = "Beta_coef") # Generate a df for ggplot
  
  SD_sub_models <- Infos_sub_models[,,,"SD"]                           # Extract sd
  SD_sub_models_df <- reshape2::melt(SD_sub_models, value.name = "SD")   # Generate a df for ggplot
  
  Signif_sub_models <- Infos_sub_models[,,,"Signif_lvl"]                                 # Extract signifiance levels
  Signif_sub_models_df <- reshape2::melt(Signif_sub_models, value.name = "Signif_lvl")   # Generate a df for ggplot
  
  # # Remove ligns with NA , useless for plots
  # Estimate_sub_models_df <- Estimate_sub_models_df[!is.na(Estimate_sub_models_df$Beta_coef),]
  # SD_sub_models_df <- SD_sub_models_df[!is.na(SD_sub_models_df$SD),]
  # Signif_sub_models_df <- Signif_sub_models_df[!is.na(Signif_sub_models_df$Signif_lvl),]
  
  eval(call("<-", as.name(paste0("Estimate_sub_models_df", i)), Estimate_sub_models_df[!is.na(Estimate_sub_models_df$Beta_coef),]))
  eval(call("<-", as.name(paste0("SD_sub_models_df", i)), SD_sub_models_df[!is.na(SD_sub_models_df$SD),]))
  eval(call("<-", as.name(paste0("Signif_sub_models_df", i)), Signif_sub_models_df[!is.na(Signif_sub_models_df$Signif_lvl),]))
  
  # pdf(file = paste0("./SM_Figures/SM_5_Summary_no_polar/Beta_coeffs_summary_",VAE,".pdf"), width = 8, height = 6)
  
  # Declare plot
  g <- ggplot(Estimate_sub_models_df1, aes(x = VE, y = Beta_coef, 
                                           ymin = Estimate_sub_models_df1$Beta_coef-SD_sub_models_df1$SD, 
                                           ymax = Estimate_sub_models_df1$Beta_coef+SD_sub_models_df1$SD,
                                           # color = Estimate_sub_models_df1$polar, 
                                           # fill = Estimate_sub_models_df1$polar,
                                           color = as.factor(c("With", "Without"))[-as.numeric(Estimate_sub_models_df1$polar)+3],
                                           # shape = Estimate_sub_models_df1$polar,
                                           size = as.factor(c("p > 0.05", "p < 0.05", "p < 0.01", "p < 0.001"))[Signif_sub_models_df1$Signif_lvl+1]
  ))
  
  # Ajout des points
  g <- g + geom_pointrange(# y = Estimate_sub_models_df1$Beta_coef,
    # ymin = Estimate_sub_models_df1$Beta_coef-SD_sub_models_df1$SD,
    # ymax = Estimate_sub_models_df1$Beta_coef+SD_sub_models_df1$SD,
    # color = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")[Estimate_sub_models_df1$model_legend], 
    # fill = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")[Estimate_sub_models_df1$model_legend],
    # size = 0.8,
    # shape = c(16,17)[Estimate_sub_models_df1$polar],
    position = position_dodge(width = 0.5),
    show.legend = T) 
  
  # Gestion de l'esth?tique
  g <- g + theme_bw() + ggtitle("(a) Species richness") +  
    ylab(expression(paste(beta, "-coefficients"))) + 
    # xlab("Explanatory variables") +
    theme(plot.title = element_text(hjust = 0, vjust = 0.5, size = 13, face = "bold"),  # Titre
          # plot.title = element_blank(),
          axis.title.x = element_blank(),
          # axis.title.x = element_text(size=15, vjust=-1.3, color = 'black'),  # Titre x-axis
          axis.text.x = element_text(size=10, vjust=0, color = 'black'),      # Labels x-axis
          axis.title.y = element_text(size=15, vjust=1.7, color = 'black'),     # Titre y-axis
          axis.text.y = element_text(size=10, vjust=0, color = 'black'),      # Labels y-axis
          legend.title = element_text(face="bold"),
          legend.position = "none",
          plot.margin = unit(c(5.5,5.5,15,12.5),"pt") # top, right, bottom and left
    ) +
    # Modification manuelle des couleurs des points
    # scale_fill_manual(values = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")) +
    # guides(colour = guide_legend("my awesome title")) +
    # scale_colour_discrete("Continents") +
    scale_color_manual(values = c("dodgerblue3", "red")) +
    # Modification manuelle de la taille des points
    scale_size_manual(values = c(1.4, 1.1, 0.8, 0.4), drop = F) +
    # Modification des limites et breaks de l'axe des y
    scale_y_continuous(limits = c(min(Estimate_sub_models_df1$Beta_coef-SD_sub_models_df1$SD), max(Estimate_sub_models_df1$Beta_coef+SD_sub_models_df1$SD)),
                       # scale_y_continuous(limits = c(-0.15, 0.15),
                       breaks = round(seq(from = (floor(min(Estimate_sub_models_df1$Beta_coef-SD_sub_models_df1$SD)*10))/10, to = (ceiling(max(Estimate_sub_models_df1$Beta_coef+SD_sub_models_df1$SD)*10))/10, by = 0.1),1)) +
    guides(col=guide_legend("Polar networks", order = 1),
           size=guide_legend("Significance levels", order = 2)) +
    # Ligne ? y = 0
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
  
  # # Create fixed text
  # grob <- grobTree(textGrob("(d) Species richness", x=0.03,  y=0.95, hjust=0,
  #                           gp=gpar(fontsize=13, fontface="bold")))
  # # Add to graph
  # g <- g + annotation_custom(grob)
  
  # Store plot with index
  eval(call("<-", as.name(paste0("g", i)), g))
  
  print(g)
  
  # dev.off()       
  
  save(g1, Estimate_sub_models_df1, SD_sub_models_df1, Signif_sub_models_df1, file = "./Data/SM_5/Compared_S_plot_data.RData")
  
  cat(paste("\n", Sys.time(), "-", VAE, "- Done \n")) 
  
}

# Reload previous graph parameter settings
par(mar = mar_old_settings) 

### 6.2/ Insect richness ####

i <- 2 ; VAE <- "I"
for (VAE in VAE) {
  
  ### Choose manually
  # VAE <- "C"
  # VAE <- "Li"
  # VAE <- "Lp"
  # VAE <- "S"
  # VAE <- "I"
  # VAE <- "P"
  ###
  
  # Extract only infos for this VAE and MAM models
  Infos_sub_models <- Infos_all_models_polar[VAE, , , , , "MAM", ] 
  
  # Convert VE name to proper short names for plots
  dimnames(Infos_sub_models)$VE <- VE_short_name_list
  
  # dimnames(Infos_sub_models)
  
  # Extract infos and format into df
  Estimate_sub_models <- Infos_sub_models[,,,"Beta_coef"]     # Extract beta-coefs 
  Estimate_sub_models_df <- reshape2::melt(Estimate_sub_models, value.name = "Beta_coef") # Generate a df for ggplot
  
  SD_sub_models <- Infos_sub_models[,,,"SD"]                           # Extract sd
  SD_sub_models_df <- reshape2::melt(SD_sub_models, value.name = "SD")   # Generate a df for ggplot
  
  Signif_sub_models <- Infos_sub_models[,,,"Signif_lvl"]                                 # Extract signifiance levels
  Signif_sub_models_df <- reshape2::melt(Signif_sub_models, value.name = "Signif_lvl")   # Generate a df for ggplot
  
  # # Remove lignes with NA , useless for plots
  # Estimate_sub_models_df <- Estimate_sub_models_df[!is.na(Estimate_sub_models_df$Beta_coef),]
  # SD_sub_models_df <- SD_sub_models_df[!is.na(SD_sub_models_df$SD),]
  # Signif_sub_models_df <- Signif_sub_models_df[!is.na(Signif_sub_models_df$Signif_lvl),]
  
  eval(call("<-", as.name(paste0("Estimate_sub_models_df", i)), Estimate_sub_models_df[!is.na(Estimate_sub_models_df$Beta_coef),]))
  eval(call("<-", as.name(paste0("SD_sub_models_df", i)), SD_sub_models_df[!is.na(SD_sub_models_df$SD),]))
  eval(call("<-", as.name(paste0("Signif_sub_models_df", i)), Signif_sub_models_df[!is.na(Signif_sub_models_df$Signif_lvl),]))
  
  # pdf(file = paste0("./SM_Figures/SM_5_Summary_no_polar/Beta_coeffs_summary_",VAE,".pdf"), width = 8, height = 6)
  
  # Declare plot
  g <- ggplot(Estimate_sub_models_df2, aes(x = VE, y = Beta_coef, 
                                           ymin = Estimate_sub_models_df2$Beta_coef-SD_sub_models_df2$SD, 
                                           ymax = Estimate_sub_models_df2$Beta_coef+SD_sub_models_df2$SD,
                                           # color = Estimate_sub_models_df2$polar, 
                                           # fill = Estimate_sub_models_df2$polar,
                                           color = as.factor(c("With", "Without"))[-as.numeric(Estimate_sub_models_df2$polar)+3],
                                           # shape = Estimate_sub_models_df2$polar,
                                           size = as.factor(c("p > 0.05", "p < 0.05", "p < 0.01", "p < 0.001"))[Signif_sub_models_df2$Signif_lvl+1]
  ))
  
  # Ajout des points
  g <- g + geom_pointrange(# y = Estimate_sub_models_df2$Beta_coef,
    # ymin = Estimate_sub_models_df2$Beta_coef-SD_sub_models_df2$SD,
    # ymax = Estimate_sub_models_df2$Beta_coef+SD_sub_models_df2$SD,
    # color = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")[Estimate_sub_models_df2$model_legend], 
    # fill = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")[Estimate_sub_models_df2$model_legend],
    # size = 0.8,
    # shape = c(16,17)[Estimate_sub_models_df2$polar],
    position = position_dodge(width = 0.5),
    show.legend = T) 
  
  # Gestion de l'esth?tique
  g <- g + theme_bw() + ggtitle("(b) Insect richness") +  
    ylab(expression(paste(beta, "-coefficients"))) + 
    # xlab("Explanatory variables") +
    theme(plot.title = element_text(hjust = 0, vjust = 0.5, size = 13, face = "bold"),  # Titre
          # plot.title = element_blank(),
          axis.title.x = element_blank(),
          # axis.title.x = element_text(size=15, vjust=-1.3, color = 'black'),  # Titre x-axis
          axis.text.x = element_text(size=10, vjust=0, color = 'black'),      # Labels x-axis
          axis.title.y = element_blank(),
          # axis.title.y = element_text(size=15, vjust=2, color = 'black'),     # Titre y-axis
          axis.text.y = element_text(size=10, vjust=0, color = 'black'),      # Labels y-axis
          legend.title = element_text(face="bold"),
          legend.position = "none",
          plot.margin = unit(c(5.5,5.5,15,5.5),"pt")
    ) +
    # Modification manuelle des couleurs des points
    # scale_fill_manual(values = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")) +
    # guides(colour = guide_legend("my awesome title")) +
    # scale_colour_discrete("Continents") +
    scale_color_manual(values = c("dodgerblue3", "red")) +
    # Modification manuelle de la taille des points
    scale_size_manual(values = c(1.4, 1.1, 0.8, 0.4), drop = F) +
    # Modification des limites et breaks de l'axe des y
    scale_y_continuous(limits = c(min(Estimate_sub_models_df2$Beta_coef-SD_sub_models_df2$SD), max(Estimate_sub_models_df2$Beta_coef+SD_sub_models_df2$SD)),
                       # scale_y_continuous(limits = c(-0.15, 0.15),
                       breaks = round(seq(from = (floor(min(Estimate_sub_models_df2$Beta_coef-SD_sub_models_df2$SD)*10))/10, to = (ceiling(max(Estimate_sub_models_df2$Beta_coef+SD_sub_models_df2$SD)*10))/10, by = 0.1),1)) +
    guides(col=guide_legend("Polar networks", order = 1),
           size=guide_legend("Significance levels", order = 2)) +
    # Ligne ? y = 0
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
  
  # # Create fixed text
  # grob <- grobTree(textGrob("(e) Insect richness", x=0.03,  y=0.95, hjust=0,
  #                           gp=gpar(fontsize=13, fontface="bold")))
  # # Add to graph
  # g <- g + annotation_custom(grob)
  
  # Store plot with index
  eval(call("<-", as.name(paste0("g", i)), g))
  
  print(g)
  
  # dev.off()       
  
  save(g2, Estimate_sub_models_df2, SD_sub_models_df2, Signif_sub_models_df2, file = "./Data/SM_5/Compared_I_plot_data.RData")
  
  cat(paste("\n", Sys.time(), "-", VAE, "- Done \n")) 
  
}

# Reload previous graph parameter settings
par(mar = mar_old_settings) 


### 6.3/ Plant richness ####

i <- 3 ; VAE <- "P"
for (VAE in VAE) {
  
  ### Choose manually
  # VAE <- "C"
  # VAE <- "Li"
  # VAE <- "Lp"
  # VAE <- "S"
  # VAE <- "I"
  # VAE <- "P"
  ###
  
  # Extract only infos for this VAE and MAM models
  Infos_sub_models <- Infos_all_models_polar[VAE, , , , , "MAM", ] 
  
  # Convert VE name to proper short names for plots
  dimnames(Infos_sub_models)$VE <- VE_short_name_list
  
  # dimnames(Infos_sub_models)
  
  # Extract infos and format into df
  Estimate_sub_models <- Infos_sub_models[,,,"Beta_coef"]     # Extract beta-coefs 
  Estimate_sub_models_df <- reshape2::melt(Estimate_sub_models, value.name = "Beta_coef") # Generate a df for ggplot
  
  SD_sub_models <- Infos_sub_models[,,,"SD"]                           # Extract sd
  SD_sub_models_df <- reshape2::melt(SD_sub_models, value.name = "SD")   # Generate a df for ggplot
  
  Signif_sub_models <- Infos_sub_models[,,,"Signif_lvl"]                                 # Extract signifiance levels
  Signif_sub_models_df <- reshape2::melt(Signif_sub_models, value.name = "Signif_lvl")   # Generate a df for ggplot
  
  # # Remove lignes with NA , useless for plots
  # Estimate_sub_models_df <- Estimate_sub_models_df[!is.na(Estimate_sub_models_df$Beta_coef),]
  # SD_sub_models_df <- SD_sub_models_df[!is.na(SD_sub_models_df$SD),]
  # Signif_sub_models_df <- Signif_sub_models_df[!is.na(Signif_sub_models_df$Signif_lvl),]
  
  eval(call("<-", as.name(paste0("Estimate_sub_models_df", i)), Estimate_sub_models_df[!is.na(Estimate_sub_models_df$Beta_coef),]))
  eval(call("<-", as.name(paste0("SD_sub_models_df", i)), SD_sub_models_df[!is.na(SD_sub_models_df$SD),]))
  eval(call("<-", as.name(paste0("Signif_sub_models_df", i)), Signif_sub_models_df[!is.na(Signif_sub_models_df$Signif_lvl),]))
  
  # pdf(file = paste0("./SM_Figures/SM_5_Summary_no_polar/Beta_coeffs_summary_",VAE,".pdf"), width = 8, height = 6)
  
  # Declare plot
  g <- ggplot(Estimate_sub_models_df3, aes(x = VE, y = Beta_coef, 
                                           ymin = Estimate_sub_models_df3$Beta_coef-SD_sub_models_df3$SD, 
                                           ymax = Estimate_sub_models_df3$Beta_coef+SD_sub_models_df3$SD,
                                           # color = Estimate_sub_models_df3$polar, 
                                           # fill = Estimate_sub_models_df3$polar,
                                           color = as.factor(c("With", "Without"))[-as.numeric(Estimate_sub_models_df3$polar)+3],
                                           # shape = Estimate_sub_models_df3$polar,
                                           size = as.factor(c("p > 0.05", "p < 0.05", "p < 0.01", "p < 0.001"))[Signif_sub_models_df3$Signif_lvl+1]
  ))
  
  # Ajout des points
  g <- g + geom_pointrange(# y = Estimate_sub_models_df3$Beta_coef,
    # ymin = Estimate_sub_models_df3$Beta_coef-SD_sub_models_df3$SD,
    # ymax = Estimate_sub_models_df3$Beta_coef+SD_sub_models_df3$SD,
    # color = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")[Estimate_sub_models_df3$model_legend], 
    # fill = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")[Estimate_sub_models_df3$model_legend],
    # size = 0.8,
    # shape = c(16,17)[Estimate_sub_models_df3$polar],
    position = position_dodge(width = 0.5),
    show.legend = T) 
  
  # Gestion de l'esth?tique
  g <- g + theme_bw() + ggtitle("(c) Plant richness") +  
    ylab(expression(paste(beta, "-coefficients"))) + 
    # xlab("Explanatory variables") +
    theme(plot.title = element_text(hjust = 0, vjust = 0.5, size = 13, face = "bold"),  # Titre
          # plot.title = element_blank(),
          axis.title.x = element_blank(),
          # axis.title.x = element_text(size=15, vjust=-1.3, color = 'black'),  # Titre x-axis
          axis.text.x = element_text(size=10, vjust=0, color = 'black'),        # Labels x-axis
          axis.title.y = element_blank(),
          # axis.title.y = element_text(size=15, vjust=2, color = 'black'),     # Titre y-axis
          axis.text.y = element_text(size=10, vjust=0, color = 'black'),        # Labels y-axis
          legend.title = element_text(face="bold"),
          legend.position = "right",
          plot.margin = unit(c(5.5,5.5,15,5.5),"pt")
    ) +
    # Modification manuelle des couleurs des points
    # scale_fill_manual(values = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")) +
    # guides(colour = guide_legend("my awesome title")) +
    # scale_colour_discrete("Continents") +
    scale_color_manual(values = c("dodgerblue3", "red")) +
    # Modification manuelle de la taille des points
    scale_size_manual(values = c(1.4, 1.1, 0.8, 0.4), drop = F) +
    # Modification des limites et breaks de l'axe des y
    scale_y_continuous(limits = c(min(Estimate_sub_models_df3$Beta_coef-SD_sub_models_df3$SD), max(Estimate_sub_models_df3$Beta_coef+SD_sub_models_df3$SD)),
                       # scale_y_continuous(limits = c(-0.15, 0.15),
                       breaks = round(seq(from = (floor(min(Estimate_sub_models_df3$Beta_coef-SD_sub_models_df3$SD)*10))/10, to = (ceiling(max(Estimate_sub_models_df3$Beta_coef+SD_sub_models_df3$SD)*10))/10, by = 0.1),1)) +
    guides(col=guide_legend("Polar networks", order = 1),
           size=guide_legend("Significance levels", order = 2)) +
    # Ligne ? y = 0
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
  
  # # Create fixed text
  # grob <- grobTree(textGrob("(f) Plant richness", x=0.03,  y=0.95, hjust=0,
  #                           gp=gpar(fontsize=13, fontface="bold")))
  # # Add to graph
  # g <- g + annotation_custom(grob)
  
  # Store plot with index
  eval(call("<-", as.name(paste0("g", i)), g))
  
  print(g)
  
  # dev.off()       
  
  save(g3, Estimate_sub_models_df3, SD_sub_models_df3, Signif_sub_models_df3, file = "./Data/SM_5/Compared_P_plot_data.RData")
  
  cat(paste("\n", Sys.time(), "-", VAE, "- Done \n")) 
  
}

# Reload previous graph parameter settings
par(mar = mar_old_settings) 


### 6.4/ Connectance ####

i <- 4 ; VAE <- "C"
for (VAE in VAE) {
  
  ### Choose manually
  # VAE <- "C"
  # VAE <- "Li"
  # VAE <- "Lp"
  # VAE <- "S"
  # VAE <- "I"
  # VAE <- "P"
  ###
  
  # Extract only infos for this VAE and MAM models
  Infos_sub_models <- Infos_all_models_polar[VAE, , , , , "MAM", ] 
  
  # Convert VE name to proper short names for plots
  dimnames(Infos_sub_models)$VE <- VE_short_name_list
  
  # dimnames(Infos_sub_models)
  
  # Extract infos and format into df
  Estimate_sub_models <- Infos_sub_models[,,,"Beta_coef"]     # Extract beta-coefs 
  Estimate_sub_models_df <- reshape2::melt(Estimate_sub_models, value.name = "Beta_coef") # Generate a df for ggplot
  
  SD_sub_models <- Infos_sub_models[,,,"SD"]                           # Extract sd
  SD_sub_models_df <- reshape2::melt(SD_sub_models, value.name = "SD")   # Generate a df for ggplot
  
  Signif_sub_models <- Infos_sub_models[,,,"Signif_lvl"]                                 # Extract signifiance levels
  Signif_sub_models_df <- reshape2::melt(Signif_sub_models, value.name = "Signif_lvl")   # Generate a df for ggplot
  
  # # Remove lignes with NA , useless for plots
  # Estimate_sub_models_df <- Estimate_sub_models_df[!is.na(Estimate_sub_models_df$Beta_coef),]
  # SD_sub_models_df <- SD_sub_models_df[!is.na(SD_sub_models_df$SD),]
  # Signif_sub_models_df <- Signif_sub_models_df[!is.na(Signif_sub_models_df$Signif_lvl),]
  
  eval(call("<-", as.name(paste0("Estimate_sub_models_df", i)), Estimate_sub_models_df[!is.na(Estimate_sub_models_df$Beta_coef),]))
  eval(call("<-", as.name(paste0("SD_sub_models_df", i)), SD_sub_models_df[!is.na(SD_sub_models_df$SD),]))
  eval(call("<-", as.name(paste0("Signif_sub_models_df", i)), Signif_sub_models_df[!is.na(Signif_sub_models_df$Signif_lvl),]))
  
  # pdf(file = paste0("./SM_Figures/SM_5_Summary_no_polar/Beta_coeffs_summary_",VAE,".pdf"), width = 8, height = 6)
  
  # Declare plot
  g <- ggplot(Estimate_sub_models_df4, aes(x = VE, y = Beta_coef, 
                                           ymin = Estimate_sub_models_df4$Beta_coef-SD_sub_models_df4$SD, 
                                           ymax = Estimate_sub_models_df4$Beta_coef+SD_sub_models_df4$SD,
                                           # color = Estimate_sub_models_df4$polar, 
                                           # fill = Estimate_sub_models_df4$polar,
                                           color = as.factor(c("With", "Without"))[-as.numeric(Estimate_sub_models_df4$polar)+3],
                                           # shape = Estimate_sub_models_df4$polar,
                                           size = as.factor(c("p > 0.05", "p < 0.05", "p < 0.01", "p < 0.001"))[Signif_sub_models_df4$Signif_lvl+1]
  ))
  
  # Ajout des points
  g <- g + geom_pointrange(# y = Estimate_sub_models_df4$Beta_coef,
    # ymin = Estimate_sub_models_df4$Beta_coef-SD_sub_models_df4$SD,
    # ymax = Estimate_sub_models_df4$Beta_coef+SD_sub_models_df4$SD,
    # color = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")[Estimate_sub_models_df4$model_legend], 
    # fill = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")[Estimate_sub_models_df4$model_legend],
    # size = 0.8,
    # shape = c(16,17)[Estimate_sub_models_df4$polar],
    position = position_dodge(width = 0.5),
    show.legend = T) 
  
  # Gestion de l'esth?tique
  g <- g + theme_bw() + ggtitle("(d) Connectance") +  
    ylab(expression(paste(beta, "-coefficients"))) + 
    xlab("Explanatory variables") +
    theme(plot.title = element_text(hjust = 0, vjust = 0.5, size = 13, face = "bold"),  # Titre
          # plot.title = element_blank(),
          # axis.title.x = element_blank(),
          axis.title.x = element_text(size=15, vjust=-0.8, color = 'black'),   # Titre x-axis
          axis.text.x = element_text(size=10, vjust=0, color = 'black'),       # Labels x-axis
          axis.title.y = element_text(size=15, vjust=1.7, color = 'black'),    # Titre y-axis
          axis.text.y = element_text(size=10, vjust=0, color = 'black'),       # Labels y-axis
          legend.title = element_text(face="bold"),
          legend.position = "none",
          plot.margin = unit(c(5.5,5.5,5,12.5),"pt") # top, right, bottom and left
    ) +
    # Manual setting of point colors
    # scale_fill_manual(values = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")) +
    # guides(colour = guide_legend("my awesome title")) +
    # scale_colour_discrete("Continents") +
    scale_color_manual(values = c("dodgerblue3", "red")) +
    # Manual settings of point sizes
    scale_size_manual(values = c(1.4, 1.1, 0.8, 0.4), drop = F) +
    # Manual settings of y-axis breaks
    scale_y_continuous(limits = c(min(Estimate_sub_models_df4$Beta_coef-SD_sub_models_df4$SD), max(Estimate_sub_models_df4$Beta_coef+SD_sub_models_df4$SD)),
                       # scale_y_continuous(limits = c(-0.15, 0.15),
                       breaks = round(seq(from = (floor(min(Estimate_sub_models_df4$Beta_coef-SD_sub_models_df4$SD)*10))/10, to = (ceiling(max(Estimate_sub_models_df4$Beta_coef+SD_sub_models_df4$SD)*10))/10, by = 0.1),1)) +
    guides(col=guide_legend("Polar networks", order = 1),
           size=guide_legend("Significance levels", order = 2)) +
    # Ass a line for y = 0
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
  
  # # Create fixed text
  # grob <- grobTree(textGrob("(a) Connectance", x=0.03,  y=0.95, hjust=0,
  #                           gp=gpar(fontsize=13, fontface="bold")))
  # # Add to graph
  # g <- g + annotation_custom(grob)
  
  # Store plot with index
  eval(call("<-", as.name(paste0("g", i)), g))
  
  print(g)
  
  # dev.off()   
  
  save(g4, Estimate_sub_models_df4, SD_sub_models_df4, Signif_sub_models_df4, file = "./Data/SM_5/Compared_C_plot_data.RData")
  
  cat(paste("\n", Sys.time(), "-", VAE, "- Done \n")) 
  
}

# Reload previous graph parameter settings
par(mar = mar_old_settings)      

### 6.5/ Link Density of insects ####

i <- 5 ; VAE <- "Li"
for (VAE in VAE) {
  
  ### Choose manually
  # VAE <- "C"
  # VAE <- "Li"
  # VAE <- "Lp"
  # VAE <- "S"
  # VAE <- "I"
  # VAE <- "P"
  ###
  
  # Extract only infos for this VAE and MAM models
  Infos_sub_models <- Infos_all_models_polar[VAE, , , , , "MAM", ] 
  
  # Convert VE name to proper short names for plots
  dimnames(Infos_sub_models)$VE <- VE_short_name_list
  
  # dimnames(Infos_sub_models)
  
  # Extract infos and format into df
  Estimate_sub_models <- Infos_sub_models[,,,"Beta_coef"]     # Extract beta-coefs 
  Estimate_sub_models_df <- reshape2::melt(Estimate_sub_models, value.name = "Beta_coef") # Generate a df for ggplot
  
  SD_sub_models <- Infos_sub_models[,,,"SD"]                           # Extract sd
  SD_sub_models_df <- reshape2::melt(SD_sub_models, value.name = "SD")   # Generate a df for ggplot
  
  Signif_sub_models <- Infos_sub_models[,,,"Signif_lvl"]                                 # Extract signifiance levels
  Signif_sub_models_df <- reshape2::melt(Signif_sub_models, value.name = "Signif_lvl")   # Generate a df for ggplot
  
  # # Remove lignes with NA , useless for plots
  # Estimate_sub_models_df <- Estimate_sub_models_df[!is.na(Estimate_sub_models_df$Beta_coef),]
  # SD_sub_models_df <- SD_sub_models_df[!is.na(SD_sub_models_df$SD),]
  # Signif_sub_models_df <- Signif_sub_models_df[!is.na(Signif_sub_models_df$Signif_lvl),]
  
  eval(call("<-", as.name(paste0("Estimate_sub_models_df", i)), Estimate_sub_models_df[!is.na(Estimate_sub_models_df$Beta_coef),]))
  eval(call("<-", as.name(paste0("SD_sub_models_df", i)), SD_sub_models_df[!is.na(SD_sub_models_df$SD),]))
  eval(call("<-", as.name(paste0("Signif_sub_models_df", i)), Signif_sub_models_df[!is.na(Signif_sub_models_df$Signif_lvl),]))
  
  # pdf(file = paste0("./SM_Figures/SM_5_Summary_no_polar/Beta_coeffs_summary_",VAE,".pdf"), width = 8, height = 6)
  
  # Declare plot
  g <- ggplot(Estimate_sub_models_df5, aes(x = VE, y = Beta_coef, 
                                           ymin = Estimate_sub_models_df5$Beta_coef-SD_sub_models_df5$SD, 
                                           ymax = Estimate_sub_models_df5$Beta_coef+SD_sub_models_df5$SD,
                                           # color = Estimate_sub_models_df5$polar, 
                                           # fill = Estimate_sub_models_df5$polar,
                                           color = as.factor(c("With", "Without"))[-as.numeric(Estimate_sub_models_df5$polar)+3],
                                           # shape = Estimate_sub_models_df5$polar,
                                           size = as.factor(c("p > 0.05", "p < 0.05", "p < 0.01", "p < 0.001"))[Signif_sub_models_df5$Signif_lvl+1]
  ))
  
  # Ajout des points
  g <- g + geom_pointrange(# y = Estimate_sub_models_df5$Beta_coef,
    # ymin = Estimate_sub_models_df5$Beta_coef-SD_sub_models_df5$SD,
    # ymax = Estimate_sub_models_df5$Beta_coef+SD_sub_models_df5$SD,
    # color = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")[Estimate_sub_models_df5$model_legend], 
    # fill = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")[Estimate_sub_models_df5$model_legend],
    # size = 0.8,
    # shape = c(16,17)[Estimate_sub_models_df5$polar],
    position = position_dodge(width = 0.5),
    show.legend = T) 
  
  # Gestion de l'esth?tique
  g <- g + theme_bw() + ggtitle("(e) Insect link density") +  
    ylab(expression(paste(beta, "-coefficients"))) + 
    xlab("Explanatory variables") +
    theme(plot.title = element_text(hjust = 0, vjust = 0.5, size = 13, face = "bold"),  # Titre
          # plot.title = element_blank(),
          # axis.title.x = element_blank(),
          axis.title.x = element_text(size=15, vjust=-0.8, color = 'black'),  # Titre x-axis
          axis.text.x = element_text(size=10, vjust=0, color = 'black'),      # Labels x-axis
          axis.title.y = element_blank(),
          # axis.title.y = element_text(size=15, vjust=2, color = 'black'),     # Titre y-axis
          axis.text.y = element_text(size=10, vjust=0, color = 'black'),      # Labels y-axis
          legend.title = element_text(face="bold"),
          legend.position = "none"
    ) +
    # Modification manuelle des couleurs des points
    # scale_fill_manual(values = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")) +
    # guides(colour = guide_legend("my awesome title")) +
    # scale_colour_discrete("Continents") +
    scale_color_manual(values = c("dodgerblue3", "red")) +
    # Modification manuelle de la taille des points
    scale_size_manual(values = c(1.4, 1.1, 0.8, 0.4), drop = F) +
    # Modification des limites et breaks de l'axe des y
    scale_y_continuous(limits = c(min(Estimate_sub_models_df5$Beta_coef-SD_sub_models_df5$SD), max(Estimate_sub_models_df5$Beta_coef+SD_sub_models_df5$SD)),
                       # scale_y_continuous(limits = c(-0.15, 0.15),
                       breaks = round(seq(from = (floor(min(Estimate_sub_models_df5$Beta_coef-SD_sub_models_df5$SD)*10))/10, to = (ceiling(max(Estimate_sub_models_df5$Beta_coef+SD_sub_models_df5$SD)*10))/10, by = 0.1),1)) +
    guides(col=guide_legend("Polar networks", order = 1),
           size=guide_legend("Significance levels", order = 2)) +
    # Ligne ? y = 0
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
  
  # # Create fixed text
  # grob <- grobTree(textGrob("(b) Insect link density", x=0.03,  y=0.95, hjust=0,
  #                           gp=gpar(fontsize=13, fontface="bold")))
  # # Add to graph
  # g <- g + annotation_custom(grob)
  
  # Store plot with index
  eval(call("<-", as.name(paste0("g", i)), g))
  
  print(g)
  
  # dev.off()       
  
  save(g5, Estimate_sub_models_df5, SD_sub_models_df5, Signif_sub_models_df5, file = "./Data/SM_5/Compared_Li_plot_data.RData")
  
  cat(paste("\n", Sys.time(), "-", VAE, "- Done \n")) 
  
}

# Reload previous graph parameter settings
par(mar = mar_old_settings) 


### 6.6/ Link Density of plants ####

i <- 6 ; VAE <- "Lp"
for (VAE in VAE) {
  
  ### Choose manually
  # VAE <- "C"
  # VAE <- "Li"
  # VAE <- "Lp"
  # VAE <- "S"
  # VAE <- "I"
  # VAE <- "P"
  ###
  
  # Extract only infos for this VAE and MAM models
  Infos_sub_models <- Infos_all_models_polar[VAE, , , , , "MAM", ] 
  
  # Convert VE name to proper short names for plots
  dimnames(Infos_sub_models)$VE <- VE_short_name_list
  
  # dimnames(Infos_sub_models)
  
  # Extract infos and format into df
  Estimate_sub_models <- Infos_sub_models[,,,"Beta_coef"]     # Extract beta-coefs 
  Estimate_sub_models_df <- reshape2::melt(Estimate_sub_models, value.name = "Beta_coef") # Generate a df for ggplot
  
  SD_sub_models <- Infos_sub_models[,,,"SD"]                           # Extract sd
  SD_sub_models_df <- reshape2::melt(SD_sub_models, value.name = "SD")   # Generate a df for ggplot
  
  Signif_sub_models <- Infos_sub_models[,,,"Signif_lvl"]                                 # Extract signifiance levels
  Signif_sub_models_df <- reshape2::melt(Signif_sub_models, value.name = "Signif_lvl")   # Generate a df for ggplot
  
  # # Remove lignes with NA , useless for plots
  # Estimate_sub_models_df <- Estimate_sub_models_df[!is.na(Estimate_sub_models_df$Beta_coef),]
  # SD_sub_models_df <- SD_sub_models_df[!is.na(SD_sub_models_df$SD),]
  # Signif_sub_models_df <- Signif_sub_models_df[!is.na(Signif_sub_models_df$Signif_lvl),]
  
  eval(call("<-", as.name(paste0("Estimate_sub_models_df", i)), Estimate_sub_models_df[!is.na(Estimate_sub_models_df$Beta_coef),]))
  eval(call("<-", as.name(paste0("SD_sub_models_df", i)), SD_sub_models_df[!is.na(SD_sub_models_df$SD),]))
  eval(call("<-", as.name(paste0("Signif_sub_models_df", i)), Signif_sub_models_df[!is.na(Signif_sub_models_df$Signif_lvl),]))
  
  # pdf(file = paste0("./SM_Figures/SM_5_Summary_no_polar/Beta_coeffs_summary_",VAE,".pdf"), width = 8, height = 6)
  
  # Declare plot
  g <- ggplot(Estimate_sub_models_df6, aes(x = VE, y = Beta_coef, 
                                           ymin = Estimate_sub_models_df6$Beta_coef-SD_sub_models_df6$SD, 
                                           ymax = Estimate_sub_models_df6$Beta_coef+SD_sub_models_df6$SD,
                                           # color = Estimate_sub_models_df6$polar, 
                                           # fill = Estimate_sub_models_df6$polar,
                                           color = as.factor(c("With", "Without"))[-as.numeric(Estimate_sub_models_df6$polar)+3],
                                           # shape = Estimate_sub_models_df6$polar,
                                           size = as.factor(c("p > 0.05", "p < 0.05", "p < 0.01", "p < 0.001"))[Signif_sub_models_df6$Signif_lvl+1]
  ))
  
  # Ajout des points
  g <- g + geom_pointrange(# y = Estimate_sub_models_df6$Beta_coef,
    # ymin = Estimate_sub_models_df6$Beta_coef-SD_sub_models_df6$SD,
    # ymax = Estimate_sub_models_df6$Beta_coef+SD_sub_models_df6$SD,
    # color = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")[Estimate_sub_models_df6$model_legend], 
    # fill = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")[Estimate_sub_models_df6$model_legend],
    # size = 0.8,
    # shape = c(16,17)[Estimate_sub_models_df6$polar],
    position = position_dodge(width = 0.5),
    show.legend = T) 
  
  # Gestion de l'esth?tique
  g <- g + theme_bw() + ggtitle("(f) Plant link density") +  
    ylab(expression(paste(beta, "-coefficients"))) + 
    xlab("Explanatory variables") +
    theme(plot.title = element_text(hjust = 0, vjust = 0.5, size = 13, face = "bold"),  # Titre
          # plot.title = element_blank(),
          # axis.title.x = element_blank(),
          axis.title.x = element_text(size=15, vjust=-0.8, color = 'black'),  # Titre x-axis
          axis.text.x = element_text(size=10, vjust=0, color = 'black'),      # Labels x-axis
          axis.title.y = element_blank(),
          # axis.title.y = element_text(size=15, vjust=2, color = 'black'),     # Titre y-axis
          axis.text.y = element_text(size=10, vjust=0, color = 'black'),      # Labels y-axis
          legend.title = element_text(face="bold"),
          legend.position = "right"
    ) +
    # Modification manuelle des couleurs des points
    # scale_fill_manual(values = c("red", "limegreen", "darkorange", "black", "dodgerblue", "darkviolet", "grey50", "wheat")) +
    # guides(colour = guide_legend("my awesome title")) +
    # scale_colour_discrete("Continents") +
    scale_color_manual(values = c("dodgerblue3", "red")) +
    # Modification manuelle de la taille des points
    scale_size_manual(values = c(1.4, 1.1, 0.8, 0.4), drop = F) +
    # Modification des limites et breaks de l'axe des y
    scale_y_continuous(limits = c(min(Estimate_sub_models_df6$Beta_coef-SD_sub_models_df6$SD), max(Estimate_sub_models_df6$Beta_coef+SD_sub_models_df6$SD)),
                       # scale_y_continuous(limits = c(-0.15, 0.15),
                       breaks = round(seq(from = (floor(min(Estimate_sub_models_df6$Beta_coef-SD_sub_models_df6$SD)*10))/10, to = (ceiling(max(Estimate_sub_models_df6$Beta_coef+SD_sub_models_df6$SD)*10))/10, by = 0.2),1)) +
    guides(col=guide_legend("Polar networks", order = 1),
           size=guide_legend("Significance levels", order = 2)) +
    # Ligne ? y = 0
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
  
  # # Create fixed text
  # grob <- grobTree(textGrob("(c) Plant link density", x=0.03,  y=0.95, hjust=0,
  #                           gp=gpar(fontsize=13, fontface="bold")))
  # # Add to graph
  # g <- g + annotation_custom(grob)
  
  # Store plot with index
  eval(call("<-", as.name(paste0("g", i)), g))
  
  print(g)
  
  # dev.off()       
  
  save(g6, Estimate_sub_models_df6, SD_sub_models_df6, Signif_sub_models_df6, file = "./Data/SM_5/Compared_Lp_plot_data.RData")
  
  cat(paste("\n", Sys.time(), "-", VAE, "- Done \n")) 
  
}

# Reload previous graph parameter settings
par(mar = mar_old_settings) 


### Plot all subplots

print(g1)
print(g2)
print(g3)
print(g4)
print(g5)
print(g6)



### 6.7/ Final Layout ####

load(file = "./Data/SM_5/Compared_C_plot_data.RData")
load(file = "./Data/SM_5/Compared_Li_plot_data.RData")
load(file = "./Data/SM_5/Compared_Lp_plot_data.RData")
load(file = "./Data/SM_5/Compared_S_plot_data.RData")
load(file = "./Data/SM_5/Compared_I_plot_data.RData")
load(file = "./Data/SM_5/Compared_P_plot_data.RData")

library("gridExtra")

?grid.arrange


pdf(file = paste0("./SM_Final_figures/Figure_S5.2.pdf"), width = 15, height = 8)

grid.arrange(
  grobs = list(g1, g2, g3, g4, g5, g6),
  widths = c(12.3, 11.5, 16),
  heights = c(7.2, 0.1, 7.5),
  layout_matrix = rbind(c(1, 2, 3),c(NA,NA,NA),
                        c(4, 5, 6))
)

dev.off()


