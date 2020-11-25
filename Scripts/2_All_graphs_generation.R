# Author: Maël DORÉ
# Contact: mael.dore@gmail.com
# License: MIT

##### Script 2: Generate all possible graphs #####


# Inputs:

  # Table with all networks variables (aggreg.webs)
  # Results of calibrated models from Script 1 (models, and summary tables)

# Outputs:

  # Single (one web coverage) and multiple plots (all web coverage) in PDF
  # Predict tables with values used to draw the predicts on the plots
  # Summary table with information on graph execution (all_single_plots_summary_all_VE)
  # Summary table with information on mulitple graph execution (all_multiple_plots_summary_all_VE)



##################################### Summary #########################################

# 1/ Generate Single plots (i.e., 1 VAE ~ 1 VE) for one web coverage

    # Single plots are "needed" to build the final multiple plot with all web_coverage displayed

# 2/ Multiples Plots (with all web_coverage)

#######################################################################################



### Clear memory
rm(list = ls())
gc() # Force R to release memory it is no longer using


####### 1/ Single plots (i.e., 1 VAE ~ 1 VE) for one web coverage #######

##### 1.1/ Generate a summary table to keep track of all single plot (response variable ~ explanatory variable) possible  ####

# Single plots are "needed" to build the final multiple plot with all web_coverage displayed

# 18 models x 12 possible explanatory variables = 216 #### But need to remove impossible combination afterwards

all_single_plots_summary_all_VE <- data.frame(matrix(nrow = 216, ncol = 15, dimnames = list(NULL ,c("ID_plot", "ID_model", "web_coverage", "model_type", "polar", "VAE", "model_dataset", "model_name", "VE", "VE_short_name", "Exists", "Covar", "Beta_coef", "p_value", "Last_runtime"))))

# load( file = "./Data/all_single_plots_summary_all_VE.RData")

load(file = "./Data/all_models_summary.RData")

VE_list <- c("ln_sptot", "ln_pl", "ln_ins", "ln_SE", "ln_time", "ln_ATS", "ln_HF", "Tot_Rainfall_IPCC", "Mean_T_IPCC", "Rainfall_Seasonality_IPCC", "T_Seasonality_IPCC", "taxo_full_sp_perc")
VE_short_name_list <-  c("S", "P", "I", "SE", "Time", "ATS", "HF", "Ptot", "Tmean", "Pvar", "Tvar", "Taxo")

# Retrieve results of all models

j <- 1
for (i in 1:nrow(all_models_summary)) {
  
  for (VE in VE_list) {
    
    all_single_plots_summary_all_VE[j, 2:8] <- all_models_summary[i, 1:7] # Model infos
    
    all_single_plots_summary_all_VE$VE[j] <- VE # VE name
    all_single_plots_summary_all_VE$VE_short_name[j] <- VE_short_name <- VE_short_name_list[which(VE == VE_list)] # VE short name
    
    # Retrieve bestmodel for this VE to find out the Covar to use for point shading
    VAE <- all_models_summary[i, "VAE"] ; web_coverage <- all_models_summary[i, "web_coverage"] ; model_name <- all_models_summary[i, "model_name"]
    
    # Check if this VE was in the model and that a best model is available
    if (file.exists(file = paste0("./Models_outputs/",VAE,"/",web_coverage,"/",model_name,"/",model_name,"_bestmod_with_", VE_short_name, ".RData"))) {
      
      all_single_plots_summary_all_VE$Exists[j] <- T
      
      bestmodel_for_VE <- readRDS(file = paste0("./Models_outputs/",VAE,"/",web_coverage,"/",model_name,"/",model_name,"_bestmod_with_", VE_short_name, ".RData"))
      
      summary_best_model_for_VE <- summary(bestmodel_for_VE) # Retrieve summary
      
      # Remove fitted (SEV) if presents
      if (length(grep(pattern = "fitted", row.names(summary_best_model_for_VE$coefficients))) != 0 ) {
        summary_best_model_for_VE$coefficients <- summary_best_model_for_VE$coefficients[-grep(pattern = "fitted", row.names(summary_best_model_for_VE$coefficients)),] # Remove ligns associated with SEV
      }
      
      # Remove Sampling Type if present
      if (length(grep(pattern = "Sampling_type", row.names(summary_best_model_for_VE$coefficients))) != 0 ) {
        summary_best_model_for_VE$coefficients <- summary_best_model_for_VE$coefficients[-grep(pattern = "Sampling_type", row.names(summary_best_model_for_VE$coefficients)),] # Remove ligns associated with Sampling_Type
      }
      
      Covar <- names(which(rank(summary_best_model_for_VE$coefficients[2:nrow(summary_best_model_for_VE$coefficients), 4]) == 1)) # Retrieve name of the variable with the most significant (i.e., lowest) p-value
      
      if(Covar == VE) { # When the selected Covariable (with the lowest p-value) is th esame than the VE evaluated, need to change for the second most significant variable
        Covar <- names(which(rank(summary_best_model_for_VE$coefficients[2:nrow(summary_best_model_for_VE$coefficients), 4]) == 2)) # Retrieve name of the variable with the second most significant (i.e., lowest) p-value
      }
      
      all_single_plots_summary_all_VE$Covar[j] <- Covar
      
    } else { # Case when the best model does not exist. Need to keep track to remove this line from the summary table afterwards
      
      all_single_plots_summary_all_VE$Exists[j] <- F
      
    }
    
    j <- j + 1
    
    if (j %% 50 == 0) {
      cat(paste("\n Plot n?", j, "on", nrow(all_single_plots_summary_all_VE), "\n"))
    }
  }
}

# Remove non-existing combinations. In the end, 153 combinations
all_single_plots_summary_all_VE <- all_single_plots_summary_all_VE[all_single_plots_summary_all_VE$Exists,] 
# Provide ID for each plot
all_single_plots_summary_all_VE$ID_plot <- 1:nrow(all_single_plots_summary_all_VE)
row.names(all_single_plots_summary_all_VE) <- all_single_plots_summary_all_VE$ID_plot

# save(all_single_plots_summary_all_VE, file = "./Data/all_single_plots_summary_all_VE.RData")



##### 1.2/ Loop to generate single plots ####

load(file = "./Data/all_single_plots_summary_all_VE.RData")


# To associate the right legend and the right variable name in the dataset with right response variable (VAE)
VAE_list <- c("C", "Li", "Lp", "S", "I", "P" )
VAE_legend_list <- c("Connectance", "Insect Link Density", "Plant Link Density", "Total Richness", "Insect Richness", "Plant Richness")
VAE_dataset_name_list <- c("Connectance", "Li", "Lp", "sptot", "full_insects", "full_plants" )

# To associate the right legend and the right variable name in the dataset with right explanatory variable (VE)
VE_short_name_list <-  c("S", "P", "I", "SE", "Time", "ATS", "HF", "Ptot", "Tmean", "Pvar", "Tvar", "Taxo")
VE_legend_list <- c("Network size", "Partner pool (plants)", "Partner pool (insects)", "Sampling effort", "Sampling time", "Annual time span of sampling", "Human Influence", "Annual total precipitation", "Mean temperature", "Precipitation seasonality", "Temperature seasonality", "Taxonomic resolution")
VE_axis_name_list <- c("sptot", "full_plants", "full_insects", "Sampling_effort", "Sampling_time", "Annual_time_span", "HF", "Tot_Rainfall_IPCC", "Mean_T_IPCC", "Rainfall_Seasonality_IPCC", "T_Seasonality_IPCC", "taxo_full_sp_perc")



k <- 1
for (j in 1:nrow(all_single_plots_summary_all_VE)) {
  
  # Load dataset
  model_dataset <- all_single_plots_summary_all_VE$model_dataset[j]
  aggreg.webs <- readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",model_dataset,".RData"))
  
  # Chose the VAE
  VAE <- all_single_plots_summary_all_VE$VAE[j]
  VAE_legend <- VAE_legend_list[which(VAE_list == VAE)]
  VAE_data <- eval(parse(text = paste0("aggreg.webs$", VAE_dataset_name_list[which(VAE_list == VAE)])))
  
  # Chose the VE
  VE <- VE_axis_name <- all_single_plots_summary_all_VE$VE[j]
  VE_short_name <- all_single_plots_summary_all_VE$VE_short_name[j]
  VE_legend <- VE_legend_list[which(VE_short_name_list == VE_short_name)]
  VE_data <- eval(parse(text = paste0("aggreg.webs$",VE)))
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
  
  # Load model
  web_coverage <- all_single_plots_summary_all_VE$web_coverage[j]
  model_name <- all_single_plots_summary_all_VE$model_name[j]
  model <- readRDS(file = paste0("./Models_outputs/",VAE,"/",web_coverage,"/",model_name,"/",model_name,"_bestmod_with_", VE_short_name, ".RData")) 
  
  # Extract beta-coef and the p-value
  model_std <- MuMIn::std.coef(x = model, partial.sd = T)
  summary_model_std <- summary(model_std)
  
  Beta_coef <- round(summary_model_std  # Matrix of best model results
                     [grep(VE, row.names(model_std)),1], 4) # Extraction of beta-coef
  all_single_plots_summary_all_VE$Beta_coef[j] <- Beta_coef
  
  p_value <- round(summary_model_std  # Matrix of best model results
                   [grep(VE, row.names(model_std)),5], 4) # Extraction of p-value
  all_single_plots_summary_all_VE$p_value[j] <- p_value
  
  # Load information to retrieve output pathway
  model_type <- all_single_plots_summary_all_VE$model_type[j]
  polar <- all_single_plots_summary_all_VE$polar[j]
  
  ### Plot
  
  # Create a function to generate a continuous color palette
  rbPal <- colorRampPalette(c('grey80','black')) 
  
  # Choose your covariable
  Covar <- all_single_plots_summary_all_VE$Covar[j]
  
  var = model$model[,grep(Covar, names(model$model))] # Load covariable
  
  aggreg.webs$Col <- rbPal(10)[as.numeric(cut(var,breaks = 10))] # This adds a column of color values based on the values of a variable
  
  ### Compute predict with other variables set to data mean
  
  # Extract model data
  index_SEV <- grep(pattern = "fitted", attr(model$terms,"term.labels")) # To detect SEV presence
  if (length(index_SEV)>0) { # In case of SEV in th emodel
    coefs <- as.numeric(model$coefficients)[1:index_SEV]
    names_var <- names(model$coefficients[2:index_SEV]) # Remove coefficients associated with SEV
    newdata <- model$model[2:(index_SEV)] # Extract VE data, except SEV
  }else{ # In case of SEV abscence
    coefs <- as.numeric(model$coefficients) # Keep all coefficients
    names_var <- names(model$coefficients[2:length(model$coefficients)])
    newdata <- model$model[2:ncol(model$model)] # Extract VE data
  }
  
  # Average VE
  newdata_temp <- NA ; newdata_mean <-  (rep(NA, ncol(newdata)))
  index_Type <- grep("Sampling_type", names(newdata)) # Detect if Sampling_type is present
  if (length(index_Type)>0) { # If Sampling type is present
    index_var_num <- which(sapply(newdata, class) == "numeric")
    newdata_temp <- newdata[,index_var_num]
    newdata_temp <- apply(X = newdata_temp, MARGIN = 2, FUN = mean) # Get VE mean
    for (i in 1:length(index_var_num)) {
      newdata_mean[index_var_num[i]] <- newdata_temp[i] # Reorder mean in VE original order
    }
    newdata <- data.frame(matrix(rep(newdata_mean, each= 1001),nrow=1001)) ; names(newdata) <- names_var # Replicate mean 1001 times
    newdata[,index_Type] <- rep(0.5, times = 1001) # Add mean Sampling_type = 0.5
  }else { # If no Sampling type, extract directly VE mean
    newdata <- apply(X = newdata, MARGIN = 2, FUN = mean) # Get VE mean
    newdata <- data.frame(matrix(rep(newdata, each= 1001),nrow=1001)) ; names(newdata) <- names_var # Replicate mean 1001 times
  }
  
  # Add dummy Variable for Intercept
  newdata <-  cbind(rep(1, times = 1001), newdata)
  
  # Replace studied VE with a regular sequence on VE range to form X-axis
  min <- min(VE_data) ; if (min<0) { min <- min*1.5 } else { min <- min * 0.5 } # Enlarge limits. Deal with negative value (ex: Tmean)
  max <- max(VE_data) ; if (max<1) { max <- max + 0.8 } else { max <- max*1.5 }   # Enlarge limits. Deal with positive values close to 0 (ex : ln_SE)
  var_abs <- seq(from = min, to=max, by = (max-min)/1000) # Generate a regular sequence on VE range to from X-axis
  newdata[,grep(VE, names(newdata))] <- var_abs # Insert VE
  
  # Predict
  predict <- NA
  for ( i in 1:nrow(newdata)) {
    predict[i] <- (as.matrix(newdata[i,])%*%as.matrix(coefs))[1,1] # Scalar product between coefficients and VE to obtain predicts
  }
  
  predict ; predict_table <- data.frame(cbind(var_abs,predict))
  
  # Generate folder if does not exist yet
  if (!dir.exists(paths = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Predicts_for_Graphs"))) { # Return T/F if folder exists or not
    dir.create(path = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Predicts_for_Graphs"), recursive = T) # Generate (sub-)folder. Recursive = T => generate also parent folders if not present yet
  } 
  
  saveRDS(predict_table, file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Predicts_for_Graphs/predict_table_",model_name,"_for_",VE_short_name,".RData"))
  
  # Compute regression coefficients for abline
  b <-  as.matrix(newdata[1,-grep(VE, names(newdata))])%*%as.vector(coefs[-grep(VE, names(newdata))])
  a <-  coefs[grep(VE, names(newdata))]
  regression_coefs <- c(b,a) 
  saveRDS(regression_coefs, file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Predicts_for_Graphs/regression_coefs_",model_name,"_for_",VE_short_name,".RData"))
  
  ### Export in pdf
  
  # Generate folder if does not exist yet
  if (!dir.exists(paths = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar))) { # Return T/F if folder exists or not
    dir.create(path = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar), recursive = T) # Generate (sub-)folder. Recursive = T => generate also parent folders if not present yet
  } 
  
  if (ln_transfo == "no") { # For models with non-transformed VE
    
    # Plot without log on X-axis
    pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/",model_name,"_for_",VE_short_name,"_inter_",Covar,".pdf"), width = 7, height = 6)
    
    par(mfcol=c(1,1)) ; plot(y = VAE_data, x = eval(parse(text = paste0("aggreg.webs$",VE_axis_name))), log = "y", 
                             xlab = VE_legend, ylab = paste(VAE_legend,"(log scale)"), 
                             main = paste("VAE :", VAE, " ;  Coverage :", web_coverage, "\n",model_type, " ", polar,"\n VE :", VE_short_name, " ;  Inter :", Covar),
                             cex.axis = 1.3, cex.lab = 1.3, type ="n")
    points(eval(parse(text = paste0("aggreg.webs$",VE_axis_name))), VAE_data, pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], col = aggreg.webs$Col, cex = 1.3)
    lines(x = predict_table$var_abs, y = exp(predict_table$predict), col = "red", lwd = 3) # Case for VE used raw in models
    legend(legend = c(paste0("Coef : ", Beta_coef), paste0("p-value : ", p_value)), text.font = 2, x = "topleft", bty ="n", cex = 0.8, bg = "white")
    
    dev.off()
  }
  
  if (ln_transfo %in% c("log", "log1p")) { # For models with transformed VE
    
    # Plot without log on X-axis
    pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/",model_name,"_for_",VE_short_name,"_inter_",Covar,".pdf"), width = 7, height = 6)
    
    par(mfcol=c(1,1)) ; plot(y = VAE_data, x = eval(parse(text = paste0("aggreg.webs$",VE_axis_name))), log = "y", 
                             xlab = VE_legend, ylab = paste(VAE_legend,"(log scale)"), 
                             main = paste("VAE :", VAE, " ;  Coverage :", web_coverage, "\n",model_type, " ", polar,"\n VE :", VE_short_name, " ;  Inter :", Covar),
                             cex.axis = 1.3, cex.lab = 1.3, type = "n")
    points(eval(parse(text = paste0("aggreg.webs$",VE_axis_name))), VAE_data, pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], col = aggreg.webs$Col, cex = 1.3)
    if (ln_transfo == "log") {
      lines(x = exp(predict_table$var_abs), y = exp(predict_table$predict), col = "red", lwd = 3) # Case for VE used under log(X) in models
    } else { # ln_transfo == "log1p"
      lines(x = exp(predict_table$var_abs)-1, y = exp(predict_table$predict), col = "red", lwd = 3) # Case for VE used under log(X+1) in models
    }
    
    legend(legend = c(paste0("Coef : ", Beta_coef), paste0("p-value : ", p_value)), text.font = 2, x = "topleft", bty ="n", cex = 0.8, bg = "white")
    
    dev.off()
    
    # Plot with log on X-axis
    pdf(paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/",model_name,"_for_",VE_short_name,"_inter_",Covar,"_log.pdf"), width = 7, height = 6)
    
    par(mfcol=c(1,1))
    
    if (ln_transfo == "log") {  # Case for VE with log(X) transformation
      plot(y = VAE_data, x = eval(parse(text = paste0("aggreg.webs$",VE_axis_name))), log = "xy", 
           xlab = paste(VE_legend, "(log scale)"), ylab = paste(VAE_legend,"(log scale)"), 
           main = paste("VAE :", VAE, " ;  Coverage :", web_coverage, "\n",model_type, " ", polar,"\n VE :", VE_short_name, " ;  Inter :", Covar),
           cex.axis = 1.3, cex.lab = 1.3, type = "n")
      points(eval(parse(text = paste0("aggreg.webs$",VE_axis_name))), VAE_data, pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], col = aggreg.webs$Col, cex = 1.3)
      lines(x = exp(predict_table$var_abs), y = exp(predict_table$predict), col = "red", lwd = 3) 
    } else {                    # Case for VE with log(X+1) transformation
      plot(y = VAE_data, x = eval(parse(text = paste0("aggreg.webs$",VE_axis_name,"+1"))), log = "xy", 
           xlab = paste(VE_legend, "(log scale)"), ylab = paste(VAE_legend,"(log scale)"), xaxt = "n",
           main = paste("VAE :", VAE, " ;  Coverage :", web_coverage, "\n",model_type, " ", polar,"\n VE :", VE_short_name, " ;  Inter :", Covar),
           cex.axis = 1.3, cex.lab = 1.3, type = "n")
      axis(side = 1, at = c(1,2,5,10,20,50,100), labels = c(0,1,5,10,20,50,100), cex.axis = 1.3, cex.lab = 1.3) # Custom axis to display real values for low values of HF
      points(eval(parse(text = paste0("aggreg.webs$",VE_axis_name,"+1"))), VAE_data, pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], col = aggreg.webs$Col, cex = 1.3)
      lines(x = exp(predict_table$var_abs), y = exp(predict_table$predict), col = "red", lwd = 3) 
    } 
      
    
    legend(legend = c(paste0("Coef : ", Beta_coef), paste0("p-value : ", p_value)), text.font = 2, x = "topleft", bty ="n", cex = 0.8, bg = "white")
    
    dev.off()
  }
  
  all_single_plots_summary_all_VE$Last_runtime[j] <- as.character(Sys.time())
  
  if (j %% 10 == 0) {
    cat(paste("\n Plot n?", j, "on", nrow(all_single_plots_summary_all_VE), "-", Sys.time(), "\n"))
  }
  
  # if (k %% 10 == 0) {
  #   cat(paste("\n Plot n?", k, "on", length(index_plot_to_plot), "-", Sys.time(), "\n"))
  # }
  
  k <- k + 1 # Index when running a reduced number of plots
  
}

# save(all_single_plots_summary_all_VE, file = "./Data/all_single_plots_summary_all_VE.RData")




##### 2/ Multiples Plots (with all web_coverage) ####

### 2.1/ Generate the summary table to keep track of all options for multiple plots ####

load(file = "./Data/all_single_plots_summary_all_VE.RData")

# Remove ID_plot, ID_model, web_coverage, Covar, Beta_coef, p-value
all_multiple_plots_summary_all_VE <- all_single_plots_summary_all_VE[,!(names(all_single_plots_summary_all_VE) %in% c("ID_plot", "ID_model", "web_coverage", "model_dataset", "model_name", "Exists", "Covar", "Beta_coef", "p_value", "Last_runtime"))]

# Extract unique lignes corresponding to a unique Multiple plot
all_multiple_plots_summary_all_VE <- unique(all_multiple_plots_summary_all_VE)

# Retrieve web_coverage, ID_plots and ID_models associated with each possible Multiple plot
temp_df_to_fill <- all_multiple_plots_summary_all_VE
temp_df_to_fill$ID_plot <- temp_df_to_fill$ID_model <- NA
for (i in 1:nrow(temp_df_to_fill)) {
  
  temp_df <- plyr::match_df(x = all_single_plots_summary_all_VE, y = all_multiple_plots_summary_all_VE[i,])
  
  temp_df_to_fill$web_coverage[i] <- paste(temp_df$web_coverage, collapse = "-")    # Retrieve web_coverage
  temp_df_to_fill$ID_plot[i] <- paste(temp_df$ID_plot, collapse = "-")    # Retrieve ID_plot
  temp_df_to_fill$ID_model[i] <- paste(temp_df$ID_model, collapse = "-")  # Retrieve ID_model
  
}
all_multiple_plots_summary_all_VE <- temp_df_to_fill ; rm(temp_df_to_fill)

# Set Last_runtime to NA 
all_multiple_plots_summary_all_VE$Last_runtime <- NA

# save(all_multiple_plots_summary_all_VE, file = "./Data/all_multiple_plots_summary_all_VE.RData")


### 2.2/ Generate all multiple plots ####

load(file = "./Data/all_single_plots_summary_all_VE.RData")
load(file = "./Data/all_multiple_plots_summary_all_VE.RData")


# To associate the right legend and the right variable name in the dataset with the right response variable (VAE)
VAE_list <- c("C", "Li", "Lp", "S", "I", "P" )
VAE_legend_list <- c("Connectance", "Insect Link Density", "Plant Link Density", "Total Richness", "Insect Richness", "Plant Richness")
VAE_dataset_name_list <- c("Connectance", "Li", "Lp", "sptot", "full_insects", "full_plants" )


# To associate the right legend and the right variable name in the dataset with the right explanatory variable (VE)
VE_short_name_list <-  c("S", "P", "I", "SE", "Time", "ATS", "HF", "Ptot", "Tmean", "Pvar", "Tvar", "Forest", "Taxo")
VE_legend_list <- c("Network size", "Partner pool (plants)", "Partner pool (insects)", "Sampling effort", "Sampling time", "Annual time span of sampling", "Human Influence", "Annual total precipitation", "Mean temperature", "Precipitation seasonality", "Temperature seasonality", "Forest cover", "Taxonomic resolution")
VE_axis_name_list <- c("sptot", "full_plants", "full_insects", "Sampling_effort", "Sampling_time", "Annual_time_span", "HF", "Tot_Rainfall_IPCC", "Mean_T_IPCC", "Rainfall_Seasonality_IPCC", "T_Seasonality_IPCC", "Forest", "taxo_full_sp_perc")


# Chose web coverage to plot on a single Multiple plot
web_coverage_list <- c("full", "dipt", "hymeno")
web_coverage_to_plot <- c("full", "dipt", "hymeno")
web_coverage_legend <- c("Full", "Diptera", "Hymenoptera")
web_coverage_legend_custom <- paste0(web_coverage_legend,":") # Add the double point for plot convenience

# Boucle
k <- 1 ; par(xpd = F)
for (j in 1:nrow(all_multiple_plots_summary_all_VE)) {
  
  web_coverage_available <- unlist(strsplit(x = all_multiple_plots_summary_all_VE$web_coverage[j], split = "-"))
  
  # Chack if all web_coverage are available for this multiple plot
  if (!all(web_coverage_to_plot %in% web_coverage_available)) { # If not, warn with a message
    
    cat(paste("Not all web coverages are available to do a Multliple plot for", all_multiple_plots_summary_all_VE$VAE[j], "~", all_multiple_plots_summary_all_VE$VE_short_name[j] , "in model", all_multiple_plots_summary_all_VE$model_type[j], all_multiple_plots_summary_all_VE$polar[j], all_multiple_plots_summary_all_VE$taxo[j], all_multiple_plots_summary_all_VE$forest[j], "= Multiple plot n?", j, "\n"))
    
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
    
    # Create an output folder if needed
    if (!dir.exists(paths = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar))) { 
      dir.create(path = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar), recursive = T) 
    }   
    
    # Plot without log on X-axis
    pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Multiple_plot_",VAE,"_",model_type,"_",polar,"_for_",VE_short_name,"_in_",paste(web_coverage_to_plot, collapse = "_"),".pdf"), width = 7, height = 6)
    
    # Plot axis and title
    par(mfcol=c(1,1))
    
    plot(y = VAE_all_coverage, x = VE_all_coverage, log = "y", xlab = VE_legend, ylab = paste(VAE_legend,"(log scale)"),
         main = paste("                         ", model_type, " ", polar, "\n"),
         cex.axis=1.3, cex.lab=1.3, type="n")
    
    for (i in 1:length(dataset_list)) {  # Plot each web_coverge iteratively
      
      aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve dataset associated with web coverage
      model <- eval(parse(text = paste0("model_",i))) # Retrieve model
      
      if (ln_transfo == "no") { # Case for VE without log transfo
        points(x = model$model[,grep(VE, names(model$model))], # No retro-transformation of the VE
               y = exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255),
               cex = 0.7)
      }
      
      if (ln_transfo == "log") { # Case with VE transformed with log(X) = X'
        
        points(x = exp(model$model[,grep(VE, names(model$model))]), # Retro-transformation of the VE tel such as Exp(X) = X' <=> X = ln(X')
               y = exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255), cex = 0.7)
      }
      
      if (ln_transfo == "log1p") { # Case with VE transformed with log(X+1) = X'
        
        points(x = exp(model$model[,grep(VE, names(model$model))]) - 1, # Retro-transformation of the VE tel such as Exp(X) - 1 = X' <=> X = ln(X' + 1)
               y = exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255), cex = 0.7)
      }
    }
    
    for (i in 1:length(dataset_list)) { # Add predict lines
      predict_table <- eval(parse(text = paste0("predict_table_",i))) # Load predict for model i
      
      if (ln_transfo == "no") { # Case for VE without log transfo
        lines(x = predict_table$var_abs, # No retro-transformation of the VE
              y = exp(predict_table$predict), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
              col = col_list[i], lwd = 3, lty = c(2,1)[signif_list][i]) # Significant = full ligne. Non-significant = dashed line
      }
      
      if (ln_transfo == "log") { # Case with VE transformed with log(X) = X'
        
        lines(x = exp(predict_table$var_abs), # Retro-transformation of the VE tel such as Exp(X) = X' <=> X = ln(X')
              y = exp(predict_table$predict),# Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
              col = col_list[i], lwd = 3, lty = c(2,1)[signif_list][i]) # Significant = full ligne. Non-significant = dashed line
      }
      
      if (ln_transfo == "log1p") { # Case with VE transformed with log(X+1) = X'
        
        lines(x = exp(predict_table$var_abs)-1, # Retro-transformation of the VE tel such as Exp(X) - 1 = X' <=> X = ln(X' + 1)
              y = exp(predict_table$predict),# Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
              col = col_list[i], lwd = 3, lty = c(2,1)[signif_list][i]) # Significant = full ligne. Non-significant = dashed line
      }
      
    }
    
    par(xpd = T)
    
    # # Add legend for line color, beta-coefs and p-values. Beta letter as a character
    # legend(legend = paste0(web_coverage_legend, ": ?? = ", beta_value_list, ", ", p_value_custom_list), 
    #        col = col_list, text.col = "black", text.font = 2,
    #        x = "topleft", inset=c(0,-0.17), 
    #        bty ="n", lty=1, lwd = 2, cex = 0.8, bg="white", horiz = F)
    
    # Add legend line by line, with beta letter as an expression
    for (i in 1:length(web_coverage_legend_custom)) {
      legend(legend = bquote(.(web_coverage_legend_custom[i]) ~ beta == ~ .(beta_value_list_custom[i]) ~  .(p_value_custom_list[i])), 
             col = col_list[i], text.col = "black", text.font = 2,
             x = "topleft", inset=c(0,-0.21 + i*0.045), 
             bty ="n", lty=1, lwd = 2, cex = 0.8, bg="white", horiz = F)
    }
    
    par(xpd = F)
    
    dev.off()  
    
    # Additional plot with log on the X-axis
    
    if (ln_transfo %in% c("log", "log1p")) { 
      
      pdf(file = paste0("./Figures/",VAE,"/",VE_short_name,"/",model_type,"/",polar,"/Multiple_plot_",VAE,"_",model_type,"_",polar,"_for_",VE_short_name,"_in_",paste(web_coverage_to_plot, collapse = "_"),"_log.pdf"), width = 7, height = 6)
      
      par(mfcol=c(1,1))
      
      if (ln_transfo == "log1p") { # Need to specify not to plot the X-axis (xaxt = "n") that will be customed for ln(HF+1)
        
        plot(y = VAE_all_coverage, x = VE_all_coverage + 1, log = "xy", # Need to add +1 to the original value because model with log(X+1)
             xlab = paste(VE_legend, "(log scale)"), ylab = paste(VAE_legend,"(log scale)"),
             main = paste("                         ", model_type, " ", polar, "\n"),
             xaxt = "n", cex.axis=1.3, cex.lab=1.3, type="n")
        
        # Custom axis to display real values for low values of HF
        axis(side = 1, at = c(1,2,5,10,20,50,100), labels = c(0,1,5,10,20,50,100), cex.axis = 1.3, cex.lab = 1.3) 
        
      } else { # Otherwise, plot title and all axis
        
        plot(y = VAE_all_coverage, x = VE_all_coverage, log = "xy", 
             xlab = paste(VE_legend, "(log scale)"), ylab = paste(VAE_legend,"(log scale)"),
             main = paste("                         ", model_type, " ", polar, "\n"),
             cex.axis=1.3, cex.lab=1.3, type="n")
      }
      
      # Plot points iiteratively for each web_coverage
      for (i in 1:length(dataset_list)) { 
        aggreg.webs <-  readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_",dataset_list[i],".RData")) # Retrieve dataset associated with web coverage
        model <- eval(parse(text = paste0("model_",i))) # Retrieve the model
        points(exp(model$model[,grep(VE, names(model$model))]), # Retro-transformation of the VE tel such as Exp(X) - 1 = X' <=> X = ln(X' + 1)
               exp(model$model[,1]), # Retro-transformation of the VAE such as Exp(Y) = Y' <=> Y = ln(Y')
               pch = c(16,17)[as.numeric(aggreg.webs$Sampling_type)], # T = circles, TO = triangles
               col = rgb(red = col2rgb(col = col_list[i])[1,1], green = col2rgb(col = col_list[i])[2,1], blue = col2rgb(col = col_list[i])[3,1], alpha = 150, maxColorValue = 255), cex = 0.7)
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
               x = "topleft", inset=c(0,-0.21 + i*0.045), 
               bty ="n", lty=1, lwd = 2, cex = 0.8, bg="white", horiz = F)
      }
      
      # # Add legend for line color, beta-coefs and p-values
      # legend(legend = paste0(web_coverage_legend, ": ?? = ", beta_value_list, ", ", p_value_custom_list), 
      #        col = col_list, text.col = "black", text.font = 2,
      #        x = "topleft", inset=c(0,-0.17), 
      #        bty ="n", lty=1, lwd = 2, cex = 0.8, bg="white", horiz = F)
      # 
      # # Add beta sign line by line because I can't find a way to include it directly in the legend
      # legend(legend = expression(beta), text.col = "black",
      #        x = "topleft", inset=c(0.103,-0.169), # Position of Beta sign on line 1
      #        bty ="n", cex = 0.8, bg="white", text.font = 2)
      # legend(legend = expression(beta), text.col = "black",
      #        x = "topleft", inset=c(0.145,-0.125), # Position of Beta sign on line 2
      #        bty ="n", cex = 0.8, bg="white", text.font = 2)
      # legend(legend = expression(beta), text.col = "black",
      #        x = "topleft", inset=c(0.219,-0.083), # Position of Beta sign on line 3
      #        bty ="n", cex = 0.8, bg="white", text.font = 2)
      
      par(xpd = F)
      
      dev.off()
    }    
    
    
    all_multiple_plots_summary_all_VE$Last_runtime[j] <- as.character(Sys.time())
    
    
  }
  
  if (j %% 10 == 0) {
    cat(paste("\n Plot n?", j, "on", nrow(all_multiple_plots_summary_all_VE), "-", Sys.time(), "\n"))
  }
  
  # if (k %% 10 == 0) {
  #   cat(paste("\n Plot n?", k, "on", length(index_plot_to_plot), "-", Sys.time(), "\n"))
  # }
  
  k <- k + 1 # Index to keep track when plotting a reduced number of plots
}

# save(all_multiple_plots_summary_all_VE, file = "./Data/all_multiple_plots_summary_all_VE.RData")



