
# Author: Maël DORÉ
# Contact: mael.dore@gmail.com
# License: MIT

##### Script SM6_1: Model pollination network responses to anthropogenic pressures and climate, but without sampling effects #####

# Generate models without sampling variables (and still no polar networks)

# Inputs:

   # Table with all networks variables (aggreg.webs)
   # Summary table with information on model parameters (all_models_summary)

# Outputs:

   # Calibrated models for all 6 response variables, without sampling effects, only for full web coverage
   # Summary tables of each model results 
   # Summary table with information on model parameters, without sampling effects (all_models_summary_no_sampling)


#####




### Clear memory
rm(list = ls())
gc() # Force R to release memory it is no longer using

### Load packages

library("car")
library("MASS")
library("spdep") ; library("spatialreg")
library("MuMIn")
options(na.action = "na.fail") # Parameter to change for dredge function in MuMIn package


##### 1/ Generate summary table of all 6 models for each response variable (VAE) ####

load(file = "./Data/all_models_summary.RData")

# Keep only models for full coverage
all_models_summary_no_sampling <- all_models_summary[all_models_summary$web_coverage == "full",]

save(all_models_summary_no_sampling, file = "./Data/all_models_summary_no_sampling.RData")

##### 2/ Modeling ####

load(file = "./Data/all_models_summary_no_sampling.RData")

# To ensure repetability of results, set seed.
set.seed(64655)

# Load response variables (= VE) names used to extract best models
VE_list <- c("ln_sptot", "ln_pl", "ln_ins", "ln_SE", "ln_time", "ln_ATS", "Sampling_type", "ln_HF", "Tot_Rainfall_IPCC", "Mean_T_IPCC", "Rainfall_Seasonality_IPCC", "T_Seasonality_IPCC", "ln_forest", "taxo_full_sp_perc")
VE_short_name_list <-  c("S", "P", "I", "SE", "Time", "ATS", "Method", "HF", "Ptot", "Tmean", "Pvar", "Tvar", "Forests", "Taxo")

k <- 1
for (j in 1:nrow(all_models_summary_no_sampling)) {
  
  # 2.1/ Define model specificity ####
  
  web_coverage <- all_models_summary_no_sampling$web_coverage[j] 
  model_type <- all_models_summary_no_sampling$model_type[j] 
  polar <- all_models_summary_no_sampling$polar[j] 
  VAE <- all_models_summary_no_sampling$VAE[j] 
  model_dataset <- all_models_summary_no_sampling$model_dataset[j]
  model_name <- all_models_summary_no_sampling$model_name[j]
  
  # 2.2/ Load associated dataset ####
  aggreg.webs <- readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_", model_dataset, ".RData"))
  
  # 2.3/ Generate the weighted neighboring matrix used to compute Moran's I and SEV ####
  # Threshold = 150km based on correlograms of OLS residuals.
  lim = 150 #
  neighbour <- dnearneigh(as.matrix(cbind(aggreg.webs$Longitude_dec,aggreg.webs$Latitude_dec)), 0, lim, longlat=TRUE) # List of neighbors networks for each network
  neigh.listw <- nb2listw(neighbour, style = "W", zero.policy=T) # Weighting by number of neighbors
  
  # 2.4/ Choose the modeling options ####
  
  if (VAE == "C") { # Model for Connectance
    eval(call("<-", as.name(model_name), lm(data = aggreg.webs, formula = log(Connectance) ~ ln_sptot + ln_HF + Tot_Rainfall_IPCC + Mean_T_IPCC + Rainfall_Seasonality_IPCC + T_Seasonality_IPCC))) 
  }
  
  if (VAE == "Li") { # Model for Link Density of Insects
    eval(call("<-", as.name(model_name), lm(data = aggreg.webs, formula = log(Li) ~ ln_pl + ln_HF + Tot_Rainfall_IPCC + Mean_T_IPCC + Rainfall_Seasonality_IPCC + T_Seasonality_IPCC))) 
  }
  
  if (VAE == "Lp") { # Model for Link Density of Plants
    eval(call("<-", as.name(model_name), lm(data = aggreg.webs, formula = log(Lp) ~ ln_ins + ln_HF + Tot_Rainfall_IPCC + Mean_T_IPCC + Rainfall_Seasonality_IPCC + T_Seasonality_IPCC))) 
  }
  
  if (VAE == "S") { # Model for Species Richness
    eval(call("<-", as.name(model_name), lm(data = aggreg.webs, formula = log1p(sptot) ~ ln_HF + Tot_Rainfall_IPCC + Mean_T_IPCC + Rainfall_Seasonality_IPCC + T_Seasonality_IPCC)))
  }
  
  if  (VAE == "I") { # Model for Insect Richness
    eval(call("<-", as.name(model_name), lm(data=aggreg.webs, formula = log1p(full_insects) ~ ln_HF + Tot_Rainfall_IPCC + Mean_T_IPCC + Rainfall_Seasonality_IPCC + T_Seasonality_IPCC)))
  }
  
  if (VAE == "P") { # Model for Plant Richness
    eval(call("<-", as.name(model_name), lm(data=aggreg.webs, formula = log1p(full_plants) ~ ln_HF + Tot_Rainfall_IPCC + Mean_T_IPCC + Rainfall_Seasonality_IPCC + T_Seasonality_IPCC)))
  }
  
  # vif(eval(parse(text = model_name))) # All VIF < 4.
  # summary(eval(parse(text = model_name)))
  
  model_OLS <- eval(parse(text = model_name))
  
  ### 2.5/ Modeling part ####
  
  # Check if storage folder exists. Create it. Save path
  
  if (!dir.exists(paths = paste0("./Models_outputs_no_sampling/",VAE,"/",model_name))) { # Revoie T/F si le dossier existe
    dir.create(path = paste0("./Models_outputs_no_sampling/",VAE,"/",model_name), recursive = T) # Cr?er un (sous-)dossier. R?cursive = T => pour aussi g?n?rer les dossiers parents si inexistents
  } 
  
  
  # Model Averaging
  set <- dredge(global.model = model_OLS, beta = "none", rank = "AICc") # To generate the complete list of models with all variable combination possible
  potMAM_AICc <- get.models(set, subset = delta<=5) # To extract only potential MAMs (delta AICc < 5), before SEVM correction
  
  # SEVM on all potential MAMs
  Moran_I <- Moran_pvalue <- SEV_nb <- AICc_ME <- Moran_I_ME <- Moran_pvalue_ME <- aggreg.webs_ME <- rep(NA,length(potMAM_AICc))
  list_potMAM_SEVM <- potMAM_AICc # Copy potential MAMs models before SEVM correction
  for (i in 1:length(potMAM_AICc)) {
    tryCatch({ # Function to only display error message when no SEV corrrection is needed, instead of stopping the loop
      MEcorr <-  NA # Initialize the list of SEV to test new SEV to incorporate in the model
      model <- potMAM_AICc[[i]] # Load model n?i in dredge
      resid=resid(model,type="pearson") # Extract residuals to compute Moran's I
      RandomTestMoran <- moran.mc(resid, listw=neigh.listw, nsim=9999, zero.policy=T) # Compute Moran's I of OLS model using the weighted neighbor matrix
      Moran_I[i] <- RandomTestMoran$statistic # Extract Moran's I
      Moran_pvalue[i] <- RandomTestMoran$p.value # Extract associated p-value
      MEcorr <- ME(as.character(model$call)[2], data = aggreg.webs, family=gaussian, listw=neigh.listw, alpha=0.1) # Test if the incorporation of SEV into the model is significant
      SEV_nb[i] <- ncol(MEcorr$vectors) # Nb of SEV included
      modelME <- lm(paste(as.character(model$call)[2]," + fitted(MEcorr)"), data = aggreg.webs) # Fit the new model with SEVs
      list_potMAM_SEVM[[i]] <- modelME # Replace OLS model with SEV model when significant SEV can be added
      AICc_ME[i] <- round(MuMIn::AICc(modelME),3) # Extract AICc of final model with SEV
      residME=resid(modelME,type="pearson") # Extract residuals of SEVM
      RandomTestMoran_ME <- moran.mc(residME, listw=neigh.listw, nsim=9999, zero.policy=T) # Compute Moran's I of SEVM using the weighted neighbor matrix
      Moran_I_ME[i] <- round(RandomTestMoran_ME$statistic,3) # Extract Moran's I
      Moran_pvalue_ME[i] <- round(RandomTestMoran_ME$p.value,3) # Extract associated p-value
    }, error=function(e){cat("ERROR Model n?",i,":",conditionMessage(e), "\n")}) # Print error message if present isntead of stopping the loop
    
    if (i %% 10 == 0) {
      cat(paste(Sys.time(), "- Model n?", i, "on", length(potMAM_AICc),"\n")) # Display time and index every 10 models
    }
    
  }
  for (i in which(is.na(SEV_nb))) { # Correction of NAs for models without SEV added
    SEV_nb[i] <- 0
    AICc_ME[i] <- set$AICc[i] 
    Moran_I_ME[i] <- Moran_I[i]
    Moran_pvalue_ME[i] <- Moran_pvalue[i]
  }
  
  models_table <- set[1:length(potMAM_AICc),] # Extraction of the table of potential MAMs
  # Add new variables extracted previously (Moran, AICc, SEV_nb)
  models_table$Moran_I <- Moran_I ; models_table$Moran_pvalue <- Moran_pvalue ; models_table$SEV_nb <- SEV_nb ; models_table$AICc_ME <- AICc_ME ; models_table$Moran_I_ME <- Moran_I_ME ; models_table$Moran_pvalue_ME <- Moran_pvalue_ME
  minAICc <- min(models_table$AICc_ME) # Extract AICc of new best models among SEVM
  for (i in 1:nrow(models_table)) {
    models_table$delta_ME[i] <- models_table$AICc_ME[i] - minAICc
  }
  # View(models_table)
  save(models_table, file = paste0("./Models_outputs_no_sampling/",VAE,"/",model_name,"/",model_name,"_Model_evaluation_table.RData")) # Save MAMs
  MAM_AICc <- list_potMAM_SEVM[which(models_table$delta_ME<=2)] # Extract MAMs after SEV correction (delta AICc < 2)
  saveRDS(MAM_AICc, file = paste0("./Models_outputs_no_sampling/",VAE,"/",model_name,"/",model_name,"_MAM_AICc.RData")) # Save MAMs
  
  bestmod_AICc <- list_potMAM_SEVM[[which(models_table$delta_ME==0)]] # Extract best model after SEV correction
  saveRDS(bestmod_AICc, file = paste0("./Models_outputs_no_sampling/",VAE,"/",model_name,"/",model_name,"_bestmod_AICc.RData")) # Save best model
  
  # Extract best model for each explanaroty variabel (VE) among potential MAMs (delta < 5)
  for (i in 1:length(VE_list)) {
    
    VE <- VE_list[i]
    VE_short_name <- VE_short_name_list[i]
    
    if (VE %in% names(models_table)) { # Check if this VE is present in models
      
      if (any(!is.na(eval(parse(text = paste0("models_table$",VE)))))) { # Check if at least one model use this VE
        
        # Extract best model including each the VE
        eval(call("<-", as.name(paste0("bestmod_with_",VE_short_name)), list_potMAM_SEVM[[which(!is.na(eval(parse(text = paste0("models_table$",VE)))))[which.min(models_table$delta_ME[which(!is.na(eval(parse(text = paste0("models_table$",VE)))))])]]]))
        # Save best model for this VE
        saveRDS(eval(parse(text = paste0("bestmod_with_",VE_short_name))), file = paste0("./Models_outputs_no_sampling/",VAE,"/",model_name,"/",model_name,"_bestmod_with_",VE_short_name,".RData"))
      } 
    }
    
    
  }
  
  # 2.6/ Generating summaries ####
  
  MAM_table <- subset(x = models_table, subset = models_table$delta_ME<=2) # Extract Model Evaluation table for MAMs
  bestmod_table <- subset(x = models_table, subset = models_table$delta_ME==0) # Extract Model Evaluation table for Bestmodel
  
  # View(MAM_table[,]) # Check if everything is okay with the models selected
  # length(MAM_AICc) # Number of MAMs = ?
  if (length(MAM_AICc)>1) { # When several MAMs are selected, need to process to model averaging
    imp_var <- importance(MAM_AICc) # Variable Importance
    summary_table_coefs <- data.frame(round(imp_var[c("ln_sptot","ln_pl","ln_ins","ln_time","ln_SE","ln_ATS","Sampling_type","ln_forest","taxo_full_sp_perc","ln_HF","Tot_Rainfall_IPCC","Mean_T_IPCC","Rainfall_Seasonality_IPCC","T_Seasonality_IPCC")],2)) # Variable Importance 
    row.names(summary_table_coefs) <- c("S", "P", "I", "Time", "SE", "ATS", "Type", "Forest", "ID", "HF", "Ptot", "Tmean", "Pvar", "Tvar") ; names(summary_table_coefs) <- "Importance"
    MAM.avg.coefs <- model.avg(MAM_AICc, beta = "none") ; summary(MAM.avg.coefs) # Look at "Full" coeffs
    summary_table_coefs$Orig_coefs_MAM <- round(MAM.avg.coefs$coefficients[1,][c("ln_sptot","ln_pl","ln_ins","ln_time","ln_SE","ln_ATS","Sampling_typeTO","ln_forest","taxo_full_sp_perc","ln_HF","Tot_Rainfall_IPCC","Mean_T_IPCC","Rainfall_Seasonality_IPCC","T_Seasonality_IPCC")],6) # Average coefs MAM models not-standardized
    summary(bestmod_AICc) # F-statistic : ???,  p-value: ???
    summary_table_coefs$Orig_coefs_Bestmod <- round(coef(bestmod_AICc)[c("ln_sptot","ln_pl","ln_ins","ln_time","ln_SE","ln_ATS","Sampling_typeTO","ln_forest","taxo_full_sp_perc","ln_HF","Tot_Rainfall_IPCC","Mean_T_IPCC","Rainfall_Seasonality_IPCC","T_Seasonality_IPCC")],6) # Coefs best model not-standardized
    MAM.avg.coefs_std <- model.avg(MAM_AICc, beta = "partial.sd") ; summary_MAM.avg.coefs_std <- summary(MAM.avg.coefs_std) ; summary_MAM.avg.coefs_std # Standardized Beta-coefficients via partial SD
    summary_table_coefs$Beta_coefs_MAM <- round(MAM.avg.coefs_std$coefficients[1,][c("ln_sptot","ln_pl","ln_ins","ln_time","ln_SE","ln_ATS","Sampling_typeTO","ln_forest","taxo_full_sp_perc","ln_HF","Tot_Rainfall_IPCC","Mean_T_IPCC","Rainfall_Seasonality_IPCC","T_Seasonality_IPCC")],3) # Average coefs MAM models standardized
    Bestmod_coefs_std <- std.coef(x = bestmod_AICc, partial.sd = T) ; summary_Bestmod_coefs_std <- summary(Bestmod_coefs_std) ; summary_Bestmod_coefs_std
    summary_table_coefs$Beta_coefs_Bestmod <- round(Bestmod_coefs_std[,1][c("ln_sptot","ln_pl","ln_ins","ln_time","ln_SE","ln_ATS","Sampling_typeTO","ln_forest","taxo_full_sp_perc","ln_HF","Tot_Rainfall_IPCC","Mean_T_IPCC","Rainfall_Seasonality_IPCC","T_Seasonality_IPCC")],3) # Coefs best model standardized
  }else{ # When there is only one MAM = Best model
    summary_table_coefs <- data.frame(as.numeric(c("ln_sptot","ln_pl","ln_ins","ln_time","ln_SE","ln_ATS","Sampling_type","ln_forest","taxo_full_sp_perc","ln_HF","Tot_Rainfall_IPCC","Mean_T_IPCC","Rainfall_Seasonality_IPCC","T_Seasonality_IPCC") %in% attr(MAM_AICc[[1]]$terms, "term.labels"))) # One MAM thus Importance = 1 if explanatory variable is present
    row.names(summary_table_coefs) <- c("S", "P", "I", "Time", "SE", "ATS", "Type", "Forest", "ID", "HF", "Ptot", "Tmean", "Pvar", "Tvar") ; names(summary_table_coefs) <- "Importance"
    MAM.avg.coefs <- MAM_AICc[[1]] # No need for averaging since one MAM = Best model
    summary_table_coefs$Orig_coefs_MAM <- round(MAM.avg.coefs$coefficients[c("ln_sptot","ln_pl","ln_ins","ln_time","ln_SE","ln_ATS","Sampling_typeTO","ln_forest","taxo_full_sp_perc","ln_HF","Tot_Rainfall_IPCC","Mean_T_IPCC","Rainfall_Seasonality_IPCC","T_Seasonality_IPCC")],6) # Coefs MAM = best model not-standardized
    summary(bestmod_AICc) # F-statistic : ???,  p-value: ???
    summary_table_coefs$Orig_coefs_Bestmod <- round(coef(bestmod_AICc)[c("ln_sptot","ln_pl","ln_ins","ln_time","ln_SE","ln_ATS","Sampling_typeTO","ln_forest","taxo_full_sp_perc","ln_HF","Tot_Rainfall_IPCC","Mean_T_IPCC","Rainfall_Seasonality_IPCC","T_Seasonality_IPCC")],6) # Coefs best model not-standardized
    MAM.avg.coefs_std <- std.coef(MAM_AICc[[1]], partial.sd = T) ; summary_MAM.avg.coefs_std <- summary(MAM.avg.coefs_std) # Beta-coefficients standardis?s via partial SD. Pas besoin de moyenne car un seul MAM = Best model
    summary_table_coefs$Beta_coefs_MAM <- round(MAM.avg.coefs_std[,1][c("ln_sptot","ln_pl","ln_ins","ln_time","ln_SE","ln_ATS","Sampling_typeTO","ln_forest","taxo_full_sp_perc","ln_HF","Tot_Rainfall_IPCC","Mean_T_IPCC","Rainfall_Seasonality_IPCC","T_Seasonality_IPCC")],3) # Coefs MAM = best model standardized
    Bestmod_coefs_std <- std.coef(x = bestmod_AICc, partial.sd = T) ; summary_Bestmod_coefs_std <- summary(Bestmod_coefs_std) ; summary_Bestmod_coefs_std
    summary_table_coefs$Beta_coefs_Bestmod <- round(Bestmod_coefs_std[,1][c("ln_sptot","ln_pl","ln_ins","ln_time","ln_SE","ln_ATS","Sampling_typeTO","ln_forest","taxo_full_sp_perc","ln_HF","Tot_Rainfall_IPCC","Mean_T_IPCC","Rainfall_Seasonality_IPCC","T_Seasonality_IPCC")],3) # Coefs best model standardized
  }
  
  # View(summary_table_coefs) 
  
  summary_table_criteria <- data.frame(round(summary(bestmod_AICc)$adj.r.squared,3)) ; names(summary_table_criteria) <- "Rsq_adj" # R? adjusted = ???
  summary_table_criteria$AICc_bestmod <- round(MuMIn::AICc(bestmod_AICc),2) # AICc = ???
  summary_table_criteria$Moran_I_bestmod <- bestmod_table$Moran_I_ME ; summary_table_criteria$Moran_pvalue_bestmod <- bestmod_table$Moran_pvalue_ME # Best model Moran's I and associated p-value
  summary_table_criteria$max_Moran_I_MAM <- max(MAM_table$Moran_I_ME) ; summary_table_criteria$min_Moran_pvalue_MAM <- min(MAM_table$Moran_pvalue_ME) # Max Moran's I and Min p-value among all MAMs 
  summary_table_criteria$nb_MAMs <- length(MAM_AICc)
  
  # View(summary_table_criteria)
  
  save(summary_MAM.avg.coefs_std, summary_Bestmod_coefs_std, summary_table_coefs, summary_table_criteria, file = paste0("./Models_outputs_no_sampling/",VAE,"/",model_name,"/",model_name,"_summaries.RData"))
  
  # par(mfcol=c(2,2)) ; plot(bestmod_AICc) # Best mod?le ok ?
  # shapiro.test(bestmod_AICc$resid) ; par(mfcol=c(1,1)) ; hist(bestmod_AICc$resid, breaks = 20) ; abline(v=0, lty=2, lwd= 3, col ="red")
  # # Check if p-value > 0.05. Not breaking normality assumption
  
  ### Load final summaries ###
  
  # load(file = paste0("./Models_outputs_no_sampling/Summaries/",model_name,"_summaries.RData"))
  # 
  # summary_MAM.avg.coefs_std
  # summary_Bestmod_coefs_std
  # View(summary_table_coefs)
  # View(summary_table_criteria)
  
  # Clean environnement
  rm(list = setdiff(ls(), c("j", "k", "all_models_summary_no_sampling", "index_model_to_compute", "VE_list", "VE_short_name_list" , "aggreg.webs", "neigh.listw", "neighbour", "lim", "model_dataset", "model_name", "model_type", "model_type_list", "polar", "polar_list", "taxo", "taxo_list", "forest", "forest_list", "VAE", "VAE_list", "web_coverage", "web_coverage_list")))
  
  # Save last runtime
  
  all_models_summary_no_sampling$Last_runtime[j] <- as.character(Sys.time())
  
  # Affiche le temps de calcul et l'avancement
  cat(paste("\n", Sys.time(), "-", model_name, "-", j, "out of ", nrow(all_models_summary_no_sampling), " - Done \n"))
  
  k <- k + 1
  
}

# save(all_models_summary_no_sampling, file = "./Data/all_models_summary_no_sampling.RData")   




##### 3/ Load final summaries to explore results ####

# Choix de la web coverage
web_coverage <- "full"
# web_coverage <- "dipt"
# web_coverage <- "hymeno"

# Choix du model_type
model_type <- "str"
model_type <- "rich"

# Choix de l'inclusion ou non des r?seaux polaires
# polar <- "with_polar"
polar <- "no_polar"

# Choix de la VAE
VAE <- "C" 
VAE <- "Li" 
VAE <- "Lp"
VAE <- "S" 
VAE <- "I"
VAE <- "P"

model_dataset <- paste0(web_coverage, "_", model_type, "_", polar)
model_name <- paste0(VAE, "_", web_coverage, "_", model_type, "_", polar)

load(file = paste0("./Models_outputs_no_sampling/",VAE,"/",model_name,"/",model_name,"_summaries.RData"))

summary_MAM.avg.coefs_std
summary_Bestmod_coefs_std
View(summary_table_coefs)
View(summary_table_criteria)

# Nb of regional networks in the model
best_model <- readRDS(file = paste0("./Models_outputs_no_sampling/",VAE,"/",model_name,"/",model_name,"_bestmod_AICc.RData"))
nrow(best_model$model)

aggreg.webs <- readRDS(file = paste0("./Data/Filtered_Datasets/aggreg.webs_", model_dataset, ".RData"))
