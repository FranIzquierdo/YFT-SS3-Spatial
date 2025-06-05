#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code for Yellowfin SS spatial model             #
# Script to get output of 100 reps for each model #
# Francisco Izquierdo                             # 
# Marta Cousido-Rocha & Giancarlo Moron           #
# francisco.izqtar@gmail.com                      #
# Last edited on 19/12(2023)                      #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Show document outline: Ctrl + Shift + O

# This script takes the 100 replicates of each alternative model and base model
# reads all the info and makes an RData with the desired quantities we will use
# for plotting. It saves the RData for each alternative model and the best model

## Start here ------------------------------------------------------------------

rm(list = ls())
library(r4ss)
require(stringr)
require(dplyr)

# List all reps ----------------------------------------------------------------

# Directories for alternative models and base model replicates (100 each run:)
a_models_directory <- paste0(getwd(), "/100 replicates/alternative model reps") 
b_models_directory <- paste0(getwd(), "/100 replicates/best model reps 4area_25") 

# Obtain the list of files (100 reps for each model) 
a_files_in_models <- list.files(a_models_directory, full.names = TRUE) 
b_files_in_models <- list.files(b_models_directory, full.names = TRUE) 

# get reps fn ------------------------------------------------------------------

process_model_results <- function(dir_mods, label_mods, saveDir) {
  dq_all = list()
  ts_all = list()
  fish_all = list()
  recD_all = list()
  mov_all = list()
  cpue_all <- list()  # New list to store CPUE data
  tag_all1 <- list()  # List to store tag data from tagdbase1
  tag_all2 <- list()  # List to store tag data from tagdbase2
  
  cList = 1
  for (j in seq_along(dir_mods)) {
    mod_path = file.path(saveDir, dir_mods[j])
    iter_name <- sub(".*dat_4A_(\\d+).*", "\\1", dir_mods[j])
    
    if (grepl("^\\d+$", iter_name)) {
      iter_name <- as.numeric(iter_name)
    } else {
      iter_name <- NA
    }    
    
    tmp_mod = SS_output(dir = mod_path, covar = FALSE, verbose = FALSE, printstats = FALSE)
    
    # Derived quantities
    B0 = tmp_mod$timeseries$Bio_all[tmp_mod$timeseries$Era == 'VIRG']
    Bstatus = tmp_mod$timeseries$Bio_all[tmp_mod$timeseries$Yr == tmp_mod$endyr] / B0
    R0 = tmp_mod$timeseries$Recruit_0[tmp_mod$timeseries$Era == 'VIRG']
    SSBmsy = tmp_mod$derived_quants[which(tmp_mod$derived_quants$Label == 'SSB_MSY'), 'Value']
    Fmsy = tmp_mod$derived_quants[which(tmp_mod$derived_quants$Label == 'annF_MSY'), 'Value']
    MSY = tmp_mod$derived_quants[which(tmp_mod$derived_quants$Label == 'Dead_Catch_MSY'), 'Value']
    Qpar = exp(tmp_mod$parameters$Value[grep(pattern = 'LnQ_base', x = tmp_mod$parameters$Label)])
    dq_df = data.frame(iter = iter_name, em = label_mods[j], 
                       B0 = B0, Bstatus = Bstatus, Area = tmp_mod$timeseries$Area[tmp_mod$timeseries$Era == 'VIRG'], 
                       Season = tmp_mod$timeseries$Seas[tmp_mod$timeseries$Era == 'VIRG'],
                       R0 = R0, SSBmsy = SSBmsy, Fmsy = Fmsy, MSY = MSY, Qpar = Qpar,
                       grad = tmp_mod$maximum_gradient_component)
    
    # Time series
    thisYear = tmp_mod$timeseries$Yr %in% tmp_mod$startyr:tmp_mod$endyr
    SSB = tmp_mod$timeseries$SpawnBio[thisYear]
    TotB = tmp_mod$timeseries$Bio_all[thisYear]
    Rec = tmp_mod$timeseries$Recruit_0[thisYear]
    ts_df = data.frame(iter = iter_name, em = label_mods[j], Area = tmp_mod$timeseries$Area[thisYear],
                       Yr = tmp_mod$timeseries$Yr[thisYear], Seas = tmp_mod$timeseries$Seas[thisYear],
                       SSB = SSB, TotB = TotB, Rec = Rec)
    
    # Fishing mortality and catch
    prevF = tmp_mod$timeseries[thisYear, c(1, 2, 4, grep(pattern = 'F:_', x = colnames(tmp_mod$timeseries)))]
    fish_df = tidyr::gather(prevF, 'Fleet', 'FishM', 4:ncol(prevF))
    prevCatch = tmp_mod$timeseries[thisYear, grep(pattern = 'dead\\(B\\)', x = colnames(tmp_mod$timeseries))]
    fish_df = cbind(fish_df, tidyr::gather(prevCatch, 'Fleet2', 'Catch', 1:ncol(prevCatch)))
    fish_df$iter = iter_name
    fish_df$em = label_mods[j]
    
    # CPUE Data
    cpue_df <- tmp_mod$cpue
    cpue_df$iter <- iter_name
    cpue_df$em <- label_mods[j]
    cpue_all[[cList]] <- cpue_df
    
    # TAG Data (from replist[["tagdbase1"]] and replist[["tagdbase2"]])
    tag_df1 <- tmp_mod$tagdbase1
    tag_df2 <- tmp_mod$tagdbase2
    tag_df1$iter <- iter_name
    tag_df1$em <- label_mods[j]
    tag_df2$iter <- iter_name
    tag_df2$em <- label_mods[j]
    
    # Save each tag dataset separately
    tag_all1[[cList]] <- tag_df1
    tag_all2[[cList]] <- tag_df2
    
    # Recruitment and movement rates (only for 4A model)
    if (tmp_mod$nareas > 1) {
      recD_df = tmp_mod$recruitment_dist$recruit_dist[, c(4, 6, 9)] 
      recD_df$iter = iter_name
      recD_df$em = label_mods[j]
      recD_all[[cList]] = recD_df
    }
    
    if (!is.null(tmp_mod$movement)) {
      prevMov = tmp_mod$movement
      inmatureRate = prevMov[,'age2']
      matureRate = prevMov[,ncol(prevMov)]
      mov_df = data.frame(iter = iter_name, em = label_mods[j], Seas = prevMov$Seas, area1 = prevMov$Source_area,
                          area2 = prevMov$Dest_area, inmatureRate = inmatureRate, matureRate = matureRate)
      mov_all[[cList]] = mov_df
    }
    
    dq_all[[cList]] = dq_df
    ts_all[[cList]] = ts_df
    fish_all[[cList]] = fish_df
    
    cList = cList + 1
    print(j)
  }
  
  # Merge lists
  save_data = list()
  save_data$dq = dplyr::bind_rows(dq_all)
  save_data$ts = dplyr::bind_rows(ts_all)
  save_data$fish = dplyr::bind_rows(fish_all)
  save_data$recD = dplyr::bind_rows(recD_all)
  save_data$mov = dplyr::bind_rows(mov_all)
  save_data$cpue <- dplyr::bind_rows(cpue_all)  # Add CPUE data
  save_data$tag1 <- dplyr::bind_rows(tag_all1)  # Save tag data from tagdbase1
  save_data$tag2 <- dplyr::bind_rows(tag_all2)  # Save tag data from tagdbase2
  
  # Modify the output filename creation
  output_filename <- sub("_1.RDS$", ".RDS", paste0("output_", gsub("[^A-Za-z0-9]+", "_", sub(" rep ", "_reps_", label_mods[1])), ".RDS"))
  output_filepath <- file.path(saveFolder, output_filename)
  
  # Save the output to an RDS file
  saveRDS(save_data, output_filepath)
  
  # Return the saved data
  return(save_data)
}

#tagdbase1 might represent a specific tagging event or type, such as the initial release of tags or tags observed at a certain point.
#tagdbase2 could represent a different tagging event or type, such as subsequent recaptures, different cohorts, or another subset of the tagging data.


# Use function -----------------------------------------------------------------

saveFolder = paste0(getwd(), '/100 replicates/output replicates') # folder to save produced RData from this script
dir.create(saveFolder)

# Directory where you have your models:

# Best model:
saveDir = b_models_directory # for base model runs
reps <- grep("dat", basename(b_files_in_models), value = TRUE) # best model

# Alternative models: 
saveDir = a_models_directory # for alternative model runs
reps <- grep("^S5.3 ", basename(a_files_in_models), value = TRUE)

# Filter files whose name starts with:
dir_mods = reps

# Labels for plotting:
label_mods <- paste(gsub("dat_4A_\\d+", "", dir_mods[1]), "rep ", as.numeric(sub(".*_(\\d+)$", "\\1", dir_mods)), sep = "")

# Use the function:
result_data <- process_model_results(dir_mods, label_mods, saveDir)

