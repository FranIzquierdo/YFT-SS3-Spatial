#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code for Yellowfin SS spatial model           #
# Script to run 100 reps for alternative models #
# Francisco Izquierdo                           # 
# Marta Cousido-Rocha & Giancarlo Moron         #
# francisco.izqtar@gmail.com                    #
# Last edited on 19/12(2023)                    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Show document outline: Ctrl + Shift + O

# This script works using as reference the 100 replicates of the best model 
# (script: 2A best model replicates)
# I.e., the best model already has the 100 stochastic datasets applied
# to its corresponding model file settings

# Here, we are going to apply the 100 replicas to the different configurations 
# of the desired alternative modelling steps

# To do this, we will first use a function that takes all the files of the 100 reps 
# of the best model and overwrites the control files with the ones coming from 
# the alternative models folder

# Keep in mind that the data files are also those of the 100 reps of the best model,
# so for specific alternative models we need to edit their data file 
# (I.e., in order to remove tagging data input, CPUE...)
# Then, we will apply a specific function that modifies the data files needed in each

# Finally, we run the 100 replicates of all alternative model configurations

# Start here -------------------------------------------------------------------

rm(list = ls())

library(fs)
library(r4ss)
library(fs)
library(r4ss)
library(dplyr)

# Function for all ctl ---------------------------------------------------------

# Here we copy all files (data ctl starter and forecast) of the best model 100 reps
# Then we overwrite in each of the 100 replicates the control file correspondent
# to each alternative model step

# These ctl alternetive models are: 

# S1.1 selex at length
# S2.1 CPUE or. Q est
# S2.2 CPUE st Q cte
# S4.1 mov 1-2 1-4 3-4 2-3
# S4.2 mov 1-2 1-4 3-4 
# S5.1 mov 1-2 1-4 3-4 tag mlp=3
# S5.2 mov 1-2 1-4 3-4 tag mlp=6
# S5.2 mov 1-2 1-4 3-4 tag mlp=1
# S6.1 Rec 1, 2
# S6.2 Rec 1, 2, 4 

copy_files_ctl_ss <- function(source_directory1, source_directory2, destination_directory) {
  # Create the destination directory if it doesn't exist
  if (!dir.exists(destination_directory)) {
    dir.create(destination_directory)
  }
  
  # Get the list of subfolders in source_directory1
  subfolders_source1 <- list.dirs(source_directory1, full.names = TRUE, recursive = FALSE)
  
  # Get the list of subfolders in source_directory2 (each containing a different control.ss file)
  subfolders_source2 <- list.dirs(source_directory2, full.names = TRUE, recursive = FALSE)
  
  # Iterate over all subfolders in source_directory1
  for (i in seq_along(subfolders_source1)) {
    subfolder_source1 <- subfolders_source1[i]
    
    # Iterate over all subfolders in source_directory2 to copy and overwrite control files
    for (subfolder_source2 in subfolders_source2) {
      # Create the customized folder name in the destination directory
      model_name <- basename(subfolder_source2)
      folder_name <- paste0(model_name, " dat_4A_", i)
      folder_destination <- file.path(destination_directory, folder_name)
      
      if (!dir.exists(folder_destination)) {
        dir.create(folder_destination)
      }
      
      # Copy the files from source_directory1 to the destination (data.ss, forecast.ss, starter.ss, control.ss)
      files_to_copy <- c("data.ss", "forecast.ss", "starter.ss", "control.ss")
      
      for (file in files_to_copy) {
        # Path in source_directory1
        path_source1 <- file.path(subfolder_source1, file)
        
        # Path in destination_directory
        path_destination <- file.path(folder_destination, file)
        
        # Copy the files if they exist
        if (file.exists(path_source1)) {
          file.copy(path_source1, path_destination, overwrite = TRUE)
          print(paste("Copied:", path_source1, "to", path_destination))
        } else {
          warning(paste(file, "not found in:", subfolder_source1))
        }
      }
      
      # Overwrite the control.ss file in the destination directory with the one from source_directory2
      path_source2_control <- file.path(subfolder_source2, "control.ss")
      path_destination_control <- file.path(folder_destination, "control.ss")
      
      if (file.exists(path_source2_control)) {
        file.copy(path_source2_control, path_destination_control, overwrite = TRUE)
        print(paste("Overwritten control.ss in", folder_destination, "with", path_source2_control))
      } else {
        warning(paste("control.ss not found in:", subfolder_source2))
      }
    }
  }
}

# Example usage
source_directory1 <- file.path(getwd(), "/100 replicates/best model reps 4area_25")
source_directory2 <- file.path(getwd(), "/100 replicates/alternative ctl")
destination_directory <- file.path(getwd(), "/100 replicates/alternative model reps")

copy_files_ctl_ss(source_directory1, source_directory2, destination_directory)

# Function for dat S1.1 --------------------------------------------------------

# In this case, we re-input the original CPUE dataset, and remove tagging

copy_files_dat_CPUE_Q <- function(source_directory1, source_directory2, destination_directory) {
  # Create the destination directory if it doesn't exist
  if (!dir_exists(destination_directory)) {
    dir_create(destination_directory)
  }
  
  # Get the list of subfolders in the source1 directory
  subfolders_source1 <- dir_ls(source_directory1, type = "directory")
  subfolders_source2 <- load(source_directory2)  # Load additional info from RData
  
  # Iterate over all subfolders in source1
  for (i in seq_along(subfolders_source1)) {
    subfolder_source1 <- subfolders_source1[i]
    
    # Construct the complete path of the source and destination folders
    folder_source1 <- subfolder_source1
    folder_destination <- file.path(destination_directory, paste0("S1.1 selex at length ", basename(folder_source1)))
    
    # Create the subfolder in the destination directory if it doesn't exist
    if (!dir_exists(folder_destination)) {
      dir_create(folder_destination)
    }
    
    # Search and copy/modify only the data.ss file
    path_source1 <- file.path(folder_source1, "data.ss")
    path_destination <- file.path(folder_destination, "data.ss")
    
    if (file_exists(path_source1)) {
      # Read the data.ss file from source1
      data_content <- r4ss::SS_readdat_3.30(file = path_source1)
      
      # Read additional information from source2
      additional_info <- get(subfolders_source2[[i]])
      
      # Transform pseudoyears into actual years and seasons
      transform_pseudoyears <- function(data) {
        pseudoyears <- seq(1, 256)
        actual_years <- rep(1952:2015, each = 4)
        seasons <- rep(c(1, 4, 7, 10), length.out = length(pseudoyears))
        
        df <- data.frame(Pseudoyears = pseudoyears, Actual_Years = actual_years, Season = seasons)
        df_vector <- data.frame(Pseudoyears = seq(81, 256))
        result <- merge(df_vector, df, by = "Pseudoyears", all.x = TRUE)
        transformed_data <- merge(data, result, by.x = "year", by.y = "Pseudoyears", all.x = TRUE)
        
        library(dplyr)
        sorted_data <- transformed_data %>%
          arrange(year, seas, index) %>%
          select(Actual_Years, Season, index, cpu, cv) %>%
          rename(year = Actual_Years, seas = Season, obs = cpu, se = cv)
        
        return(sorted_data)
      }
      
      # Apply the transformation to the CPUE data
      transformed_data <- transform_pseudoyears(additional_info$CPUE)
      data_content$CPUE <- transformed_data
      
      # Remove tagging information
      data_content$do_tags <- 0
      data_content$tag_releases <- NULL
      data_content$tag_recaps <- NULL
      
      # Write the edited content back to the data.ss file
      r4ss::SS_writedat_3.30(datlist = data_content, outfile = path_destination, overwrite = TRUE)
    }
  }
}

# Use the function
source_directory1 <- file.path(getwd(), "/100 replicates/best model reps 4area_25")
source_directory2 <- file.path(getwd(), "/100 replicates/obs 4area_1_100_ESS_25/YFT_4area_observations_1_100_ESS_25.RData")
destination_directory <- file.path(getwd(), "/100 replicates/alternative model reps")

copy_files_dat_CPUE_Q(source_directory1, source_directory2, destination_directory)

# Function for dat S2.1 --------------------------------------------------------

# In this case, we re-input the original CPUE dataset, set Q parameter estimation,
# and remove tagging

copy_files_dat_CPUE_Q <- function(source_directory1, source_directory2, destination_directory) {
  # Create the destination directory if it doesn't exist
  if (!dir_exists(destination_directory)) {
    dir_create(destination_directory)
  }
  
  # Get the list of subfolders in the source1 directory
  subfolders_source1 <- dir_ls(source_directory1, type = "directory")
  subfolders_source2 <- load(source_directory2)  # Load additional info from RData
  
  # Iterate over all subfolders in source1
  for (i in seq_along(subfolders_source1)) {
    subfolder_source1 <- subfolders_source1[i]
    
    # Construct the complete path of the source and destination folders
    folder_source1 <- subfolder_source1
    folder_destination <- file.path(destination_directory, paste0("S2.1 CPUE or. Q est ", basename(folder_source1)))
    
    # Create the subfolder in the destination directory if it doesn't exist
    if (!dir_exists(folder_destination)) {
      dir_create(folder_destination)
    }
    
    # Search and copy/modify only the data.ss file
    path_source1 <- file.path(folder_source1, "data.ss")
    path_destination <- file.path(folder_destination, "data.ss")
    
    if (file_exists(path_source1)) {
      # Read the data.ss file from source1
      data_content <- r4ss::SS_readdat_3.30(file = path_source1)
      
      # Read additional information from source2
      additional_info <- get(subfolders_source2[[i]])  # Adjusted to match S1.1
      
      # Transform pseudoyears into actual years and seasons
      transform_pseudoyears <- function(data) {
        pseudoyears <- seq(1, 256)
        actual_years <- rep(1952:2015, each = 4)
        seasons <- rep(1:4, length.out = length(pseudoyears))
        
        df <- data.frame(Pseudoyears = pseudoyears, Actual_Years = actual_years, Season = seasons)
        df_vector <- data.frame(Pseudoyears = seq(81, 256))
        result <- merge(df_vector, df, by = "Pseudoyears", all.x = TRUE)
        transformed_data <- merge(data, result, by.x = "year", by.y = "Pseudoyears", all.x = TRUE)
        transformed_data <- transformed_data[, c("Actual_Years", "Season", "index", "cpu", "cv")]
        colnames(transformed_data) <- c("year", "seas", "index", "obs", "se")
        
        return(transformed_data)
      }
      
      # Apply the transformation to the CPUE data
      transformed_data <- transform_pseudoyears(additional_info$CPUE)
      data_content$CPUE <- transformed_data
      
      # Remove tagging information
      data_content$do_tags <- 0
      data_content$tag_releases <- NULL
      data_content$tag_recaps <- NULL
      
      # Write the edited content back to the data.ss file
      r4ss::SS_writedat_3.30(datlist = data_content, outfile = path_destination, overwrite = TRUE)
    }
  }
}

# Use the function
source_directory1 <- file.path(getwd(), "/100 replicates/best model reps 4area_25")
source_directory2 <- file.path(getwd(), "/100 replicates/obs 4area_1_100_ESS_25/YFT_4area_observations_1_100_ESS_25.RData")
destination_directory <- file.path(getwd(), "/100 replicates/alternative model reps")

copy_files_dat_CPUE_Q(source_directory1, source_directory2, destination_directory)

# Function for dat S2.2 --------------------------------------------------------

# In this case we remove tagging data

copy_files_dat_CPUE_Q <- function(source_directory1, destination_directory) {
  # Create the destination directory if it doesn't exist
  if (!dir_exists(destination_directory)) {
    dir_create(destination_directory)
  }
  
  # Get the list of subfolders in the source directory
  subfolders_source1 <- dir_ls(source_directory1, type = "directory")
  
  # Iterate over all subfolders
  for (i in seq_along(subfolders_source1)) {
    subfolder_source1 <- subfolders_source1[i]
    
    # Construct the complete path of source and destination folders
    folder_source1 <- subfolder_source1
    folder_destination <- file.path(destination_directory, paste0("S2.2 CPUE st Q cte ", basename(folder_source1)))
    
    # Create the subfolder in the destination directory if it doesn't exist
    if (!dir_exists(folder_destination)) {
      dir_create(folder_destination)
    }
    
    # Define the file to edit and copy (data.ss)
    path_source1 <- file.path(folder_source1, "data.ss")
    path_destination <- file.path(folder_destination, "data.ss")
    
    if (file_exists(path_source1)) {
      # Print the path for debugging
      print(paste("Reading:", path_source1))
      
      # Read the data.ss file from source1
      data_content <- r4ss::SS_readdat_3.30(file = path_source1)
      
      # Remove tagging information
      data_content$do_tags <- 0
      data_content$tag_releases <- NULL
      data_content$tag_recaps <- NULL
      
      # Print the path for debugging
      print(paste("Writing:", path_destination))
      
      # Write the edited content back to the data.ss file
      r4ss::SS_writedat_3.30(datlist = data_content, outfile = path_destination, overwrite = TRUE)
    }
  }
}

# Use the function
source_directory1 <- file.path(getwd(), "/100 replicates/best model reps 4area_25")
destination_directory <- file.path(getwd(), "/100 replicates/alternative model reps")
copy_files_dat_CPUE_Q(source_directory1, destination_directory)

# Function for dat S4.1 --------------------------------------------------------

# Remove tag data
library(fs)
library(r4ss)

copy_files_dat_CPUE_Q <- function(source_directory1, destination_directory) {
  # Create the destination directory if it doesn't exist
  if (!dir_exists(destination_directory)) {
    dir_create(destination_directory)
  }
  
  # Get the list of subfolders in the source directory
  subfolders_source1 <- dir_ls(source_directory1, type = "directory")
  
  # Iterate over all subfolders
  for (i in seq_along(subfolders_source1)) {
    subfolder_source1 <- subfolders_source1[i]
    
    # Construct the complete path of source and destination folders
    folder_source1 <- subfolder_source1
    folder_destination <- file.path(destination_directory, paste0("S4.1 mov 1-2 1-4 3-4 2-3 ", basename(folder_source1)))
    
    # Create the subfolder in the destination directory if it doesn't exist
    if (!dir_exists(folder_destination)) {
      dir_create(folder_destination)
    }
    
    # Define the file to edit and copy (data.ss)
    path_source1 <- file.path(folder_source1, "data.ss")
    path_destination <- file.path(folder_destination, "data.ss")
    
    if (file_exists(path_source1)) {
      # Print the path for debugging
      print(paste("Reading:", path_source1))
      
      # Read the data.ss file from source1
      data_content <- r4ss::SS_readdat_3.30(file = path_source1)
      
      # Remove tagging information
      data_content$do_tags <- 0
      data_content$tag_releases <- NULL
      data_content$tag_recaps <- NULL
      
      # Print the path for debugging
      print(paste("Writing:", path_destination))
      
      # Write the edited content back to the data.ss file
      r4ss::SS_writedat_3.30(datlist = data_content, outfile = path_destination, overwrite = TRUE)
    }
  }
}

# Use the function
source_directory1 <- file.path(getwd(), "/100 replicates/best model reps 4area_25")
destination_directory <- file.path(getwd(), "/100 replicates/alternative model reps")

copy_files_dat_CPUE_Q(source_directory1, destination_directory)

# Function for dat S4.2 --------------------------------------------------------

# In this case we remove tagging data
library(fs)
library(r4ss)

copy_files_dat_CPUE_Q <- function(source_directory1, destination_directory) {
  # Create the destination directory if it doesn't exist
  if (!dir_exists(destination_directory)) {
    dir_create(destination_directory)
  }
  
  # Get the list of subfolders in the source directory
  subfolders_source1 <- dir_ls(source_directory1, type = "directory")
  
  # Iterate over all subfolders
  for (i in seq_along(subfolders_source1)) {
    subfolder_source1 <- subfolders_source1[i]
    
    # Construct the complete path of source and destination folders
    folder_source1 <- subfolder_source1
    folder_destination <- file.path(destination_directory, paste0("S4.2 mov 1-2 1-4 3-4 ", basename(folder_source1)))
    
    # Create the subfolder in the destination directory if it doesn't exist
    if (!dir_exists(folder_destination)) {
      dir_create(folder_destination)
    }
    
    # Define the file to edit and copy (data.ss)
    path_source1 <- file.path(folder_source1, "data.ss")
    path_destination <- file.path(folder_destination, "data.ss")
    
    if (file_exists(path_source1)) {
      # Print the path for debugging
      print(paste("Reading:", path_source1))
      
      # Read the data.ss file from source1
      data_content <- r4ss::SS_readdat_3.30(file = path_source1)
      
      # Remove tagging information
      data_content$do_tags <- 0
      data_content$tag_releases <- NULL
      data_content$tag_recaps <- NULL
      
      # Print the path for debugging
      print(paste("Writing:", path_destination))
      
      # Write the edited content back to the data.ss file
      r4ss::SS_writedat_3.30(datlist = data_content, outfile = path_destination, overwrite = TRUE)
    }
  }
}

# Use the function
source_directory1 <- file.path(getwd(), "/100 replicates/best model reps 4area_25")
destination_directory <- file.path(getwd(), "/100 replicates/alternative model reps")
copy_files_dat_CPUE_Q(source_directory1, destination_directory)

# Function for dat S5.1 --------------------------------------------------------

# In this case we change mixing latency period from 3 to 6

library(fs)
library(r4ss)

copy_files_dat_CPUE_Q <- function(source_directory1, destination_directory) {
  # Create the destination directory if it doesn't exist
  if (!dir_exists(destination_directory)) {
    dir_create(destination_directory)
  }
  
  # Get the list of subfolders in the source directory
  subfolders_source1 <- dir_ls(source_directory1, type = "directory")
  
  # Iterate over all subfolders
  for (i in seq_along(subfolders_source1)) {
    subfolder_source1 <- subfolders_source1[i]
    
    # Construct the complete path of source and destination folders
    folder_source1 <- subfolder_source1
    folder_destination <- file.path(destination_directory, paste0("S5.1 mov 1-2 1-4 3-4 tag mlp=6 ", basename(folder_source1)))
    
    # Create the subfolder in the destination directory if it doesn't exist
    if (!dir_exists(folder_destination)) {
      dir_create(folder_destination)
    }
    
    # Define the file to edit and copy (data.ss)
    path_source1 <- file.path(folder_source1, "data.ss")
    path_destination <- file.path(folder_destination, "data.ss")
    
    if (file_exists(path_source1)) {
      # Print the path for debugging
      print(paste("Reading:", path_source1))
      
      # Read the data.ss file from source1
      data_content <- r4ss::SS_readdat_3.30(file = path_source1)
      
      # Modify the mixing latency period
      data_content$mixing_latency_period <- 6
      
      # Print the path for debugging
      print(paste("Writing:", path_destination))
      
      # Write the edited content back to the data.ss file
      r4ss::SS_writedat_3.30(datlist = data_content, outfile = path_destination, overwrite = TRUE)
    }
  }
}

# Use the function
source_directory1 <- file.path(getwd(), "/100 replicates/best model reps 4area_25")
destination_directory <- file.path(getwd(), "/100 replicates/alternative model reps")
copy_files_dat_CPUE_Q(source_directory1, destination_directory)

# Function for dat S5.3 --------------------------------------------------------

# In this case we change mixing latency period from 3 to 1

library(fs)
library(r4ss)

copy_files_dat_CPUE_Q <- function(source_directory1, destination_directory) {
  # Create the destination directory if it doesn't exist
  if (!dir_exists(destination_directory)) {
    dir_create(destination_directory)
  }
  
  # Get the list of subfolders in the source directory
  subfolders_source1 <- dir_ls(source_directory1, type = "directory")
  
  # Iterate over all subfolders
  for (i in seq_along(subfolders_source1)) {
    subfolder_source1 <- subfolders_source1[i]
    
    # Construct the complete path of source and destination folders
    folder_source1 <- subfolder_source1
    folder_destination <- file.path(destination_directory, paste0("S5.3 mov 1-2 1-4 3-4 tag mlp=1 ", basename(folder_source1)))
    
    # Create the subfolder in the destination directory if it doesn't exist
    if (!dir_exists(folder_destination)) {
      dir_create(folder_destination)
    }
    
    # Define the file to edit and copy (data.ss)
    path_source1 <- file.path(folder_source1, "data.ss")
    path_destination <- file.path(folder_destination, "data.ss")
    
    if (file_exists(path_source1)) {
      # Print the path for debugging
      print(paste("Reading:", path_source1))
      
      # Read the data.ss file from source1
      data_content <- r4ss::SS_readdat_3.30(file = path_source1)
      
      # Modify the mixing latency period
      data_content$mixing_latency_period <- 1
      
      # Print the path for debugging
      print(paste("Writing:", path_destination))
      
      # Write the edited content back to the data.ss file
      r4ss::SS_writedat_3.30(datlist = data_content, outfile = path_destination, overwrite = TRUE)
    }
  }
}

# Use the function
source_directory1 <- file.path(getwd(), "/100 replicates/best model reps 4area_25")
destination_directory <- file.path(getwd(), "/100 replicates/alternative model reps")
copy_files_dat_CPUE_Q(source_directory1, destination_directory)

# Run models -------------------------------------------------------------------

# Libraries:
library(r4ss)
library(Rfssa)
library(doSNOW)
library(parallel)
nCoresRemain <- 4  # num. of cores

# Read data --------------------------------------------------------------------

# Dir folder "alternative models"
a_models_directory <- paste0(getwd(), "/100 replicates/alternative model reps")

# Obtain the list of files (100 reps for each alternative model)
a_files_in_models <- list.files(a_models_directory, full.names = TRUE)

# Change here the name of each set of models and run each time
reps <- grep("^S6.1 ", basename(a_files_in_models), value = TRUE)

# Number of cores ---------------------------------------------------------------

cores <- detectCores()
cl <- makeCluster(cores[1] - nCoresRemain)
registerDoSNOW(cl)

# Run SS parallel ---------------------------------------------------------------

# TODO: run for catch_reps; rec_reps; selex_reps; CPUE_reps; time_series_reps

# Increase memory limit
memory.limit(size = 100000)

mydata <- reps

foreach(ix = seq_along(mydata)) %dopar% {
  # Run SS3:
  dir <- paste0(a_models_directory, '/', mydata[ix])
  command <- paste("cd", dir, "& ss -nohess", sep = " ")
  ss <- shell(cmd = command, intern = TRUE, wait = TRUE)
}

stopCluster(cl)
