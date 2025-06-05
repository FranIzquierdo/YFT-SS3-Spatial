#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code for Yellowfin SS spatial model
# Script to run 100 best model replicates
# Francisco Izquierdo 
# Marta Cousido-Rocha & Giancarlo Moron
# francisco.izqtar@gmail.com
# Last edited on 19/12(2023)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script takes the 100 simulated YFT datasets, 
# Takes the best model configuration (S6.3) ss files as a template
# It structures the 100 rep datasets using the function get_initial_files
# Finally, it runs the 100 base model replicates

# Show document outline: Ctrl + Shift + O

# Start here -------------------------------------------------------------------

rm(list = ls())

# Set working directory
mainDir <- getwd()
setwd(mainDir)

# Libraries
library(r4ss)
library(Rfssa)
library(doSNOW)
library(parallel)
nCoresRemain <- 3 # num. of cores to be not used

# Read data --------------------------------------------------------------------

# 100 datasets with stochasticity provided by the NOAA experiment organizers
mydata <- load("C:/Users/frank/Desktop/Core/Cap. 2 - Yellowfin spatial/Code/Paper spatial complexity/Four areas/100 replicates/obs 4area_1_100_ESS_25/YFT_4area_observations_1_100_ESS_25.RData")
type_data <- '100 replicates/best model reps 4area_25'
dir.create(path = type_data)

# SS templates -----------------------------------------------------------------

# Select the best model files (S6.3 rec RW2) as a template for the 100 replicates

# Templates of the Stock Synthesis files with our final best model configuration
template_dat <- r4ss::SS_readdat_3.30(file = 'modelling steps/S6.3 rec RW2/data.ss')
template_ctl <- r4ss::SS_readctl_3.30(file = 'modelling steps/S6.3 rec RW2/control.ss', datlist = 'modelling steps/S6.3 rec RW2/data.ss_new')
template_start <- r4ss::SS_readstarter(file = 'modelling steps/S6.3 rec RW2/starter.ss')
template_fore <- r4ss::SS_readforecast(file = 'modelling steps/S6.3 rec RW2/forecast.ss')

# Fn get initial files ---------------------------------------------------------

# Function that helps to structure the 100 datasets and the 100 standardized 
# CPUE trends in the SS base model configuration

get_initial_files <- function(cpue_bst, sim_dat, dat) {
  
  # Initial info
  OutDataFile <- dat
  myDataFile <- sim_dat
  
  # Data ss structure
  OutDataFile$N_areas <- myDataFile$N_areas
  OutDataFile$Nfleets <- myDataFile$Nfleet + myDataFile$Nsurveys
  OutDataFile$fleetinfo <- data.frame(type = c(rep(1, times = myDataFile$Nfleet), rep(3, times = myDataFile$Nsurveys)), 
                                      surveytiming = c(rep(-1, times = myDataFile$Nfleet), rep(5.5, times = myDataFile$Nsurveys)),
                                      area = as.vector(as.matrix(myDataFile$fleetinfo1[2,])),
                                      units = 2, 
                                      need_catch_mult = 0,
                                      fleetname = myDataFile$fleetnames)
  OutDataFile$fleetnames <- myDataFile$fleetnames
  OutDataFile$surveytiming <- c(rep(-1, times = myDataFile$Nfleet), rep(5.5, times = myDataFile$Nsurveys))
  OutDataFile$units_of_catch <- rep(2, times = OutDataFile$Nfleets)
  OutDataFile$areas <- as.vector(as.matrix(myDataFile$fleetinfo1[2,]))
  
  # Catch data
  aux <- OutDataFile$catch
  cat <- myDataFile$catch
  l <- nrow(cat)
  
  l_time <- length(1952:2015)
  time <- data.frame(sort(rep(1952:2015, 4)), rep(1:4, l_time))
  laux <- dim(cat)[1]
  for (i in 1:laux) {
    cat[i, 17:18] <- time[cat$year[i], ]
  }
  
  library(reshape2)
  cat <- melt(cat, id.vars = c("year", "seas"))
  cat$variable <- sort(rep(1:16, dim(time)[1]))
  cat$catch_se <- rep(myDataFile$se_log_catch)
  colnames(cat) <- c("year", "seas", "fleet", "catch", "catch_se")
  OutDataFile$catch <- as.data.frame(cat) 
  
  # CPUE data
  OutDataFile$CPUEinfo <- data.frame(Fleet = myDataFile$CPUEinfo$Fleet, Units = 0, Errtype = myDataFile$CPUEinfo$Errtype,
                                     SD_Report = 0)
  rownames(OutDataFile$CPUEinfo) <- myDataFile$fleetnames
  index_vec <- as.numeric(as.character(myDataFile$CPUE$index))
  
  # Standardized CPUE new input
  OutDataFile$CPUE <- cpue_bst
  
  #Length composition data
  OutDataFile$len_info <- data.frame(mintailcomp = -0.001, addtocomp = myDataFile$add_to_comp, 
                                     combine_M_F = 0, CompressBins = 0, CompError = 0, ParmSelect = 0, #Dirichlet parameters
                                     minsamplesize = rep(0.01, times = OutDataFile$Nfleets))
  rownames(OutDataFile$len_info) <- OutDataFile$fleetnames
  
  # Pseudoyears to years
  l_time <- length(1952:2015)
  time <- data.frame(sort(rep(1952:2015, 4)), rep(c(1, 4, 7, 10), l_time))
  b <- myDataFile$lencomp
  lb <- dim(b)[1]
  for (i in 1:lb) {
    b[i, 1:2] <- time[b$Yr[i], ]
  }
  OutDataFile$lencomp <- b
  OutDataFile$lencomp$Nsamp <- rep(25) # NSamp must be 25
  OutDataFile$lencomp$Yr <- OutDataFile$lencomp$Yr
  OutDataFile$Nfleet <- myDataFile$Nfleet
  OutDataFile$Nsurveys <- myDataFile$Nsurveys
  OutDataFile$fleetinfo1 <- t(OutDataFile$fleetinfo[, c('surveytiming', 'area', 'type')])
  OutDataFile$fleetinfo2 <- t(data.frame(units = OutDataFile$units_of_catch, need_catch_mult = 0))
  OutDataFile$max_combined_lbin <- rep(myDataFile$max_combined_lbin, times = OutDataFile$Nfleets)
  
  # Tag data
  OutDataFile$do_tags <- myDataFile$do_tags
  OutDataFile$N_tag_groups <- myDataFile$N_tag_groups
  OutDataFile$N_recap_events <- myDataFile$N_recap_events
  OutDataFile$mixing_latency_period <- 3 #myDataFile$mixing_latency_period
  OutDataFile$max_periods <- 6
  
  # Pseudo-years to years
  l_time <- length(1952:2015)
  time <- data.frame(sort(rep(1952:2015, 4)), rep(1:4, l_time))
  age_new <- data.frame(1:28, sort(c(0, 0, 0, rep(1:6, 4), 7))); colnames(age_new) <- c("pseudo", "final")
  aux <- myDataFile$tag_releases
  laux <- dim(aux)[1]
  for (i in 1:laux) {
    aux[i, 3:4] <- time[aux$yr[i], ]
    aux[i, 5] <- time[aux$tfill[i], 1]
    aux[i, 7] <- age_new[aux$age[i], 2]
  }
  
  OutDataFile$tag_releases <- aux
  
  aux <- myDataFile$tag_recaps
  laux <- dim(aux)[1]
  for (i in 1:laux) {
    aux[i, 2:3] <- time[aux$yr[i], ]
  }
  OutDataFile$tag_recaps <- aux
  
  # Output files
  myFiles <- list(dat = OutDataFile)
  return(myFiles)
  
}

# Loop SS files ----------------------------------------------------------------

# Function that loops and creates the 100 control.ss and data.ss files

for (i in seq_along(mydata)) {
  
  dir.create(path = file.path(type_data, mydata[i]))
  this_data <- get(mydata[i])
  
  # Read new CPUE 
  cpue_path <- paste0(getwd(), "/base model files/standardized CPUE/SSinputs/")
  j <- substr(mydata[i], 8, 9)
  j <- as.numeric(as.character(j))
  cpue_sim <- paste0("input_SS_4A_", j, ".RData")
  load(paste0(cpue_path, cpue_sim))
  # 4 area, besag st: year seas index obs se
  library(dplyr)
  CPUE_bst4 <- input_SS_4A %>% select(year, seas, regionID, sumX50, sumSd)
  CPUE_bst4$regionID <- as.factor(CPUE_bst4$regionID)
  levels(CPUE_bst4$regionID) <- c(17, 18, 19, 20)  # Rename regions 1 2 3 4 with fleet 17 18 19 20
  levels(CPUE_bst4$seas) <- c(1, 4, 7, 10)  # Season levels
  CPUE_bst4$se <- sqrt(log(1 + (0.1^2)))  # Standard error for CV=0.2
  CPUE_bst4 <- CPUE_bst4[, c(1, 2, 3, 4, 6)]  # Order
  colnames(CPUE_bst4) <- c("year", "seas", "index", "obs", "se")  # Rename columns
  CPUE_bst4$year <- as.numeric(as.character(CPUE_bst4$year))
  CPUE_bst4$seas <- as.numeric(as.character(CPUE_bst4$seas))
  CPUE_bst4$index <- as.numeric(as.character(CPUE_bst4$index))
  CPUE_bst4 <- round(CPUE_bst4, 4)
  
  # Create dat and ctl 
  outFiles <- get_initial_files(cpue_bst = CPUE_bst4, sim_dat = this_data, dat = template_dat)
  
  # Write SS files 
  r4ss::SS_writedat_3.30(datlist = outFiles$dat, outfile = paste0(file.path(type_data, mydata[i]), '/data.ss'), overwrite = TRUE)
  r4ss::SS_writestarter(mylist = template_start, file = paste0(file.path(type_data,  mydata[i]), '/starter.ss'), overwrite = TRUE)
  r4ss::SS_writeforecast(mylist = template_fore, file = paste0(file.path(type_data, mydata[i]), '/forecast.ss'), overwrite = TRUE)
  r4ss::SS_writectl_3.30(ctllist = template_ctl, outfile = paste0(file.path(type_data, mydata[i]), '/control.ss'), overwrite = TRUE)
  
  print(j)
  
}

# Number of cores --------------------------------------------------------------

cores <- detectCores()
cl <- makeCluster(cores[1] - nCoresRemain)
registerDoSNOW(cl)

# Run SS parallel --------------------------------------------------------------
 
# Increase memory limit
memory.limit(size = 100000)


mydata <- CPUE_reps

foreach(ix = seq_along(mydata)) %dopar% {
  # Run SS3:
  dir <- paste0(a_models_directory, '/', mydata[ix])
  command <- paste("cd", dir, "& ss -nohess", sep = " ")
  ss <- shell(cmd = command, intern = TRUE, wait = TRUE)
}

stopCluster(cl)
