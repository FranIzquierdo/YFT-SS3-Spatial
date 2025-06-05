#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code for Yellowfin SS spatial model          #
# Script to run single dat_4A_1 modeling steps #
# Francisco Izquierdo                          #
# Marta Cousido-Rocha & Giancarlo Moron        #
# francisco.izqtar@gmail.com                   #
# Last edited on 19/12(2023)                   #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Show document outline: Ctrl + Shift + O

# This script takes the single dat_4A_1 YFT reference dataset, 
# Run all modelling steps folders (manual configuration) with hessian
# Do retrospective analysis
# Run ss3diags

# Modelling steps:

# S0 initial setup
# S1.1 selex at length
# S2.1 CPUE or. Q est
# S2.2 CPUE st. Q cte
# S3.1 recdevs 1980
# S3.2 recdevs SS 1972
# S4.1 mov 1-2 1-4
# S4.2 mov 1-2 1-4 3-4
# S5.1 mov 1-2 1-4 3-4 tag mlp = 6
# S5.2 mov 1-2 1-4 3-4 tag mlp = 3
# S5.3 mov 1-2 1-4 tag mlp = 1
# S6.1 rec 1,2 
# S6.2 rec 1,2,3
# S6.3 rec RW2
# S7.1 weights Francis
# S7.2 weights MI

# Start here -------------------------------------------------------------------

rm(list=ls()) ## Clean environment
library(r4ss)

## Change manually model step (mod_path) folder
mod_path <- paste0(getwd(), "./modelling steps/S0 initial setup - copia 325", sep="") ## Select model
dir.create(mod_path) ## check that exists

# Run --------------------------------------------------------------------------

## Run models with hessian (-nox)
r4ss::run_SS_models(dirvec = mod_path, model = "ss", exe_in_path = TRUE,
                    verbose=TRUE, extras = "-nox") ## "-nox" or "-nohess" 

# r4ss plot --------------------------------------------------------------------

## Read output
replist <- SS_output(dir = mod_path, verbose=TRUE, printstats=TRUE, covar = T)

## Plot
SS_plots(replist, pdf=F, png=T, html=T, printfolder = "r4ss plots") ## html

## Summary plot
png(file=paste(mod_path,"/a_summaryplot.png",sep=""), width = 900, height = 800,pointsize=18)
par(mfrow=c(3,2))
SSplotCatch(replist, subplots=10); title("Landings")
SSplotSelex(replist, subplot = 1)
SSplotBiology(replist,subplots = 1)
SSplotBiology(replist,subplots = 6); title("Mat")
SSplotSummaryF(replist); title("F")
SSplotTimeseries(replist, subplot = 7, maxyr = 2016); title("Biomass")
dev.off()

#SS_tune_comps( dir = mod_path, replist=replist, option="Francis")
#SS_tune_comps( dir = mod_path, replist=replist, option="MI")

# retros -----------------------------------------------------------------------

rm(list=ls()) ## Clean environment
library(r4ss) 
library(icesAdvice)

## Change manually model step (mod_path) folder
mod_path <- paste0(getwd(), "./modelling steps/S0 initial setup", sep="") ## Select model

## Dir for retros
plotdir_retro<- paste(mod_path, "/retros", sep="")
dir.create(plotdir_retro)

## Retros analysis by creating new directories, copying model files, and 
## changing the starter file to set the number of years of data to exclude

## Note that ss.exe file must be in the model folder

yper=0:-5 ## years period for retros

SS_doRetro(masterdir=mod_path, oldsubdir="", newsubdir = "retros", extras="-nox",
           subdirstart = "retro",years = yper, overwrite = TRUE, exefile = "ss")

retroModels <- SSgetoutput(dirvec=file.path(mod_path, "retros",
                                            paste("retro",yper,sep="")))

retroSummary <- SSsummarize(retroModels) # retro 0 is replist 1

endyrvec <- retroSummary$endyrs + yper

# retro plots -------------------------------------------------------------------

## Comparison plot pdf
SSplotComparisons(retroSummary, endyrvec=endyrvec, xlim=c(1955,2016), 
                  legendlabels=paste("Data",yper,"years"), print=FALSE, pdf=TRUE, 
                  plotdir = plotdir_retro)

# ss3diags ---------------------------------------------------------------------

rm(list=ls()) ## Clean environment
library(r4ss)
library(ss3diags)

## Set model folder
mod_path <- paste0(getwd(), "./modelling steps/S6.1 rec 1,2", sep="") ## Select model

## Create subfolder for plots
dirplot<-paste0(mod_path,"/ss3diags", sep="")
dir.create(dirplot)

yper=0:-5 ## years period for retros

retroModels <- SSgetoutput(dirvec=file.path(mod_path, "/retros",
                                            paste("retro",yper,sep="")))

## Reference run
ss3rep = retroModels[[1]]

## Check Data
sspar()
SSplotData(ss3rep,subplot = 2)
dev.print(jpeg,paste0(dirplot,"/DataSetup_",".jpg"), width = 8, 
          height = 6, res = 300, units = "in")

## For cpue
pdf(paste0(dirplot,"/RunsTestResiduals.pdf"), height = 6.5, width = 11)
sspar(mfrow=c(2,1),plot.cex = 0.8)
SSplotRunstest(ss3rep,subplots="cpue",add=T)
dev.off()

## For length
pdf(paste0(dirplot,"/RunsTestResiduals.pdf"), height = 6.5, width = 11)
sspar(mfrow=c(3,3),plot.cex = 0.8)
SSplotRunstest(ss3rep,subplots="len",add=T)
dev.off()

## Check conflict between mean lengths
pdf(paste0(dirplot,"/JointResiduals_.pdf"), height = 6.5, width = 11)
sspar(mfrow=c(1,2),plot.cex = 0.8)
SSplotJABBAres(ss3rep,subplots="cpue",add=T)
SSplotJABBAres(ss3rep,subplots="len",add=T)
dev.off()

## Check starter file
starter = SSsettingsBratioF(ss3rep)

## Get uncertainty from MVLN for F/F_Btrg with original F setting F_abs
sspar(mfrow=c(1,1),plot.cex = 0.9)
mvn = SSdeltaMVLN(ss3rep,plot = T,years=1972:2016)
mvn$labels # the out put is SB/SBtrg
dev.print(jpeg,paste0(dirplot,"/Kobe_.jpg"), width = 6.5, 
          height = 6.5, res = 300, units = "in")

pdf(paste0(dirplot,"/MVLN_TRJ_.pdf"), height = 6.5, width = 11)
sspar(mfrow=c(3,3),plot.cex = 0.9)
SSplotEnsemble(mvn$kb,ylabs = mvn$labels,add=T)
dev.off()

## Summarize the list of retroModels
retroSummary <- r4ss::SSsummarize(retroModels)

## Now Check retros Analysis with one-step ahead Forecasts
sspar(mfrow=c(2,1),plot.cex = 0.9)
SSplotRetro(retroSummary,forecast = F,add=T)
SSplotRetro(retroSummary,forecast = F,add=T, xmin=2000)
dev.print(jpeg,paste0(dirplot,"/Retroforecast_.jpg"), width = 8, 
          height = 9, res = 300, units = "in")

## Do Hindcast with Cross-Validation of CPUE observations
pdf(paste0(dirplot,"/HCxvalIndex.pdf"), height = 6.5, width = 11)
sspar(mfrow=c(3,2),plot.cex = 0.9)
SSplotHCxval(retroSummary,xmin=1953,add=T)
dev.off()

## Also test new feature of Hindcast with Cross-Validation for mean length
## Use new converter fuction SSretroComps()
hccomps = SSretroComps(retroModels)
pdf(paste0(dirplot,"/HCxvalLen_.pdf"), height = 6.5, width = 11)
sspar(mfrow=c(3,3),plot.cex = 0.7)
SSplotHCxval(hccomps,add=T,subplots = "len",legendloc="topleft")
dev.off()
