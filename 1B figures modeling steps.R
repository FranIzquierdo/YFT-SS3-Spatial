#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code for Yellowfin SS spatial model          #
# Script to plot single dat_4A_1 modeling steps#
# Francisco Izquierdo                          #
# Marta Cousido-Rocha & Giancarlo Moron        #
# francisco.izqtar@gmail.com                   #
# Last edited on 19/12(2023)                   #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Show document outline: Ctrl + Shift + O

# This script plot the single reference dataset (dat_4A_1 YFT) modelling steps
# The plot displays SSB/SSBMSY and F/FMSY
# Las section calculates AIC

## Clean environment
rm(list=ls())
library(r4ss)
library(dplyr)

## Create plot dir
dir_plot<-paste0(getwd(),"/modelling steps/Figures")
dir.create(dir_plot)

## Data dir
dir_dat<-paste0(getwd(),"/modelling steps/")
dir.create(dir_dat)

## Select models to compare
models <- c("/S0 initial setup - copia",
            "/S1.1 selex at length",
            "/S2.2 CPUE st Q cte",
            "/S3.2 recdevs SS 1972",
            "/S4.1 mov 1-2 1-4 3-4 2-3",
            "/S4.2 mov 1-2 1-4 3-4",
            "/S5.1 mov 1-2 1-4 3-4 tag mlp=1",
            "/S5.2 mov 1-2 1-4 3-4 tag mlp=3",
            "/S6.3 rec RW2"
) 


# Extract the lab identifiers using regular expressions
labs <- gsub("^/|\\s.*$", "", models)

nmodels <- length(models)
filemod <- paste0(dir_dat, models) 

## Read and summarize output
retroModels <- SSgetoutput(dirvec = filemod)
retroSummary <- SSsummarize(retroModels) 

nforecastyears <- 3 

tablecomp <- SStableComparisons(retroSummary)
#write.csv(tablecomp, paste(dir_plot, "/table models comparison single area.csv", sep = ""))

library(reshape2)
library(ggplot2)
library(stringr)
startyr <- unique(retroSummary$startyrs) 
endyr <- unique(retroSummary$endyrs)
years <- (startyr:endyr)
nyears <- length(years)
yearsfore <- c(years, years[nyears] + (1:nforecastyears))
nyearsfore <- length(yearsfore) - length(years)

## SSBMSY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SSB
SSBm <- as.data.frame(retroSummary$SpawnBio)
SSBl <- as.data.frame(retroSummary$SpawnBioLower) # lower
SSBh <- as.data.frame(retroSummary$SpawnBioUpper) # upper

SSBt <- cbind(SSBm, SSBl, SSBh)
nrSSB <- nrow(SSBt)
#SSB <- SSBt[-c(1,2),] 

SSB <- SSBt[, c(1,12,23, #S1 ssb, lower, upper
               2,13,24, #S2 ssb, lower, upper
               3,14,25,
               4,15,26,
               5,16,27,
               6,17,28,
               7,18,29,
               8,19,30,
               9,20,31,
               11 #year
               )]

names(SSB) <- c("S1 ssb","S1 low","S1 up",
                "S2 ssb","S2 low","S2 up",
                "S3 ssb","S3 low","S3 up",
                "S4 ssb","S4 low","S4 up",
                "S5 ssb","S5 low","S5 up",
                "S6 ssb","S6 low","S6 up",
                "S7 ssb","S7 low","S7 up",
                "S8 ssb","S8 low","S8 up",
                "S9 ssb","S9 low","S9 up",
                "Year")

## MSY
ssbMSY <- rep(0, nmodels)
ssbMSY[1] <- retroModels$replist1$derived_quants$Value[retroModels$replist1$derived_quants$Label == "SSB_MSY"]
ssbMSY[2] <- retroModels$replist2$derived_quants$Value[retroModels$replist2$derived_quants$Label == "SSB_MSY"]
ssbMSY[3] <- retroModels$replist3$derived_quants$Value[retroModels$replist3$derived_quants$Label == "SSB_MSY"]
ssbMSY[4] <- retroModels$replist4$derived_quants$Value[retroModels$replist4$derived_quants$Label == "SSB_MSY"]
ssbMSY[5] <- retroModels$replist5$derived_quants$Value[retroModels$replist5$derived_quants$Label == "SSB_MSY"]
ssbMSY[6] <- retroModels$replist6$derived_quants$Value[retroModels$replist6$derived_quants$Label == "SSB_MSY"]
ssbMSY[7] <- retroModels$replist7$derived_quants$Value[retroModels$replist7$derived_quants$Label == "SSB_MSY"]
ssbMSY[8] <- retroModels$replist8$derived_quants$Value[retroModels$replist8$derived_quants$Label == "SSB_MSY"]
ssbMSY[9] <- retroModels$replist9$derived_quants$Value[retroModels$replist9$derived_quants$Label == "SSB_MSY"]

library(ggplot2)
library(viridis)

# Define your custom Viridis palette
viridis_palette <- c("#001934FF",
                     "#00204DFF",
                     "#3F5F94FF",
                     "#A34D4DFF",
                     "#6069A1FF",
                     "#9A8DA6FF",
                     "#7D9A90FF", 
                     "#BADDAAFF",
                     "#FFD966FF")



# Labels for the legend
labs <- c("S0.1", "S1.1", "S2.1", "S3.2", "S4.1", "S4.2", "S5.1", "S5.3", "S6.3")

# Plot with the custom Viridis palette
ssb_msy <- ggplot(data = SSB, aes(x = Year)) +
  geom_hline(yintercept = 1, col = "black", size = 0.5, linetype = "dotted") +
  
  # Use a higher transparency for ribbons
  geom_ribbon(aes(ymin = `S1 low` / ssbMSY[1], ymax = `S1 up` / ssbMSY[1], fill = "S1"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S2 low` / ssbMSY[2], ymax = `S2 up` / ssbMSY[2], fill = "S2"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S3 low` / ssbMSY[3], ymax = `S3 up` / ssbMSY[3], fill = "S3"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S4 low` / ssbMSY[4], ymax = `S4 up` / ssbMSY[4], fill = "S4"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S5 low` / ssbMSY[5], ymax = `S5 up` / ssbMSY[5], fill = "S5"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S6 low` / ssbMSY[6], ymax = `S6 up` / ssbMSY[6], fill = "S6"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S7 low` / ssbMSY[7], ymax = `S7 up` / ssbMSY[7], fill = "S7"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S8 low` / ssbMSY[8], ymax = `S8 up` / ssbMSY[8], fill = "S8"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S9 low` / ssbMSY[9], ymax = `S9 up` / ssbMSY[9], fill = "S9"), alpha = 0.1, color = "transparent") +
  
  # Place geom_line after geom_ribbon to ensure lines are on top
  geom_line(aes(y = `S1 ssb` / ssbMSY[1], color = "S1"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S2 ssb` / ssbMSY[2], color = "S2"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S3 ssb` / ssbMSY[3], color = "S3"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S4 ssb` / ssbMSY[4], color = "S4"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S5 ssb` / ssbMSY[5], color = "S5"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S6 ssb` / ssbMSY[6], color = "S6"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S7 ssb` / ssbMSY[7], color = "S7"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S8 ssb` / ssbMSY[8], color = "S8"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S9 ssb` / ssbMSY[9], color = "S9"), size = 0.9, linetype = "solid") +
  
  # Use the custom Viridis palette for colors and fills
  scale_color_manual(values = viridis_palette, name = "Model", labels = labs) +
  scale_fill_manual(values = viridis_palette, name = "Model", labels = labs) +
  
  # Set X axis with breaks every 10 years from 1960 to 2020
  scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +
  
  theme_light() +
  ylab("SSB/SSBMSY") +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(c(0.9), "cm"),
    legend.key.height = unit(c(0.4), "cm"),
    legend.box.spacing = unit(0, "pt"),
    legend.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 9),   # Increased size for legend text
    legend.title = element_text(size = 9),  # Increased size for legend title
    axis.title.x = element_text(size = 10), # Increased size for X axis title
    axis.text.x = element_text(size = 9),   # Increased size for X axis labels
    axis.text.y = element_text(size = 9),   # Increased size for Y axis labels
    axis.title.y = element_text(size = 10), # Increased size for Y axis title
    plot.margin = unit(c(10, 10, 0, 10), "pt")
  ) +
  guides(
    fill = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.4, "cm")),
    color = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.4, "cm"))
  )

# Plot
ssb_msy

# Save the plot
ggsave(paste0(dir_plot,"/ssb_msy_plot.png"), plot = ssb_msy, width = 8, height = 6, dpi = 800)

# SSB ABS facet ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyr)
library(dplyr)
library(ggplot2)

# Define custom Viridis palette and labels
viridis_palette <- c("#001934FF",
                     "#00204DFF",
                     "#3F5F94FF",
                     "#A34D4DFF",
                     "#6069A1FF",
                     "#9A8DA6FF",
                     "#7D9A90FF", 
                     "#BADDAAFF",
                     "#FFD966FF")

labs <- c("S0.1", "S1.1", "S2.1", "S3.2", "S4.1", "S4.2", "S5.1", "S5.3", "S6.3")

# 1. Transformar el dataframe: separar modelo y medida
# Se asume que SSB tiene columnas: "S1 ssb", "S1 low", "S1 up", "S2 ssb", "S2 low", "S2 up", ..., "S9 ssb", "S9 low", "S9 up", y "Year"
SSB_long <- SSB %>% 
  pivot_longer(
    cols = -Year,
    names_to = c("Model", "Measure"),
    names_pattern = "^(S\\d)\\s+(\\w+)$"
  ) %>% 
  pivot_wider(
    names_from = Measure,
    values_from = value
  )

# 2. Recodificar la variable 'Model' para que tenga las etiquetas deseadas
SSB_long <- SSB_long %>%
  mutate(Model = factor(Model, 
                        levels = c("S1","S2","S3","S4","S5","S6","S7","S8","S9"),
                        labels = labs))

# 3. Crear una variable de grupo para el facet: 
#    separamos S0.1 (que corresponde al recodificado de "S1") de los demás modelos.
SSB_long <- SSB_long %>% 
  mutate(Group = ifelse(Model == "S0.1", "S0", "S1 to S6"))

# 4. Graficar con facet_wrap, usando el mismo tema y leyenda que en el plot original
ssb_facet <- ggplot(data = SSB_long, aes(x = Year, y = ssb)) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = Model), alpha = 0.2, color = "transparent") +
  geom_line(aes(color = Model), size = 0.9, linetype = "solid") +
  #facet_wrap(~ Group, scales = "free_y", dir="v") +
  scale_color_manual(values = viridis_palette, name = "Model", labels = labs) +
  scale_fill_manual(values = viridis_palette, name = "Model", labels = labs) +
  scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +
  theme_light() +
  ylab("SSB") +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.box.spacing = unit(0, "pt"),
    legend.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(size = 10),
    plot.margin = unit(c(10, 10, 0, 10), "pt")
  ) +
  guides(
    fill = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.4, "cm")),
    color = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.4, "cm"))
  )

# Mostrar el gráfico
ssb_facet

ggsave(paste0(dir_plot,"/ssb_abs_plot.png"), plot = ssb_facet, width = 8, height = 6, dpi = 800)



## FMSY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SSB
Fv <- as.data.frame(retroSummary$Fvalue)
Fl <- as.data.frame(retroSummary$FvalueLower) 
Fh <- as.data.frame(retroSummary$FvalueUpper) 

Ft <- cbind(Fv, Fl, Fh)
nrF <- nrow(Ft)

Ft <- Ft[, c(1,12,23, #S1 ssb, lower, upper
             2,13,24, #S2 ssb, lower, upper
             3,14,25,
             4,15,26,
             5,16,27,
             6,17,28,
             7,18,29,
             8,19,30,
             9,20,31,
             11 #year
)]

names(Ft) <- c("S1 F","S1 low","S1 up",
                "S2 F","S2 low","S2 up",
                "S3 F","S3 low","S3 up",
                "S4 F","S4 low","S4 up",
                "S5 F","S5 low","S5 up",
                "S6 F","S6 low","S6 up",
                "S7 F","S7 low","S7 up",
                "S8 F","S8 low","S8 up",
                "S9 F","S9 low","S9 up",
               
                "Year")


## MSY
fMSY <- rep(0, nmodels)
fMSY[1] <- retroModels$replist1$derived_quants$Value[retroModels$replist1$derived_quants$Label == "annF_MSY"]
fMSY[2] <- retroModels$replist2$derived_quants$Value[retroModels$replist2$derived_quants$Label == "annF_MSY"]
fMSY[3] <- retroModels$replist3$derived_quants$Value[retroModels$replist3$derived_quants$Label == "annF_MSY"]
fMSY[4] <- retroModels$replist4$derived_quants$Value[retroModels$replist4$derived_quants$Label == "annF_MSY"]
fMSY[5] <- retroModels$replist5$derived_quants$Value[retroModels$replist5$derived_quants$Label == "annF_MSY"]
fMSY[6] <- retroModels$replist6$derived_quants$Value[retroModels$replist6$derived_quants$Label == "annF_MSY"]
fMSY[7] <- retroModels$replist7$derived_quants$Value[retroModels$replist7$derived_quants$Label == "annF_MSY"]
fMSY[8] <- retroModels$replist8$derived_quants$Value[retroModels$replist8$derived_quants$Label == "annF_MSY"]
fMSY[9] <- retroModels$replist9$derived_quants$Value[retroModels$replist9$derived_quants$Label == "annF_MSY"]

library(ggplot2)
library(viridis)

# Create the F plot
f_msy <- ggplot(data = Ft, aes(x = Year)) +
  geom_hline(yintercept = 1, col = "black", size = 0.5, linetype = "dotted") +
  
  # Use a higher transparency for ribbons similar to the ssb_msy plot
  geom_ribbon(aes(ymin = `S1 low` / fMSY[1], ymax = `S1 up` / fMSY[1], fill = "S1"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S2 low` / fMSY[2], ymax = `S2 up` / fMSY[2], fill = "S2"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S3 low` / fMSY[3], ymax = `S3 up` / fMSY[3], fill = "S3"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S4 low` / fMSY[4], ymax = `S4 up` / fMSY[4], fill = "S4"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S5 low` / fMSY[5], ymax = `S5 up` / fMSY[5], fill = "S5"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S6 low` / fMSY[6], ymax = `S6 up` / fMSY[6], fill = "S6"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S7 low` / fMSY[7], ymax = `S7 up` / fMSY[7], fill = "S7"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S8 low` / fMSY[8], ymax = `S8 up` / fMSY[8], fill = "S8"), alpha = 0.1, color = "transparent") +
  geom_ribbon(aes(ymin = `S9 low` / fMSY[9], ymax = `S9 up` / fMSY[9], fill = "S9"), alpha = 0.1, color = "transparent") +
  
  # Place geom_line after geom_ribbon to ensure lines are on top
  geom_line(aes(y = `S1 F` / fMSY[1], color = "S1"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S2 F` / fMSY[2], color = "S2"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S3 F` / fMSY[3], color = "S3"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S4 F` / fMSY[4], color = "S4"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S5 F` / fMSY[5], color = "S5"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S6 F` / fMSY[6], color = "S6"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S7 F` / fMSY[7], color = "S7"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S8 F` / fMSY[8], color = "S8"), size = 0.9, linetype = "solid") +
  geom_line(aes(y = `S9 F` / fMSY[9], color = "S9"), size = 0.9, linetype = "solid") +
  
  # Apply the custom Viridis palette and settings
  scale_color_manual(values = viridis_palette, name = "Model", labels = labs) +
  scale_fill_manual(values = viridis_palette, name = "Model", labels = labs) +
  
  # Set X axis with breaks every 10 years from 1960 to 2020
  scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +
  
  theme_light() +
  ylab("F/FMSY") +
  xlab("Year") +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.box.spacing = unit(0, "pt"),
    legend.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 9),   # Match legend text size to ssb_msy
    legend.title = element_text(size = 9),  # Match legend title size to ssb_msy
    axis.title.x = element_text(size = 10), # Match X axis title size to ssb_msy
    axis.text.x = element_text(size = 9),   # Match X axis label size to ssb_msy
    axis.text.y = element_text(size = 9),   # Match Y axis label size to ssb_msy
    axis.title.y = element_text(size = 10), # Match Y axis title size to ssb_msy
    plot.margin = unit(c(10, 10, 0, 10), "pt")
  ) +
  guides(
    fill = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.4, "cm")),
    color = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.4, "cm"))
  )

# Plot
f_msy

# Save the plot with high quality
ggsave(paste0(dir_plot,"/F_msy_plot.png"), plot = f_msy, width = 8, height = 6, dpi = 800)

# combined plot ---------------------------------------------------------------
library(ggpubr)

# Combine the two plots into one, with a single shared legend at the bottom
combined_plot <- ggarrange(ssb_msy, f_msy, 
                           ncol = 1, nrow = 2,  # Arrange plots vertically
                           common.legend = TRUE, legend = "bottom")

# Display the combined plot
combined_plot

# Save the combined plot
ggsave(paste0(dir_plot,"/combined_msy_plot.png"), plot = combined_plot, width = 8, height = 10, dpi = 800)

# AIC --------------------------------------------------------------------------

## Clean environment and load required libraries
rm(list = ls())
library(r4ss)    # For Stock Synthesis output functions
library(dplyr)   # For data manipulation

## Create directory for plots if it doesn't exist
dir_plot <- paste0(getwd(), "/modelling steps/Figures")
dir.create(dir_plot, showWarnings = FALSE)

## Create directory for data if it doesn't exist
dir_dat <- paste0(getwd(), "/modelling steps/")
dir.create(dir_dat, showWarnings = FALSE)

## Select models to compare by specifying their folder names (relative to dir_dat)
models <- c("/S0 initial setup - copia",
            "/S1.1 selex at length",
            "/S2.2 CPUE st Q cte",
            "/S3.1 recdevs 1980",
            "/S3.2 recdevs SS 1972",
            "/S4.1 mov 1-2 1-4 3-4 2-3",
            "/S4.2 mov 1-2 1-4 3-4",
            "/S5.1 mov 1-2 1-4 3-4 tag mlp=1",
            #"/S5.1 mov 1-2 1-4 3-4 tag mlp=6", hacer a mano
            "/S5.2 mov 1-2 1-4 3-4 tag mlp=3",
            "/S6.1 rec 1,2",
            "/S6.2 rec 1,2,4",
            "/S6.3 rec RW2")

# Optionally, extract lab identifiers from model names (e.g., removing the initial "/" and trailing text)
labs <- gsub("^/|\\s.*$", "", models)

# Determine the number of models to compare
nmodels <- length(models)
filemod <- paste0(dir_dat, models)  # Construct full paths for each model folder

## Read Stock Synthesis output and summarize across models
retroModels <- SSgetoutput(dirvec = filemod)
retroSummary <- SSsummarize(retroModels)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AIC Calculation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The AIC metric is computed as:
# AIC = -2 * (log likelihood) + 2 * (number of estimated parameters)
#
# In this example, we extract the "TOTAL" log likelihood from the retroSummary object.
# It is assumed that row 1 of retroSummary$likelihoods corresponds to the TOTAL likelihood.
# We then match the likelihoods to the models and use the provided number of active parameters.

# Number of active parameters for each model (from retroSummary output)
numPars <- c(78, 74, 74, 80, 90, 106, 102, 108, 108, 106, 107, 175)

# Extract the TOTAL likelihood for each model as a numeric vector.
# Note: retroSummary$likelihoods is assumed to be a matrix with each column corresponding to a model.
totalLik <- as.numeric(retroSummary$likelihoods[1, -ncol(retroSummary$likelihoods)])

# Remove any extra NA values by subsetting to match the number of models
totalLik <- totalLik[1:length(models)]

# Create a data frame with the model name, total likelihood, and number of parameters
model_df <- data.frame(Model = models,
                       Likelihood = totalLik,
                       NumPars = numPars)

# Calculate AIC using the formula: OJO USAMOS -LOGLIKELIHOOD, le quitamos el - al 2
# AIC = -2 * (-log likelihood) + 2 * (number of estimated active parameters)
model_df$AIC <- - 2 * (- model_df$Likelihood) + 2 * model_df$NumPars

# Print the final data frame to view the AIC comparison
print(model_df)

# We want to minimize -loglikelihood, for AIC the lower value its better,
# we penalyze by the number of parameters.
