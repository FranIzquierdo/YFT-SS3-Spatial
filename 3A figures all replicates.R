#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code for Yellowfin SS spatial model               #
# Script to make figures of 100 reps for each model #
# Francisco Izquierdo                               # 
# Marta Cousido-Rocha & Giancarlo Moron             #
# francisco.izqtar@gmail.com                        #
# Last edited on 19/12(2023)                        #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Show document outline: Ctrl + Shift + O

# This script takes the 100 replicates of each alternative model and base model
# reads and plots all the RData for each alternative model and the best model

# Start here -------------------------------------------------------------------

# Load packages:
rm(list = ls())
library(r4ss)
require(dplyr)
require(ggplot2)
library(viridis)
library(conflicted)
library(ggplot2)

# Resolve conflicts (if any) with dplyr functions
conflicted::conflicts_prefer(dplyr::filter)

# EM's -------------------------------------------------------------------------

# Select model reps ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Select the 100 model runs of the different alternative models RData
set_names<-"All models"
name_prefixes<-c("S1_1_selex_at_length",
                 "S2_1_CPUE_st_Q_cte",
                 "S4_1_mov_1_2_1_4_3_4_2_3",
                 "S4_2_mov_1_2_1_4_3_4",
                 "S5_1_mov_1_2_1_4_3_4_tag_mlp_1",
                 "S5_2_mov_1_2_1_4_3_4_tag_mlp_3",
                 "S6_1_rec_1_2", 
                  "best_model") 

# Load model reps ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set working directory to source file location
saveFolder = paste0(getwd(),"/100 replicates/output replicates/Figures/", set_names,"/")
dir.create(saveFolder)

# Define a function to load and set names for each set
load_and_set_names <- function(name_prefix) {
  set <- readRDS(paste0(getwd(), "/100 replicates/output replicates/output_", name_prefix, "_reps.RDS"))
  set$dq$em = name_prefix
  set$ts$em = name_prefix
  set$fish$em = name_prefix
  set$selex$em = name_prefix
  set$cpue$em = name_prefix 
  return(set)
}

# Load and set names for each set of models using a loop
all_sets <- list()
for (name_prefix in name_prefixes) {
  all_sets[[name_prefix]] <- load_and_set_names(name_prefix)
}

# Datasets ---------------------------------------------------------------------

# Prepare different EM's datasets for plotting

# Convergence ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Select max gradient for convergence
max_grad = 0.01

# Combine all sets into one dataset
combined_data_dq <- do.call(rbind, lapply(all_sets, function(set) set$dq))
data_dq = combined_data_dq
data_conv = data_dq[data_dq$Season == 1 & data_dq$Area == 1, ]

# Non-Convergence rate:
nonconv_rate = data_conv %>% dplyr::group_by(em) %>% dplyr::summarise(nonconvrate = sum((grad > max_grad) | is.nan(R0))/max(iter))
nonconv_rate$conv_rate = 1 - nonconv_rate$nonconvrate
nonconv_rate
write.csv(x = nonconv_rate, file = paste0(saveFolder, set_names, '_reps_table_conv_rate.csv'), row.names = FALSE)

# Save nonconvergent replicates:
data_conv = data_conv %>% mutate(replicate = paste0(em, '_', iter))
nonconv_rep = data_conv$replicate[which((data_conv$grad > max_grad) | is.nan(data_conv$R0))]

# Derived quants ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Derived quants: filter by gradient and R0
temp_dq = data_dq[!(data_dq$grad > max_grad | is.nan(data_dq$R0)), ]

# By season
temp_dq = temp_dq[temp_dq$Season == 1, ]

# By area
temp_dq = temp_dq %>% dplyr::group_by(em, iter) %>% dplyr::summarise(B0 = sum(B0), Bstatus = mean(Bstatus), R0 = sum(R0),
                                                                     SSBmsy = mean(SSBmsy), MSY = mean(MSY))
save_b0 = temp_dq

temp_dq = tidyr::gather(temp_dq, 'variable', 'value', 3:7)

# Time series ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Combine all sets into one dataset
combined_data_ts <- do.call(rbind, lapply(all_sets, function(set) set$ts))

# Merge data in one dataset
data_ts = combined_data_ts
data_ts = data_ts %>% mutate(replicate = paste0(em, '_', iter))
data_ts = data_ts[!(data_ts$replicate %in% nonconv_rep), ]

# By season
temp_ts = data_ts %>% dplyr::group_by(em, iter, Yr, Area) %>% dplyr::summarise(SSB = mean(SSB, na.rm = TRUE), TotB = mean(TotB), Rec = mean(Rec))
data_area = temp_ts

# By Area
temp_ts = temp_ts %>% dplyr::group_by(em, iter, Yr) %>% dplyr::summarise(SSB = sum(SSB), TotB = sum(TotB), Rec = sum(Rec))
temp_ts_2 = temp_ts

# Continue:
temp_ts = tidyr::gather(temp_ts, 'variable', 'value', 4:6)
temp_ts = temp_ts %>% dplyr::group_by(em, Yr, variable) %>% dplyr::summarise(q025 = quantile(value, probs = 0.025),
                                                                             q50 = quantile(value, probs = 0.5),
                                                                             q975 = quantile(value, probs = 0.975))
data_plot_ts = temp_ts[temp_ts$variable != 'TotB', ]

# Depletion ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate depletion from TS
temp_ts_2 = temp_ts_2 %>% mutate(replicate = paste0(em, '_', iter))
save_b0 = save_b0 %>% mutate(replicate = paste0(em, '_', iter))
temp_ts_2$b0 = save_b0$B0[match(temp_ts_2$replicate, save_b0$replicate)]
temp_ts_2 = temp_ts_2 %>% mutate(depletion = TotB/b0)

# Combine all sets into one dataset
combined_data_fish <- do.call(rbind, lapply(all_sets, function(set) set$fish))

# Calculate Exp rate:
data_fish = combined_data_fish
data_fish = data_fish %>% mutate(replicate = paste0(em, '_', iter))
data_fish = data_fish[!(data_fish$replicate %in% nonconv_rep), ] # remove nonconvergent replicates
temp_fish = data_fish %>% dplyr::group_by(em, iter, Yr) %>% dplyr::summarise(Catch = sum(Catch))

# Merge:
temp_ts_2$totCatch = temp_fish$Catch
temp_ts_2 = temp_ts_2 %>% mutate(expRate = totCatch/TotB)

#Save table:
table_1 = temp_ts_2 %>% dplyr::group_by(em, Yr) %>% dplyr::summarise(CVSSB = sd(SSB)/mean(SSB), 
                                                                     CVR = sd(Rec)/mean(Rec),
                                                                     CVexR = sd(expRate)/mean(expRate))
(table_1 = table_1 %>% dplyr::group_by(em) %>% dplyr::summarise(CVSSB = mean(CVSSB), CVR = mean(CVR), CVexR = mean(CVexR)))
write.csv(x = table_1, file = paste0(saveFolder, set_names, '_reps_table_CV.csv'), row.names = FALSE)

# Continue:
temp_ts_2 = temp_ts_2 %>% select('em', 'Yr', 'iter', 'depletion', 'expRate')
temp_ts_2 = tidyr::gather(temp_ts_2, 'variable', 'value', 4:5)
temp_ts_2 = temp_ts_2 %>% dplyr::group_by(em, Yr, variable) %>% dplyr::summarise(q025 = quantile(value, probs = 0.025),
                                                                                 q50 = quantile(value, probs = 0.5),
                                                                                 q975 = quantile(value, probs = 0.975))
data_plot_dep = temp_ts_2

# OM ---------------------------------------------------------------------------

# Load OM datasets (prepared in other script) for plotting

# Load 100 iterations of OM simulated values by region, year without quantiles
load("C:/Users/frank/Desktop/Core/Cap. 2 - Yellowfin spatial/Code/Paper spatial complexity/Values OM/OM_values by iter.RData")
OM_ssb_depl_by_iter_year

# Load 100 iterations of OM simulated values with quantiles
load("C:/Users/frank/Desktop/Core/Cap. 2 - Yellowfin spatial/Code/Paper spatial complexity/Values OM/OM values.RData")
OM_ssb_reg <- data.frame(OM_list[1]) 
OM_ssb_tot <- data.frame(OM_list[2]) 
OM_rec_reg <- data.frame(OM_list[3]) 
OM_rec_tot <- data.frame(OM_list[4]) 

# Plots ------------------------------------------------------------------------

# Make article main figures

# Bias -------------------------------------------------------------------------

# Mean Absolute Percentage Error (MAPE), note we call it BIAS

library(dplyr)
library(ggplot2)

# Define the original and recoded model names and colors
original_models <- c("S1_1_selex_at_length",
                     "S2_1_CPUE_st_Q_cte",
                     "S4_1_mov_1_2_1_4_3_4_2_3",
                     "S4_2_mov_1_2_1_4_3_4",
                     "S5_1_mov_1_2_1_4_3_4_tag_mlp_1",
                     "S5_2_mov_1_2_1_4_3_4_tag_mlp_3",
                     "S6_1_rec_1_2", 
                     "best_model")

recoded_models <- c("S1.1", "S2.1", "S4.1", "S4.2", "S5.1", "S5.3", "S6.1", "S6.3")

viridis_palette <- c("#00204DFF",
                     "#4668A3FF",
                     "#6069A1FF",
                     "#9A8DA6FF",
                     "#7D9A90FF", 
                     "#BADDAAFF",
                     "#EFD2B9FF", 
                     "#FFD966FF")

# Define the output directory
output_dir_mape <- file.path(getwd(), "100 replicates", "output replicates", "Figures", "MAPE")
dir.create(output_dir_mape, recursive = TRUE, showWarnings = FALSE)

# OM SSB Data Processing
OM_ssb_by_iter_year <- OM_ssb_depl_by_iter_year[,-7]
colnames(OM_ssb_by_iter_year) <- c("Iter", "Model", "Region", "Year", "Season", "SSB")

OM_ssb_by_year <- OM_ssb_by_iter_year %>%
  group_by(Iter, Model, Region, Year) %>%
  summarise(SSB = mean(SSB, na.rm = TRUE), .groups = 'drop')

OM_ssb_with_depletion <- OM_ssb_by_year %>%
  group_by(Iter, Model, Region) %>%
  mutate(Depletion = SSB / first(SSB)) %>%
  ungroup()

# EM SSB Data Processing
EM_ssb_by_iter_year <- combined_data_ts %>%
  select(iter, em, Area, Yr, Seas, SSB)
colnames(EM_ssb_by_iter_year) <- c("Iter", "Model", "Region", "Year", "Season", "SSB")

EM_ssb_by_year <- EM_ssb_by_iter_year %>%
  filter(Season == 1) %>%
  select(-Season)

EM_ssb_with_depletion <- EM_ssb_by_year %>%
  group_by(Iter, Model, Region) %>%
  mutate(Depletion = SSB / first(SSB)) %>%
  ungroup()

EM_ssb_with_depletion <- EM_ssb_with_depletion %>%
  mutate(Region = as.character(Region))

# Combine OM and EM datasets (the EM "Model" becomes Model_est)
combined_data <- EM_ssb_with_depletion %>%
  inner_join(OM_ssb_with_depletion, by = c("Iter", "Region", "Year"), suffix = c("_est", "_true"))

# Aggregation for the overall plot (no region breakdown)
aggregated_data <- combined_data %>%
  group_by(Model_est, Year, Iter) %>%
  summarize(
    SSB_est = mean(SSB_est, na.rm = TRUE),
    Depletion_est = mean(Depletion_est, na.rm = TRUE),
    SSB_true = mean(SSB_true, na.rm = TRUE),
    Depletion_true = mean(Depletion_true, na.rm = TRUE),
    .groups = "drop"
  )

# Aggregation for the regional plots (by Region)
aggregated_data_region <- combined_data %>%
  group_by(Model_est, Region, Year, Iter) %>%
  summarize(
    SSB_est = mean(SSB_est, na.rm = TRUE),
    Depletion_est = mean(Depletion_est, na.rm = TRUE),
    SSB_true = mean(SSB_true, na.rm = TRUE),
    Depletion_true = mean(Depletion_true, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate Absolute Percentage Error (APE)
combined_data2 <- aggregated_data %>%
  mutate(
    APE_SSB = abs((SSB_est - SSB_true) / SSB_true) * 100,
    APE_Depletion = abs((Depletion_est - Depletion_true) / Depletion_true) * 100
  )

combined_data2_region <- aggregated_data_region %>%
  mutate(
    APE_SSB = abs((SSB_est - SSB_true) / SSB_true) * 100,
    APE_Depletion = abs((Depletion_est - Depletion_true) / Depletion_true) * 100
  )

# Calculate Mean Absolute Percentage Error (MAPE)
mape_results <- combined_data2 %>%
  group_by(Model_est, Year) %>%
  summarise(
    MAPE_SSB = mean(APE_SSB, na.rm = TRUE),
    MAPE_Depletion = mean(APE_Depletion, na.rm = TRUE),
    .groups = "drop"
  )

mape_results_region <- combined_data2_region %>%
  group_by(Model_est, Region, Year) %>%
  summarise(
    MAPE_SSB = mean(APE_SSB, na.rm = TRUE),
    MAPE_Depletion = mean(APE_Depletion, na.rm = TRUE),
    .groups = "drop"
  )

# Recode Model names in results
mape_results <- mape_results %>%
  mutate(Model_est = recode(Model_est,
                            "S1_1_selex_at_length" = "S1.1",
                            "S2_1_CPUE_st_Q_cte" = "S2.1",
                            "S4_1_mov_1_2_1_4_3_4_2_3" = "S4.1",
                            "S4_2_mov_1_2_1_4_3_4" = "S4.2",
                            "S5_1_mov_1_2_1_4_3_4_tag_mlp_1" = "S5.1",
                            "S5_2_mov_1_2_1_4_3_4_tag_mlp_3" = "S5.3",
                            "S6_1_rec_1_2" = "S6.1",
                            "best_model" = "S6.3"))
mape_results$Model_est <- factor(mape_results$Model_est, levels = recoded_models)

mape_results_region <- mape_results_region %>%
  mutate(Model_est = recode(Model_est,
                            "S1_1_selex_at_length" = "S1.1",
                            "S2_1_CPUE_st_Q_cte" = "S2.1",
                            "S4_1_mov_1_2_1_4_3_4_2_3" = "S4.1",
                            "S4_2_mov_1_2_1_4_3_4" = "S4.2",
                            "S5_1_mov_1_2_1_4_3_4_tag_mlp_1" = "S5.1",
                            "S5_2_mov_1_2_1_4_3_4_tag_mlp_3" = "S5.3",
                            "S6_1_rec_1_2" = "S6.1",
                            "best_model" = "S6.3"))
mape_results_region$Model_est <- factor(mape_results_region$Model_est, levels = recoded_models)

# Save aggregated MAPE values
mape_aggregated <- mape_results %>%
  group_by(Model_est) %>%
  summarise(
    MAPE_SSB_avg = mean(MAPE_SSB, na.rm = TRUE),
    MAPE_Depletion_avg = mean(MAPE_Depletion, na.rm = TRUE),
    .groups = "drop"
  )
write.csv(mape_aggregated, file = file.path(output_dir_mape, "mape_aggregated.csv"), row.names = FALSE)
write.csv(mape_results_region, file = file.path(output_dir_mape, "mape_aggregated_by_region.csv"), row.names = FALSE)

custom_theme <- theme_bw() +
  theme(
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    legend.key.width = unit(1, 'cm')
    # Removed axis.text.x = element_blank() so year labels appear
  )

mape_plot1 <- ggplot(mape_results, aes(x = Year, color = Model_est)) +
  geom_line(aes(y = MAPE_SSB), linewidth = 1) +
  geom_line(aes(y = MAPE_Depletion), linewidth = 1, linetype = "dashed") +
  scale_color_manual(values = viridis_palette) +
  labs(
    title = " ",
    x = "Year",
    y = "Bias",
    color = "Models"
  ) +
  facet_wrap(~ Model_est, scales = "fixed") +
  custom_theme

mape_plot_ssb <- ggplot(mape_results_region, aes(x = Year, y = MAPE_SSB, color = Model_est)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = viridis_palette) +
  labs(
    title = " ",
    x = "Year",
    y = "Bias SSB",
    color = "Models"
  ) +
  facet_wrap(~ Region, scales = "free_y") +
  custom_theme

mape_plot_depletion <- ggplot(mape_results_region, aes(x = Year, y = MAPE_Depletion, color = Model_est)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = viridis_palette) +
  labs(
    title = "",
    x = "Year",
    y = "Bias depletion",
    color = "Models"
  ) +
  facet_wrap(~ Region, scales = "free_y") +
  custom_theme


# Print and Save the Plots
print(mape_plot1)
print(mape_plot_ssb)
print(mape_plot_depletion)

ggsave(file.path(output_dir_mape, "mape_plot.png"), plot = mape_plot1, width = 8, height = 6, dpi = 800)
ggsave(file.path(output_dir_mape, "mape_plot_ssb_by_region.png"), plot = mape_plot_ssb, width = 8, height = 6, dpi = 800)
ggsave(file.path(output_dir_mape, "mape_plot_depletion_by_region.png"), plot = mape_plot_depletion, width = 8, height = 6, dpi = 800)

# OM vs EM's -------------------------------------------------------------------

# SSB DEPL ----------------------------------------------------------------------

## OM SSB total ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Select only ssb and not depl values:
OM_ssb_tot_p<-OM_ssb_tot[,-c(5,6,7)]

# Create column to identify
OM_ssb_tot_p$type<-rep("SSB")

# Set colnames
colnames(OM_ssb_tot_p)<-c("Year", "median", "q025","q975", "type")

# Merge OM data and time series data
merged_data_ssb <- merge(OM_ssb_tot_p, data_plot_ts, by.x = c("Year", "type"), by.y = c("Yr", "variable"))

merged_data_ssb<-merged_data_ssb[,c(1,3,4,5,6,2,7,8,9)]

colnames(merged_data_ssb)<-c("year", "median_om", "q025_om", "q975_om",
                             "em", "variable", "q025_var", "q50_var", "q975_var")

## OM depletion ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

OM_depl_tot<-OM_ssb_tot[,-c(2,3,4)]

# Filter depl
conflict_prefer("filter", "dplyr")
data_plot_dep<- data_plot_dep %>% 
  filter(variable == "depletion")

merged_data_dep <- merge(OM_depl_tot, data_plot_dep, by.x = c("year"), by.y = c("Yr"))

colnames(merged_data_dep)<-c("year", "median_om", "q025_om", "q975_om",
                             "em", "variable", "q025_var", "q50_var", "q975_var")

## arrange ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

merged_data_full<-rbind(merged_data_ssb, merged_data_dep)


merged_data_full<-merged_data_full%>% mutate (em = recode (em,
                                                           "S1_1_selex_at_length" ="S1.1",
                                                           "S2_1_CPUE_st_Q_cte" = "S2.1",
                                                           "S4_1_mov_1_2_1_4_3_4_2_3" = "S4.1",                                                           "S6_2_rec_1_2_4" = "S6.2"     ,
                                                           "S4_2_mov_1_2_1_4_3_4" = "S4.2",
                                                           "S5_1_mov_1_2_1_4_3_4_tag_mlp_1" = "S5.1",
                                                           "S5_2_mov_1_2_1_4_3_4_tag_mlp_3" = "S5.3",
                                                           "S6_1_rec_1_2" ="S6.1",
                                                           "best_model" = "S6.3"   
))

# Define the models with the recoded names
name_prefixes <- c("S0",
                   "S1.1",
                   "S2.1",
                   "S3.2",
                   "S4.1", 
                   "S4.2",
                   "S5.1", 
                   "S5.3",
                   "S6.1", 
                   "S6.3")

viridis_palette<- c("#001934FF",
                    "#00204DFF",
                    "#4668A3FF",
                    "#A34D4DFF",
                    "#6069A1FF",
                    "#9A8DA6FF",
                    "#7D9A90FF", 
                    "#BADDAAFF",
                    "#EFD2B9FF", 
                    "#FFD966FF")

# Define the color mapping based on the recoded model names
color_mapping <- setNames(viridis_palette, name_prefixes)

# Ensure all levels are included in the factor
merged_data_full$em <- factor(merged_data_full$em, levels = name_prefixes)
unique(merged_data_full$em)

ggplot(merged_data_full, aes(x = year, group = em)) +
  geom_ribbon(aes(ymin = q025_var, ymax = q975_var, fill = em), alpha = 0.3, color = NA) +
  geom_line(aes(y = q50_var, col = em), size = 1) +
  labs(x = "Year", y = "Values") +
  scale_color_manual(values = color_mapping, name = "Model steps") +  # Apply custom colors
  scale_fill_manual(values = color_mapping, name = "Model steps") +   # Apply custom colors
  facet_grid(variable ~ em, scales = "free") +  # Custom labeller for facet labels
  geom_line(aes(y = median_om, group = 1), linetype = "dotdash", size = 1.2, col = "black") +
  theme_bw() +
  theme(
    legend.position = 'bottom', 
    legend.box = "horizontal",
    axis.text.x = element_text(size = 10),  # Adjust text size if necessary
    strip.background = element_rect(fill = "lightgrey"),  # Set facet wrap background to white
    strip.text = element_text(color = "black")  # Ensure text is still visible
  ) +
  scale_x_continuous(
    breaks = seq(1960, 2020, by = 20),  # Ticks every 20 years (1960, 1980, 2000, 2020)
    labels = c("1960", "", "2000", "")  # Only label 1960 and 2020
  )

# Save the plot
ggsave(filename = paste0(saveFolder, set_names, '_reps_OM_SSB_depletion.png'), width = 220, height = 140, dpi = 800, units = 'mm')

# SSB reg ----------------------------------------------------------------------

# OM SSB by area ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_ssb_area = data_area

data_plot = data_ssb_area %>% 
  dplyr::group_by(em, Yr, Area) %>% 
  dplyr::summarise(q025 = quantile(SSB, probs = 0.025), 
                   q50 = quantile(SSB, probs = 0.5), 
                   q975 = quantile(SSB, probs = 0.975))
data_plot$Area = factor(as.character(data_plot$Area), 
                        levels = c('1', '4', '2', '3'), 
                        labels = c('Region 1', 'Region 4', 'Region 2', 'Region 3'))

# Define the mapping between Region and Area
region_to_area_mapping <- c("1" = "Region 1", "2" = "Region 2", "3" = "Region 3", "4" = "Region 4")

# Perform the join
merged_data <- OM_ssb_reg %>%
  mutate(Area = region_to_area_mapping[region]) %>%
  left_join(data_plot, by = c("year" = "Yr", "Area"))

# Continue with your recoding and filtering
merged_data <- merged_data %>% mutate(em = recode(
  em,
  "S1_1_selex_at_length" = "S1.1",
  "S2_1_CPUE_st_Q_cte" = "S2.1",
  "S4_1_mov_1_2_1_4_3_4_2_3" = "S4.1",
  "S6_2_rec_1_2_4" = "S6.2",
  "S4_2_mov_1_2_1_4_3_4" = "S4.2",
  "S5_1_mov_1_2_1_4_3_4_tag_mlp_1" = "S5.1",
  "S5_2_mov_1_2_1_4_3_4_tag_mlp_3" = "S5.3",
  "S6_1_rec_1_2" = "S6.1",
  "S6_2_rec_1_2_4" = "S6.2",
  "best_model" = "S6.3"
))

# Filter the DataFrame using dplyr
merged_data_filt <- merged_data %>%
  filter(em %in% em_values)

# Plot with updated labels
ggplot(merged_data, aes(x = year, y = q50, ymin = q025, ymax = q975)) +
  xlab('') + ylab('SSB (mt)') +
  geom_ribbon(aes(fill = em, color = em), alpha = 0.2, colour = NA) + 
  geom_line(aes(color = em), lwd = 1) +
  geom_line(aes(y = median_ssb, group = 1), lwd=0.8, linetype="dotdash") +
  scale_fill_manual(values = color_mapping) +
  scale_color_manual(values = color_mapping) +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    legend.key.width = unit(1, 'cm')
  ) +
  guides(
    color = guide_legend(title = 'Models'),
    fill = guide_legend(title = 'Models')
  ) +
  facet_wrap(.~ Area, ncol = 2)

ggsave(filename = paste0(saveFolder, set_names, '_reps_OM_SSB_area_all.png'), width = 190, height = 140, dpi = 800, units = 'mm')

# without ribbon

ggplot(merged_data, aes(x = year, y = q50, ymin = q025, ymax = q975)) +
  xlab('') + ylab('SSB (mt)') +
  #geom_ribbon(aes(fill = em, color = em), alpha = 0.2, colour = NA) + 
  geom_line(aes(color = em), lwd = 1) +
  geom_line(aes(y = median_ssb, group = 1), lwd=0.8, linetype="dotdash") +
  scale_fill_manual(values = color_mapping) +
  scale_color_manual(values = color_mapping) +
  theme_bw() +
  theme(
    legend.position = 'bottom',  # Move legend to the bottom
    legend.direction = 'horizontal',  # Set legend direction to horizontal
    legend.key.width = unit(1, 'cm'),  # Adjust legend key width if needed
  ) +
  guides(
    color = guide_legend(title = 'Models'),  # Set legend title
    fill = guide_legend(title = 'Models')  # Set legend title for fill
  ) +
  facet_wrap(.~ Area, ncol = 2)+
  ylim(0,3100000)

ggsave(filename = paste0(saveFolder, set_names, '_reps_OM_SSB_area_no_ribbon_all.png'), width = 190, height = 140, dpi = 800, units = 'mm')

# CPUE S1 S2 -------------------------------------------------------------------

# Load packages and clear the environment
rm(list = ls())
library(r4ss)
library(dplyr)
library(ggplot2)
library(viridis)
library(conflicted)

# Select set of models
set_names <- "CPUE S1 S2"
name_prefixes <- c("S1_1_selex_at_length", "S2_1_CPUE_st_Q_cte")

# Load model reps ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set working directory and create save folder
saveFolder <- paste0(getwd(), "/100 replicates/output replicates/Figures/", set_names, "/")
dir.create(saveFolder)

# Function to load and set names for each set
load_and_set_names <- function(name_prefix) {
  set <- readRDS(paste0(getwd(), "/100 replicates/output replicates/output_", name_prefix, "_reps.RDS"))
  set$dq$em <- name_prefix
  set$ts$em <- name_prefix
  set$fish$em <- name_prefix
  set$selex$em <- name_prefix
  set$cpue$em <- name_prefix 
  return(set)
}

# Load and set names for each model
all_sets <- list()
for (name_prefix in name_prefixes) {
  all_sets[[name_prefix]] <- load_and_set_names(name_prefix)
}

# Convergence ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define max gradient for convergence
max_grad <- 0.01

# Combine all datasets into one
combined_data_dq <- do.call(rbind, lapply(all_sets, function(set) set$dq))
data_dq <- combined_data_dq
data_conv <- data_dq[data_dq$Season == 1 & data_dq$Area == 1, ]

# Calculate non-convergence rate
nonconv_rate <- data_conv %>% 
  group_by(em) %>% 
  summarise(nonconvrate = sum((grad > max_grad) | is.nan(R0)) / max(iter))
nonconv_rate$conv_rate <- 1 - nonconv_rate$nonconvrate
print(nonconv_rate)

# Identify non-convergent replicates
data_conv <- data_conv %>% mutate(replicate = paste0(em, '_', iter))
nonconv_rep <- data_conv$replicate[which((data_conv$grad > max_grad) | is.nan(data_conv$R0))]
print(nonconv_rep)  # Output: 0

# Plots ------------------------------------------------------------------------

# Combine CPUE data from all models
combined_data_cpue <- do.call(rbind, lapply(all_sets, function(set) set$cpue))
data_CPUE <- combined_data_cpue

# Recode model and region names
data_CPUE <- data_CPUE %>% 
  mutate(em = recode(em,
                     "S1_1_selex_at_length" ="S1.1",
                     "S2_1_CPUE_st_Q_cte" = "S2.1",
                     "S4_1_mov_1_2_1_4_3_4_2_3" = "S4.1",                                                           "S6_2_rec_1_2_4" = "S6.2"     ,
                     "S4_2_mov_1_2_1_4_3_4" = "S4.2",
                     "S5_1_mov_1_2_1_4_3_4_tag_mlp_1" = "S5.1",
                     "S5_2_mov_1_2_1_4_3_4_tag_mlp_3" = "S5.2",
                     "S6_1_rec_1_2" ="S6.1",
                     "best_model" = "S6.3"
  )) %>%
  # Change Area labels to Region labels
  mutate(Area = recode(Area,
                       "1" = "Region 1",
                       "2" = "Region 2",
                       "3" = "Region 3",
                       "4" = "Region 4"))

# Step 1: Calculate the median and uncertainty bounds for both observed and expected CPUE
summary_CPUE <- data_CPUE %>%
  group_by(em, Yr, Area, Seas) %>%
  summarise(
    median_Obs = median(Obs, na.rm = TRUE),
    lower_Obs = quantile(Obs, 0.025, na.rm = TRUE),
    upper_Obs = quantile(Obs, 0.975, na.rm = TRUE),
    median_Exp = median(Exp, na.rm = TRUE),
    lower_Exp = quantile(Exp, 0.025, na.rm = TRUE),
    upper_Exp = quantile(Exp, 0.975, na.rm = TRUE)
  ) %>%
  ungroup()

# Plot the CPUE data
ggplot(summary_CPUE, aes(x = Yr)) +
  # Ribbon and solid line for observed values
  geom_ribbon(aes(ymin = lower_Obs, ymax = upper_Obs, fill = paste("Observed", em)), alpha = 0.1) +
  geom_line(aes(y = median_Obs, color = paste("Observed", em)), size = 1, linetype = "solid") +
  # Ribbon and solid line for expected values
  geom_ribbon(aes(ymin = lower_Exp, ymax = upper_Exp, fill = "Expected"), alpha = 0.1) +
  geom_line(aes(y = median_Exp, color = "Expected"), size = 1, linetype = "solid") +
  # Faceting by model and region (updated from Area to Region)
  facet_grid(em ~ Area, scales = "free") + 
  labs(
    x = "Year",
    y = "CPUE",
    color = "CPUE",
    fill = "CPUE",
    caption = " "
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    plot.caption = element_text(size = 10, hjust = 0.5),  # Centered caption
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10)  # Adjusted margins
  ) +
  scale_x_continuous(
    breaks = seq(1970, 2010, by = 10),  # Set ticks every 10 years
    labels = function(x) ifelse(x %in% c(1970, 1990, 2010), as.character(x), "")  # Label only 1970, 1990, 2010
  ) +
  # Custom colors for specific models with custom labels
  scale_color_manual(
    values = c("Observed S1.1" = "#00204DFF", "Observed S2.1" = "#3F5F94FF", "Expected" = "red2")
  ) +
  scale_fill_manual(
    values = c("Observed S1.1" = "#00204DFF", "Observed S2.1" = "#3F5F94FF", "Expected" = "red2")
  )

# Save the plot
ggsave(filename = paste0(saveFolder, set_names, '_reps_comparison_observed_expected_standardized_by_model_area_and_seasonNOSTD.png'), width = 190, height = 90, dpi = 800, units = 'mm')

# Expected vs observed fit ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Step 1: Standardize the CPUE values to have a mean of 1 within each combination of em, Area, and Seas
standardized_CPUE <- data_CPUE %>%
  group_by(em, Area, Seas) %>%
  mutate(
    standardized_Obs = Obs / mean(Obs, na.rm = TRUE),
    standardized_Exp = Exp / mean(Exp, na.rm = TRUE)
  ) %>%
  ungroup()

# plot
ggplot(standardized_CPUE, aes(x = standardized_Obs, y = standardized_Exp, color = factor(Seas))) +
  geom_point(alpha = 0.7, size = 2) +  # Scatter plot of observed vs expected
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +  # Optional trend line
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray") +  # Reference line y=x
  labs(
    x = "Observed CPUE",
    y = "Expected CPUE",
    color = "Season",
    fill = "Season",
    caption = "Values of 100 replicates. Standardized by mean."
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA),  # Set the plot background to white
    panel.background = element_rect(fill = "white", color = NA),  # Set the panel background to white
    strip.background = element_rect(fill = "grey90", color = "black")  # Set facet label background to grey and border to black
  ) +
  scale_color_viridis_d() +  # Use viridis color scale for seasons
  facet_grid(em ~ Area)  # Facet by model and area

# Save the plot
ggsave(filename = paste0(saveFolder, set_names, '_reps_observed_VS_expected_standardized.png'), width = 190, height = 90, dpi = 800, units = 'mm')

# Obs and expected residuals ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Step 1: Log-transform the observed and expected values and calculate residuals
residuals_CPUE <- data_CPUE %>%
  mutate(
    log_Obs = log(Obs),
    log_Exp = log(Exp),
    residual = (log_Obs - log_Exp) / SE
  )

# Step 2: Summarize residuals by median and uncertainty bounds (2.5th and 97.5th percentiles)
summary_residuals <- residuals_CPUE %>%
  group_by(em, Yr, Area, Seas) %>%
  summarise(
    median_residual = median(residual, na.rm = TRUE),
    lower_residual = quantile(residual, 0.025, na.rm = TRUE),
    upper_residual = quantile(residual, 0.975, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 3: Create the residual plot with summarized data and ribbon without boundary lines
ggplot(summary_residuals, aes(x = Yr, y = median_residual, color = factor(Seas))) +
  geom_ribbon(aes(ymin = lower_residual, ymax = upper_residual, fill = factor(Seas)), 
              alpha = 0.3, color = NA) +  # Ribbon without boundary lines
  geom_line(size = 1) +  # Median residuals
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  # Reference line at 0
  labs(
    x = "Year",
    y = "CPUE residuals",
    color = "Season",
    fill = "Season",
    caption = "Summarized Residuals of Log(Observed) vs. Log(Expected) CPUE by Model, Area, and Season"
  ) +
  theme_bw() +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    plot.caption = element_text(size = 10, hjust = 0.5),  # Centered caption
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10)  # Adjusted margins
  ) +
  scale_x_continuous(
    breaks = seq(1970, 2010, by = 10),  # Set ticks every 10 years
    labels = function(x) ifelse(x %in% c(1970, 1990, 2010), as.character(x), "")  # Label only 1970, 1990, 2010
  ) +
  scale_color_viridis_d(option="cividis") +  # Use cividis color scale for seasons
  scale_fill_viridis_d(option="cividis") +   # Use matching cividis fill scale for ribbons
  facet_grid(em ~ Area)  # Facet by model and area

# Save the plot
ggsave(filename = paste0(saveFolder, set_names, '_reps_observed_VS_expected_residuals_standardized.png'), width = 190, height = 90, dpi = 800, units = 'mm')

# Mov S4 S5 S6 -----------------------------------------------------------------

# Load packages
rm(list = ls())
library(r4ss)
require(dplyr)
require(ggplot2)
library(viridis)
library(conflicted)
library(ggplot2)

# Select set of models
set_names<-"Mov S4 S5 S6"
name_prefixes<-c("S4_1_mov_1_2_1_4_3_4_2_3",
                 "S4_2_mov_1_2_1_4_3_4",
                 "S5_1_mov_1_2_1_4_3_4_tag_mlp_1",
                 "S5_2_mov_1_2_1_4_3_4_tag_mlp_3",
                 "S6_1_rec_1_2", "best_model") # Mov and rec

# Load model reps ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# Set working directory to source file location
saveFolder = paste0(getwd(),"/100 replicates/output replicates/Figures/", set_names,"/")
dir.create(saveFolder)

# Define a function to load and set names for each set
load_and_set_names <- function(name_prefix) {
  set <- readRDS(paste0(getwd(), "/100 replicates/output replicates/output_", name_prefix, "_reps.RDS"))
  set$dq$em = name_prefix
  set$ts$em = name_prefix
  set$fish$em = name_prefix
  set$selex$em = name_prefix
  set$cpue$em = name_prefix 
  return(set)
}

# Load and set names for each set of models using a loop
all_sets <- list()
for (name_prefix in name_prefixes) {
  all_sets[[name_prefix]] <- load_and_set_names(name_prefix)
}

# Convergence ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Select max gradient for convergence
max_grad = 0.01

# Combine all sets into one dataset
combined_data_dq <- do.call(rbind, lapply(all_sets, function(set) set$dq))
data_dq = combined_data_dq
data_conv = data_dq[data_dq$Season == 1 & data_dq$Area == 1, ]

# Non-Convergence rate:
nonconv_rate = data_conv %>% dplyr::group_by(em) %>% dplyr::summarise(nonconvrate = sum((grad > max_grad) | is.nan(R0))/max(iter))
nonconv_rate$conv_rate = 1 - nonconv_rate$nonconvrate
nonconv_rate

# Save nonconvergent replicates:
data_conv = data_conv %>% mutate(replicate = paste0(em, '_', iter))
nonconv_rep = data_conv$replicate[which((data_conv$grad > max_grad) | is.nan(data_conv$R0))]
nonconv_rep # 17

# Plots ------------------------------------------------------------------------

# Combine all sets into one dataset
combined_data_mov <- do.call(rbind, lapply(all_sets, function(set) set$mov))

data_mov = combined_data_mov

# Correct em names
data_mov$em <-rownames(data_mov) 
data_mov<-data_mov %>%
  mutate(em = sub("\\.\\d+$", "", em))

data_mov = data_mov %>% mutate(Mov = paste0('From R', area1, ' to R', area2))

data_plot = tidyr::gather(data_mov, 'state', 'value', 6:7)
data_plot$state = factor(data_plot$state, levels = c('inmatureRate', 'matureRate'), labels = c('Immature', 'Mature'))

data_plot<-data_plot %>% 
  mutate(em = recode(em,
                     "S1_1_selex_at_length" ="S1.1",
                     "S2_1_CPUE_st_Q_cte" = "S2.1",
                     "S4_1_mov_1_2_1_4_3_4_2_3" = "S4.1",                                                           "S6_2_rec_1_2_4" = "S6.2"     ,
                     "S4_2_mov_1_2_1_4_3_4" = "S4.2",
                     "S5_1_mov_1_2_1_4_3_4_tag_mlp_1" = "S5.1",
                     "S5_2_mov_1_2_1_4_3_4_tag_mlp_3" = "S5.3",
                     "S6_1_rec_1_2" ="S6.1",
                     "best_model" = "S6.3"   
  )) 


# Define the models with the recoded names
name_prefixes <- c("S0",
                   "S1.1",
                   "S2.1",
                   "S3.2",
                   "S4.1", 
                   "S4.2",
                   "S5.1", 
                   "S5.3",
                   "S6.1", 
                   "S6.3")

viridis_palette<- c("#001934FF",
                    "#00204DFF",
                    "#4668A3FF",
                    "#A34D4DFF",
                    "#6069A1FF",
                    "#9A8DA6FF",
                    "#7D9A90FF", 
                    "#BADDAAFF",
                    "#EFD2B9FF", 
                    "#FFD966FF")

# Define the color mapping based on the recoded model names
color_mapping <- setNames(viridis_palette, name_prefixes)

# All movs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(data_plot, aes(x = state, y = value, color = em, fill = em)) +
  geom_boxplot(alpha = 0.5) +
  xlab('') + ylab('Mov. rate') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  facet_wrap(. ~ Mov, nrow = 2) +
  scale_color_manual(name = "Models", values = color_mapping) +  
  scale_fill_manual(name = "Models", values = color_mapping)

ggsave(filename = paste0(saveFolder, set_names, '_reps_MOV_ALL.png'), width = 190, height = 120, dpi = 800, units = 'mm')

# Inmature ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

conflicted::conflicts_prefer(dplyr::filter)

# Filter the dataset to include only rows where the state is "Immature"
data_plot_inmature <- data_plot %>% filter(state == "Immature")

# Create the plot
ggplot(data_plot_inmature, aes(x = state, y = value, color = em, fill = em)) +
  geom_boxplot(alpha = 0.5) +
  xlab('') + ylab('Mov. rate') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  facet_wrap(. ~ Mov, nrow = 2) +
  scale_color_manual(name = "Models", values = color_mapping) +  
  scale_fill_manual(name = "Models", values = color_mapping)

ggsave(filename = paste0(saveFolder, set_names, '_reps_MOV_ALL_inmature.png'), width = 190, height = 120, dpi = 800, units = 'mm')

# Mature ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
conflicted::conflicts_prefer(dplyr::filter)

# Filter the dataset to include only rows where the state is "Immature"
data_plot_mature <- data_plot %>% filter(state == "Mature")

# Create the plot
ggplot(data_plot_mature, aes(x = state, y = value, color = em, fill = em)) +
  geom_boxplot(alpha = 0.5) +
  xlab('') + ylab('Mov. rate') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  facet_wrap(. ~ Mov, nrow = 2) +
  scale_color_manual(name = "Models", values = color_mapping) +  
  scale_fill_manual(name = "Models", values = color_mapping)

ggsave(filename = paste0(saveFolder, set_names, '_reps_MOV_ALL_MATURE.png'), width = 190, height = 120, dpi = 800, units = 'mm')

# esta parte ya veremos  
data_plot_mature14 = data_plot_mature[data_plot_mature$Mov %in% c(
  'From R1 to R4','From R4 to R1'
),]
# Create the plot
d14<-  ggplot(data_plot_mature14, aes(x = state, y = value, color = em, fill = em)) +
  geom_boxplot(alpha = 0.5) +
  xlab('') + ylab(' ') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  facet_wrap(. ~ Mov, nrow = 1) +
  scale_color_manual(name = "Models", values = color_mapping) +  
  scale_fill_manual(name = "Models", values = color_mapping)

d14

# esta parte ya veremos  
data_plot_mature34 = data_plot_mature[data_plot_mature$Mov %in% c(
  'From R3 to R4','From R4 to R3'
),]
# Create the plot
d34<-  ggplot(data_plot_mature34, aes(x = state, y = value, color = em, fill = em)) +
  geom_boxplot(alpha = 0.5) +
  xlab('') + ylab('Mov. rate') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  facet_wrap(. ~ Mov, nrow = 1) +
  scale_color_manual(name = "Models", values = color_mapping) +  
  scale_fill_manual(name = "Models", values = color_mapping)

d34


# esta parte ya veremos  
data_plot_mature12 = data_plot_mature[data_plot_mature$Mov %in% c(
  'From R1 to R2','From R2 to R1'
),]
# Create the plot
d12<-  ggplot(data_plot_mature12, aes(x = state, y = value, color = em, fill = em)) +
  geom_boxplot(alpha = 0.5) +
  xlab('') + ylab('Mov. rate') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  facet_wrap(. ~ Mov, nrow = 1) +
  scale_color_manual(name = "Models", values = color_mapping) +  
  scale_fill_manual(name = "Models", values = color_mapping)+ ylim(0,1)

d12


# esta parte ya veremos  
data_plot_mature23 = data_plot_mature[data_plot_mature$Mov %in% c(
  'From R2 to R3','From R3 to R2'
),]
# Create the plot
d23<-  ggplot(data_plot_mature23, aes(x = state, y = value, color = em, fill = em)) +
  geom_boxplot(alpha = 0.5) +
  xlab('') + ylab(' ') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  facet_wrap(. ~ Mov, nrow = 1) +
  scale_color_manual(name = "Models", values = color_mapping) +  
  scale_fill_manual(name = "Models", values = color_mapping)

d23

# Modify each plot to remove "Mature" from x-axis labels
d14 <- d14 + scale_x_discrete(labels = function(x) gsub("Mature", "", x))
d34 <- d34 + scale_x_discrete(labels = function(x) gsub("Mature", "", x))
d12 <- d12 + scale_x_discrete(labels = function(x) gsub("Mature", "", x))
d23 <- d23 + scale_x_discrete(labels = function(x) gsub("Mature", "", x))

# Arrange the plots in a 2x2 grid
combined_plot <- ggarrange(d12, d14, d34, d23,   
                           ncol = 2, nrow = 2,
                           common.legend = TRUE, legend = "bottom")

# Display the combined plot
print(combined_plot)

ggsave(filename = paste0(saveFolder, set_names, '_reps_arrange_S4_mature.png'), width = 190, height = 120, dpi = 800, units = 'mm')

# Rec S4 S5 S6 -----------------------------------------------------------------

# Load packages
rm(list = ls())
library(r4ss)
require(dplyr)
require(ggplot2)
library(viridis)
library(conflicted)
library(ggplot2)

# Select set of models
set_names<-"Rec S4 S5 S6"
name_prefixes<-c("S4_1_mov_1_2_1_4_3_4_2_3",
                 "S4_2_mov_1_2_1_4_3_4",
                 "S5_1_mov_1_2_1_4_3_4_tag_mlp_1",
                 "S5_2_mov_1_2_1_4_3_4_tag_mlp_3",
                 "S6_1_rec_1_2", "best_model") # Mov and rec

# Load model reps ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# Set working directory to source file location
saveFolder = paste0(getwd(),"/100 replicates/output replicates/Figures/", set_names,"/")
dir.create(saveFolder)

# Define a function to load and set names for each set
load_and_set_names <- function(name_prefix) {
  set <- readRDS(paste0(getwd(), "/100 replicates/output replicates/output_", name_prefix, "_reps.RDS"))
  set$dq$em = name_prefix
  set$ts$em = name_prefix
  set$fish$em = name_prefix
  set$selex$em = name_prefix
  set$cpue$em = name_prefix 
  return(set)
}

# Load and set names for each set of models using a loop
all_sets <- list()
for (name_prefix in name_prefixes) {
  all_sets[[name_prefix]] <- load_and_set_names(name_prefix)
}

# Convergence ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Select max gradient for convergence
max_grad = 0.01

# Combine all sets into one dataset
combined_data_dq <- do.call(rbind, lapply(all_sets, function(set) set$dq))
data_dq = combined_data_dq
data_conv = data_dq[data_dq$Season == 1 & data_dq$Area == 1, ]

# Non-Convergence rate:
nonconv_rate = data_conv %>% dplyr::group_by(em) %>% dplyr::summarise(nonconvrate = sum((grad > max_grad) | is.nan(R0))/max(iter))
nonconv_rate$conv_rate = 1 - nonconv_rate$nonconvrate
nonconv_rate

# Save nonconvergent replicates:
data_conv = data_conv %>% mutate(replicate = paste0(em, '_', iter))
nonconv_rep = data_conv$replicate[which((data_conv$grad > max_grad) | is.nan(data_conv$R0))]
nonconv_rep # 17

# Plots ------------------------------------------------------------------------

# Combine all sets into one dataset
combined_data_ts <- do.call(rbind, lapply(all_sets, function(set) set$ts))

# Merge data in one dataset
data_ts = combined_data_ts
data_ts = data_ts %>% mutate(replicate = paste0(em, '_', iter))
data_ts = data_ts[!(data_ts$replicate %in% nonconv_rep), ]

# By season
temp_ts = data_ts %>% dplyr::group_by(em, iter, Yr, Area) %>% dplyr::summarise(SSB = mean(SSB, na.rm = TRUE), TotB = mean(TotB), Rec = mean(Rec))
data_area = temp_ts

# By Area
temp_ts = temp_ts %>% dplyr::group_by(em, iter, Yr) %>% dplyr::summarise(SSB = sum(SSB), TotB = sum(TotB), Rec = sum(Rec))
temp_ts_2 = temp_ts

# Continue:
temp_ts = tidyr::gather(temp_ts, 'variable', 'value', 4:6)
temp_ts = temp_ts %>% dplyr::group_by(em, Yr, variable) %>% dplyr::summarise(q025 = quantile(value, probs = 0.025),
                                                                             q50 = quantile(value, probs = 0.5),
                                                                             q975 = quantile(value, probs = 0.975))
data_plot = temp_ts[temp_ts$variable != 'TotB', ]

# Plot Rec by area ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Merge data in one dataset
data_rec_area = data_area
data_rec_area = data_rec_area %>% mutate(replicate = paste0(em, '_', iter))
data_rec_area = data_rec_area[!(data_rec_area$replicate %in% nonconv_rep), ]

# By Area
data_plot = data_rec_area %>% 
  dplyr::group_by(em, Yr, Area) %>% 
  dplyr::summarise(q025 = quantile(Rec, probs = 0.025), 
                   q50 = quantile(Rec, probs = 0.5), 
                   q975 = quantile(Rec, probs = 0.975))

# Update 'Area' to 'Region' in the data
data_plot$Area <- factor(as.character(data_plot$Area), 
                         levels = c('1', '4', '2', '3'), 
                         labels = c('Region 1', 'Region 4', 'Region 2', 'Region 3'))


# another:
library(ggplot2)
library(dplyr)

# Calculate the maximum q50 for each Area
max_q50_per_area <- data_plot %>%
  group_by(Area) %>%
  summarise(max_q50 = max(q50))

# Join the max values back to the original data
data_plot_percent <- data_plot %>%
  left_join(max_q50_per_area, by = "Area") %>%
  mutate(
    q50_percent = (q50 / max_q50) * 100,
    q025_percent = (q025 / max_q50) * 100,
    q975_percent = (q975 / max_q50) * 100
  )

# Summarize the data to get the mean percentages and quantiles by area and model
summary_data <- data_plot_percent %>%
  group_by(em, Area) %>%
  summarise(
    mean_q50_percent = mean(q50_percent, na.rm = TRUE),
    mean_q025_percent = mean(q025_percent, na.rm = TRUE),
    mean_q975_percent = mean(q975_percent, na.rm = TRUE)
  )


# Define the models with the recoded names
name_prefixes <- c("S0",
                   "S1.1",
                   "S2.1",
                   "S3.2",
                   "S4.1", 
                   "S4.2",
                   "S5.1", 
                   "S5.3",
                   "S6.1", 
                   "S6.3")

viridis_palette<- c("#001934FF",
                    "#00204DFF",
                    "#4668A3FF",
                    "#A34D4DFF",
                    "#6069A1FF",
                    "#9A8DA6FF",
                    "#7D9A90FF", 
                    "#BADDAAFF",
                    "#EFD2B9FF", 
                    "#FFD966FF")

# Define the color mapping based on the recoded model names
color_mapping <- setNames(viridis_palette, name_prefixes)


summary_data<-summary_data  %>% 
  mutate(em = recode(em,
                     "S1_1_selex_at_length" ="S1.1",
                     "S2_1_CPUE_st_Q_cte" = "S2.1",
                     "S4_1_mov_1_2_1_4_3_4_2_3" = "S4.1",                                                           "S6_2_rec_1_2_4" = "S6.2"     ,
                     "S4_2_mov_1_2_1_4_3_4" = "S4.2",
                     "S5_1_mov_1_2_1_4_3_4_tag_mlp_1" = "S5.1",
                     "S5_2_mov_1_2_1_4_3_4_tag_mlp_3" = "S5.3",
                     "S6_1_rec_1_2" ="S6.1",
                     "best_model" = "S6.3"   
  )) 


# Plot the summarized data with error bars in a stronger color
ggplot(summary_data, aes(x = em, y = mean_q50_percent, fill = em)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_q025_percent, ymax = mean_q975_percent, color = em), width = 0.2, position = position_dodge(0.9), alpha = 1) +
  xlab('') + ylab('Median recruitment (%)') +
  scale_fill_manual(values = color_mapping) +
  scale_color_manual(values = color_mapping) +
  theme_bw() +
  theme(
    legend.position = 'bottom',  # Move legend to the bottom
    legend.direction = 'horizontal',  # Set legend direction to horizontal
    legend.key.width = unit(1, 'cm'),  # Adjust legend key width if needed
    axis.text.x = element_blank()  # Remove x-axis labels
  ) +
  guides(
    fill = guide_legend(title = 'Models'),  # Set legend title for fill
    color = "none"  # Remove separate legend for error bar colors
  ) +
  facet_wrap(~ Area, scales = "free_y")

ggsave(filename = paste0(saveFolder, set_names, '_reps_TS_rec_%area.png'), width = 190, height = 110, dpi = 800, units = 'mm')

# Tag S5 S6 --------------------------------------------------------------------

# Load packages and clear the environment
rm(list = ls())
library(r4ss)
library(dplyr)
library(ggplot2)
library(viridis)
library(conflicted)

# Select set of models
set_names <- "Tag S5 S6"
name_prefixes <- c("S5_1_mov_1_2_1_4_3_4_tag_mlp_6", "S5_2_mov_1_2_1_4_3_4_tag_mlp_3",
                   "S6_1_rec_1_2", "best_model")

# Load model reps ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set working directory and create save folder
saveFolder <- paste0(getwd(), "/100 replicates/output replicates/Figures/", set_names, "/")
dir.create(saveFolder)

# Function to load and set names for each set
load_and_set_names <- function(name_prefix) {
  set <- readRDS(paste0(getwd(), "/100 replicates/output replicates/output_", name_prefix, "_reps.RDS"))
  set$dq$em <- name_prefix
  set$ts$em <- name_prefix
  set$fish$em <- name_prefix
  set$selex$em <- name_prefix
  set$cpue$em <- name_prefix 
  return(set)
}

# Load and set names for each model
all_sets <- list()
for (name_prefix in name_prefixes) {
  all_sets[[name_prefix]] <- load_and_set_names(name_prefix)
}

# Convergence ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define max gradient for convergence
max_grad <- 0.01

# Combine all datasets into one
combined_data_dq <- do.call(rbind, lapply(all_sets, function(set) set$dq))
data_dq <- combined_data_dq
data_conv <- data_dq[data_dq$Season == 1 & data_dq$Area == 1, ]

# Calculate non-convergence rate
nonconv_rate <- data_conv %>% 
  group_by(em) %>% 
  summarise(nonconvrate = sum((grad > max_grad) | is.nan(R0)) / max(iter))
nonconv_rate$conv_rate <- 1 - nonconv_rate$nonconvrate
print(nonconv_rate)

# Identify non-convergent replicates
data_conv <- data_conv %>% mutate(replicate = paste0(em, '_', iter))
nonconv_rep <- data_conv$replicate[which((data_conv$grad > max_grad) | is.nan(data_conv$R0))]
print(nonconv_rep)  # Output: 18

# Tag ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

combined_data_tag1 <- do.call(rbind, lapply(all_sets, function(set) set$tag1))
combined_data_tag2 <- do.call(rbind, lapply(all_sets, function(set) set$tag2))

# Combine the two tag datasets
combined_data <- rbind(combined_data_tag1, combined_data_tag2)

# Ensure conflicts are resolved
conflicted::conflicts_prefer(dplyr::filter)

# Load necessary libraries
library(dplyr)
library(stringr)

# Recode the em variable to simplify it to "S5.1" or "S5.2"
combined_data <- combined_data %>%
  mutate(
    em_recoded = case_when(
      grepl("^S5_1", rownames(combined_data)) ~ "S5.1",
      grepl("^S5_2", rownames(combined_data)) ~ "S5.3",
      grepl("^S6_1", rownames(combined_data)) ~ "S6.1",
      grepl("^best", rownames(combined_data)) ~ "S6.3",
      TRUE ~ NA_character_
    )
  )

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Step 1: Summarize the Data by Mean and Standard Deviation
summary_data <- combined_data %>%
  group_by(em_recoded, Yr.S, Fleet, Area) %>%
  summarise(
    mean_Obs = mean(Obs, na.rm = TRUE),
    lower_Obs = mean(Obs, na.rm = TRUE) - sd(Obs, na.rm = TRUE),
    upper_Obs = mean(Obs, na.rm = TRUE) + sd(Obs, na.rm = TRUE),
    mean_Exp = mean(Exp, na.rm = TRUE),
    lower_Exp = mean(Exp, na.rm = TRUE) - sd(Exp, na.rm = TRUE),
    upper_Exp = mean(Exp, na.rm = TRUE) + sd(Exp, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2: Aggregate Tag Recaptures by Year
agg_data <- summary_data %>%
  group_by(Yr.S, em_recoded) %>%
  summarise(
    Total_Obs = sum(mean_Obs),
    Total_Exp = sum(mean_Exp),
    lower_Total_Obs = sum(lower_Obs),
    upper_Total_Obs = sum(upper_Obs),
    lower_Total_Exp = sum(lower_Exp),
    upper_Total_Exp = sum(upper_Exp)
  )

# Define the models with the recoded names
name_prefixes <- c("S0",
                   "S1.1",
                   "S2.1",
                   "S3.2",
                   "S4.1", 
                   "S4.2",
                   "S5.1", 
                   "S5.3",
                   "S6.1", 
                   "S6.3")

viridis_palette<- c("#001934FF",
                    "#00204DFF",
                    "#4668A3FF",
                    "#A34D4DFF",
                    "#6069A1FF",
                    "#9A8DA6FF",
                    "#7D9A90FF", 
                    "#BADDAAFF",
                    "#EFD2B9FF", 
                    "#FFD966FF")

# Define the color mapping based on the recoded model names
color_mapping <- setNames(viridis_palette, name_prefixes)

# Plots ------------------------------------------------------------------------
  
# Plot 2: Line and bar plot with Total Recaptures (Observed vs. Expected)
ggplot(agg_data, aes(x = Yr.S)) +
  geom_bar(aes(y = Total_Exp, fill = em_recoded), stat = "identity", color = "grey43", alpha = 0.7, width = 0.3) +
  geom_line(aes(y = Total_Obs, color = "Observed"), size = 0.8, group = 1) +  # Corrected legend for geom_line
  geom_point(aes(y = Total_Obs), size = 1.1, color = "black") +  # Points in black
  labs(x = "Year", y = "Mean Post-latency tag recaptures") +
  theme_bw() +
  facet_wrap(~em_recoded) +
  scale_fill_manual(values = color_mapping, name = "Expected") +
  scale_color_manual(values = c("Observed" = "black"), name = "Observed") +  # Corrected scale_color_manual
  theme(legend.position = "bottom")  # Position legend at the bottom

ggsave(filename = paste0(saveFolder, set_names, '_reps_tag_recaptures_EObs.png'), width = 190, height = 110, dpi = 800, units = 'mm')


# Step 2: Aggregate Tag Recaptures by Year
agg_data <- summary_data %>%
  group_by(Yr.S, em_recoded) %>%
  summarise(
    Total_Obs = sum(mean_Obs),
    Total_Exp = sum(mean_Exp),
    lower_Total_Obs = sum(lower_Obs),
    upper_Total_Obs = sum(upper_Obs),
    lower_Total_Exp = sum(lower_Exp),
    upper_Total_Exp = sum(upper_Exp)
  )


# Plot 3: Line and bar plot with Total Recaptures (Observed vs. Expected)
ggplot(agg_data, aes(x = Yr.S)) +
  geom_bar(aes(y = Total_Exp, fill = em_recoded), stat = "identity", color = "black", alpha = 0.7, width = 0.3) +
  geom_errorbar(aes(ymin = Total_Exp, ymax = upper_Total_Exp), width = 0.2, color = "black") +  # Error bars only for positive part
  geom_line(aes(y = Total_Obs, color = "Observed"), size = 0.9, group = 1) +  # Map color to "Observed"
  geom_point(aes(y = Total_Obs, color = "Observed"), size = 1.3) +  # Map color to "Observed"
  labs(x = "Year", y = "Mean Post-latency tag recaptures") +  # Match the y-axis label to the previous plot
  theme_bw() +
  facet_wrap(~em_recoded) +
  scale_fill_manual(values = color_mapping, name = "Expected") +
  scale_color_manual(values = c("Observed" = "black"), name = "Observed") +  # Corrected color scale
  theme(legend.position = "bottom")  # Position legend at the bottom

# Save the plot
ggsave(filename = paste0(saveFolder, set_names, '_reps_tag_recaptures_EObs2.png'), width = 190, height = 110, dpi = 800, units = 'mm')

library(ggplot2)

# Define the models with the recoded names
name_prefixes <- c("S0",
                   "S1.1",
                   "S2.1",
                   "S3.2",
                   "S4.1", 
                   "S4.2",
                   "S5.1", 
                   "S5.3",
                   "S6.1", 
                   "S6.3")

viridis_palette<- c("#001934FF",
                    "#00204DFF",
                    "#4668A3FF",
                    "#A34D4DFF",
                    "#6069A1FF",
                    "#9A8DA6FF",
                    "#7D9A90FF", 
                    "#BADDAAFF",
                    "#EFD2B9FF", 
                    "#FFD966FF")

# Define the color mapping based on the recoded model names
color_mapping <- setNames(viridis_palette, name_prefixes)

# Plot 3: Line and bar plot with Total Recaptures (Observed vs. Expected)
ggplot(agg_data, aes(x = Yr.S)) +
  # Expected values bar with error bars
  geom_bar(aes(y = Total_Exp, fill = em_recoded), stat = "identity", color = "black", alpha = 0.7, width = 0.3) +
  geom_errorbar(aes(ymin = Total_Exp, ymax = upper_Total_Exp), width = 0.2, color = "black") +
  
  # Observed values line with ribbon for uncertainty (from 0 to upper bound)
  geom_ribbon(aes(ymin = 0, ymax = upper_Total_Obs), fill = "grey68", alpha = 0.2) +
  geom_line(aes(y = Total_Obs, color = "Observed"), size = 0.9, group = 1) +  # Set line color to black
  geom_point(aes(y = Total_Obs, color = "Observed"), size = 1.3, group = 1) +  # Set point color to black
  
  labs(x = "Year", y = "Mean Post-latency tag recaptures") +  # Match y-axis label to previous plot
  theme_bw() +
  facet_wrap(~em_recoded) +
  scale_fill_manual(values = color_mapping, name = "Expected") +  # Use custom color mapping
  scale_color_manual(values = c("Observed" = "black"), name = "Observed") +  # Legend for line and points
  theme(legend.position = "bottom")  # Position legend at the bottom

# Save the plot
ggsave(filename = paste0(saveFolder, set_names, '_reps_tag_recaptures_EObs3.png'), width = 190, height = 110, dpi = 800, units = 'mm')

# Plot 3: Residuals for post-latency tag recaptures
agg_data <- agg_data %>%
  mutate(Residual = (Total_Obs - Total_Exp) / sqrt(Total_Exp))

ggplot(agg_data, aes(x = Yr.S, y = Residual)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Year", y = "Residuals (obs-exp)/sqrt(exp)", title = "Residuals for Post-latency Tag Recaptures") +
  theme_bw() +
  facet_wrap(~em_recoded)

ggsave(filename = paste0(saveFolder, set_names, '_reps_tag_recaptures_EObs3residuals.png'), width = 190, height = 110, dpi = 800, units = 'mm')

# by fleet ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Step 1: Summarize the Data by Mean and Standard Deviation
summary_data <- combined_data %>%
  group_by(em_recoded, Yr.S, Fleet, Area) %>%
  summarise(
    mean_Obs = mean(Obs, na.rm = TRUE),
    lower_Obs = mean(Obs, na.rm = TRUE) - sd(Obs, na.rm = TRUE),
    upper_Obs = mean(Obs, na.rm = TRUE) + sd(Obs, na.rm = TRUE),
    mean_Exp = mean(Exp, na.rm = TRUE),
    lower_Exp = mean(Exp, na.rm = TRUE) - sd(Exp, na.rm = TRUE),
    upper_Exp = mean(Exp, na.rm = TRUE) + sd(Exp, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2: Aggregate Tag Recaptures by Year and Fleet
agg_data <- summary_data %>%
  group_by(Yr.S, em_recoded, Fleet) %>%
  summarise(
    Total_Obs = sum(mean_Obs),
    Total_Exp = sum(mean_Exp),
    lower_Total_Obs = sum(lower_Obs),
    upper_Total_Obs = sum(upper_Obs),
    lower_Total_Exp = sum(lower_Exp),
    upper_Total_Exp = sum(upper_Exp)
  ) %>%
  ungroup()

# Step 3: Recode Fleet values and filter for specific Fleets (11, 12, 13)
agg_data_filtered <- agg_data %>%
  filter(Fleet %in% c(11, 12, 13)) %>%
  mutate(Fleet = recode(Fleet, `11` = "Area 1", `12` = "Area 2", `13` = "Area 4"))

# Step 4: Plot with facets by Fleet and em_recoded
ggplot(agg_data_filtered, aes(x = Yr.S)) +
  # Expected values bar with error bars
  geom_bar(aes(y = Total_Exp), stat = "identity", fill = "grey", color = "black", alpha = 0.7, width = 0.3) +
  geom_errorbar(aes(ymin = Total_Exp, ymax = upper_Total_Exp), width = 0.2, color = "grey") +
  
  # Observed values line with ribbon for uncertainty (from 0 to upper bound)
  geom_ribbon(aes(ymin = 0, ymax = upper_Total_Obs), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = Total_Obs), size = 0.9, color = "blue", group = 1) +
  geom_point(aes(y = Total_Obs), size = 1.3, color = "blue") +
  
  labs(x = "Year", y = "Frequency", title = "Post-latency tag recaptures aggregated across tag groups") +
  theme_bw() +
  facet_grid(Fleet ~ em_recoded, scales="free_y")  # Use facet_grid to split by both Fleet and em_recoded

ggsave(filename = paste0(saveFolder, set_names, '_reps_tag_recaptures_other.png'), width = 190, height = 110, dpi = 800, units = 'mm')
