# Model assessment figures
# Author: Mary Lofton
# Date: 03APR25

# Purpose: four panel figure, first two panels are pred vs obs across all spp, second
# panel is quantile of pred distribution that obs falls into across all spp;
# also supplemental figures that break it out by spp
# ends up being four panels because focusing on two models, delta-env and N-species

# load packages
library(tidyverse)
library(lubridate)
library(arrow)
library(ggpubr)
library(ggthemes)


source("./other_code/assess_model_performance.R")

p1 <- assess_model_performance(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", 
                               model_output_folder = "./experiments/short-term_long-term",
                               plot_title = "")
p1$plot1
p1$plot2
ggsave(plot = p1, filename = "./visualizations/final_figures/Figure3_supp1.tif",
       device = "tiff", height = 5, width = 6.6, units = "in",bg = "white")
ggsave(plot = p2, filename = "./visualizations/final_figures/Figure3_supp2.tif",
       device = "tiff", height = 8, width = 8, units = "in",bg = "white")
ggsave(plot = p3, filename = "./visualizations/final_figures/Figure3_log.tif",
       device = "tiff", height = 8, width = 8, units = "in",bg = "white")

p2 <- assess_model_performance(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", 
                               model_output_folder = "./experiments/N_species_saveTreeEffect",
                               plot_title = "Model with environmental variables and two N species")
p2$plot1
p2$plot2
ggsave(plot = p2$plot1, filename = "./visualizations/model_skill_N_species.tif",
       device = "tiff", height = 5, width = 10, units = "in",bg = "white")
ggsave(plot = p2$plot2, filename = "./visualizations/pred_vs_obs_N_species.tif",
       device = "tiff", height = 10, width = 10, units = "in",bg = "white")
