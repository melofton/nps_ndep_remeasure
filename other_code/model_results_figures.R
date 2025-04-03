# Model results figure 2
# Author: Mary Lofton
# Date: 03APR25

# Purpose: two panel figure, first panel is delta growth vs delta Ndep,
# second panel is histograms of marginal effect of delta Ndep on growth
# per year per individual

# load packages
library(tidyverse)
library(lubridate)
library(arrow)
library(ggpubr)
library(ggthemes)

# delta growth vs delta Ndep
p1 <- delta_growth_vs_delta_Ndep(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", 
                           model_output_folder = "./experiments/new_delta_Ndep_only_saveTreeEffect",
                           experiment_name = "new_delta_Ndep_only_saveTreeEffect")
p1
experiment_name = "new_delta_Ndep_only_saveTreeEffect"
ggsave(plot = p1, filename = paste0("./visualizations/delta_growth_vs_delta_Ndep-",experiment_name,".tif"),
       device = "tiff", height = 5, width = 8, units = "in",bg = "white")

p2 <- delta_growth_vs_delta_Ndep(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", 
                                 model_output_folder = "./experiments/delta_env_saveTreeEffect",
                                 experiment_name = "delta_env_saveTreeEffect")
p2
experiment_name = "delta_env_saveTreeEffect"
ggsave(plot = p2, filename = paste0("./visualizations/delta_growth_vs_delta_Ndep-",experiment_name,".tif"),
       device = "tiff", height = 4, width = 6, units = "in",bg = "white")

p3 <- marginal_effect_Ndep(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv")
p3
ggsave(plot = p3, filename = "./visualizations/marginal_effect_Ndep.tif",
       device = "tiff", height = 8, width = 10, units = "in",bg = "white")

p4 <- growth_vs_size(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", 
                     model_output_folder = "./experiments/delta_env_saveTreeEffect")
p4
ggsave(plot = p4, filename = "./visualizations/growth_vs_size.tif",
       device = "tiff", height = 10, width = 10, units = "in",bg = "white")
