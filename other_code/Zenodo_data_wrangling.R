# Title: Make Zenodo data publication
# Author: Mary Lofton
# Date: 09MAR26

# Purpose: trim down the McDonnell et al dataset to just be the variables used in this study

library(tidyverse)

og_df <- read_csv("./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", show_col_types = FALSE) 

focal_df <- og_df %>%
  select(tree_ID,
         plot_ID,
         tree_CN_m1,
         tree_CN_m2,
         plot_CN_m1,
         plot_CN_m2,
         date_m1,
         date_m2,
         interval_no,
         lat,
         lon,
         elev,
         aspect,
         slope,
         species_code,
         species,
         common_name,
         subp_BA_m1,
         subp_BA_GT_m1,
         subp_BA_ratio_m1,
         subp_BA_species_ratio_m1,
         MAT,
         MAP,
         Dep_N,
         Dep_N15,
         Dep_S,
         Dep_S15,
         Dep_Noxi,
         Dep_Noxi15,
         Dep_Nred,
         Dep_Nred15,
         Dep_Nratio,
         Dep_Nratio15,
         Ozone_avg,
         Ozone_max,
         basal_area_m1,
         basal_area_m2,
         AG_carbon_m1,
         AG_carbon_m2,
         AG_carbon_pYear,
         live_m2)

write.csv(focal_df, "./data/TreeData_2024_10_11.csv", row.names = FALSE)
