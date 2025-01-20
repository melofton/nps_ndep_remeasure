# Job script to run model
# Author: Mary Lofton
# Date: 13JAN24

# Purpose: workflow script to run ss model for one species

library(tidyverse)
library(rjags)
library(tidybayes)
library(bayesplot)
library(furrr)

df <- read_csv("./McDonnell_etal_InPrep_TreeData_2024_10_11.csv", show_col_types = FALSE)

total_species <- length(unique(df$common_name))

sim <- "testS2"

source("./ss_modeling_example.R")

# for(k in 8:total_species){
#   run_model(k, df, sim)
# }

future::plan("future::multisession", workers = 4) # workers is cores

total_species <- length(unique(df$common_name))
furrr::future_walk(8:total_species, run_model, df, sim)
