# Job script to run model
# Author: Mary Lofton
# Date: 13JAN24

# Purpose: workflow script to run ss model for one species

library(tidyverse)
library(rjags)
library(tidybayes)
library(bayesplot)
library(furrr)

mem.maxVSize(vsize = Inf)

og_df <- read_csv("./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", show_col_types = FALSE) %>%
 dplyr::filter(!common_name %in% c("Douglas-fir","western hemlock")) 
  #filter(common_name %in% c("eastern cottonwood"))

focal_df <- og_df %>%
  select(common_name, plot_ID, tree_ID, interval_no, Dep_N, Dep_N15, Dep_Noxi15, Dep_Nred15, Dep_Noxi, Dep_Nred, 
         Dep_S, Dep_S15, MAT, MAP, date_m2, date_m1, AG_carbon_pYear, AG_carbon_m1, 
         AG_carbon_m2, subp_BA_GT_m1, live_m2, Ozone_avg) 

cumsum_vars <- focal_df %>%
  select(date_m1, date_m2, Dep_N, Dep_N15, tree_ID, interval_no) %>%
  distinct(.) %>%
  arrange(tree_ID, date_m1) %>%
  mutate(dt = as.numeric(date_m2 - date_m1)/365) 

hist(cumsum_vars$dt, main = "Length of measurement intervals (yrs)",
     xlab = "years")

interval_ndep <- cumsum_vars %>%
  select(tree_ID, interval_no, Dep_N) %>%
  group_by(tree_ID) %>%
  pivot_wider(names_from = interval_no, values_from = Dep_N) %>%
  ungroup()
colnames(interval_ndep) <- c("tree_ID","depn_1","depn_2","depn_3","depn_4")

interval_dt <- cumsum_vars %>%
  select(tree_ID, interval_no, dt) %>%
  group_by(tree_ID) %>%
  pivot_wider(names_from = interval_no, values_from = dt) %>%
  ungroup()
colnames(interval_dt) <- c("tree_ID","dt_1","dt_2","dt_3","dt_4")

plot_ante <- focal_df %>%
  select(tree_ID, interval_no, date_m1, date_m2, Dep_N15, Dep_N) %>%
  distinct(.) %>%
  group_by(tree_ID) %>%
  filter(interval_no == 1) %>%
  arrange(tree_ID) %>%
  mutate(dt = as.numeric(date_m2 - date_m1)/365) %>%
  mutate(total_years = 15 + dt) %>%
  mutate(Dep_Nhistoric = (Dep_N15*total_years - Dep_N*dt) / 15) %>%
  select(tree_ID, Dep_Nhistoric) %>%
  ungroup()

cumsum_vars2 <- left_join(interval_ndep, interval_dt, by = "tree_ID") %>%
  left_join(plot_ante, by = "tree_ID") %>%
  mutate(wgt_avg_1 = Dep_Nhistoric,
         wgt_avg_2 = (Dep_Nhistoric*15 + depn_1*dt_1)/(15 + dt_1),
         wgt_avg_3 = (Dep_Nhistoric*15 + depn_1*dt_1 + depn_2*dt_2)/(15 + dt_1 + dt_2),
         wgt_avg_4 = (Dep_Nhistoric*15 + depn_1*dt_1 + depn_2*dt_2 + depn_3*dt_3)/(15 + dt_1 + dt_2 + dt_3)) %>%
  select(tree_ID, wgt_avg_1, wgt_avg_2, wgt_avg_3, wgt_avg_4) %>%
  group_by(tree_ID) %>%
  pivot_longer(wgt_avg_1:wgt_avg_4, names_to = "interval_temp",values_to = "Dep_N_wgt_avg_to_date") %>%
  ungroup() %>%
  filter(!is.na(Dep_N_wgt_avg_to_date)) %>%
  separate(interval_temp, into = c(NA,NA,"interval_no"), sep = "_") %>%
  mutate(interval_no = as.numeric(interval_no))

cumsum_vars3 <- left_join(cumsum_vars, cumsum_vars2, by = c("tree_ID","interval_no")) %>%
  mutate(Dep_Ndeviation = Dep_N - Dep_N_wgt_avg_to_date) %>%
  select(tree_ID, interval_no, Dep_N_wgt_avg_to_date, Dep_Ndeviation) %>%
  filter(complete.cases(.)) 

#### orthogonalization code - have to run this on ARC
for(i in 1:max(cumsum_vars3$interval_no)){
  
  print(i)
  
  test <- cumsum_vars3 %>%
    filter(interval_no == i) %>%
    group_by(Dep_Ndeviation, Dep_N_wgt_avg_to_date) %>%
    mutate(group_id = cur_group_id())
  
  current_dat <- test %>%
    select(group_id, Dep_Ndeviation, Dep_N_wgt_avg_to_date) %>%
    distinct(.)
  
  S <- current_dat %>%
    pull(Dep_Ndeviation)

  L <- current_dat %>%
    pull(Dep_N_wgt_avg_to_date)

  n = length(S)

  X = cbind(1, S) # design matrix needs to include intercept [1, S]
  Phat = X %*% solve(t(X) %*% X, t(X)) # projection onto span{1,S}
  M = diag(n) - Phat
  O_proj = as.numeric(M %*% L)

  current_dat$Dep_N_wgt_avg_to_date_ortho <- O_proj
  
  test2 <- left_join(test, current_dat, by = c("group_id", "Dep_Ndeviation", "Dep_N_wgt_avg_to_date"))

  if(i == 1){
    final <- test2
  } else {
    final <- bind_rows(final, test2)
  }

}

cumsum_vars4 <- final %>%
  rename(ortho_group_id = group_id)
####

baseline_vars <- focal_df %>%
  select(plot_ID, date_m1, date_m2, Dep_N, Dep_Noxi, Dep_Nred, Dep_S, MAT, MAP, Ozone_avg) %>%
  distinct(.) %>%
  group_by(plot_ID) %>%
  summarize(Dep_Nbaseline = mean(Dep_N, na.rm = TRUE),
            Dep_Noxibaseline = mean(Dep_Noxi, na.rm = TRUE),
            Dep_Nredbaseline = mean(Dep_Nred, na.rm = TRUE),
            Dep_Sbaseline = mean(Dep_S, na.rm = TRUE),
            MAT_baseline = mean(MAT, na.rm = TRUE),
            MAP_baseline = mean(MAP, na.rm = TRUE),
            Ozone_avg_baseline = mean(Ozone_avg, na.rm = TRUE)) %>%
  ungroup()

baseline_vars2 <- focal_df %>%
  select(plot_ID, date_m1, date_m2, Dep_N15, Dep_N, Dep_Noxi,
         Dep_Noxi15, Dep_Nred, Dep_Nred15, Dep_S, Dep_S15) %>%
  distinct(.) %>%
  group_by(plot_ID) %>%
  filter(date_m1 == min(date_m1, na.rm = TRUE)) %>%
  arrange(plot_ID) %>%
  mutate(dt = as.numeric(date_m2 - date_m1)/365) %>%
  mutate(total_years = 15 + dt) %>%
  mutate(Dep_Nhistoric = (Dep_N15*total_years - Dep_N*dt) / 15,
         Dep_Noxihistoric = (Dep_Noxi15*total_years - Dep_Noxi*dt) / 15,
         Dep_Nredhistoric = (Dep_Nred15*total_years - Dep_Nred*dt) / 15,
         Dep_Shistoric = (Dep_S15*total_years - Dep_S*dt) / 15) %>%
  mutate(Dep_NpropBaseline = Dep_N / Dep_Nhistoric,
         Dep_NoxipropBaseline = Dep_Noxi / Dep_Noxihistoric,
         Dep_NredpropBaseline = Dep_Nred / Dep_Nredhistoric) %>%
  select(plot_ID, Dep_Nhistoric, Dep_Noxihistoric, Dep_Nredhistoric, Dep_Shistoric,
         Dep_NpropBaseline, Dep_NoxipropBaseline, Dep_NredpropBaseline)

focal_df2 <- left_join(focal_df, baseline_vars, by = "plot_ID") %>%
  left_join(baseline_vars2, by = "plot_ID") %>%
  left_join(cumsum_vars4, by = c("tree_ID","interval_no")) %>%
  group_by(plot_ID) %>%
  mutate(Dep_Ndelta = Dep_N - Dep_Nbaseline,
         Dep_Noxidelta = Dep_Noxi - Dep_Noxibaseline,
         Dep_Nreddelta = Dep_Nred - Dep_Nredbaseline,
         Dep_Sdelta = Dep_S - Dep_Sbaseline,
         MAP_delta_dm = (MAP - MAP_baseline) * 0.01,
         MAT_delta = MAT - MAT_baseline,
         Ozone_avg_delta = Ozone_avg - Ozone_avg_baseline,
         Dep_Ndiff = Dep_N - Dep_Nhistoric,
         Dep_Noxidiff = Dep_Noxi - Dep_Noxihistoric,
         Dep_Nreddiff = Dep_Nred - Dep_Nredhistoric,
         Dep_Sdiff = Dep_S - Dep_Shistoric,
         MAP_baseline_dm = MAP_baseline * 0.01,
         Dep_N_LTchange = Dep_Nbaseline - Dep_Nhistoric) %>%
  ungroup() 

live_tree_ids <- focal_df2 |> 
  summarise(count = n(),
            sum = sum(live_m2), .by = tree_ID) |> 
  dplyr::filter(count >= sum) |> 
  pull(tree_ID)

focal_df3 <- focal_df2 |> 
  dplyr::filter(tree_ID %in% live_tree_ids) |> 
  group_by(tree_ID) |> 
  tidyr::fill(subp_BA_GT_m1, .direction = "down") |> 
  ungroup() 

df <- focal_df3 %>%
  dplyr::filter(complete.cases(.)) %>%
  group_by(tree_ID) %>%
  mutate(num_intervals = max(interval_no, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::filter(num_intervals >= 1)

# write.csv(df, "./data/processed_data.csv", row.names = FALSE)

total_species <- length(unique(df$common_name))

sim <- "ss_space_vs_time"

if(sim %in% c("historic_deviation_interaction","historic_deviation",
              "historic_deviation_S","ss_space_vs_time")){
df <- focal_df3 %>%
  dplyr::filter(complete.cases(.)) %>%
  group_by(tree_ID) %>%
  mutate(num_intervals = max(interval_no, na.rm = TRUE)) %>%
  ungroup() 
}

source("./modeling_code/ss_space_vs_time.R")

# for(k in 8:total_species){
#   run_model(k, df, sim)
# }

future::plan("future::multisession", workers = 8) # workers is number of sessions

total_species <- length(unique(df$common_name))
furrr::future_walk(1:total_species, run_model, df, sim)
