# Model predictions vs observations
# Author: Mary Lofton
# Date: 03MAR25

# Purpose: calculate model predictions using JAGS model output and compare to observations

pred_vs_obs <- function(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", 
                        model_output_folder = "./experiments/delta_Ndep"){
  
  ## READ IN AND WRANGLE MODEL OUTPUT
  # list files
  out <- list.files(model_output_folder,pattern = ".parquet",
                    full.names = TRUE)
  
  for(i in 1:length(out)){
    
    model_name = str_split(out[i], pattern = "-")[[1]][2]
    temp <- read_parquet(file = out[i]) %>%
      mutate(model_id = model_name)
    
    if(i == 1){
      final <- temp
    } else {
      final <- bind_rows(final, temp)
    }
    
  }
  
  final <- final %>%
    mutate(model_id = ifelse(model_id == "yellow","yellow poplar",model_id))
  
  param_sum <- final %>%
    select(-c(.chain,.iteration,.draw)) %>%
    pivot_longer(-model_id,names_to = "param", values_to = "param_value") %>%
    group_by(model_id, param) %>%
    summarize(mean_param_value = mean(param_value, na.rm = TRUE)) %>%
    pivot_wider(names_from = "param", values_from = "mean_param_value") %>%
    ungroup() %>%
    rename(common_name = model_id) 
  
  final_param_sum <- param_sum %>% 
    select(common_name, p2:p5)
  
  tree_effects <- param_sum %>%
    select(common_name,contains(c("tree_effect["))) %>%
    pivot_longer(-common_name, names_to = "param", values_to = "tree_effect") %>%
    separate(param, sep = '\\[', into = c("param","tree_num")) %>%
    mutate(tree_index = str_remove_all(tree_num, "[\\]]")) %>%
    select(-c(param, tree_num)) %>%
    mutate(tree_index = as.double(tree_index)) 
  
  
  ## READ IN AND WRANGLE DATA
  df <- read_csv(data, show_col_types = FALSE) %>%
    filter(!common_name %in% c("Douglas-fir","western hemlock"))
  
  live_tree_ids <- df |> 
    summarise(count = n(),
              sum = sum(live_m2), .by = tree_ID) |> 
    dplyr::filter(count >= sum) |> 
    pull(tree_ID)
  
  focal_data <- df |> 
    dplyr::filter(tree_ID %in% live_tree_ids) |> 
    group_by(common_name, tree_ID) |> 
    tidyr::fill(subp_BA_GT_m1, .direction = "down") |> 
    dplyr::filter(!is.na(subp_BA_GT_m1) & !is.na(Dep_N)) |>
    ungroup() 
  
  baseline_Ndep <- focal_data %>%
    select(common_name, tree_ID, interval_no, Dep_N15, Dep_N, date_m2, date_m1) %>%
    filter(interval_no == 1) %>%
    mutate(dt = as.numeric(date_m2 - date_m1)/365) %>%
    mutate(total_years = 15 + dt) %>%
    mutate(Dep_Nbaseline = (Dep_N15*total_years - Dep_N*dt) / 15) %>%
    select(tree_ID, Dep_Nbaseline)
  
  focal_data2 <- left_join(focal_data, baseline_Ndep, by = "tree_ID") %>%
    mutate(Dep_Ndelta = Dep_N - Dep_Nbaseline) %>%
    filter(!is.na(Dep_Nbaseline) & !is.na(Dep_Ndelta))
  
  trees_index <- focal_data2 |> 
    group_by(common_name) |> 
    distinct(tree_ID) |> 
    dplyr::filter(tree_ID %in% live_tree_ids) |> 
    mutate(tree_index = 1:n())
  
  plots <- focal_data2 |> 
    group_by(common_name) |> 
    dplyr::filter(tree_ID %in% live_tree_ids) |> 
    distinct(plot_ID) |> 
    mutate(plot_index = 1:n()) #USE THIS TO GET PLOT_INDEX BELOW
  
  df1 <- focal_data2 |> 
    mutate(dt = as.numeric(date_m2 - date_m1)/365) |> 
    select(common_name, AG_carbon_pYear, AG_carbon_m1, AG_carbon_m2, tree_ID, plot_ID, dt, Dep_N, Dep_Nbaseline, Dep_Ndelta, subp_BA_GT_m1, MAT, MAP, Dep_S) |>
    dplyr::filter(tree_ID %in% live_tree_ids) |>
    left_join(plots, by = join_by(common_name, plot_ID)) |> 
    left_join(trees_index, by = join_by(common_name, tree_ID)) |>
    filter(tree_index %in% unique(tree_effects$tree_index)) |>
    mutate(common_name = ifelse(common_name == "yellow-poplar","yellow poplar",common_name)) |>
    left_join(tree_effects, by = join_by(common_name, tree_index)) |>
    left_join(final_param_sum, by = join_by(common_name))
  
  n_measures <- nrow(df1)
  tree_effect <- df1$tree_effect
  ndep_baseline <- df1$Dep_Nbaseline
  p4 <- df1$p4
  ndep_delta <- df1$Dep_Ndelta
  p5 <- df1$p5
  tree_agb_obs <- df1$AG_carbon_m1
  p2 <- df1$p2
  ba_gt <- df1$subp_BA_GT_m1
  p3 <- df1$p3

  pred <- NULL
  
  for(n in 1:n_measures){
    pred[n] <-  ((tree_effect[n] + ndep_baseline[n]*p4[n] + ndep_delta[n]*p5[n]) * tree_agb_obs[n] ^ p2[n])  * exp(-ba_gt[n]*p3[n]) 
  }
  
  df1$pred <- pred
  
  p1 <- ggplot(data = df1)+
    geom_point(aes(x = AG_carbon_pYear, y = pred, color = as.factor(tree_ID)))+
    geom_abline(slope = 1)+
    theme_bw()+
    theme(legend.position = "none")+
    ggtitle("Displaying repeated measures")
  
  plot_data <- df1 %>%
    group_by(tree_ID) %>%
    slice(1)
  
  p2 <- ggplot(data = plot_data)+
    geom_point(aes(x = AG_carbon_pYear, y = pred, color = as.factor(tree_ID)))+
    geom_abline(slope = 1)+
    theme_bw()+
    theme(legend.position = "none")+
    ggtitle("First measure per tree only")
  
  return(list(df = df1, plot1 = p1, plot2 = p2))
    
  }
