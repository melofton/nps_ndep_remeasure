# Model predictions vs observations
# Author: Mary Lofton
# Date: 03MAR25

# Purpose: calculate model predictions using JAGS model output and compare to observations

pred_vs_obs <- function(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", 
                        model_output_folder = "./experiments/new_delta_Ndep_only"){
  
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
  og_df <- read_csv(data, show_col_types = FALSE) %>%
    dplyr::filter(!common_name %in% c("Douglas-fir","western hemlock")) 
  #filter(common_name %in% c("eastern cottonwood"))
  
  focal_df <- og_df %>%
    select(common_name, plot_ID, tree_ID, interval_no, Dep_N, Dep_Noxi, Dep_Nred, 
           Dep_S, MAT, MAP, date_m2, date_m1, AG_carbon_pYear, AG_carbon_m1, 
           AG_carbon_m2, subp_BA_GT_m1, live_m2) 
  
  baseline_vars <- focal_df %>%
    group_by(plot_ID) %>%
    summarize(Dep_Nbaseline = mean(Dep_N, na.rm = TRUE),
              Dep_Noxibaseline = mean(Dep_Noxi, na.rm = TRUE),
              Dep_Nredbaseline = mean(Dep_Nred, na.rm = TRUE),
              Dep_Sbaseline = mean(Dep_S, na.rm = TRUE),
              MAT_baseline = mean(MAT, na.rm = TRUE),
              MAP_baseline = mean(MAP, na.rm = TRUE)) %>%
    ungroup()
  
  focal_df2 <- left_join(focal_df, baseline_vars, by = "plot_ID") %>%
    group_by(plot_ID) %>%
    mutate(Dep_Ndelta = Dep_N - Dep_Nbaseline,
           Dep_Noxidelta = Dep_Noxi - Dep_Noxibaseline,
           Dep_Nreddelta = Dep_Nred - Dep_Nredbaseline,
           Dep_Sdelta = Dep_S - Dep_Sbaseline,
           MAP_delta_dm = (MAP - MAP_baseline) * 0.01,
           MAT_delta = MAT - MAT_baseline) %>%
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
    dplyr::filter(num_intervals >= 2) 
  
  trees_index <- df |> 
    group_by(common_name) |> 
    distinct(tree_ID) |> 
    mutate(tree_index = 1:n())
  
  df1 <- df %>%
    left_join(trees_index, by = join_by(common_name, tree_ID)) |>
    filter(tree_index %in% unique(tree_effects$tree_index)) |>
    mutate(common_name = ifelse(common_name == "yellow-poplar","yellow poplar",common_name)) |>
    left_join(tree_effects, by = join_by(common_name, tree_index)) |>
    left_join(final_param_sum, by = join_by(common_name))
  
  n_measures <- nrow(df1)
  tree_effect <- df1$tree_effect
  ndep_delta <- df1$Dep_Ndelta
  p5 <- df1$p5
  tree_agb_obs <- df1$AG_carbon_m1
  p2 <- df1$p2
  ba_gt <- df1$subp_BA_GT_m1
  p3 <- df1$p3

  pred <- NULL
  
  for(n in 1:n_measures){
    pred[n] <-  ((tree_effect[n] + ndep_delta[n]*p5[n]) * tree_agb_obs[n] ^ p2[n])  * exp(-ba_gt[n]*p3[n]) 
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
