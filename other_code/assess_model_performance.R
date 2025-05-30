# Model predictions vs observations
# Author: Mary Lofton
# Date: 03MAR25

# Purpose: calculate model predictions using JAGS model output and compare to observations

source("./other_code/get_model_inputs.R")

assess_model_performance <- function(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", 
                        model_output_folder = "./experiments/short-term_long-term",
                        plot_title = ""){
  
  ## READ IN AND WRANGLE MODEL OUTPUT
  # list files
  out <- list.files(model_output_folder,pattern = "treeEffect.parquet",
                    full.names = TRUE)
  
  if(grep("short-term_long-term",model_output_folder)){
    for(i in 1:length(out)){
      
      spp_name = str_split(out[i], pattern = "-")[[1]][6]
      model_name = str_split(out[i], pattern = "/")[[1]][3]
      temp <- read_parquet(file = out[i]) %>%
        mutate(spp_id = spp_name,
               model_id = model_name)
      
      if(i == 1){
        final <- temp
      } else {
        final <- bind_rows(final, temp)
      }
      
    }
  } else {
  
  for(i in 1:length(out)){
    
    model_name = str_split(out[i], pattern = "-")[[1]][2]
    iters <- sample(c(1:3000),100)
    
    temp <- read_parquet(file = out[i], as_data_frame = FALSE,col_select = c("n","tree_effect",".iteration")) |>
      filter(.iteration %in% iters) |>
      select(-.iteration) |>
      collect() |>
      mutate(model_id = model_name)
    
    if(i == 1){
      final <- temp
    } else {
      final <- bind_rows(final, temp)
    }
    
  }
  }
  
  final <- final %>%
    mutate(model_id = ifelse(model_id == "yellow","yellow poplar",model_id))
  
  tree_effects <- final %>%
    group_by(model_id, n) %>%
    summarize(mean_tree_effect = mean(tree_effect, na.rm = TRUE)) %>%
    rename(tree_index = n,
           common_name = model_id)
  
  # list files
  out1 <- list.files(model_output_folder,pattern = "mcmc.parquet",
                    full.names = TRUE)
  
  for(i in 1:length(out1)){
    
    model_name = str_split(out1[i], pattern = "-")[[1]][2]

    temp <- read_parquet(file = out1[i]) %>%
      mutate(model_id = model_name)
    
    if(i == 1){
      final1 <- temp
    } else {
      final1 <- bind_rows(final1, temp)
    }
    
  }
  
  final1 <- final1 %>%
    mutate(model_id = ifelse(model_id == "yellow","yellow poplar",model_id))
  
  param_sum <- final1 %>%
    select(-c(.chain,.iteration,.draw)) %>%
    pivot_longer(-model_id,names_to = "param", values_to = "param_value") %>%
    group_by(model_id, param) %>%
    summarize(mean_param_value = mean(param_value, na.rm = TRUE)) %>%
    pivot_wider(names_from = "param", values_from = "mean_param_value") %>%
    ungroup() %>%
    rename(common_name = model_id) 
  
  final_param_sum <- param_sum 
  
  
  ## READ IN AND WRANGLE DATA
  df <- get_model_inputs(data) 
  
  trees_index <- df |> 
    group_by(common_name) |> 
    distinct(tree_ID) |> 
    mutate(tree_index = 1:n())
  
  df1 <- df %>%
    left_join(trees_index, by = join_by(common_name, tree_ID)) |>
    mutate(common_name = ifelse(common_name == "yellow-poplar","yellow poplar",common_name)) |>
    left_join(tree_effects, by = join_by(common_name, tree_index)) |>
    left_join(final_param_sum, by = join_by(common_name)) 
  
  if(grep("delta_env",model_output_folder)){
    df1 <- df1 %>% mutate(pred = ((mean_tree_effect + Dep_Ndelta*p5 + Dep_Sdelta*p6 + MAT_delta*p7 + MAP_delta_dm*p8) * AG_carbon_m1 ^ p2)  * exp(-subp_BA_GT_m1*p3) ) 
  }
  if(grep("N_species",model_output_folder)){
    df1 <- df1 %>% mutate(pred = ((mean_tree_effect + Dep_Noxidelta*p4 + Dep_Nreddelta*p5 + Dep_Sdelta*p6 + MAT_delta*p7 + MAP_delta_dm*p8) * AG_carbon_m1 ^ p2)  * exp(-subp_BA_GT_m1*p3) ) 
  }
  if(grep("short-term_long-term",model_output_folder)){
    df1 <- df1 %>% mutate(pred = ((mean_tree_effect + Dep_Ndelta*p5 + Dep_Sdelta*p6 + MAT_delta*p7 + MAP_delta_dm*p8 + Dep_N_LTchange*p9 + Dep_Nhistoric*p10) * AG_carbon_m1 ^ p2)  * exp(-subp_BA_GT_m1*p3) )
  }
  
  rsq <- function(pred, obs){
    1 - (sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE))
  }
  
  mod_assess <- df1 %>%
    group_by(common_name, species) %>%
    summarize(r2 = rsq(pred = pred, obs = AG_carbon_pYear),
              rmse = sqrt(mean((AG_carbon_pYear - pred)^2, na.rm = TRUE)),
              mae = mean(abs(pred - AG_carbon_pYear), na.rm = TRUE))

  r2 <- ggplot(mod_assess, aes(x=reorder(species, r2), y=r2, color=as.factor(species))) + 
    geom_point() +
    geom_segment(aes(x=species,xend=species,y=0,yend=r2)) +
    ylab(expression(paste(R^2))) +
    xlab("Species") +
    coord_flip() +
    scale_color_colorblind()+
    theme_bw()+
    theme(legend.position = "none")
  
  rmse <- ggplot(mod_assess, aes(x=reorder(species, -rmse), y=rmse, color=as.factor(species))) + 
    geom_point() +
    geom_segment(aes(x=species,xend=species,y=0,yend=rmse)) +
    ylab(expression(paste("RMSE (kg C ", y^-1," ",ind^-1,")"))) +
    xlab("Species") +
    coord_flip() +
    scale_color_colorblind()+
    theme_bw()+
    theme(legend.position = "none")
  
  mae <- ggplot(mod_assess, aes(x=reorder(species, -mae), y=mae, color=as.factor(species))) + 
    geom_point() +
    geom_segment(aes(x=species,xend=species,y=0,yend=mae)) +
    ylab(expression(paste("MAE (kg C ", y^-1," ",ind^-1,")"))) +
    xlab("Species") +
    coord_flip() +
    scale_color_colorblind()+
    theme_bw()+
    theme(legend.position = "none")
  
  p1 <- ggarrange(r2, rmse, mae,
                  ncol = 3)
  p1 <- annotate_figure(p1, top = text_grob(plot_title, 
                                        color = "black", face = "bold", size = 14))
  
  p2 <- ggplot(data = df1)+
    geom_point(aes(x = AG_carbon_pYear, y = pred))+
    geom_abline(slope = 1)+
    theme_bw()+
    theme(legend.position = "none")+
    ggtitle("Displaying repeated measures")+
    facet_wrap(facets = vars(species), scales = "free")
  
  return(list(df = mod_assess, plot1 = p1, plot2 = p2))
    
  }
