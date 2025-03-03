# Model predictions vs observations
# Author: Mary Lofton
# Date: 03MAR25

# Purpose: calculate model predictions using JAGS model output and compare to observations

delta_growth_vs_delta_Ndep <- function(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", 
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
    select(common_name, global_tree_effect:p5)
  
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
  
  df1 <- focal_data2 %>%
    group_by(common_name) %>%
    summarize(mean_baseline_Ndep = mean(Dep_Nbaseline, na.rm = TRUE),
              mean_size = mean(AG_carbon_m1, na.rm = TRUE)) %>%
    mutate(common_name = ifelse(common_name == "yellow-poplar","yellow poplar",common_name))
  
  delta_growth_v_delta_Ndep <- function(global_tree_effect,
                            x,
                            p4,
                            ndep_baseline,
                            p5,
                            tree_agb_obs,
                            p2){
    growth_baseline =  ((global_tree_effect + ndep_baseline*p4 + 0*p5) * tree_agb_obs ^ p2)
    growth_delta =  ((global_tree_effect + ndep_baseline*p4 + x*p5) * tree_agb_obs ^ p2) 
    y = growth_delta - growth_baseline
    return(y)
  }
  
  species_list <- unique(df1$common_name)
  
  Ndep_range <- focal_data2 %>%
    summarize(max_delta_Ndep = max(Dep_Ndelta, na.rm = TRUE),
              min_delta_Ndep = min(Dep_Ndelta, na.rm = TRUE),
              mean_delta_Ndep = mean(Dep_Ndelta, na.rm = TRUE),
              median_delta_Ndep = median(Dep_Ndelta, na.rm = TRUE))
  
  max_delta_Ndep = Ndep_range$max_delta_Ndep
  min_delta_Ndep = Ndep_range$min_delta_Ndep
  
  plot_data <- left_join(df1, final_param_sum, by = join_by(common_name))
  
  my.cols <- c("black cherry" = "brown",
               "eastern cottonwood" = "red",
               "paper birch" = "orange",
               "ponderosa pine" = "yellow",
               "quaking aspen" = "green",
               "red spruce" = "blue",
               "sugar maple" = "violet",
               "yellow poplar" = "purple")
  
  plot_params <- plot_data %>%
    select(-p3,-common_name) %>%
    rename(ndep_baseline = mean_baseline_Ndep,
           tree_agb_obs = mean_size) %>%
    mutate(color = my.cols) 
    
  
  p1 <- ggplot(data = plot_params) +
    purrr::pmap(
      plot_params,
      function(global_tree_effect,p4,ndep_baseline,p5,tree_agb_obs,p2,color) geom_function(data = plot_params,fun = function(x){
        growth_baseline =  ((global_tree_effect + ndep_baseline*p4 + 0*p5) * tree_agb_obs ^ p2)
        growth_delta =  ((global_tree_effect + ndep_baseline*p4 + x*p5) * tree_agb_obs ^ p2) 
        y = growth_delta - growth_baseline
        return(y)
      }, col = color)
    ) +
    xlim(-5,5) +
    theme_bw() +
    xlab("Delta N dep.") +
    ylab("Delta growth")
  p1
  
  plot_params_legend <- plot_data %>%
    select(-p3,) %>%
    rename(ndep_baseline = mean_baseline_Ndep,
           tree_agb_obs = mean_size) %>%
    mutate(color = my.cols) 
  
  leg_p1_plot <- ggplot(data = plot_params_legend, aes(x = p4, y = p5,
                                                  group = common_name, color = common_name))+
    geom_line()+
    theme_bw()+
    scale_color_manual(values = my.cols)
  
  # Extract the legend. Returns a gtable
  leg_p1 <- get_legend(leg_p1_plot)
  
  # Convert to a ggplot and print
  leg <- as_ggplot(leg_p1)
  leg
  
  p <- ggarrange(p1,leg,
                  ncol = 2,nrow = 1,
                  widths = c(1,0.3)
  ) + bgcolor("white")
  
  p
  
  ggsave(plot = p, filename = "./visualizations/delta_growth_vs_delta_Ndep.tif",
         device = "tiff", height = 3, width = 6, units = "in")
  
  message("all done!")
    
  }
