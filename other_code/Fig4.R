# Marginal effect of N deposition on growth
# Author: Mary Lofton
# Date: 03APR25

# Purpose: figure with marginal effect of N deposition (decrease of 1 kg
# per hectare per year) on growth (kg C per year per individual)

# load packages
library(tidyverse)
library(lubridate)
library(arrow)
library(ggpubr)
library(ggthemes)
library(stringr)

data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv"

# list files in each model output folder
out <- list.files("./experiments/ortho_log_t_interaction_adj_priors",pattern = "mcmc.parquet",
                   full.names = TRUE)

# read in and combine files
for(i in 1:length(out)){
  
  spp_id = str_split(out[i], pattern = "-")[[1]][2]
  model_id = str_split(out[i], pattern = "/")[[1]][3]
  temp <- read_parquet(file = out[i]) %>%
    mutate(spp_id = spp_id,
           model_id = model_id)
  
  if(i == 1){
    final <- temp
  } else {
    final <- bind_rows(final, temp)
  }
  
}

final1 <- final %>%
  mutate(spp_id = ifelse(spp_id == "yellow","yellow poplar",spp_id)) %>%
  select(model_id, spp_id, global_tree_effect, p2, p3, p5, p6, p7, p8, p9, p10, p11, p12) 

df <- read_csv("./data/processed_data.csv")

spp_df <- read_csv(data) %>%
  select(common_name, species) %>%
  distinct(.)

df1 <- left_join(df, spp_df, by = "common_name") %>%
  group_by(species, common_name, ecoregion_ortho) %>%
  summarize(log_mean_size = log(mean(AG_carbon_m1, na.rm = TRUE)),
            log_mean_ba_gt = log(mean(subp_BA_GT_m1, na.rm = TRUE) + 1),
            mean_Dep_Nhistoric = mean(Dep_Nhistoric, na.rm = TRUE),
            mean_Dep_Shistoric = mean(Dep_Shistoric, na.rm = TRUE),
            c = mean(c, na.rm = TRUE),
            mean_size = mean(AG_carbon_m1, na.rm = TRUE),
            n_trees_ecoregion = n_distinct(tree_ID)) %>%
  mutate(common_name = ifelse(common_name == "yellow-poplar","yellow poplar",common_name)) %>%
  ungroup()

# write csv for Table S2
tab_s2 <- df1 %>%
  select(species, ecoregion_ortho, n_trees_ecoregion)
write.csv(tab_s2, "./data/tab_s2.csv", row.names = FALSE)

# get model predictions for recent N dep changes

common_names <- unique(df1$common_name)

final_pred_shortterm <- NULL

for(i in 1:length(common_names)){
  
  model_data <- df1 %>% filter(common_name == common_names[i])
  model_output <- final1 %>% filter(spp_id == common_names[i],
                              model_id == "ortho_log_t_interaction_adj_priors")
  params <- model_output[sample(nrow(model_output), 1000), ]
  
  for(j in 1:length(model_data$ecoregion_ortho)){
  
  p5_og <- params$p5 - model_data$c[j] * params$p9
  p12_og <- params$p12 - model_data$c[j] * params$p11
  
  pred_baseline_log <- ((params$global_tree_effect + 0*p5_og + 0*params$p6 + 0*params$p7 + 0*params$p8 + model_data$mean_Dep_Nhistoric[j]*params$p9 + model_data$mean_Dep_Shistoric[j]*params$p10 + model_data$mean_Dep_Nhistoric[j]*0*params$p11 + 0*0*params$p12) 
                        + params$p2*model_data$log_mean_size[j]) - params$p3*log(1)
  m2_baseline = exp(pred_baseline_log + model_data$log_mean_size[j])
  pred_baseline = m2_baseline - model_data$mean_size[j]
  
  pred_increase_log <- ((params$global_tree_effect + 1*p5_og + 0*params$p6 + 0*params$p7 + 0*params$p8 + model_data$mean_Dep_Nhistoric[j]*params$p9 + model_data$mean_Dep_Shistoric[j]*params$p10 + model_data$mean_Dep_Nhistoric[j]*1*params$p11 + 1*1*params$p12) 
                    + params$p2*model_data$log_mean_size[j]) - params$p3*log(1)
  m2_increase = exp(pred_increase_log + model_data$log_mean_size[j])
  pred_increase = m2_increase - model_data$mean_size[j]
  
  growth_delta <- pred_increase - pred_baseline
  
  pred <- data.frame(species = model_data$species[1],
                     model_id = model_output$model_id[1],
                     ecoregion = model_data$ecoregion_ortho[j],
                     pred = growth_delta)
  
  if(i == 1 & j == 1){
    final_pred_shortterm <- pred
  } else {
    final_pred_shortterm <- bind_rows(final_pred_shortterm, pred)
  }
  
  }
  
}


# get model predictions for antecedent Ndep changes 

final_pred_longterm <- NULL

for(i in 1:length(common_names)){
  
  model_data <- df1 %>% filter(common_name == common_names[i])
  model_output <- final1 %>% filter(spp_id == common_names[i],
                                    model_id == "ortho_log_t_interaction_adj_priors")
  params <- model_output[sample(nrow(model_output), 1000), ]
  
  for(j in 1:length(model_data$ecoregion_ortho)){
    
    p5_og <- params$p5 - model_data$c[j] * params$p9
    
    pred_baseline_log <- ((params$global_tree_effect + 0*p5_og + 0*params$p6 + 0*params$p7 + 0*params$p8 + model_data$mean_Dep_Nhistoric[j]*params$p9 + model_data$mean_Dep_Shistoric[j]*params$p10 + model_data$mean_Dep_Nhistoric[j]*0*params$p11 + 0*0*params$p12) 
                          + params$p2*model_data$log_mean_size[j]) - params$p3*log(1) 
    m2_baseline = exp(pred_baseline_log + model_data$log_mean_size[j])
    pred_baseline = m2_baseline - model_data$mean_size[j]
    
    pred_increase_log <- ((params$global_tree_effect + 0*p5_og + 0*params$p6 + 0*params$p7 + 0*params$p8 + (model_data$mean_Dep_Nhistoric[j]+1)*params$p9 + model_data$mean_Dep_Shistoric[j]*params$p10 + (model_data$mean_Dep_Nhistoric[j]+1)*0*params$p11 + 0*0*params$p12) 
                          + params$p2*model_data$log_mean_size[j]) - params$p3*log(1)
    m2_increase = exp(pred_increase_log + model_data$log_mean_size[j])
    pred_increase = m2_increase - model_data$mean_size[j]
    
    growth_delta <- pred_increase - pred_baseline
    
    pred <- data.frame(species = model_data$species[1],
                       model_id = model_output$model_id[1],
                       ecoregion = model_data$ecoregion_ortho[j],
                       pred = growth_delta)
    
    if(i == 1 & j == 1){
      final_pred_longterm <- pred
    } else {
      final_pred_longterm <- bind_rows(final_pred_longterm, pred)
    }
    
  }
  
}

# get weighted average of predictions
df2 <- df1 %>%
  group_by(species, common_name) %>%
  summarize(n_trees_total = sum(n_trees_ecoregion))

spp <- unique(df1$species)

for(i in 1:length(spp)){
  
  n_trees_total <- df2 %>%
    filter(species == spp[i]) %>%
    pull(n_trees_total)
  
  n_trees_ecoregion <- df1 %>%
    filter(species == spp[i]) %>%
    select(ecoregion_ortho, n_trees_ecoregion)
  
  tree_ecos <- unique(n_trees_ecoregion$ecoregion_ortho)
 
  for(j in 1:length(n_trees_ecoregion$ecoregion_ortho)){
    
    species_pred_shortterm_eco <- final_pred_shortterm %>%
      filter(species == spp[i] & ecoregion == tree_ecos[j]) %>%
      pull(pred)
    
    species_pred_longterm_eco <- final_pred_longterm %>%
      filter(species == spp[i] & ecoregion == tree_ecos[j]) %>%
      pull(pred)
    
    n_trees_this_eco <- n_trees_ecoregion %>%
      filter(ecoregion_ortho == tree_ecos[j]) %>%
      pull(n_trees_ecoregion)
    
    adj_shortterm_pred <- species_pred_shortterm_eco*n_trees_this_eco / n_trees_total
    adj_longterm_pred <- species_pred_longterm_eco*n_trees_this_eco / n_trees_total
    
    if(j == 1){
      pred_shortterm <- adj_shortterm_pred
      pred_longterm <- adj_longterm_pred
    } else {
      pred_shortterm <- pred_shortterm + adj_shortterm_pred
      pred_longterm <- pred_longterm + adj_longterm_pred
    }
    
  }
  
  pred <- c(pred_shortterm, pred_longterm)
  
  final_pred_species <- data.frame(species = spp[i],
                                   change_type = rep(c("short-term","antecedent"),each = 1000),
                                   pred = pred)
  
  if(i == 1){
    final_pred <- final_pred_species
  } else {
    final_pred <- bind_rows(final_pred, final_pred_species)
  }
  
  
  
}

my_col <- c(RColorBrewer::brewer.pal(3, "Blues")[3],"orange")

mean_growth <- left_join(df, spp_df, by = "common_name") %>%
  group_by(species, common_name) %>%
  summarize(mean_growth = mean(AG_carbon_pYear, na.rm = TRUE)) %>%
  select(species, mean_growth) %>%
  ungroup() %>%
  arrange(species) %>%
  mutate(labels = paste0(letters[seq_len(length(unique(species)))],".")) %>%
  separate_wider_delim(species, delim = " ", names = c("genus","spp"), cols_remove = FALSE) %>%
  mutate(facet_labels = paste0("atop(italic(",labels,"~",genus,"~",spp,"), mean~growth~rate:",round(mean_growth,1),")"))

plot_data <- left_join(final_pred, mean_growth, by = "species") 

  
  p <- ggplot(data = plot_data)+
    geom_density(aes(x = pred, group = change_type, 
                     color = change_type, fill = change_type),
                 alpha = 0.5)+
    facet_wrap(~facet_labels, scales = "free", labeller = label_parsed)+
    theme_bw()+
    scale_color_manual(values = my_col)+
    scale_fill_manual(values = my_col)+
    geom_vline(xintercept = 0)+
    labs(color = "", fill = "", x = expression(paste("change in growth (kg C ", y^-1," ",ind^-1,")")))+
    ggtitle(expression(paste("Marginal effect of N deposition increase (per kg N ", ha^-1," ",y^-1,")")))
  
  ggsave(p,filename = "./visualizations/final_figures/Figure4.png",
         device = "png", height = 6.5, width = 7.5, units = "in")

  mean_pred_responses <- final_pred %>%
    group_by(species, change_type) %>%
    summarize(mean_pred = mean(pred, na.rm = TRUE)) %>%
    left_join(., mean_growth, by = "species") %>%
    select(species, change_type, mean_pred, mean_growth) %>%
    mutate(perc_change = mean_pred / mean_growth * 100) %>%
    arrange(change_type, perc_change) %>%
    mutate(change_group = ifelse(species %in% c("Acer saccharum", "Betula papyrifera", "Picea rubens"), 1,
                                 ifelse(species %in% c("Liriodendron tulipifera", "Populus tremuloides", "Prunus serotina"), 2,
                                        ifelse(species == "Pinus pondersa", 3, 4))))






