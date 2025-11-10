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
library(ggh4x)

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

final_pred_longterm$change_type <- "antecedent"
final_pred_shortterm$change_type <- "short-term"


final_pred <- bind_rows(final_pred_longterm, final_pred_shortterm) 

my_col <- c(RColorBrewer::brewer.pal(3, "Blues")[3],"orange")

sp <- unique(df1$species)

plot_data1 <- final_pred %>%
  filter(!(species == "Betula papyrifera" & ecoregion == "GREAT PLAINS")) %>%
  filter(!(species == "Betula papyrifera" & ecoregion == "NORTH AMERICAN DESERTS")) %>%
  filter(!(species == "Populus deltoides" & ecoregion == "NORTHERN FORESTS")) 

plot_data2 <- plot_data1 %>%
  filter(!species %in% c("Liriodendron tulipifera","Picea rubens","Populus deltoides"))

spp_eco <- plot_data1 %>%
  select(species, ecoregion) %>%
  distinct(.)

spp_eco1 <- plot_data2 %>%
  select(species, ecoregion) %>%
  distinct(.)

facet_label_letters <- paste0(c(letters,"aa","bb","cc"),".")

facet_label_df <- left_join(df, spp_df, by = "common_name") %>%
  select(species, ecoregion_ortho) %>%
  rename(ecoregion = ecoregion_ortho) %>%
  distinct() %>%
  right_join(., spp_eco) %>%
  arrange(species, ecoregion) %>%
  mutate(row = row_number()) %>%
  mutate(labels = facet_label_letters[seq_len(length(unique(row)))]) %>%
  separate_wider_delim(species, delim = " ", names = c("genus","spp"), cols_remove = FALSE) %>%
  separate_wider_delim(ecoregion, delim = "&", names = c("er1","er2"), too_few = "align_start",cols_remove = FALSE) %>%
  mutate(er1 = str_replace_all(er1, " ", "~"),
         er2 = str_replace_all(er2, " ", "~"),
         er1 = str_remove(er1, "~$"),
         er2 = str_remove(er2, "^~"),
         facet_labels = paste0("atop(italic(",labels,"~",genus,"~",spp,"), atop(",er1, ",",er2,"))")) %>%
  select(species, ecoregion, facet_labels) 

nice_labels <- unique(facet_label_df$facet_labels)

facet_label_df1 <- left_join(df, spp_df, by = "common_name") %>%
  select(species, ecoregion_ortho) %>%
  rename(ecoregion = ecoregion_ortho) %>%
  distinct() %>%
  right_join(., spp_eco1) %>%
  arrange(species, ecoregion) %>%
  mutate(row = row_number()) %>%
  mutate(labels = facet_label_letters[seq_len(length(unique(row)))]) %>%
  separate_wider_delim(species, delim = " ", names = c("genus","spp"), cols_remove = FALSE) %>%
  separate_wider_delim(ecoregion, delim = "&", names = c("er1","er2"), too_few = "align_start",cols_remove = FALSE) %>%
  mutate(er1 = str_replace_all(er1, " ", "~"),
         er2 = str_replace_all(er2, " ", "~"),
         er1 = str_remove(er1, "~$"),
         er2 = str_remove(er2, "^~"),
         facet_labels = paste0("atop(italic(",labels,"~",genus,"~",spp,"), atop(",er1, ",",er2,"))")) %>%
  select(species, ecoregion, facet_labels) 

nice_labels1 <- unique(facet_label_df1$facet_labels)

plot_data <- left_join(plot_data1, facet_label_df, by = c("species", "ecoregion") ) %>%
  mutate(facet_labels = factor(facet_labels, levels = nice_labels))

plot_data3 <- left_join(plot_data2, facet_label_df1, by = c("species", "ecoregion") ) %>%
  mutate(facet_labels = factor(facet_labels, levels = nice_labels1))

design1 <- "
 AB##C##
 D###EF#
 G###H##
 I###J##
 #KLM#NO
 PQ#####
 RS#TUVW
 XY##Z##
"

design2 <- "
 AB#C###
 D###EF#
 #GHI#JK
 LM#NOPQ
 RS##T##
"


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
  
  p1 <- ggplot(data = plot_data)+
    geom_density(aes(x = pred, group = change_type, 
                     color = change_type, fill = change_type),
                 alpha = 0.5)+
    facet_manual(~facet_labels, design = design1, scales = "free", labeller = label_parsed)+
    theme_bw()+
    scale_color_manual(values = my_col)+
    scale_fill_manual(values = my_col)+
    geom_vline(xintercept = 0)+
    labs(color = "", fill = "", x = expression(paste("change in growth (kg C ", y^-1," ",ind^-1,")")))+
    ggtitle(expression(paste("Marginal effect of N deposition increase (per kg N ", ha^-1," ",y^-1,")")))
  
  p2 <- ggplot(data = plot_data3)+
    geom_density(aes(x = pred, group = change_type, 
                     color = change_type, fill = change_type),
                 alpha = 0.5)+
    facet_manual(~facet_labels, design = design2, scales = "free", labeller = label_parsed)+
    theme_bw()+
    scale_color_manual(values = my_col)+
    scale_fill_manual(values = my_col)+
    geom_vline(xintercept = 0)+
    labs(color = "", fill = "", x = expression(paste("change in growth (kg C ", y^-1," ",ind^-1,")")))+
    ggtitle(expression(paste("Marginal effect of N deposition increase (per kg N ", ha^-1," ",y^-1,")")))
  
  p3 <- ggplot(data = plot_data3)+
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
  
  ggsave(p,filename = "./visualizations/final_figures/Figure5_new.png",
         device = "png", height = 8, width = 14, units = "in")
  ggsave(p1,filename = "./visualizations/final_figures/Figure5_new1.png",
         device = "png", height = 12, width = 16, units = "in")
  ggsave(p2,filename = "./visualizations/final_figures/Figure5_new2.png",
         device = "png", height = 10, width = 16, units = "in")
  ggsave(p3,filename = "./visualizations/final_figures/Figure5_new3.png",
         device = "png", height = 8, width = 14, units = "in")
p  
  




