# Marginal effect of N deposition on growth
# Author: Mary Lofton
# Date: 03APR25

# Purpose: figure with marginal effect of N deposition (decrease of 1 kg
# per hectare per year) on growth (kg C per year per individual)

library(scales)

data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv"

growth_change_baseline_log <- function(data){
  
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
  select(model_id, spp_id, global_tree_effect, p2, p3, p5, p6, p7, p8, p9, p10, p11, p12) %>%
  group_by(model_id, spp_id) %>%
  summarise(across(global_tree_effect:p12, \(x) mean(x, na.rm = TRUE)))

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

final_pred_df <- NULL

N_ranges <- NULL

for(i in 1:length(common_names)){
  
  model_data <- df1 %>% filter(common_name == common_names[i])
  params <- final1 %>% filter(spp_id == common_names[i],
                              model_id == "ortho_log_t_interaction_adj_priors")
  
  Nrange_dat <- df %>%
    mutate(common_name = ifelse(common_name == "yellow-poplar","yellow poplar",common_name)) %>%
    filter(common_name == common_names[i])
  
  min_Nhistoric <- min(Nrange_dat$Dep_Nhistoric, na.rm = TRUE)
  max_Nhistoric <- max(Nrange_dat$Dep_Nhistoric, na.rm = TRUE)
  min_Ndiff <- min(Nrange_dat$Dep_Ndiff, na.rm = TRUE)
  max_Ndiff <- max(Nrange_dat$Dep_Ndiff, na.rm = TRUE)
  
  ante_Ndep_values = seq(min_Nhistoric, max_Nhistoric, by = 0.1)
  rec_Ndep_values = seq(min_Ndiff, max_Ndiff, by = 0.1)
  
  N_ranges <- data.frame(ante_Ndep = rep(ante_Ndep_values, times = length(rec_Ndep_values)),
                         rec_Ndep = rep(rec_Ndep_values, each = length(ante_Ndep_values)),
                         species = model_data$species[1],
                         spp_id = model_data$common_name[1]) %>%
    mutate(spp_id = ifelse(spp_id == "yellow-poplar","yellow poplar",spp_id)) %>%
    filter(ante_Ndep + rec_Ndep >= 0)
  
  for(j in 1:length(model_data$ecoregion_ortho)){
    
    if(i == 2 & j == 3) next
  
  p5_og <- params$p5 - model_data$c[j] * params$p9
  
  pred_log <- ((params$global_tree_effect + N_ranges$rec_Ndep*p5_og + 0*params$p6 + 0*params$p7 + 0*params$p8 + N_ranges$ante_Ndep*params$p9 + model_data$mean_Dep_Shistoric[j]*params$p10 + N_ranges$ante_Ndep*N_ranges$rec_Ndep*params$p11 + N_ranges$rec_Ndep*N_ranges$rec_Ndep*params$p12) 
                        + params$p2*model_data$log_mean_size[j]) - params$p3*log(1)
  m2 = exp(pred_log + model_data$log_mean_size[j])
  pred = m2 - model_data$mean_size[j]
  
  pred_perc = (pred / model_data$mean_size[j]) * 100
  
  pred_df <- data.frame(species = model_data$species[1],
                     model_id = params$model_id[1],
                     ecoregion = model_data$ecoregion_ortho[j],
                     pred = pred_perc,
                     N_ante = N_ranges$ante_Ndep,
                     N_rec = N_ranges$rec_Ndep)
  
  if(i == 1 & j == 1){
    final_pred_df <- pred_df
  } else {
    final_pred_df <- bind_rows(final_pred_df, pred_df)
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
    
    if(i == 2 & j == 3) next
    
    species_pred_eco <- final_pred_df %>%
      filter(species == spp[i] & ecoregion == tree_ecos[j]) %>%
      pull(pred)
    
    n_trees_this_eco <- n_trees_ecoregion %>%
      filter(ecoregion_ortho == tree_ecos[j]) %>%
      pull(n_trees_ecoregion)
    
    adj_pred <- species_pred_eco*n_trees_this_eco / n_trees_total

    if(j == 1){
      pred <- adj_pred
    } else {
      pred <- pred + adj_pred
    }
    
  }
  
  spp_N_ranges <- final_pred_df %>%
    filter(species == spp[i] & ecoregion == tree_ecos[j])
  
  final_pred_species <- data.frame(species = spp[i],
                                   pred = pred,
                                   N_ante = spp_N_ranges$N_ante,
                                   N_rec = spp_N_ranges$N_rec)
  
  if(i == 1){
    final_pred <- final_pred_species
  } else {
    final_pred <- bind_rows(final_pred, final_pred_species)
  }
  
  
  
}

  
  plots <- NULL
  
  species_list <- unique(final_pred$species)
  
  for(j in 1:length(species_list)){
    
    plot_dat <- final_pred %>%
      filter(species == species_list[j]) 
    
    plots[[j]] <- ggplot(data = plot_dat)+
      geom_tile(aes(x = N_ante, y = N_rec, fill = pred))+
      scale_fill_viridis_c()+
      ggtitle(species_list[j])+
      theme_bw()+
      xlab(expression(paste("antecedent N deposition (kg N ", ha^-1," ",yr^-1,")")))+
      ylab(expression(paste("recent N dep. change  (kg N ", ha^-1," ",yr^-1,")")))+
      labs(fill = "growth (%)")
    
  }
  
  money_plot <- ggarrange(plotlist = plots, ncol = 3, nrow = 3)#, labels = LETTERS[1:length(plots)])

  
  ggsave(money_plot,filename = "./visualizations/final_figures/Figure6.png",
         device = "png", scale = 2, bg = "white")
  
}

