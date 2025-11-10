# Marginal effect of N deposition on growth
# Author: Mary Lofton
# Date: 03APR25

# Purpose: figure with marginal effect of N deposition (decrease of 1 kg
# per hectare per year) on growth (kg C per year per individual)

library(scales)
library(bayestestR)

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
  mutate(spp_id = ifelse(spp_id == "yellow","yellow-poplar",spp_id)) %>%
  select(model_id, spp_id, global_tree_effect, p2, p3, p5, p6, p7, p8, p9, p10, p11, p12, .chain, .iteration, .draw) %>%
  rename(common_name = spp_id,
         p1 = global_tree_effect,
         p4 = p5,
         p5 = p6,
         p6 = p7,
         p7 = p8,
         p8 = p9,
         p9 = p10,
         p10 = p11,
         p11 = p12)

spp_df <- read_csv(data) %>%
  select(common_name, species) %>%
  distinct(.)

plot_dat <- left_join(final1, spp_df, by = "common_name") %>%
  pivot_longer(p1:p11, names_to = "params", values_to = "values") %>%
  mutate(params = factor(params, levels = c("p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","p11")))

spp <- unique(plot_dat$species)

for(i in 1:length(spp)){
  
  current_plot_dat <- plot_dat %>%
    filter(species == spp[i])
  
  param_sum_spp <- current_plot_dat %>%
    group_by(species, params) %>%
    summarize(mean_param_value = mean(values, na.rm = TRUE))
  
p1 <- ggplot(current_plot_dat, aes(x = .iteration, y = values, group = as.factor(.chain), color = as.factor(.chain)))+
  geom_line()+
  facet_wrap(facets = vars(params), scales = "free")+
  theme_bw()+
  labs(color = NULL)+
  xlab("iteration")+
  ylab("parameter values")+
  ggtitle(spp[i])
  
  ggsave(p1,filename = paste0("./visualizations/final_figures/traceplots_all/",spp[i],".png"),
         device = "png", bg = "white", height = 7, width = 7.5, units = "in")
  
  if(i == 1){
    param_sum <- param_sum_spp
  } else {
    param_sum <- bind_rows(param_sum, param_sum_spp)
  }
}
  
final_param_sum <- param_sum %>%
  pivot_wider(names_from = "params", values_from = "mean_param_value") %>%
  arrange(species)
write.csv(final_param_sum, "./experiments/ortho_log_t_interaction_adj_priors/posterior_param_estimates.csv",row.names = FALSE)
