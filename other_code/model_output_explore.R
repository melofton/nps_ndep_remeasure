# model_output_explore
# Author: Mary Lofton
# Date: 26FEB25

# load packages
library(tidyverse)
library(lubridate)
library(arrow)
library(ggpubr)

# list files
out <- list.files("./experiments/delta_Ndep_only",pattern = ".parquet",
                  full.names = TRUE)

for(i in 1:length(out)){
  
  spp_name = str_split(out[i], pattern = "-")[[1]][2]
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

final <- final %>%
  mutate(spp_id = ifelse(spp_id == "yellow","yellow poplar",spp_id))

ggplot(data = final)+
  geom_density(aes(x = p5, group = model_id, color = model_id, fill = model_id),
               alpha = 0.5)+
  theme_classic()+
  facet_wrap(facets = vars(spp_id), scales = "free")+
  ggtitle("")

Ndep_param_sum <- final %>%
  select(model_id, p4, p5) %>%
  pivot_longer(p4:p5,names_to = "param", values_to = "param_value") %>%
  group_by(model_id, param) %>%
  summarize(mean_param_value = mean(param_value, na.rm = TRUE)) %>%
  pivot_wider(names_from = "param", values_from = "mean_param_value") %>%
  ggplot(aes(x = p4, y = p5, group = model_id, color = model_id))+
  geom_point(size = 2)+
  theme_bw()+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)
Ndep_param_sum

# compare obs vs pred
source("./other_code/pred_vs_obs.R")
compare_pred_vs_obs <- pred_vs_obs(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", 
                                    model_output_folder = "./experiments/new_delta_Ndep_only")
compare_pred_vs_obs$plot2 
compare_pred_vs_obs$plot1
rsq <- function(pred, obs){
  1 - (sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE))
}

r2 <- rsq(pred = compare_pred_vs_obs$df$pred, obs = compare_pred_vs_obs$df$AG_carbon_pYear)

# growth vs size
growth_vs_size(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", 
            model_output_folder = "./experiments/delta_Ndep")

# growth vs Ndep
growth_vs_Ndep(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", 
               model_output_folder = "./experiments/delta_Ndep")

# delta growth vs delta Ndep
delta_growth_vs_delta_Ndep(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", 
               model_output_folder = "./experiments/delta_Ndep",
               experiment_name = "delta_Ndep_only")

# checking param distributions with Ndep only vs all env variables vs
# separating N into different species

# list files
out1 <- list.files("./experiments/new_delta_Ndep_only_saveTreeEffect",pattern = "mcmc.parquet",
                  full.names = TRUE)
out2 <- list.files("./experiments/delta_env_saveTreeEffect",pattern = "mcmc.parquet",
                   full.names = TRUE)
out3 <- list.files("./experiments/N_species_saveTreeEffect",pattern = "mcmc.parquet",
                   full.names = TRUE)
out <- c(out1,out2, out3)

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
  select(model_id, spp_id, p2, p3, p4, p5, p6, p7, p8) %>%
  pivot_longer(p2:p8, names_to = "parameter_name", values_to = "parameter_value") %>%
  mutate(parameter_name = ifelse(model_id %in% c("new_delta_Ndep_only_saveTreeEffect") & parameter_name == "p5","delta_Ndep_only",
                                 ifelse(model_id == "N_species_saveTreeEffect" & parameter_name == "p4","delta_Ndep_oxi",
                                        ifelse(model_id == "N_species_saveTreeEffect" & parameter_name == "p5","delta_Ndep_red",
                                               ifelse(model_id == "delta_env_saveTreeEffect" & parameter_name == "p5","delta_Ndep_env",parameter_name))))) %>%
  filter(parameter_name %in% c("delta_Ndep_only","delta_Ndep_oxi","delta_Ndep_red", "delta_Ndep_env"))

ggplot(data = final1)+
  geom_density(aes(x = parameter_value, group = parameter_name, 
                   color = parameter_name, fill = parameter_name),
               alpha = 0.5)+
  facet_wrap(facets = vars(spp_id), scales = "free")+
  theme_bw()

final2 <- final1 %>%
  filter(parameter_name %in% c("delta_Ndep_only","delta_Ndep_env"))

ggplot(data = final2)+
  geom_density(aes(x = parameter_value, group = parameter_name, 
                   color = parameter_name, fill = parameter_name),
               alpha = 0.5)+
  facet_wrap(facets = vars(spp_id), scales = "free")+
  theme_bw()

final3 <- final1 %>%
  filter(parameter_name %in% c("delta_Ndep_oxi","delta_Ndep_red","delta_Ndep_env"))

ggplot(data = final3)+
  geom_density(aes(x = parameter_value, group = parameter_name, 
                   color = parameter_name, fill = parameter_name),
               alpha = 0.5)+
  facet_wrap(facets = vars(spp_id), scales = "free")+
  theme_bw()

ggplot(data = final)+
  geom_density(aes(x = p4, group = model_id, color = model_id, fill = model_id),
               alpha = 0.5)+
  theme_classic()

ggplot(data = final)+
  geom_density(aes(x = p5, group = model_id, color = model_id, fill = model_id),
               alpha = 0.5)+
  theme_classic()+
  ggtitle("Delta Ndep only")+
  xlim(c(-0.12, 0.12))
