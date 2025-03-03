# model_output_explore
# Author: Mary Lofton
# Date: 26FEB25

# load packages
library(tidyverse)
library(lubridate)
library(arrow)
library(ggpubr)

# list files
out <- list.files("./experiments/delta_Ndep",pattern = ".parquet",
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

ggplot(data = final)+
  geom_density(aes(x = p4, group = model_id, color = model_id, fill = model_id),
               alpha = 0.5)+
  theme_classic()

ggplot(data = final)+
  geom_density(aes(x = p5, group = model_id, color = model_id, fill = model_id),
               alpha = 0.5)+
  theme_classic()

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

compare_pred_vs_obs <- pred_vs_obs(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", 
                                    model_output_folder = "./experiments/delta_Ndep")
compare_pred_vs_obs$plot2 

# growth vs size
growth_vs_size(data = "./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", 
            model_output_folder = "./experiments/delta_Ndep")
