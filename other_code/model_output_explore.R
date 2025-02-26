# model_output_explore
# Author: Mary Lofton
# Date: 26FEB25

# load packages
library(tidyverse)
library(lubridate)
library(arrow)

# list files
out <- list.files("./experiments/linear_model",pattern = ".parquet",
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
