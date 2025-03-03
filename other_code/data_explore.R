# Explore data to inform model structure
# Author: Mary Lofton
# Date: 20JAN25

# Purpose: determine how many trees there are per plot and whether species
# are experiencing increases and decreases in N dep

# load packages
library(tidyverse)

# load data
df <- read_csv("./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", show_col_types = FALSE)
colnames(df)

# how many trees per plot for each species?
unique(df$common_name)
length(unique(df$plot_ID))
df1 <- df %>%
  group_by(common_name, plot_ID) %>%
  summarize(num_per_plot = n_distinct(tree_ID)) 

num_per_plot_fig <- df1 %>%
  ggplot(aes(x = num_per_plot, group = common_name)) +
  geom_histogram() +
  facet_wrap(facets = vars(common_name), scales = "free_y") + 
  theme_bw()
num_per_plot_fig

median_per_plot <- df1 %>%
  ungroup() %>%
  group_by(common_name) %>%
  summarize(med_per_plot = median(num_per_plot, na.rm = TRUE))

num_plots_species <- df1 %>%
  ungroup() %>%
  group_by(common_name) %>%
  summarize(total_plots = sum(n_distinct(plot_ID)))

num_plots_multiple <- df1 %>%
  ungroup() %>%
  filter(!num_per_plot == 1) %>%
  group_by(common_name) %>%
  summarize(num_plots_multiple = sum(n_distinct(plot_ID)))

prop_plots_multiple <- left_join(num_plots_species, num_plots_multiple,
                                 by = "common_name") %>%
  mutate(prop_plots_multiple = num_plots_multiple / total_plots)

# how many trees per species experience both increases and decreases in Ndep?
colnames(df)

df2 <- df %>%
  select(common_name, tree_ID, interval_no, Dep_N) %>%
  group_by(tree_ID) %>%
  mutate(num_intervals = max(interval_no, na.rm = TRUE))

num_intervals_fig <- df2 %>%
  ggplot(aes(x = num_intervals, group = common_name)) +
  geom_histogram() +
  facet_wrap(facets = vars(common_name), scales = "free_y") + 
  theme_bw()
num_intervals_fig

inc_dec <- df2 %>%
  filter(num_intervals > 1) %>%
  select(-num_intervals) %>%
  pivot_wider(names_from = interval_no, values_from = Dep_N) %>%
  rename(interval_1 = `1`,
         interval_2 = `2`,
         interval_3 = `3`,
         interval_4 = `4`) %>%
  mutate(inc_1_2 = interval_2 - interval_1,
         inc_2_3 = interval_3 - interval_2,
         inc_3_4 = interval_4 - interval_3) %>%
  select(-interval_1, -interval_2, -interval_3, -interval_4) %>%
  pivot_longer(inc_1_2:inc_3_4, names_to = "interval_span",
               values_to = "delta_ndep") %>%
  filter(!is.na(delta_ndep)) %>%
  ungroup()

min <- inc_dec %>% filter(delta_ndep == min(delta_ndep))
max <- inc_dec %>% filter(delta_ndep == max(delta_ndep))

inc_dec_fig <- inc_dec %>%
  ggplot(aes(x = delta_ndep, group = common_name)) +
  geom_density(color = "black", fill = "white") +
  geom_vline(xintercept = 0, color = "blue") +
  xlim(c(-5,5)) +
  facet_wrap(facets = vars(common_name), scales = "free_y") + 
  theme_bw()
inc_dec_fig

baseline_Ndep <- df %>%
  select(common_name, tree_ID, interval_no, Dep_N15, Dep_N, date_m2, date_m1) %>%
  filter(interval_no == 1) %>%
  mutate(dt = as.numeric(date_m2 - date_m1)/365) %>%
  mutate(total_years = 15 + dt) %>%
  mutate(Dep_Nbaseline = (Dep_N15*total_years - Dep_N*dt) / 15) %>%
  select(tree_ID, Dep_Nbaseline)
hist(baseline_Ndep$Dep_Nbaseline)

delta_Ndep <- df %>%
  select(common_name, tree_ID, interval_no, Dep_N) %>%
  left_join(baseline_Ndep, by = "tree_ID") %>%
  mutate(Dep_Ndelta = Dep_N - Dep_Nbaseline) %>%
  filter(!common_name %in% c("Douglas-fir","western hemlock")) 

plot_delta_Ndep <- ggplot(data = delta_Ndep, aes(x = Dep_Ndelta,
                                                 group = interval_no, fill = as.factor(interval_no)))+
  geom_density()+
  facet_wrap(facets = vars(common_name), scales = "free_y")+
  xlim(c(-15,15))+
  geom_vline(xintercept = 0)+
  theme_bw()
plot_delta_Ndep  

check_intervals <- df %>%
  select(interval_no, date_m1, date_m2) %>%
  pivot_longer(date_m1:date_m2, names_to = "m1_m2", values_to = "datetime")

plot_interval_times <- ggplot(data = check_intervals, 
                              aes(x = datetime, group = m1_m2,
                                  fill = m1_m2))+
  geom_histogram()+
  facet_wrap(facets = vars(interval_no))+
  theme_bw()
plot_interval_times

int4_trees <- df %>%
  group_by(common_name, tree_ID) %>%
  summarise(count = n()) %>%
  filter(count == 4) %>%
  group_by(common_name) %>%
  summarise(num_int4_trees = n())


