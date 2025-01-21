# Explore data to inform model structure
# Author: Mary Lofton
# Date: 20JAN25

# Purpose: determine how many trees there are per plot and whether species
# are experiencing increases and decreases in N dep

# load packages
library(tidyverse)

# load data
df <- read_csv("./McDonnell_etal_InPrep_TreeData_2024_10_11.csv", show_col_types = FALSE)
colnames(df)
dict <- read_csv("./")

# how many trees per plot for each species?
unique(df$common_name)
length(unique(df$plot_ID))
df1 <- df %>%
  group_by(common_name, plot_ID) %>%
  summarize(num_per_plot = n_distinct(tree_ID)) 

num_per_plot_fig <- df1 %>%
  ggplot(aes(x = num_per_plot, group = common_name)) +
  geom_histogram() +
  facet_wrap(facets = vars(common_name)) + 
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
  facet_wrap(facets = vars(common_name)) + 
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
  mutate(inc_1_2 = ifelse(interval_2 - interval_1 >= 0,"yes","no"),
         inc_2_3 = ifelse(interval_3 - interval_2 >= 0,"yes","no"),
         inc_3_4 = ifelse(interval_4 - interval_3 >= 0,"yes","no")) %>%
  select(-interval_1, -interval_2, -interval_3, -interval_4) %>%
  pivot_longer(inc_1_2:inc_3_4, names_to = "interval_span",
               values_to = "inc_observed") %>%
  filter(!is.na(inc_observed)) %>%
  group_by(tree_ID) %>%
  mutate(Ndep_pattern = ifelse("yes" %in% inc_observed & "no" %in% inc_observed,"both",
                               ifelse("yes" %in% inc_observed,"increase",
                                      ifelse("no" %in% inc_observed, "decrease",NA)))) %>%
  select(-inc_observed, -interval_span) %>%
  distinct()

inc_dec_fig <- inc_dec %>%
  ggplot(aes(x = Ndep_pattern, group = common_name)) +
  geom_bar(stat = "count") +
  facet_wrap(facets = vars(common_name)) + 
  theme_bw()
inc_dec_fig
