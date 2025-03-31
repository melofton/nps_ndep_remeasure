# McDonnell data figure
# Author: Mary Lofton
# Date: 31MAR25

# Purpose: data figure to illustrate:

#'A. length and size of McDonnell timeseries
#'B. number of remeasurements per species
#'C. mean N deposition over measurement range across span of dataset
#'D. interannual variability in N dep across plots in dataset


# load packages
library(tidyverse)
library(lubridate)

og_df <- read_csv("./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", show_col_types = FALSE) %>%
  dplyr::filter(!common_name %in% c("Douglas-fir","western hemlock")) 
#filter(common_name %in% c("eastern cottonwood"))

focal_df <- og_df %>%
  select(species, common_name, plot_ID, tree_ID, interval_no, Dep_N, Dep_Noxi, Dep_Nred, 
         Dep_S, MAT, MAP, date_m2, date_m1, AG_carbon_pYear, AG_carbon_m1, 
         AG_carbon_m2, subp_BA_GT_m1, live_m2) 

baseline_vars <- focal_df %>%
  group_by(plot_ID) %>%
  summarize(Dep_Nbaseline = mean(Dep_N, na.rm = TRUE),
            Dep_Noxibaseline = mean(Dep_Noxi, na.rm = TRUE),
            Dep_Nredbaseline = mean(Dep_Nred, na.rm = TRUE),
            Dep_Sbaseline = mean(Dep_S, na.rm = TRUE),
            MAT_baseline = mean(MAT, na.rm = TRUE),
            MAP_baseline = mean(MAP, na.rm = TRUE)) %>%
  ungroup()

focal_df2 <- left_join(focal_df, baseline_vars, by = "plot_ID") %>%
  group_by(plot_ID) %>%
  mutate(Dep_Ndelta = Dep_N - Dep_Nbaseline,
         Dep_Noxidelta = Dep_Noxi - Dep_Noxibaseline,
         Dep_Nreddelta = Dep_Nred - Dep_Nredbaseline,
         Dep_Sdelta = Dep_S - Dep_Sbaseline,
         MAP_delta_dm = (MAP - MAP_baseline) * 0.01,
         MAT_delta = MAT - MAT_baseline) %>%
  ungroup() 

live_tree_ids <- focal_df2 |> 
  summarise(count = n(),
            sum = sum(live_m2), .by = tree_ID) |> 
  dplyr::filter(count >= sum) |> 
  pull(tree_ID)

focal_df3 <- focal_df2 |> 
  dplyr::filter(tree_ID %in% live_tree_ids) |> 
  group_by(tree_ID) |> 
  tidyr::fill(subp_BA_GT_m1, .direction = "down") |> 
  ungroup() 

df <- focal_df3 %>%
  dplyr::filter(complete.cases(.)) %>%
  group_by(tree_ID) %>%
  mutate(num_intervals = max(interval_no, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::filter(num_intervals >= 2)

species_guide <- df %>%
  select(species, common_name) %>%
  distinct(.)

# A. stacked timeseries of number of measurements per year by species

dat_a <- df %>%
  mutate(year = year(date_m2)) %>%
  group_by(species, year) %>%
  summarize(num_measures = length(unique(tree_ID)))

ggplot(data = dat_a)+
  geom_area(aes(x = year, y = num_measures, group = species,
                fill = species), color = "black")+
  theme_classic()+
  xlab("")+
  ylab("Number of trees measured (end of interval)")+
  labs(fill = "Species")

# B. number of remeasurements by species

ggplot(data = df)+
  geom_bar(aes(x = num_intervals, group = species, fill = species),
           color = "black", position = "dodge")+
  theme_classic()+
  xlab("Number of measurement intervals")+
  ylab("Number of trees")+
  labs(fill = "Species")

# C. mean N deposition map

# D. interannual variability in N deposition over time by plot

mod <- lm(Dep_Ndelta ~ date_m2, data = df)
intercept = mod$coefficients[1]
slope = mod$coefficients[2]
summary(mod)$r.squared #0.41
summary(mod)$coefficients #both 0 

# function for p-value of model overall
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
lmp(mod) # 0


ggplot(data = df, aes(x = date_m2, y = Dep_Ndelta, group = plot_ID))+
  geom_line(color = "gray")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_abline(slope = slope, intercept = intercept, color = "blue", linewidth = 1)+
  theme_bw()+
  xlab("")+
  ylab(expression(paste("Interval deviation from mean N deposition (kg N ", ha^-1," ",y^-1,")")))
