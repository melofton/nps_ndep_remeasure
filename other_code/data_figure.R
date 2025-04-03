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
library(sf)
library(viridis)
library(ggthemes)
library(ggpubr)

og_df <- read_csv("./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", show_col_types = FALSE) %>%
  dplyr::filter(!common_name %in% c("Douglas-fir","western hemlock")) 
#filter(common_name %in% c("eastern cottonwood"))

focal_df <- og_df %>%
  select(species, common_name, plot_ID, tree_ID, interval_no, Dep_N, Dep_Noxi, Dep_Nred, 
         Dep_S, MAT, MAP, date_m2, date_m1, AG_carbon_pYear, AG_carbon_m1, 
         AG_carbon_m2, subp_BA_GT_m1, live_m2, lat, lon) 

baseline_vars <- focal_df %>%
  select(plot_ID, date_m1, date_m2, Dep_N, Dep_Noxi, Dep_Nred, Dep_S, MAT, MAP) %>%
  distinct(.) %>%
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

fig1_a <- ggplot(data = dat_a)+
  geom_area(aes(x = year, y = num_measures, group = species,
                fill = species), color = "black")+
  theme_classic()+
  xlab("")+
  ylab("Number of trees measured")+
  labs(fill = "Species")+
  scale_fill_colorblind()+
  theme(legend.position = "none")

# B. number of remeasurements by species

fig1_b <- ggplot(data = df)+
  geom_bar(aes(x = num_intervals, group = species, fill = species),
           color = "black", position = "dodge")+
  theme_classic()+
  xlab("Number of measurement intervals")+
  ylab("Number of trees")+
  labs(fill = "Species")+
  scale_fill_colorblind()

# C. mean N deposition map

usa_coordinates <- map_data("state")
map_data <- df %>%
  select(plot_ID, lat, lon, Dep_Nbaseline, Dep_Sbaseline,
         MAT_baseline, MAP_baseline, Dep_Noxibaseline, Dep_Nredbaseline) %>%
  mutate(MAP_baseline_dm = 0.01*MAP_baseline) %>%
  distinct(.)

fig1_c <- ggplot() +
  geom_map(
    data = usa_coordinates, map = usa_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill = "white")+
  geom_point(
    data = map_data,
    aes(lon, lat, color = Dep_Nbaseline),
    shape = 16, alpha = 1, size = 1
  ) +
  scale_color_viridis(option = "B")+
  xlab("")+
  ylab("")+
  theme_classic()+
  labs(color = expression(paste("mean N deposition (kg N ", ha^-1," ",y^-1,")")))+
  theme(legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank())

fig1_supp1a <- ggplot() +
  geom_map(
    data = usa_coordinates, map = usa_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill = "white")+
  geom_point(
    data = map_data,
    aes(lon, lat, color = Dep_Sbaseline),
    shape = 16, alpha = 1, size = 1
  ) +
  scale_color_viridis(option = "B")+
  xlab("")+
  ylab("")+
  theme_classic()+
  labs(color = expression(paste("mean S deposition (kg S ", ha^-1," ",y^-1,")")))+
  theme(legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank())+
  guides(color = guide_colorbar(title.position = "top", title.hjust=0.5))

fig1_supp1b <- ggplot() +
  geom_map(
    data = usa_coordinates, map = usa_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill = "white")+
  geom_point(
    data = map_data,
    aes(lon, lat, color = MAT_baseline),
    shape = 16, alpha = 1, size = 1
  ) +
  scale_color_viridis(option = "B")+
  xlab("")+
  ylab("")+
  theme_classic()+
  labs(color = expression(paste("mean temperature (°C)")))+
  theme(legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank())+
  guides(color = guide_colorbar(title.position = "top", title.hjust=0.5))

fig1_supp1c <- ggplot() +
  geom_map(
    data = usa_coordinates, map = usa_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill = "white")+
  geom_point(
    data = map_data,
    aes(lon, lat, color = MAP_baseline_dm),
    shape = 16, alpha = 1, size = 1
  ) +
  scale_color_viridis(option = "B")+
  xlab("")+
  ylab("")+
  theme_classic()+
  labs(color = expression(paste("mean precipitation (dm ", y^-1,")")))+
  theme(legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank())+
  guides(color = guide_colorbar(title.position = "top", title.hjust=0.5))

fig1_supp1d <- ggplot() +
  geom_map(
    data = usa_coordinates, map = usa_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill = "white")+
  geom_point(
    data = map_data,
    aes(lon, lat, color = Dep_Noxibaseline),
    shape = 16, alpha = 1, size = 1
  ) +
  scale_color_viridis(option = "B")+
  xlab("")+
  ylab("")+
  theme_classic()+
  labs(color = expression(paste("mean oxidized N deposition (kg N ", ha^-1," ",y^-1,")")))+
  theme(legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank())+
  guides(color = guide_colorbar(title.position = "top", title.hjust=0.5))

fig1_supp1e <- ggplot() +
  geom_map(
    data = usa_coordinates, map = usa_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill = "white")+
  geom_point(
    data = map_data,
    aes(lon, lat, color = Dep_Nredbaseline),
    shape = 16, alpha = 1, size = 1
  ) +
  scale_color_viridis(option = "B")+
  xlab("")+
  ylab("")+
  theme_classic()+
  labs(color = expression(paste("mean reduced N deposition (kg N ", ha^-1," ",y^-1,")")))+
  theme(legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank())+
  guides(color = guide_colorbar(title.position = "top", title.hjust=0.5))


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


fig1_d <- ggplot(data = df, aes(x = date_m2, y = Dep_Ndelta, group = plot_ID))+
  geom_line(color = "gray")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_abline(slope = slope, intercept = intercept, color = "blue", linewidth = 1)+
  theme_bw()+
  xlab("")+
  ylab(expression(paste("N deposition deviation (kg N ", ha^-1," ",y^-1,")")))

mod <- lm(Dep_Sdelta ~ date_m2, data = df)
intercept = mod$coefficients[1]
slope = mod$coefficients[2]

fig1_supp2a <- ggplot(data = df, aes(x = date_m2, y = Dep_Sdelta, group = plot_ID))+
  geom_line(color = "gray")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_abline(slope = slope, intercept = intercept, color = "blue", linewidth = 1)+
  theme_bw()+
  xlab("")+
  ylab(expression(paste("S deposition deviation (kg S ", ha^-1," ",y^-1,")")))

mod <- lm(MAT_delta ~ date_m2, data = df)
intercept = mod$coefficients[1]
slope = mod$coefficients[2]

fig1_supp2b <- ggplot(data = df, aes(x = date_m2, y = MAT_delta, group = plot_ID))+
  geom_line(color = "gray")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_abline(slope = slope, intercept = intercept, color = "blue", linewidth = 1)+
  theme_bw()+
  xlab("")+
  ylab(expression(paste("temperature deviation (°C)")))

mod <- lm(MAP_delta_dm ~ date_m2, data = df)
intercept = mod$coefficients[1]
slope = mod$coefficients[2]

fig1_supp2c <- ggplot(data = df, aes(x = date_m2, y = MAP_delta_dm, group = plot_ID))+
  geom_line(color = "gray")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_abline(slope = slope, intercept = intercept, color = "blue", linewidth = 1)+
  theme_bw()+
  xlab("")+
  ylab(expression(paste("precipitation deviation (dm ", y^-1,")")))

mod <- lm(Dep_Noxidelta ~ date_m2, data = df)
intercept = mod$coefficients[1]
slope = mod$coefficients[2]

fig1_supp2d <- ggplot(data = df, aes(x = date_m2, y = Dep_Noxidelta, group = plot_ID))+
  geom_line(color = "gray")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_abline(slope = slope, intercept = intercept, color = "blue", linewidth = 1)+
  theme_bw()+
  xlab("")+
  ylab(expression(paste("oxidized N deviation (kg N ", ha^-1," ",y^-1,")")))

mod <- lm(Dep_Nreddelta ~ date_m2, data = df)
intercept = mod$coefficients[1]
slope = mod$coefficients[2]

fig1_supp2e <- ggplot(data = df, aes(x = date_m2, y = Dep_Nreddelta, group = plot_ID))+
  geom_line(color = "gray")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_abline(slope = slope, intercept = intercept, color = "blue", linewidth = 1)+
  theme_bw()+
  xlab("")+
  ylab(expression(paste("reduced N deviation (kg N ", ha^-1," ",y^-1,")")))



p1 <- ggarrange(fig1_a, fig1_b, fig1_c, fig1_d,
                nrow = 2, ncol = 2,
                labels = c("A","B","C","D")
)
p1
ggsave(plot = p1, filename = "./visualizations/final_figures/Figure1.tif",
       device = "tiff", height = 7, width = 10, units = "in")

p1_supp1 <- ggarrange(fig1_supp1a, fig1_supp1b, fig1_supp1c, fig1_supp1d, fig1_supp1e, 
                nrow = 3, ncol = 2,
                labels = c("A","B","C","D","E")
)
p1_supp1
ggsave(plot = p1_supp1, filename = "./visualizations/final_figures/Figure1_supp1.tif",
       device = "tiff", height = 10, width = 8, units = "in",bg = "white")

p1_supp2 <- ggarrange(fig1_supp2a, fig1_supp2b, fig1_supp2c, fig1_supp2d, fig1_supp2e, 
                      nrow = 3, ncol = 2,
                      labels = c("A","B","C","D","E")
)
p1_supp2
ggsave(plot = p1_supp2, filename = "./visualizations/final_figures/Figure1_supp2.tif",
       device = "tiff", height = 10, width = 10, units = "in",bg = "white")
