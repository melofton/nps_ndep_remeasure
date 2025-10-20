# McDonnell data figure
# Author: Mary Lofton
# Date: 20OCT25

# Purpose: data figure to illustrate:

#'A. map of mean N deposition during measurement period
#'B. timeseries of antecedent N deposition by plot (color by ecoregion?)
#'C. timeseries of short-term differences in N deposition by plot (color by ecoregion?)

# load packages
library(tidyverse)
library(lubridate)
library(sf)
library(viridis)
library(ggthemes)
library(ggpubr)
library(rnaturalearth)

og_df <- read_csv("./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", show_col_types = FALSE) %>%
  dplyr::filter(!common_name %in% c("Douglas-fir","western hemlock")) 
#filter(common_name %in% c("eastern cottonwood"))

df <- read_csv("./data/processed_data.csv")

spp_df <- og_df %>%
  select(species, common_name) %>%
  distinct(.)

#'A. map of mean N deposition during measurement period

usa_coordinates <- map_data("state")
map_data <- left_join(df, spp_df, by = "common_name") %>%
  select(plot_ID, lat, lon, Dep_N) %>%
  distinct(.) %>%
  group_by(plot_ID, lat, lon) %>%
  summarize(Dep_Nmean = mean(Dep_N, na.rm = TRUE))

fig2_a <- ggplot() +
  geom_map(
    data = usa_coordinates, map = usa_coordinates,
    aes(long, lat, map_id = region),
    color = "black", fill = "white")+
  geom_point(
    data = map_data,
    aes(lon, lat, color = Dep_Nmean),
    shape = 16, alpha = 1, size = 1
  ) +
  scale_color_viridis(option = "H")+
  xlab("")+
  ylab("")+
  theme_classic()+
  labs(color = expression(paste("mean N deposition during measurement periods (kg N ", ha^-1," ",y^-1,")")))+
  theme(legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank())

fig2_a

# B. timeseries of antecedent N deposition by plot (color by ecoregion?)

dat_b <- df %>%
  select(plot_ID, date_m1, Dep_N15, ecoregion_ortho) %>%
  distinct(.) 

fig2_b <- ggplot(dat_b, aes(x = date_m1, y = Dep_N15, group = plot_ID, color = ecoregion_ortho)) + 
  geom_line() +
  theme_classic() +
  ylab(expression(paste("antecednet N deposition (kg N ", ha^-1," ",y^-1,")"))) +
  xlab("start date of measurement period")

fig2_b

# C. stacked timeseries of number of measurements per year by species

spp_df <- og_df %>%
  select(common_name, species) %>%
  distinct(.)

dat_c <- left_join(df, spp_df, by = "common_name") %>%
  mutate(year = year(date_m2)) %>%
  group_by(species, year) %>%
  summarize(num_measures = length(unique(tree_ID)))

fig1_c <- ggplot(data = dat_c)+
  geom_area(aes(x = year, y = num_measures, group = species,
                fill = species), color = "black")+
  theme_classic()+
  xlab("")+
  ylab("Number of trees measured")+
  labs(fill = "Species")+
  scale_fill_colorblind()

# Assemble figure

p1 <- ggarrange(fig1_a, 
                ggarrange(fig1_b, fig1_c, ncol = 2, labels = c("(b)","(c)"),
                          hjust = c(0.03, 0.03), widths = c(1, 1.5)), 
                nrow = 2,
                labels = "(a)",
                hjust = c(0.03)
)
p1
ggsave(plot = p1, filename = "./visualizations/final_figures/Figure1.tif",
       device = "tiff", height = 7, width = 10, units = "in", bg = "white")
