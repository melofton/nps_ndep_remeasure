# McDonnell data figure
# Author: Mary Lofton
# Date: 20OCT25

# Purpose: data figure to illustrate:

#'A. map of mean N deposition during measurement period (including 15 yr prior to 1st measure)
#'B. timeseries of short-term differences in N deposition by plot and ecoregion

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

#'A. map of mean N deposition during measurement period (including 15 yr prior to 1st measure)

usa_coordinates <- map_data("state")
map_data_ante <- left_join(df, spp_df, by = "common_name") %>%
  select(plot_ID, lat, lon, Dep_N15, Dep_N, date_m1, date_m2) %>%
  distinct(.) %>%
  group_by(plot_ID, lat, lon) %>%
  dplyr::filter(date_m1 == min(date_m1, na.rm = TRUE)) %>%
  mutate(dt = as.numeric(date_m2 - date_m1)/365) %>%
  mutate(total_years = 15 + dt) %>%
  mutate(Dep_Nante = (Dep_N15*total_years - Dep_N*dt)) %>%
  select(plot_ID, lat, lon, Dep_Nante) %>%
  distinct(.)

map_data <- left_join(df, spp_df, by = "common_name") %>%
  select(plot_ID, lat, lon, Dep_N, date_m1, date_m2) %>%
  distinct(.) %>%
  mutate(dt = as.numeric(date_m2 - date_m1)/365,
         Dep_Ncum = Dep_N*dt) %>%
  group_by(plot_ID, lat, lon) %>%
  summarize(Dep_Ncumall = sum(Dep_Ncum),
            dt_all = sum(dt)) %>%
  left_join(map_data_ante, by = c("plot_ID","lat","lon")) %>%
  mutate(Dep_Nmean = (Dep_Nante + Dep_Ncumall) / (dt_all + 15)) %>%
  select(plot_ID, lat, lon, Dep_Nmean)

plot <- df %>%
  filter(level1_ecoregion == "EASTERN TEMPERATE FORESTS" & num_intervals == 4) %>%
  select(plot_ID, Dep_Nhistoric, Dep_N, date_m1, date_m2, interval_no, lat, lon) %>%
  distinct() %>%
  filter(Dep_Nhistoric > 20 & interval_no == 1 & date_m1 == "2000-05-10") %>%
  pull(plot_ID)

plot_data <- df %>%
  select(plot_ID, Dep_Nhistoric, Dep_N, date_m1, date_m2, interval_no, lat, lon) %>%
  filter(plot_ID == plot) %>%
  distinct()

segment_df <- plot_data[1,] %>%
  mutate(lat_start = lat - 5, lon_start = lon + 5, lon = lon + 0.35, lat = lat - 0.35)
  
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
  geom_point(
    data = plot_data,
    aes(lon, lat),
    shape = 0, size = 2, stroke = 2
  ) +
  geom_segment(
    data = segment_df,
    aes(x = lon_start, y = lat_start, xend = lon, yend = lat),
    arrow = arrow(length = unit(0.2,"cm"))
  ) +
  annotate("text", x = segment_df$lon_start, y = segment_df$lat_start - 1.2, label = "plot location \nused in (b)") +
  scale_color_viridis(option = "H")+
  xlab("")+
  ylab("")+
  theme_classic()+
  labs(color = expression(paste("mean N deposition (kg N ", ha^-1," ",y^-1,")")))+
  guides(color = guide_colorbar(title.position = "bottom", title.hjust = 0.0))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.background = element_rect(fill = "transparent"))+
  theme(legend.position = "inside", legend.position.inside =  c(0.2, 0.15),
        legend.direction = "horizontal", plot.margin = unit(c(0.1,0,0.1,0), "pt"))+
  ylim(c(min(usa_coordinates$lat),max(usa_coordinates$lat)))


fig2_a

# B. conceptual figure of how antecedent and recent N dep are calculated for example plot

plot_data2 <- plot_data %>%
  mutate(datetime_ante_finish = date_m1) %>%
  pivot_longer(date_m1:date_m2, names_to = "measurement_time", values_to = "datetime") %>%
  pivot_longer(Dep_Nhistoric:Dep_N, names_to = "dep_type", values_to = "Dep_N") %>%
  mutate(interval_no_by_type = paste(dep_type, interval_no, sep = "_"),
         datetime_ante_start = datetime %m-% years(15),
         datetime = ifelse((measurement_time == "date_m1" & dep_type == "Dep_Nhistoric"), datetime_ante_start,
                           ifelse((measurement_time == "date_m2" & dep_type == "Dep_Nhistoric"), datetime_ante_finish, datetime)),
         datetime = as.Date(datetime, origin = "1970-01-01"))

interval_starts <- plot_data2 %>%
  filter(measurement_time == "date_m1" & dep_type == "Dep_N") %>%
  select(datetime, measurement_time)

diffs <- plot_data %>%
  select(date_m1, Dep_Nhistoric, Dep_N, interval_no) %>%
  mutate(linewidth_var = "Dep_Ndiff")

fig2_b <- ggplot()+
  geom_vline(data = interval_starts, aes(xintercept = datetime, linetype = measurement_time, alpha = measurement_time), color = "gray")+
  geom_line(data = plot_data2, aes(x = datetime, y = Dep_N, group = interval_no_by_type, color = as.factor(interval_no), alpha = dep_type, linetype = dep_type), linewidth = 1)+
  geom_point(data = plot_data2, aes(x = datetime, y = Dep_N, group = interval_no_by_type, color = as.factor(interval_no)))+
  geom_segment(data = diffs, aes(x = date_m1, y = Dep_Nhistoric, xend = date_m1, yend = Dep_N, color = as.factor(interval_no), linewidth = linewidth_var), arrow = arrow(length = unit(0.2,"cm")))+
  theme_classic()+
  scale_color_discrete(name = "Measurement interval")+
  scale_alpha_manual(values = c(date_m1 = 1, Dep_N = 0.5, Dep_Nhistoric = 1), name = NULL, labels = c("measurement \ninterval start date","current N dep.","antecedent N dep."))+
  scale_linetype_manual(values = c(date_m1 = 2, Dep_N = 2, Dep_Nhistoric = 1), name = NULL, labels = c("measurement \ninterval start date","current N dep.","antecedent N dep."))+
  scale_linewidth_manual(values = c("Dep_Ndiff" = 0.5), name = NULL, labels = c("short-term change in N dep."))+
  xlab("")+
  ylab(expression(paste("N deposition (kg N ", ha^-1," ",y^-1,")")))+
  theme(legend.spacing.y = unit(0.0, "cm"),
        legend.key.width = unit(3, "line"),
        legend.key.height = unit(1.5, "line"))+
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(override.aes = list(
      linetype = c(2, 2, 1), # Example values
      alpha = c(1, 0.5, 1) # Example values
    ), reverse = TRUE),
    alpha = guide_legend(override.aes = list(
      linetype = c(1, 2, 2), # Example values
      alpha = c(1, 0.5, 1) # Example values
    ), reverse = TRUE),
    linewidth = guide_legend(order = 4)
  )

fig2_b

ggsave(plot = fig2_b, filename = "./visualizations/final_figures/Figure2b.tif",
       device = "tiff", height = 4, width = 7, units = "in", bg = "white")

# Assemble figure

p2 <- ggarrange(fig2_a, fig2_b,
                nrow = 2, ncol = 1,
                labels = c("(a)","(b)")
)
p2
ggsave(plot = p2, filename = "./visualizations/final_figures/Figure2.tif",
       device = "tiff", height = 8, width = 7.5, units = "in", bg = "white")

# Additional figure versions for CLAD presentation

fig2_b_1 <- ggplot()+
  geom_vline(data = interval_starts, aes(xintercept = datetime, linetype = measurement_time, alpha = measurement_time), color = "gray")+
  geom_line(data = subset(plot_data2, dep_type == "Dep_Nhistoric" & interval_no == 1), aes(x = datetime, y = Dep_N, group = interval_no_by_type, color = as.factor(interval_no), alpha = dep_type, linetype = dep_type), linewidth = 1)+
  geom_point(data = subset(plot_data2, dep_type == "Dep_Nhistoric" & interval_no == 1), aes(x = datetime, y = Dep_N, group = interval_no_by_type, color = as.factor(interval_no)))+
  #geom_segment(data = diffs, aes(x = date_m1, y = Dep_Nhistoric, xend = date_m1, yend = Dep_N, color = as.factor(interval_no), linewidth = linewidth_var), arrow = arrow(length = unit(0.2,"cm")))+
  theme_classic()+
  scale_color_discrete(name = "Measurement interval")+
  scale_alpha_manual(values = c(date_m1 = 1, Dep_Nhistoric = 1), name = NULL, labels = c("measurement \ninterval start date","antecedent N dep."))+
  scale_linetype_manual(values = c(date_m1 = 2, Dep_Nhistoric = 1), name = NULL, labels = c("measurement \ninterval start date","antecedent N dep."))+
  scale_linewidth_manual(values = c("Dep_Ndiff" = 0.5), name = NULL, labels = c("short-term change in N dep."))+
  xlab("")+
  ylab(expression(paste("N deposition (kg N ", ha^-1," ",y^-1,")")))+
  theme(legend.spacing.y = unit(0.0, "cm"),
        legend.key.width = unit(3, "line"),
        legend.key.height = unit(1.5, "line"))+
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(override.aes = list(
      linetype = c(2, 1), # Example values
      alpha = c(1, 1) # Example values
    ), reverse = TRUE),
    alpha = guide_legend(override.aes = list(
      linetype = c(1, 2), # Example values
      alpha = c(1, 1) # Example values
    ), reverse = TRUE),
    linewidth = guide_legend(order = 4)
  )+
  xlim(c(min(plot_data2$datetime_ante_start), max(plot_data2$datetime)))+
  ylim(c(min(plot_data2$Dep_N), max(plot_data2$Dep_N)))

fig2_b_1

ggsave(plot = fig2_b_1, filename = "./visualizations/final_figures/Figure2b_1.tif",
       device = "tiff", height = 4, width = 6.5, units = "in", bg = "white")

fig2_b_2 <- ggplot()+
  geom_vline(data = interval_starts, aes(xintercept = datetime, linetype = measurement_time, alpha = measurement_time), color = "gray")+
  geom_line(data = subset(plot_data2, dep_type == "Dep_Nhistoric"), aes(x = datetime, y = Dep_N, group = interval_no_by_type, color = as.factor(interval_no), alpha = dep_type, linetype = dep_type), linewidth = 1)+
  geom_point(data = subset(plot_data2, dep_type == "Dep_Nhistoric"), aes(x = datetime, y = Dep_N, group = interval_no_by_type, color = as.factor(interval_no)))+
  #geom_segment(data = diffs, aes(x = date_m1, y = Dep_Nhistoric, xend = date_m1, yend = Dep_N, color = as.factor(interval_no), linewidth = linewidth_var), arrow = arrow(length = unit(0.2,"cm")))+
  theme_classic()+
  scale_color_discrete(name = "Measurement interval")+
  scale_alpha_manual(values = c(date_m1 = 1, Dep_Nhistoric = 1), name = NULL, labels = c("measurement \ninterval start date","antecedent N dep."))+
  scale_linetype_manual(values = c(date_m1 = 2, Dep_Nhistoric = 1), name = NULL, labels = c("measurement \ninterval start date","antecedent N dep."))+
  scale_linewidth_manual(values = c("Dep_Ndiff" = 0.5), name = NULL, labels = c("short-term change in N dep."))+
  xlab("")+
  ylab(expression(paste("N deposition (kg N ", ha^-1," ",y^-1,")")))+
  theme(legend.spacing.y = unit(0.0, "cm"),
        legend.key.width = unit(3, "line"),
        legend.key.height = unit(1.5, "line"))+
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(override.aes = list(
      linetype = c(2, 1), # Example values
      alpha = c(1, 1) # Example values
    ), reverse = TRUE),
    alpha = guide_legend(override.aes = list(
      linetype = c(1, 2), # Example values
      alpha = c(1, 1) # Example values
    ), reverse = TRUE),
    linewidth = guide_legend(order = 4)
  )+
  xlim(c(min(plot_data2$datetime_ante_start), max(plot_data2$datetime)))+
  ylim(c(min(plot_data2$Dep_N), max(plot_data2$Dep_N)))

fig2_b_2

ggsave(plot = fig2_b_2, filename = "./visualizations/final_figures/Figure2b_2.tif",
       device = "tiff", height = 4, width = 6.5, units = "in", bg = "white")
