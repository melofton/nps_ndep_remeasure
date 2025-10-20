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
  labs(color = expression(paste("mean N deposition (kg N ", ha^-1," ",y^-1,")")))+
  guides(color = guide_colorbar(title.position = "bottom", title.hjust = 0.0))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.background = element_rect(fill = "transparent"))+
  theme(legend.position = "inside", legend.position.inside =  c(0.3, 0.08),
        legend.direction = "horizontal", plot.margin = unit(c(0.1,0,0.1,0), "pt"))+
  ylim(c(min(usa_coordinates$lat),max(usa_coordinates$lat)))


fig2_a

# B. timeseries of short-term differences in N deposition by plot and ecoregion

dat_b <- df %>%
  select(plot_ID, date_m1, Dep_Ndiff, ecoregion_ortho) %>%
  distinct(.) 

ortho_ecoregions <- unique(dat_b$ecoregion_ortho)

fig2_plotlist <- list()

for(i in 1:length(ortho_ecoregions)){
  
  dat_plot <- dat_b %>%
    filter(ecoregion_ortho == ortho_ecoregions[i])
  
  fig2_plotlist[[i]] <- ggplot(dat_plot, aes(x = date_m1, y = Dep_Ndiff, group = plot_ID)) + 
    geom_line(color = "gray") +
    geom_point(color = "black") +
    theme_classic() +
    geom_hline(yintercept = 0, color = "blue") +
    ylab(expression(paste(Delta," N dep. (kg N ", ha^-1," ",y^-1,")"))) +
    xlab("measurement period start date") + 
    ggtitle(str_wrap(ortho_ecoregions[i], width = 30))
  
}


# Assemble figure

p2 <- ggarrange(fig2_a, fig2_plotlist[[1]],
                fig2_plotlist[[2]], fig2_plotlist[[3]],
                fig2_plotlist[[4]], fig2_plotlist[[5]],
                fig2_plotlist[[6]], fig2_plotlist[[7]],
                nrow = 4, ncol = 2,
                labels = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)")
)
p2
ggsave(plot = p2, filename = "./visualizations/final_figures/Figure2.tif",
       device = "tiff", height = 10.5, width = 8.5, units = "in", bg = "white")
