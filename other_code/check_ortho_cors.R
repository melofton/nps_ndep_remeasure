# Check correlations of orthogonalized variables
# Author: Mary Lofton
# Date: 12SEP25

# Purpose: check whether "historic" and "short-term" N deposition variables are
# still orthogonal if grouped by ecoregion

# read in processed data
processed_dat <- read_csv("./data/processed_data.csv")

# read in ecoregions
ecoregions <- read_csv("./data/plot_ecoregions.csv") 

# join dataframes
dat <- left_join(processed_dat, ecoregions, by = c("plot_ID")) %>%
  select(level1_ecoregion, interval_no, Dep_Ndeviation, Dep_N_wgt_avg_to_date_ortho) %>%
  distinct()

# check correlation by interval only - should be 0!
interval_cor <- dat %>%
  group_by(interval_no) %>%
  summarize(n = n(),
            correlation = cor(Dep_Ndeviation, Dep_N_wgt_avg_to_date_ortho))

# check correlation by ecoregion and interval
regions <- unique(ecoregions$level1_ecoregion)
intervals <- c(1:4)

final_df <- data.frame(ecoregion = rep(regions, each = 4),
                       interval = rep(intervals, times = length(unique(ecoregions$level1_ecoregion))),
                       n = NA,
                       cor = NA)
row = 1

for(i in 1:length(regions)){
  
  for(j in 1:max(intervals)){
  current_dat <- dat %>%
    filter(level1_ecoregion == regions[i] & interval_no == intervals[j]) 
  current_cor <- cor(current_dat$Dep_Ndeviation, current_dat$Dep_N_wgt_avg_to_date_ortho)
  final_df$n[row] <- length(current_dat$Dep_Ndeviation)
  final_df$cor[row] <- current_cor
  if(length(current_dat$Dep_Ndeviation) > 0){
    p <- ggplot(data = current_dat, aes(x = Dep_Ndeviation, y = Dep_N_wgt_avg_to_date_ortho))+
      geom_point()+
      ggtitle(paste(regions[i],intervals[j]))
    plot_name <- paste0("./visualizations/corr_plots/",regions[i],"_",intervals[j],".png")
    ggsave(p, filename = plot_name, device = "png")
  }
  row = row + 1
  }
}

write.csv(final_df, "./data/ortho_correlations_by_ecoregion.csv",row.names = FALSE)

slide_df <- final_df %>%
  filter(!n == 0)
# visualize correlations
og_df <- read_csv("./data/McDonnell_etal_InPrep_TreeData_2024_10_11.csv", show_col_types = FALSE) %>%
  dplyr::filter(!common_name %in% c("Douglas-fir","western hemlock")) 
#filter(common_name %in% c("eastern cottonwood"))
interval_hist <- og_df %>%
  filter(interval_no == 1)
hist(interval_hist$date_m1, breaks = "years", main = "Histogram of interval start dates",
     xlab = "Start date of first measurement interval for tree")

plot_ids <- unique(og_df$plot_ID)
plot1 <- og_df %>%
  filter(plot_ID == plot_ids[4])
