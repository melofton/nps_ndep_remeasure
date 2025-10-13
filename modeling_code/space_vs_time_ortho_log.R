library(tidyverse)
library(rjags)
library(tidybayes)
library(bayesplot)

#df <- read_csv("./McDonnell_etal_InPrep_TreeData_2024_10_11.csv", show_col_types = FALSE)

#df |>
#  dplyr::filter(common_name == "black cherry") |> 
#  mutate(diff = Dep_N - Dep_N15,
#         early = Dep_N15 - Dep_N) |> 
#  ggplot(aes(x = diff)) +
#  geom_histogram()
#k <- 1
#sim <- "testS2"

#k=1

run_model <- function(k, df, sim){
  
  tree_species <- unique(df$common_name)[k]
  
  print(tree_species)
  
  focal_data <- df %>%
    dplyr::filter(common_name == tree_species) 
  
  trees_index <- focal_data |> 
    distinct(tree_ID) |> 
    mutate(tree_index = 1:n())
  
  plots <- focal_data |> 
    distinct(plot_ID) |> 
    mutate(plot_index = 1:n()) #USE THIS TO GET PLOT_INDEX BELOW
  
  df1 <- focal_data |> 
    left_join(plots, by = join_by(plot_ID)) |> 
    left_join(trees_index, by = join_by(tree_ID)) |>
    mutate(dt = as.numeric(date_m2 - date_m1)/365) |>
    mutate(log_AG_carbon_pYear = (log(AG_carbon_m2) - log(AG_carbon_m1)) / dt,,
           log_AG_carbon_m1 = log(AG_carbon_m1),
           log_subp_BA_GT_m1 = log(subp_BA_GT_m1 + 1)) #|>
  #slice(c(1:100))
  
  log_mean_annual_avg_growth <- df1$log_AG_carbon_pYear
  log_start_measures <- df1$log_AG_carbon_m1
  plot_index <- df1$plot_index
  tree_index <- df1$tree_index
  #dt <- df1$dt
  log_ba_gt <- df1$log_subp_BA_GT_m1
  #ndep_delta <- df1$Dep_Ndelta
  sdep_historic <- df1$Dep_Shistoric
  sdep_diff <- df1$Dep_Sdiff
  mat_delta <- df1$MAT_delta
  map_delta_dm <- df1$MAP_delta_dm
  #ndep_longterm_delta <- df1$Dep_N_LTchange
  ndep_historic <- df1$Dep_Nhistoric_ortho
  ndep_diff <- df1$Dep_Ndiff_ortho
  
  n_measures <- nrow(df1)

  ssData   <- list(log_tree_agb_obs = log_start_measures,
                   log_tree_growth_obs = log_mean_annual_avg_growth,
                   ntrees = length(unique(df1$tree_ID)), 
                   n_plots = length(unique(df1$plot_index)),
                   n_measures = n_measures,
                   log_ba_gt = log_ba_gt,
                   #dt = dt,
                   #ndep_delta = ndep_delta,
                   sdep_historic = sdep_historic,
                   sdep_diff = sdep_diff,
                   mat_delta = mat_delta,
                   map_delta_dm = map_delta_dm,
                   #ndep_longterm_delta = ndep_longterm_delta,
                   ndep_historic = ndep_historic,
                   ndep_diff = ndep_diff,
                   plot_index = plot_index,
                   tree_index = tree_index)
                   #max_ndep = max(c(ndep), na.rm = TRUE),
                   #min_ndep = min(c(ndep), na.rm = TRUE))
                   #temp = temp,
                   #max_temp = max(c(temp), na.rm = TRUE),
                   #min_temp = min(c(temp), na.rm = TRUE),
                   #precip = precip,
                   #max_precip = max(c(precip), na.rm = TRUE),
                   #min_precip = min(c(precip), na.rm = TRUE),
                   #sdep = sdep,
                   #max_sdep = max(c(sdep), na.rm = TRUE),
                   #min_sdep = min(c(sdep), na.rm = TRUE),
                   #obsErr = 1)
  
  cat( "model {

  global_tree_effect ~ dunif(-10,10)
  tau_global ~ dunif(0.0001,10)
  tau_plot ~ dunif(0.0001,10)
  procErr ~ dunif(0.0001,10)
  p2 ~ dunif(0,2) 
  p3 ~ dunif(0,100) #Sample in log space
  p5 ~ dnorm(0,0.001)
  p6 ~ dnorm(0,0.001)
  p7 ~ dnorm(0,0.001)
  p8 ~ dnorm(0,0.001)
  p9 ~ dnorm(0,0.001)
  p10 ~ dnorm(0,0.001)
  #lp4 ~ dunif(log(min_ndep), log(max_ndep*2))
  #p5inv2 ~ dexp(0.001)  
  #p6 ~ dunif(min_temp*0.5, max_temp*2)
  #p7 ~ dunif(0.3, 20)  
  #p8 ~ dunif(min_precip*0.5, max_precip*2)
  #p9 ~ dunif(0.3, 20)   
  #p10 ~ dunif(min_sdep*0.5, max_sdep*2)
  #p10 ~ dunif(min_sdep, max_sdep*2)
  #p11 ~ dunif(0.3, 10)    
  
  for(p in 1:n_plots){
    plot_effect[p] ~ dnorm(global_tree_effect, tau_global)
  }

  for(n in 1:ntrees){
    # tree_effect[n] ~ dnorm(global_tree_effect, tau_global)
    tree_effect[n] ~ dnorm(plot_effect[plot_index[n]], tau_plot)
  }
  
  for(t in 1:n_measures){

    #tree_growth_mean[t] <-  ((tree_effect[tree_index[t]] + ndep_diff[t]*p5 + sdep_diff[t]*p6 + mat_delta[t]*p7 + map_delta_dm[t]*p8 + ndep_historic[t]*p9 + sdep_historic[t]*p10)
    #* tree_agb_obs[t] ^ p2) 
    #    * exp(-ba_gt[t]*p3)  # add one here if go logged route b/c if you are the biggest tree it would be 0 and therefore undefined
    
     log_tree_growth_mean[t] <-  ((tree_effect[tree_index[t]] + ndep_diff[t]*p5 + sdep_diff[t]*p6 + mat_delta[t]*p7 + map_delta_dm[t]*p8 + ndep_historic[t]*p9 + sdep_historic[t]*p10) #if on log scale would interpret these in terms of multiplicative change
     + p2*log_tree_agb_obs[t]) 
         - p3*log_ba_gt[t] # how much do we want to penalize the log growth based on basal area
    # log beforehand b/c a little computationally quicker

    log_tree_growth_obs[t] ~ dnorm(log_tree_growth_mean[t], procErr) # consider t distribution
    # could model this as normal w/ lambda paramter modeled as gamma
    # gamma has two parameters determined by nu
    # prior on nu is exponential and bounded at 2; if it goes to infinity you basically
    # have a normal distribution
    # if JAGS grumps at truncation could hack with a uniform distibution bounded at 2 and some very large number

  }

  }  ", fill=T,file=paste0("./experiments/",sim,"/",sim,"-growthModel-",tree_species,".txt"))
  
  #black cherry, eastern cottonwood, sugar maple, yellow-poplar, quaking aspen
  #ponderosa pine, paper birch, red spruce
  init_values <- data.frame(species = c("black cherry","eastern cottonwood","sugar maple",
                                        "yellow-poplar","quaking aspen","ponderosa pine",
                                        "paper birch","red spruce"),
                            p5 = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
                            p5_delta = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
                            p6 = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
                            p6_delta = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
                            p7 = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
                            p7_delta = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
                            p8 = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
                            p8_delta = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
                            p9 = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
                            p9_delta = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
                            p10 = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
                            p10_delta = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5))
  
  inits <- list(list("global_tree_effect" = 1,
                     "tau_global" = 1,
                     "tau_plot" = 1,
                     "procErr" = 0.001,
                     "p2" = 0.6,
                     "p3" = 1,
                     "p5" = init_values[k,"p5"] + init_values[k,"p5_delta"],
                     "p6" = init_values[k,"p6"] + init_values[k,"p6_delta"],
                     "p7" = init_values[k,"p7"] + init_values[k,"p7_delta"],
                     "p8" = init_values[k,"p8"] + init_values[k,"p8_delta"],
                     "p9" = init_values[k,"p9"] + init_values[k,"p9_delta"],
                     "p10" = init_values[k,"p10"] + init_values[k,"p10_delta"]),
                list("global_tree_effect" = 1,
                     "tau_global" = 1,
                     "tau_plot" = 1,
                     "procErr" = 0.001,
                     "p2" = 0.4,
                     "p3" = 1,
                     "p5" = init_values[k,"p5"],
                     "p6" = init_values[k,"p6"],
                     "p7" = init_values[k,"p7"],
                     "p8" = init_values[k,"p8"],
                     "p9" = init_values[k,"p9"],
                     "p10" = init_values[k,"p10"]),
                list("global_tree_effect" = 1,
                     "tau_global" = 1,
                     "tau_plot" = 1,
                     "procErr" = 0.001,
                     "p2" = 0.4,
                     "p3" = 1,
                     "p5" = init_values[k,"p5"] - init_values[k,"p5_delta"],
                     "p6" = init_values[k,"p6"] - init_values[k,"p6_delta"],
                     "p7" = init_values[k,"p7"] - init_values[k,"p7_delta"],
                     "p8" = init_values[k,"p8"] - init_values[k,"p8_delta"],
                     "p9" = init_values[k,"p9"] - init_values[k,"p9_delta"],
                     "p10" = init_values[k,"p10"] - init_values[k,"p10_delta"]))
  
  ssFit    <- jags.model(data=ssData, file=paste0("./experiments/",sim,"/",sim,"-growthModel-",tree_species,".txt"), n.chains = 3, inits = inits, n.adapt = 5000)
  parNames <- c("tree_effect", 
                "p2", 
                "p3", 
                "p5",
                "p6",
                "p7",
                "p8",
                "p9",
                "p10",
                "procErr", 
                "global_tree_effect",
                "plot_effect")
  
  ssFit <- coda.samples(ssFit, variable.names = parNames, n.iter=15000, thin = 5)
  mcmc <- spread_draws(ssFit,
                       p2, 
                       p3, 
                       p5,
                       p6,
                       p7,
                       p8,
                       p9,
                       p10,
                       procErr, 
                       global_tree_effect) 
  mcmc_treeEffect <- spread_draws(ssFit,
                                  `tree_effect`[n])
  
  
  arrow::write_parquet(mcmc, sink = paste0("./experiments/",sim,"/",sim, "-",tree_species, "-mcmc.parquet"))
  arrow::write_parquet(mcmc_treeEffect, sink = paste0("./experiments/",sim,"/",sim, "-",tree_species, "-mcmc-treeEffect.parquet"))
  
  p <- mcmc |> 
    rename(chain = .chain, iteration = .iteration, draw = .draw) |> 
    select(-draw) |> 
    pivot_longer(-c("chain", "iteration"), names_to = "par", values_to = "value") |> 
    ggplot(aes(x = iteration, y = value, color = factor(chain))) + 
    geom_line() + 
    facet_wrap(~par, scales = "free")
  
  ggsave(filename = paste0("./experiments/",sim,"/",sim, "-",tree_species,"-mcmc.pdf"), plot = p, device = "pdf")
  
  mcmc_burned <- mcmc |> 
    dplyr::filter(.iteration > 1000)
  
  p <- mcmc_burned |> 
    rename(chain = .chain,
           iteration = .iteration) |> 
    select(chain:p10) |> 
    select(-.draw, -iteration) |> 
    mcmc_pairs()
  
  
  ggsave(filename = paste0("./experiments/",sim,"/",sim, "-", tree_species,"-pairs.pdf"), plot = p, device = "pdf", height = 12, width = 12)
  

}
