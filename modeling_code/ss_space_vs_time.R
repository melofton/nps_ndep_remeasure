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
    dplyr::filter(common_name == tree_species) %>%
    mutate(dt = as.numeric(date_m2 - date_m1)/365) 
  
  trees_index <- focal_data |> 
    distinct(tree_ID) |> 
    mutate(tree_index = 1:n()) #|>
    #slice(c(1:100))
  
  plots <- focal_data |> 
    distinct(plot_ID) |> 
    mutate(plot_index = 1:n()) #USE THIS TO GET PLOT_INDEX BELOW
  
  df1 <- focal_data |> 
    left_join(plots, by = join_by(plot_ID)) |> 
    left_join(trees_index, by = join_by(tree_ID)) 
  
  tree_measures <- focal_data |> 
    dplyr::filter(common_name == tree_species) |> 
    dplyr::filter(tree_ID %in% live_tree_ids) |> 
    group_by(tree_ID) |> 
    summarise(count = n()+1)
  
  max_measures <- max(tree_measures$count)
  
  # mean_annual_avg_growth <- df1$AG_carbon_pYear
  # start_measures <- df1$AG_carbon_m1
  # plot_index <- df1$plot_index
  # tree_index <- df1$tree_index
  # #dt <- df1$dt
  # ba_gt <- df1$subp_BA_GT_m1
  # #ndep_delta <- df1$Dep_Ndelta
  # sdep_delta <- df1$Dep_Sdelta
  # mat_delta <- df1$MAT_delta
  # map_delta_dm <- df1$MAP_delta_dm
  # #ndep_longterm_delta <- df1$Dep_N_LTchange
  # #ndep_historic <- df1$Dep_Nhistoric
  # ndep_wgt_avg <- df1$Dep_N_wgt_avg_to_date
  # ndep_dev <- df1$Dep_Ndeviation
  # 
  # n_measures <- nrow(df1)
  
  tree_matrix <- matrix(NA, nrow(trees_index), max_measures)
  n_measures <- rep(NA, nrow(trees_index))
  plot_index <- rep(NA, nrow(trees_index))
  dt <- matrix(NA, nrow(trees_index), max_measures)
  ba_gt <- matrix(NA, nrow(trees_index), max_measures)
  sdep_delta <- matrix(NA, nrow(trees_index), max_measures)
  mat_delta <- matrix(NA, nrow(trees_index), max_measures)
  map_delta_dm <- matrix(NA, nrow(trees_index), max_measures)
  ndep_wgt_avg <- matrix(NA, nrow(trees_index), max_measures)
  ndep_dev <- matrix(NA, nrow(trees_index), max_measures)

  # NEED TO MAKE SURE HAVE PLOT ID AS AN INDEX IN DATA (1:N_PLOTS)
  # add n_status (updown variable to data) - LATER
  
  for(i in 1:nrow(trees_index)){ # PLOT INDEX NEEDS TO BE ASSIGNED IN HERE; EXISTS IN TREE_INFO
    measures <- tree_measures |> 
      dplyr::filter(tree_ID == trees_index$tree_ID[i]) |> 
      pull(count)
    
    n_measures[i] <- measures
    
    plot <- df1 |>
      dplyr::filter(tree_ID == trees_index$tree_ID[i]) |>
      pull(plot_index)
    
    plot_index[i] <- plot[1]
    
    curr_tree <- df1 |> 
      dplyr::filter(tree_ID == trees_index$tree_ID[i])
    
      for(j in 2:(measures)){
        if(j == 2){
        tree_matrix[i, j-1] <-  curr_tree$AG_carbon_m1[j-1]
        }
        tree_matrix[i, j] <-  curr_tree$AG_carbon_m2[j-1]
        dt[i,j] <- curr_tree$dt[j-1]
        ba_gt[i, j] <- curr_tree$subp_BA_GT_m1[j-1] 
        sdep_delta[i, j] <- curr_tree$Dep_Sdelta[j-1]
        mat_delta[i, j] <- curr_tree$MAT_delta[j-1]
        map_delta_dm[i, j] <- curr_tree$MAP_delta_dm[j-1]
        ndep_wgt_avg[i, j] <- curr_tree$Dep_N_wgt_avg_to_date[j-1]
        ndep_dev[i, j] <- curr_tree$Dep_Ndeviation[j-1] 
      }
    
  }

  ssData   <- list(tree_agb_obs = tree_matrix,
                   ntrees = length(tree_index), 
                   n_plots = length(plot_index),
                   n_measures = n_measures,
                   ba_gt = ba_gt,
                   dt = dt,
                   sdep_delta = sdep_delta,
                   mat_delta = mat_delta,
                   map_delta_dm = map_delta_dm,
                   ndep_wgt_avg = ndep_wgt_avg,
                   ndep_dev = ndep_dev,
                   plot_index = plot_index,
                   obsErr = 1)
                   
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
  #p10 ~ dnorm(0,0.001)
  #lp4 ~ dunif(log(min_ndep), log(max_ndep*2))
  #p5inv2 ~ dexp(0.001)  
  #p6 ~ dunif(min_temp*0.5, max_temp*2)
  #p7 ~ dunif(0.3, 20)  
  #p8 ~ dunif(min_precip*0.5, max_precip*2)
  #p9 ~ dunif(0.3, 20)   
  #p10 ~ dunif(min_sdep*0.5, max_sdep*2)
  #p10 ~ dunif(min_sdep, max_sdep*2)
  #p11 ~ dunif(0.3, 10)  
  
  for(n in 1:ntrees){
    tree_agb_latent[n,1] ~ dnorm(tree_agb_obs[n, 1], obsErr)
  }
  
  for(p in 1:n_plots){
    plot_effect[p] ~ dnorm(global_tree_effect, tau_global)
  }
  
  for(n in 1:ntrees){
      tree_effect[n] ~ dnorm(plot_effect[plot_index[n]], tau_plot)
  
  for(t in 2:n_measures[n]){

    tree_agb_mean[n,t] <-  tree_agb_latent[n, t-1] +  dt[n,t] * ((tree_effect[n] + ndep_dev[n,t]*p5 + sdep_delta[n,t]*p6 + mat_delta[n,t]*p7 + map_delta_dm[n,t]*p8 + ndep_wgt_avg[n,t]*p9 ) * tree_agb_latent[n, t-1] ^ p2) 
        * exp(-ba_gt[n,t]*p3)  

    tree_agb_latent[n,t] ~ dnorm(tree_agb_mean[n, t], procErr/dt[n,t])
    tree_agb_obs[n,t] ~ dnorm(tree_agb_latent[n,t], obsErr)

  }
  }

  }  ", fill=T,file=paste0("./experiments/",sim,"/",sim,"-ssModel-",tree_species,".txt"))
  
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
                            p9_delta = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01))
  
  inits <- list(list("global_tree_effect" = 1,
                     "tau_global" = 1,
                     "tau_plot" = 1,
                     "procErr" = 0.001,
                     "p2" = 0.4,
                     "p3" = 1,
                     "p5" = init_values[k,"p5"] + init_values[k,"p5_delta"],
                     "p6" = init_values[k,"p6"] + init_values[k,"p6_delta"],
                     "p7" = init_values[k,"p7"] + init_values[k,"p7_delta"],
                     "p8" = init_values[k,"p8"] + init_values[k,"p8_delta"],
                     "p9" = init_values[k,"p9"] + init_values[k,"p9_delta"]),
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
                     "p9" = init_values[k,"p9"]),
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
                     "p9" = init_values[k,"p9"] - init_values[k,"p9_delta"]))
  
  ssFit    <- jags.model(data=ssData, file=paste0("./experiments/",sim,"/",sim,"-ssModel-",tree_species,".txt"), n.chains = 3, inits = inits, n.adapt = 5000)
  parNames <- c("tree_effect", 
                "p2", 
                "p3", 
                "p5",
                "p6",
                "p7",
                "p8",
                "p9",
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
    select(chain:p9) |> 
    select(-.draw, -iteration) |> 
    mcmc_pairs()
  
  
  ggsave(filename = paste0("./experiments/",sim,"/",sim, "-", tree_species,"-pairs.pdf"), plot = p, device = "pdf", height = 12, width = 12)
  

}
