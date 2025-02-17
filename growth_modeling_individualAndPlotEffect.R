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
  
  live_tree_ids <- df |> 
    dplyr::filter(common_name == tree_species) |> 
    summarise(count = n(),
              sum = sum(live_m2), .by = tree_ID) |> 
    dplyr::filter(count >= sum) |> 
    pull(tree_ID)
  
  focal_data <- df |> 
    dplyr::filter(tree_ID %in% live_tree_ids) |> 
    group_by(tree_ID) |> 
    tidyr::fill(subp_BA_GT_m1, .direction = "down") |> 
    dplyr::filter(!is.na(subp_BA_GT_m1) & !is.na(Dep_N)) |>
    ungroup()
  
  trees_index <- focal_data |> 
    dplyr::filter(common_name == tree_species) |> 
    distinct(tree_ID) |> 
    dplyr::filter(tree_ID %in% live_tree_ids) |> 
    mutate(tree_index = 1:n())
  
  plots <- focal_data |> 
    dplyr::filter(common_name == tree_species) |> 
    dplyr::filter(tree_ID %in% live_tree_ids) |> 
    distinct(plot_ID) |> 
    mutate(plot_index = 1:n()) #USE THIS TO GET PLOT_INDEX BELOW
  
  df1 <- focal_data |> 
    mutate(dt = as.numeric(date_m2 - date_m1)/365) |> 
    select(AG_carbon_pYear, AG_carbon_m1, AG_carbon_m2, tree_ID, plot_ID, dt, Dep_N, subp_BA_GT_m1, MAT, MAP, Dep_S) |>
    dplyr::filter(tree_ID %in% live_tree_ids) |>
    left_join(plots, by = join_by(plot_ID)) |> 
    left_join(trees_index, by = join_by(tree_ID)) 
  
  mean_annual_avg_growth <- df1$AG_carbon_pYear
  start_measures <- df1$AG_carbon_m1
  plot_index <- df1$plot_index
  tree_index <- df1$tree_index
  #dt <- df1$dt
  ba_gt <- df1$subp_BA_GT_m1
  ndep <- df1$Dep_N
  n_measures <- nrow(df1)

  ssData   <- list(tree_agb_obs = start_measures,
                   tree_growth_obs = mean_annual_avg_growth,
                   ntrees = length(unique(df1$tree_ID)), 
                   n_plots = length(unique(df1$plot_index)),
                   n_measures = n_measures,
                   ba_gt = ba_gt,
                   #dt = dt,
                   ndep = ndep,
                   plot_index = plot_index,
                   tree_index = tree_index,
                   max_ndep = max(c(ndep), na.rm = TRUE),
                   min_ndep = min(c(ndep), na.rm = TRUE),
                   #temp = temp,
                   #max_temp = max(c(temp), na.rm = TRUE),
                   #min_temp = min(c(temp), na.rm = TRUE),
                   #precip = precip,
                   #max_precip = max(c(precip), na.rm = TRUE),
                   #min_precip = min(c(precip), na.rm = TRUE),
                   #sdep = sdep,
                   #max_sdep = max(c(sdep), na.rm = TRUE),
                   #min_sdep = min(c(sdep), na.rm = TRUE),
                   obsErr = 1)
  
  cat( "model {

  global_tree_effect ~ dunif(-10,10)
  tau_global ~ dunif(0.0001,10)
  tau_plot ~ dunif(0.0001,10)
  procErr ~ dunif(0.0001,10)
  p2 ~ dunif(0,2) 
  p3 ~ dunif(0,100) #Sample in log space
  p4 ~ dunif(-1000, 1000)#; NEED TO VECTORIZE THIS TO N=3 FOR DIFFERENT STATUSES
  p5 ~ dunif(0.3, 20) # ; ALSO VECTORIZE THIS
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
    tree_agb_latent[t] ~ dnorm(tree_agb_obs[t], obsErr)
    
    tree_growth_mean[t] <-  (tree_effect[tree_index[t]] * tree_agb_latent[t] ^ p2) 
        * exp(-ba_gt[t]*p3)  
        * exp(-0.5 * (log(ndep[t] / p4)/ p5) ^ 2)  
        
    tree_growth_latent[t] ~ dnorm(tree_growth_mean[t], procErr)
    tree_growth_obs[t] ~ dnorm(tree_growth_latent[t], obsErr)
    
  }

  }  ", fill=T,file=paste0(sim,"-growthModel-",k,".txt"))
  
  inits <- list(list("global_tree_effect" = 1,
                     "tau_global" = 1,
                     "tau_plot" = 1,
                     "procErr" = 0.001,
                     "p2" = 0.6,
                     "p3" = 1,
                     "p4" = max(c(ndep), na.rm = TRUE), 
                     "p5" = 1.3),
                list("global_tree_effect" = 1,
                     "tau_global" = 1,
                     "tau_plot" = 1,
                     "procErr" = 0.001,
                     "p2" = 0.4,
                     "p3" = 1,
                     "p4" = 2 * min(c(ndep), na.rm = TRUE), 
                     "p5" = 2),
                list("global_tree_effect" = 1,
                     "tau_global" = 1,
                     "tau_plot" = 1,
                     "procErr" = 0.001,
                     "p2" = 0.4,
                     "p3" = 1,
                     "p4" = 0.5 * max(c(ndep), na.rm = TRUE), 
                     "p5" = 5))
  
  ssFit    <- jags.model(data=ssData, file=paste0(sim,"-growthModel-",k,".txt"), n.chains = 3, inits = inits, n.adapt = 5000)
  parNames <- c("tree_effect", 
                "p2", 
                "p3", 
                "p4", "p5",
                "procErr", 
                "global_tree_effect",
                "plot_effect")
  
  ssFit <- coda.samples(ssFit, variable.names = parNames, n.iter=15000, thin = 5)
  mcmc <- spread_draws(ssFit,
                       p2, 
                       p3, 
                       p4,p5,
                       procErr, 
                       `tree_effect[1]`,
                       `tree_effect[51]`,
                       `tree_effect[99]`,
                       global_tree_effect,
                       `plot_effect[1]`,
                       `plot_effect[10]`,
                       `plot_effect[20]`) 
  
  
  arrow::write_parquet(mcmc, sink = paste0(sim, "-",k,"-",tree_species, "-mcmc.parquet"))
  
  p <- mcmc |> 
    rename(chain = .chain, iteration = .iteration, draw = .draw) |> 
    select(-draw) |> 
    pivot_longer(-c("chain", "iteration"), names_to = "par", values_to = "value") |> 
    ggplot(aes(x = iteration, y = value, color = factor(chain))) + 
    geom_line() + 
    facet_wrap(~par, scales = "free")
  
  ggsave(filename = paste0(sim, "-",k,"-",tree_species,"-mcmc.pdf"), plot = p, device = "pdf")
  
  n_samples <- 1000
  samples <- sample(nrow(mcmc), n_samples, replace = FALSE)
  
  max_ndep = max(c(ndep), na.rm = TRUE)
  min_ndep = min(c(ndep), na.rm = TRUE)
  
  ndep_pred <- seq(min_ndep, max_ndep, length.out = 100)
  
  pred_post <- NULL
  
  mcmc_burned <- mcmc |> 
    dplyr::filter(.iteration > 1000)
  
  for(i in 1:length(samples)){
    
    curr_ndep <- tibble(sample = i,
                        variable = "ndep (kg N/m2/yr)",
                        value = ndep_pred,
                        pred = exp(-0.5 * (log(ndep_pred / mcmc_burned$p4[samples[i]])/ mcmc_burned$p5[samples[i]]) ^ 2),
                        type = "posterier")
    
    pred_post <- bind_rows(pred_post, curr_ndep)
  }
  
  pred_prior <- NULL
  
  for(i in 1:1000){
    p4 <- runif(1, min_ndep*0.5, max_ndep*2)
    p5 <- runif(1, 0.3, 20) 
    
    curr_ndep <- tibble(sample = i,
                        variable = "ndep (kg N/m2/yr)",
                        value = ndep_pred,
                        pred = exp(-0.5 * (log(ndep_pred / p4)/ p5) ^ 2),
                        type = "prior")
    
    pred_prior <- bind_rows(pred_prior, curr_ndep)
    
  }
  
  p <- bind_rows(pred_prior, pred_post) |> 
    mutate(id = paste0(sample,type)) |> 
    ggplot(aes(x = value, y = pred, group = id, color = type)) +
    geom_line() +
    facet_wrap(~variable, scales = "free_x") +
    theme_bw()
  
  ggsave(filename = paste0(sim, "-", k,"-",tree_species,"-prior-post.pdf"), plot = p, device = "pdf")
  
  p <- mcmc_burned |> 
    rename(chain = .chain,
           iteration = .iteration) |> 
    select(chain:p5) |> 
    select(-.draw, -iteration) |> 
    mcmc_pairs()
  
  
  ggsave(filename = paste0(sim, "-", k,"-",tree_species,"-pairs.pdf"), plot = p, device = "pdf", height = 12, width = 12)
  

}
