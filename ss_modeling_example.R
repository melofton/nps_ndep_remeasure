library(tidyverse)
library(rjags)
library(tidybayes)
library(bayesplot)

df <- read_csv("./McDonnell_etal_InPrep_TreeData_2024_10_11.csv", show_col_types = FALSE)

#df |>
#  filter(common_name == "black cherry") |> 
#  mutate(diff = Dep_N - Dep_N15,
#         early = Dep_N15 - Dep_N) |> 
#  ggplot(aes(x = diff)) +
#  geom_histogram()
#k <- 1
sim <- "testS2"

k=1
  
  tree_species <- unique(df$common_name)[k]
  
  print(tree_species)
  
  live_tree_ids <- df |> 
    filter(common_name == tree_species) |> 
    summarise(count = n(),
              sum = sum(live_m2), .by = tree_ID) |> 
    filter(count >= sum) |> 
    pull(tree_ID)
  
  focal_data <- df |> 
    filter(tree_ID %in% live_tree_ids) |> 
    group_by(tree_ID) |> 
    tidyr::fill(subp_BA_GT_m1, .direction = "down") |> 
    ungroup()
  
  trees_index <- focal_data |> 
    filter(common_name == tree_species) |> 
    distinct(tree_ID) |> 
    filter(tree_ID %in% live_tree_ids) |> 
    mutate(tree_index = 1:n())
  
  plots <- focal_data |> 
    filter(common_name == tree_species) |> 
    filter(tree_ID %in% live_tree_ids) |> 
    distinct(plot_ID) |> 
    mutate(plot_index = 1:n())
  
  tree_measures <- focal_data |> 
    filter(common_name == tree_species) |> 
    filter(tree_ID %in% live_tree_ids) |> 
    group_by(tree_ID) |> 
    summarise(count = n())
  
  tree_info <- focal_data |> 
    filter(common_name == tree_species) |> 
    filter(tree_ID %in% live_tree_ids) |> 
    distinct(tree_ID, plot_ID) |> 
    left_join(plots, by = join_by(plot_ID)) |> 
    left_join(tree_measures, by = join_by(tree_ID)) |> 
    left_join(trees_index, by = join_by(tree_ID))
  
  max_measures <- max(tree_info$count)
  
  df1 <- focal_data |> 
    mutate(dt = as.numeric(date_m2 - date_m1)/365) |> 
    select(AG_carbon_m1, AG_carbon_m2, tree_ID, dt, Dep_N, subp_BA_GT_m1, MAT, MAP, Dep_S)
  
  tree_matrix <- matrix(NA, nrow(tree_info), max_measures)
  n_measures <- rep(NA, nrow(tree_info))
  dt <- matrix(NA, nrow(tree_info), max_measures)
  ba_gt <- matrix(NA, nrow(tree_info), max_measures)
  ndep <- matrix(NA, nrow(tree_info), max_measures)
  
  for(i in 1:nrow(tree_info)){
    measures <- tree_info |> 
      filter(tree_ID == tree_info$tree_ID[i]) |> 
      pull(count)
    
    n_measures[i] <- measures
    
    curr_tree <- df1 |> 
      filter(tree_ID == tree_info$tree_ID[i])
    
    if(measures > 1){
      for(j in 2:(measures)){
        tree_matrix[i, j-1] <-  curr_tree$AG_carbon_m1[j]
        tree_matrix[i, j] <-  curr_tree$AG_carbon_m2[j]
        dt[i,j] <- curr_tree$dt[j]
        ba_gt[i, j] <- curr_tree$subp_BA_GT_m1[j] 
        ndep[i, j] <- curr_tree$Dep_N[j] 
      }
    }else{
      tree_matrix[i, 1] <-  curr_tree$AG_carbon_m1[1]
      tree_matrix[i, 2] <-  curr_tree$AG_carbon_m2[1]
      dt[i,2] <- curr_tree$dt[1]
      ba_gt[i, 2] <- curr_tree$subp_BA_GT_m1[1] 
      ndep[i, 2] <- curr_tree$Dep_N[1] 
    }
  }
  
  example_tree_matrix <- tree_matrix[!is.na(tree_matrix[,4]),]
  example_n_measures <- n_measures[!is.na(tree_matrix[,4])]
  example_ba_gt <- ba_gt[!is.na(tree_matrix[,4]),]
  example_dt <- dt[!is.na(tree_matrix[,4]),]
  example_ndep <- ndep[!is.na(tree_matrix[,4]),]
  
  ssData   <- list(tree_agb_obs = example_tree_matrix, 
                   ntrees = nrow(example_tree_matrix), 
                   n_measures = example_n_measures,
                   ba_gt = example_ba_gt,
                   dt = example_dt,
                   ndep = example_ndep,
                   max_ndep = max(c(example_ndep), na.rm = TRUE),
                   min_ndep = min(c(example_ndep), na.rm = TRUE),
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

  #global_tree_effect ~ dunif(-10,10)
  #tau_global ~ dunif(0.0001,10)
  procErr ~ dunif(0.0001,10)
  p1 ~ 
  p2 ~ dunif(0,2) 
  p3 ~ dunif(0,100) #Sample in log space
  p4 ~ dunif(min_ndep*0.5, max_ndep*2)
  p5 ~ dunif(0.3, 20)  
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

  for(n in 1:ntrees){

    tree_effect[n] ~ dnorm(global_tree_effect, tau_global)

  
    for(t in 2:n_measures[n]){
      tree_agb_mean[n, t] <-  tree_agb_latent[n, t-1] +  dt[n,t] * (tree_effect[n] * tree_agb_latent[n, t-1] ^ p2) 
        * exp(-ba_gt[n,t]*p3)  
        * exp(-0.5 * (log(ndep[n,t] / p4)/ p5) ^ 2)  
        #* exp(-p5inv2 * log(ndep[n,t] - lp4))  
        #* exp(-0.5 * (log(temp[n,t] / p6)/ p7) ^ 2)
        #* exp(-0.5 * (log(sdep[n,t] / p10)/ p11) ^ 2)
        #* 1/ (1 + ((sdep[n,t]-min_sdep)/p10)^p11)
        #* exp(-0.5 * (log(precip[n,t] / p8)/ p9) ^ 2)
        
      tree_agb_latent[n,t] ~ dnorm(tree_agb_mean[n, t], procErr/dt[n,t])
      tree_agb_obs[n,t] ~ dnorm(tree_agb_latent[n,t], obsErr)
    }
  }

  }  ", fill=T,file=paste0(sim,"-stateSpaceModel-",k,".txt"))
  
  cat( "model {

  global_tree_effect ~ dunif(-10,10)
  tau_global ~ dunif(0.0001,10)
  procErr ~ dunif(0.0001,10)
  p2 ~ dunif(0,2) 
  p3 ~ dunif(0,100) #Sample in log space
  p4 ~ dunif(min_ndep*0.5, max_ndep*2)
  p5 ~ dunif(0.3, 20)  
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

  for(n in 1:ntrees){

    tree_effect[n] ~ dnorm(global_tree_effect, tau_global)

  
    for(t in 2:n_measures[n]){
      tree_agb_mean[n, t] <-  tree_agb_latent[n, t-1] +  dt[n,t] * (tree_effect[n] * tree_agb_latent[n, t-1] ^ p2) 
        * exp(-ba_gt[n,t]*p3)  
        * exp(-0.5 * (log(ndep[n,t] / p4)/ p5) ^ 2)  
        #* exp(-p5inv2 * log(ndep[n,t] - lp4))  
        #* exp(-0.5 * (log(temp[n,t] / p6)/ p7) ^ 2)
        #* exp(-0.5 * (log(sdep[n,t] / p10)/ p11) ^ 2)
        #* 1/ (1 + ((sdep[n,t]-min_sdep)/p10)^p11)
        #* exp(-0.5 * (log(precip[n,t] / p8)/ p9) ^ 2)
        
      tree_agb_latent[n,t] ~ dnorm(tree_agb_mean[n, t], procErr/dt[n,t])
      tree_agb_obs[n,t] ~ dnorm(tree_agb_latent[n,t], obsErr)
    }
  }

  }  ", fill=T,file=paste0(sim,"-stateSpaceModel-",k,".txt"))
  
  
  inits <- list(list("global_tree_effect" = 1,
                     "tau_global" = 1,
                     "procErr" = 0.001,
                     "p2" = 0.6,
                     "p3" = 1,
                     "p4" = max(c(example_ndep), na.rm = TRUE), 
                     "p5" = 1.3),
                list("global_tree_effect" = 1,
                     "tau_global" = 1,
                     "procErr" = 0.001,
                     "p2" = 0.4,
                     "p3" = 1,
                     "p4" = 2 * min(c(example_ndep), na.rm = TRUE), 
                     "p5" = 2),
                list("global_tree_effect" = 1,
                     "tau_global" = 1,
                     "procErr" = 0.001,
                     "p2" = 0.4,
                     "p3" = 1,
                     "p4" = 0.5 * max(c(example_ndep), na.rm = TRUE), 
                     "p5" = 5))
  
  ssFit    <- jags.model(data=ssData, file=paste0(sim,"stateSpaceModel-",k,".txt"), n.chains = 3, inits = inits, n.adapt = 5000)
  parNames <- c("tree_effect", 
                "p2", 
                "p3", 
                "p4", "p5",
                "procErr", 
                "global_tree_effect")
  
  ssFit <- coda.samples(ssFit, variable.names = parNames, n.iter=15000, thin = 5)
  mcmc <- spread_draws(ssFit,
                       p2, 
                       p3, 
                       p4,p5,
                       procErr, 
                       `tree_effect[1]`,
                       `tree_effect[51]`,
                       `tree_effect[101]`,
                       global_tree_effect) 
  
  
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
  
  max_ndep = max(c(example_ndep), na.rm = TRUE)
  min_ndep = min(c(example_ndep), na.rm = TRUE)
  
  ndep_pred <- seq(min_ndep, max_ndep, length.out = 100)
  
  pred_post <- NULL
  
  mcmc_burned <- mcmc |> 
    filter(.iteration > 1000)
  
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
  
  
future::plan("future::multisession", workers = 4)

total_species <- length(unique(df$common_name))
furrr::future_walk(1:total_species, run_species, df, sim)
