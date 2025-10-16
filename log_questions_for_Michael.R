# Option 1: term with tree effect and environmental covariates is not logged
# this is the model we are currently using

cat( "model {

  global_tree_effect ~ dunif(-10,10)
  tau_global ~ dunif(0.0001,10)
  tau_plot ~ dunif(0.0001,10)
  procErr ~ dgamma(1,1) 
  nu ~ dexp(1/30) T(2,) # truncated exponential
  p2 ~ dunif(0,2) 
  p3 ~ dunif(0,100) 
  p5 ~ dnorm(0,0.001)
  p6 ~ dnorm(0,0.001)
  p7 ~ dnorm(0,0.001)
  p8 ~ dnorm(0,0.001)
  p9 ~ dnorm(0,0.001)
  p10 ~ dnorm(0,0.001)
  p11 ~ dnorm(0,0.001)
  p12 ~ dnorm(0,0.001)
  
  for(p in 1:n_plots){
    plot_effect[p] ~ dnorm(global_tree_effect, tau_global)
  }

  for(n in 1:ntrees){
    tree_effect[n] ~ dnorm(plot_effect[plot_index[n]], tau_plot)
  }
  
  for(t in 1:n_measures){

     log_tree_growth_mean[t] <-  ((tree_effect[tree_index[t]] + ndep_diff[t]*p5 + sdep_diff[t]*p6 + mat_delta[t]*p7 + map_delta_dm[t]*p8 + ndep_historic[t]*p9 + sdep_historic[t]*p10 + ndep_historic[t]*ndep_diff[t]*p11 + ndep_diff[t]*ndep_diff[t]*p12) 
     + p2*log_tree_agb_obs[t]) 
         - p3*log_ba_gt[t] 

    lambda[t] ~ dgamma(nu/2, nu/2)

    log_tree_growth_obs[t] ~ dnorm(log_tree_growth_mean[t], procErr*lambda[t]) # multiplied b/c JAGS uses precision
  
  }

  }  ", fill=T,file=paste0("./experiments/",sim,"/",sim,"-growthModel-",tree_species,".txt"))

# Option 2: term with tree effect and environmental covariates is logged

cat( "model {

  global_tree_effect ~ dunif(-10,10)
  tau_global ~ dunif(0.0001,10)
  tau_plot ~ dunif(0.0001,10)
  procErr ~ dgamma(1,1) 
  nu ~ dexp(1/30) T(2,) # truncated exponential
  p2 ~ dunif(0,2) 
  p3 ~ dunif(0,100) 
  p5 ~ dnorm(0,0.001)
  p6 ~ dnorm(0,0.001)
  p7 ~ dnorm(0,0.001)
  p8 ~ dnorm(0,0.001)
  p9 ~ dnorm(0,0.001)
  p10 ~ dnorm(0,0.001)
  p11 ~ dnorm(0,0.001)
  p12 ~ dnorm(0,0.001)
  
  for(p in 1:n_plots){
    plot_effect[p] ~ dnorm(global_tree_effect, tau_global)
  }

  for(n in 1:ntrees){
    tree_effect[n] ~ dnorm(plot_effect[plot_index[n]], tau_plot)
  }
  
  for(t in 1:n_measures){

     log_tree_growth_mean[t] <-  log((tree_effect[tree_index[t]] + ndep_diff[t]*p5 + sdep_diff[t]*p6 + mat_delta[t]*p7 + map_delta_dm[t]*p8 + ndep_historic[t]*p9 + sdep_historic[t]*p10 + ndep_historic[t]*ndep_diff[t]*p11 + ndep_diff[t]*ndep_diff[t]*p12) 
     + p2*log_tree_agb_obs[t]) 
         - p3*log_ba_gt[t] 

    lambda[t] ~ dgamma(nu/2, nu/2)

    log_tree_growth_obs[t] ~ dnorm(log_tree_growth_mean[t], procErr*lambda[t]) # multiplied b/c JAGS uses precision
  
  }

  }  ", fill=T,file=paste0("./experiments/",sim,"/",sim,"-growthModel-",tree_species,".txt"))

# Option 3: predicted growth is logged before being fed to observation model

cat( "model {

  global_tree_effect ~ dunif(-10,10)
  tau_global ~ dunif(0.0001,10)
  tau_plot ~ dunif(0.0001,10)
  procErr ~ dgamma(1,1) 
  nu ~ dexp(1/30) T(2,) # truncated exponential
  p2 ~ dunif(0,2) 
  p3 ~ dunif(0,100) 
  p5 ~ dnorm(0,0.001)
  p6 ~ dnorm(0,0.001)
  p7 ~ dnorm(0,0.001)
  p8 ~ dnorm(0,0.001)
  p9 ~ dnorm(0,0.001)
  p10 ~ dnorm(0,0.001)
  p11 ~ dnorm(0,0.001)
  p12 ~ dnorm(0,0.001)
  
  for(p in 1:n_plots){
    plot_effect[p] ~ dnorm(global_tree_effect, tau_global)
  }

  for(n in 1:ntrees){
    tree_effect[n] ~ dnorm(plot_effect[plot_index[n]], tau_plot)
  }
  
  for(t in 1:n_measures){

     tree_growth_mean[t] <-  ((tree_effect[tree_index[t]] + ndep_diff[t]*p5 + sdep_diff[t]*p6 + mat_delta[t]*p7 + map_delta_dm[t]*p8 + ndep_historic[t]*p9 + sdep_historic[t]*p10)
    * tree_agb_obs[t] ^ p2) 
        * exp(-ba_gt[t]*p3)
        
    log_tree_growth_mean[t] <- log(tree_growth_mean[t])

    lambda[t] ~ dgamma(nu/2, nu/2)

    log_tree_growth_obs[t] ~ dnorm(log_tree_growth_mean[t], procErr*lambda[t]) # multiplied b/c JAGS uses precision
  
  }

  }  ", fill=T,file=paste0("./experiments/",sim,"/",sim,"-growthModel-",tree_species,".txt"))
