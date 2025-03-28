# nps_ndep_remeasure

## Guide to folders:

**data**: contains all data used for project

**experiments**: contains results from modeling experiments, including model txt files, model output as parquet files, and default model output visualizations

**modeling_code**: workflow code used to run different modeling experiments

**other_code**: code used, e.g., for data exploration or analysis of model output

**visualizations**: all visualizations other than the default model output visualizations housed in the *experiments* folder

## Guide to experiments, listed in chronological order of when they were performed: 

**individual_effect**: state-space model with individual tree effect only

**plot_effect**: state-space model with plot effect only

**individual_and_plot_effect**: state-space model with both individual and plot effects

**model_growth_directly**: modeling growth directly (not a state space model) with both individual and plot effects

**broader_p4_prior**: modeling growth directly (not a state space model) with both individual and plot effects and a broader p4 prior (dunif(-1000,1000)) to avoid constraining the lognormal curve

**linear_model**: modeling growth directly with a linear rather than lognormal term for the effect of N deposition on growth, where the effect of N deposition is added to the nested individual/plot effect

**quadratic_model**: modeling growth directly with a quadratic rather than lognormal term for the effect of N deposition on growth, where the effect of N deposition is added to the nested individual/plot effect 

**delta_Ndep**: modeling growth directly with a linear term for the historic rate of N deposition, which is N deposition in the 15 years prior to the first measurement for a tree, as well as a linear term for delta Ndep, which is the N deposition rate at the current measurement - the historic rate

**delta_Ndep_only**: modeling growth directly with a linear term for delta Ndep, which is the N deposition rate at the current measurement - the historic rate; the separate linear term for the historic rate of N deposition is omitted from this model

**delta_env**: modeling growth directly with a linear term for delta Ndep as well as similar linear terms for deltas of air temperature, precip, and sulfur deposition; deltas are calculated differently from previous models; deltas are now calculated as the difference from the mean of all the observations of that variable for a given tree, rather than as a difference from a 'historical level'

**new_delta_Ndep_only**: modeling growth directly with a linear term for delta Ndep; deltas are calculated differently from previous models; deltas are now calculated as the difference from the mean of all the observations of that variable for a given tree, rather than as a difference from a 'historical level'

**N_species**: modeling growth directly with a linear term for delta Ndep, with Ndep broken out into Ndep from oxidized and reduced forms of nitrogen; all other environmental variables are also included (S, temp, precip)

