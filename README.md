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

