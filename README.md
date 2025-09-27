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

**N_species_saveTreeEffect**: same as *N_species* model but saves all tree effects
to permit assessment of model performance across all individuals

**new_delta_Ndep_only_saveTreeEffect**: same as *new_delta_Ndep_only* but saves all tree effects to assessment of model performance across all individuals

**delta_env_saveTreeEffect**: same as *delta_env* but saves all tree effects to permit assessment of model performance across all individuals

**ozone**: modeling growth directly with a linear term for delta Ndep, which is calculated as the difference between Ndep during the current interval and Ndep averaged across all intervals for a plot, with Ndep broken out into Ndep from oxidized and reduced forms of nitrogen; delta terms for all other environmental variables are also included (S, temp, precip, and ozone)

**unpacking_plot_effect**: same as *ozone* but includes terms for interval means
of reduced and oxidized N deposition in addition to all the delta terms

**historic_deviation**: modeling growth directly with a linear term for the historic rate of N deposition, which is N deposition in the 15 years prior to the first measurement for a plot, as well as a linear term for delta Ndep, which is the N deposition rate at the current measurement - the historic rate; also saves tree effects and models N as two species, reduced and oxidized

**historic_deviation_interaction**: modeling growth directly with a linear term for the historic rate of N deposition, which is N deposition in the 15 years prior to the first measurement for a plot, as well as a linear term for delta Ndep, which is the N deposition rate at the current measurement - the historic rate, as well as a linear term for the interaction between the baseline and the deviation (beta* deviation * baseline); also saves tree effects and models N as two species, reduced and oxidized

**historic_deviation_S**: modeling growth directly with a linear term for the historic rate of N deposition, which is N deposition in the 15 years prior to the first measurement for a plot, as well as a linear term for delta Ndep, which is the N deposition rate at the current measurement - the historic rate; also saves tree effects and models N as two species, reduced and oxidized; also includes linear terms for historic rates of S deposition and delta Sdep

**baseline_proportion**: modeling growth directly with a linear term for delta Ndep as well as similar linear terms for deltas of air temperature, precip, and sulfur deposition; deltas are calculated differently from previous models; deltas are now calculated as the difference from the mean of all the observations of that variable for a given tree, rather than as a difference from a 'historical level'; importantly, this model includes a linear term relating growth to the 'baseline proportion' of N deposition, where the baseline is N deposition in the 15 years prior to the first measurement for a plot and the proportion is calculated by dividing the current interval measurement of N deposition by that baseline

**baseline_proportion_N_species**: modeling growth directly with a linear term for delta Ndep as well as similar linear terms for deltas of air temperature, precip, and sulfur deposition; deltas are calculated differently from previous models; deltas are now calculated as the difference from the mean of all the observations of that variable for a given tree, rather than as a difference from a 'historical level'; importantly, this model includes a linear term relating growth to the 'baseline proportion' of N deposition, where the baseline is N deposition in the 15 years prior to the first measurement for a plot and the proportion is calculated by dividing the current interval measurement of N deposition by that baseline; in this model, N deposition in broken out into oxidized and reduced N deposition

**baseline_prop_interaction**: modeling growth directly with a linear term for delta Ndep as well as similar linear terms for deltas of air temperature, precip, and sulfur deposition; deltas are calculated differently from previous models; deltas are now calculated as the difference from the mean of all the observations of that variable for a given tree, rather than as a difference from a 'historical level'; importantly, this model includes a linear term relating growth to the 'baseline proportion' of N deposition, where the baseline is N deposition in the 15 years prior to the first measurement for a plot and the proportion is calculated by dividing the current interval measurement of N deposition by that baseline; in addition to this baseline term, there are three more linear terms for the interactions between: 1) N deposition deviation and mean S deposition across the entire measurement period of a plot; 2) N deposition deviation and mean air temperature across the entire measurement period of a plot; 3) N deposition deviation and mean precipitation across the entire measurement period of a plot

**short-term_long-term**: modeling growth directly with three linear terms representing N deposition: 1) "baseline" N deposition, which is the N deposition from the fifteen years prior to the first measurement of a plot; 2) "long-term" change N deposition, which is mean N deposition across all the measurement intervals of a plot within our dataset subtracted from the baseline; and 3) "short-term" change in N deposition, which is N deposition during the current measurement interval subtracted from the mean N deposition across all the measurement intervals of a plot

**space_vs_time**: modeling growth directly with two linear terms representing N deposition: 1) "spatial" N deposition, which is the weighted average of N deposition from the fifteen years prior to the first measurement as well as N deposition of all measurement intervals up to the current interval; 2) "temporal" N deposition, which is the difference between N deposition during the current interval and the weighted average of previous N deposition; delta terms for other environmental variables (S deposition, temperature, precipitation) are also included

**space_vs_time_ortho**: modeling growth directly with two linear terms representing N deposition: 1) "spatial" N deposition, which is the antecedent N deposition for the 15 years prior to the current interval; 2) "temporal" N deposition, which is the difference between the current interval N deposition and antecedent N deposition; there are also two equivalent terms for S deposition; moreover, the terms for N and S deposition are all orthogonalized with respect to each other following Michael Schwob's email on 18SEP25 with an approach for double orthogonalization