# DRAFT Code repository for Lofton et al. 2026, "INSERT SUBMITTED MANUSCRIPT TITLE HERE"
Mary E. Lofton<sup>1</sup>, Michael Schwob<sup>2</sup>, Michael Bell<sup>3</sup>, Christopher Clark<sup>4</sup>, Emmi Felker-Quinn<sup>3</sup>, Todd McDonnell<sup>5</sup>, R. Quinn Thomas<sup>1</sup> 

*<sup>1</sup>Center for Ecosystem Forecasting, Virginia Tech, Blacksburg, VA, USA*<br>
*<sup>2</sup>Department of Statistics, Virginia Tech, Blacksburg, VA, USA*<br>
*<sup>3</sup>US National Park Service Air Resources Division, Denver, CO, USA*<br> 
*<sup>4</sup>US Environmental Protection Agency Office of Research and Development, Washington DC, USA*<br>
*<sup>5</sup>E&S Environmental, Corvallis, OR, USA* 

### Accessing data associated with this repository:
This section will be developed once we discuss a data publication plan with co-authors.

### Guide to files and folders:

**data**: contains all data used for project

**experiments**: contains results from modeling experiments, including model txt files, model output as parquet files, and default model output visualizations

**figure_code**: code used to generate figures used either in the main manuscript or the supplement

**install.R**: R code to install and load packages (libraries) that are needed to run the models and generate figures

**modeling_code**: workflow code used to run different modeling experiments

**other_code**: code used, e.g., for data exploration or figures for presentations

**visualizations**: all visualizations other than the default model output visualizations housed in the *experiments* folder

### Guide for reviewers:

#### Recommended workflow for review

1. Run `install.R` to be sure you have the necessary libraries loaded.
2. Because the Bayesian hierarchical models are compute-intensive (we used a high-performance cluster to complete model runs for the manuscript), we recommend to run the example model for 100 individuals for one tree species, located in `modeling_code/run_model_example.R` to review the modeling workflow. This script will load USFS FIA data from our data publication, generate a `.csv` file of processed data which will be needed to generate the final figures for the manuscript, and do an example run of the Bayesian hierarchical model for 100 individuals of *Prunus serotina*.
3. Complete all analyses and generate final figures in the manuscript using the scripts in the `figure_code` file. Specifically, to generate the manuscript figure, run:
    a. `Fig1.R`
    b. `Fig2.R`
    c. `Fig3.R`
    d. `Fig4.R`
    e. `Fig5.R`
    
#### Funding
Funding for this project was provided by the U.S. National Park Service.

#### Disclaimer text
Is there anything here that is needed?