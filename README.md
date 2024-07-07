# Modeling the population-level impact of a third dose of MMR vaccine on a mumps outbreak at the University of Iowa

------

This repository contains all code used for the analysis.

* `R`: contains R scripts for combining axes (for plotting) and simulating the main model
* `analysis_seirv_final`: contains R scripts for exclusion/inclusion analyses and vaccine strategy simulations
* `doc`: contains latex files for the manuscript
* `figure`: contains diagram and time series figures
* `figure_stanfit_seirv_final`: contains all other figures presented in the paper
  * `figure_stanfit_Rt_comp.R`: contains R scripts for estimating Rt using the Wallinga-Teunis approach and plotting
  * `figure_stanfit_effects.R`: contains R scripts for summarizing and plotting the results of exclusion and inclusion analyses
  * `figure_stanfit_param_all.R`: contains R scripts for plotting posterior distributions for the main model for different delays between vaccination and protection
  * `figure_stanfit_param_avg.R`: contains R scripts for plotting posterior distributions for the model that assumes a single holiday effect term
  * `figure_stanfit_strategy.R`: contrains 
* `pomp`: contains R scripts for evaluating the likelihood using the stochastic model
* `stanfit`: contains R scripts for fitting deterministic models using rstan
* `stanmodel`: constrains stan scripts for deterministic models 