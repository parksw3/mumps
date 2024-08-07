# Modeling the population-level impact of a third dose of MMR vaccine on a mumps outbreak at the University of Iowa

------

This repository contains all code used for the analysis.

* `R`: contains R scripts for combining axes (for plotting) and simulating the main model
* `analysis_seirv_final`: contains R scripts for exclusion/inclusion analyses and vaccine scenario simulations
* `doc`: contains latex files for the manuscript
* `figure`: contains R scripts for plotting model diagram (Figure S1) and time series (Figure 1)
* `figure_stanfit_seirv_final`: contains all other figures presented in the paper
  * `figure_stanfit_Rt_comp.R`: R script for estimating Rt using the Wallinga-Teunis approach and plotting (Figure S3)
  * `figure_stanfit_effects.R`: R script for summarizing and plotting the results of exclusion and inclusion analyses (Figure 3, S7)
  * `figure_stanfit_param_all.R`: R script for plotting posterior distributions for the main model for different delays between vaccination and protection (Figure S4)
  * `figure_stanfit_param_avg.R`: R script for plotting posterior distributions for the model that assumes a single holiday effect term (Figure S5)
  * `figure_stanfit_strategy.R`: R scripts for plotting vaccine strategy simulations (Figure 4, S8)
  * `figure_stanfit_total.R`: R script for plotting the total number of cases among second and third dose recipients (Figure S2)
  * `figure_stanfit_trajectory.R`: R script for plotting the model fit (Figure 2)
* `pomp`: contains R scripts for evaluating the likelihood using the stochastic model
  * `pomp_model.R`: R script for setting up the POMP model
  * `pomp_loglik.R`: R script for evaluating the likelihood using the stochastic model
  * `figure_pomp_loglik.R`: R script for calculating the profile likelihood and plotting
* `stanfit`: contains R scripts for fitting deterministic models using rstan
  * `stanfit_seirv_final.R`: fitting deterministic models assuming a 7-day protection between vaccination and protection
  * `stanfit_seirv_final_14.R`: fitting deterministic models assuming a 14-day protection between vaccination and protection
  * `stanfit_seirv_final_21.R`: fitting deterministic models assuming a 21-day protection between vaccination and protection
  * `stanfit_seirv_avg.R`: fitting deterministic models assuming a single holiday effect
* `stanmodel`: constrains stan scripts for deterministic models 
  * `seirv_final.R`: stan script for the main model
  * `seirv_avg.R`: stan script for the supplementary model that assumes a single holiday effect