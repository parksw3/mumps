library(rstan)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(egg)
load("../data_processed/iowa.rda")
load("../stanfit/stanfit_seirv_final.rda")
source("../R/simulate_seirv_final.R")

# extract posterior
ee <- rstan::extract(stanfit_seirv_final)

# number of posterior samples
npost <- length(ee$beta0)

# simulate without any intervention
none <- lapply(1:npost, function(x) {
  ss <- simulate_seirv_final(
    nday=length(iowa$date),
    N=20496,
    sigma=1/17,
    gamma=1/7,
    drate=1/7,
    nu=iowa$vax_doses/20496,
    holiday1=ifelse(iowa$date >= "2015-11-22" & iowa$date <= "2015-11-29", 1, 0), ## thanksgiving
    holiday2=ifelse(iowa$date >= "2015-12-11" & iowa$date <= "2015-12-28", 1, 0), ## winter break
    holiday3=ifelse(iowa$date >= "2016-01-15" & iowa$date <= "2016-01-19", 1, 0), ## between-semester
    holiday4=ifelse(iowa$date >= "2016-03-13" & iowa$date <= "2016-03-20", 1, 0), ## spring break
    beta0=ee$beta0[x],
    reduction1=0,
    reduction2=0,
    reduction3=0,
    reduction4=0,
    S0=ee$S0[x],
    E0=ee$E0[x],
    I0=ee$I0[x],
    reporting=ee$reporting[x],
    efficacy=0,
    efficacyD=ee$efficacyD[x],
    phi=ee$phi[x]
  )
  
  data.frame(
    date=iowa$date,
    incidence_S=ss$inf,
    incidence_V=ss$infV,
    incidence_P=ss$infP,
    cases=ss$cases_s+ss$cases_v
  )
}) %>%
  bind_rows(.id="sim")

# simulate with vaccination only
vacc <- lapply(1:npost, function(x) {
  ss <- simulate_seirv_final(
    nday=length(iowa$date),
    N=20496,
    sigma=1/17,
    gamma=1/7,
    drate=1/7,
    nu=iowa$vax_doses/20496,
    holiday1=ifelse(iowa$date >= "2015-11-22" & iowa$date <= "2015-11-29", 1, 0), ## thanksgiving
    holiday2=ifelse(iowa$date >= "2015-12-11" & iowa$date <= "2015-12-28", 1, 0), ## winter break
    holiday3=ifelse(iowa$date >= "2016-01-15" & iowa$date <= "2016-01-19", 1, 0), ## between-semester
    holiday4=ifelse(iowa$date >= "2016-03-13" & iowa$date <= "2016-03-20", 1, 0), ## spring break
    beta0=ee$beta0[x],
    reduction1=0,
    reduction2=0,
    reduction3=0,
    reduction4=0,
    S0=ee$S0[x],
    E0=ee$E0[x],
    I0=ee$I0[x],
    reporting=ee$reporting[x],
    efficacy=ee$efficacy[x],
    efficacyD=ee$efficacyD[x],
    phi=ee$phi[x]
  )
  
  data.frame(
    date=iowa$date,
    incidence_S=ss$inf,
    incidence_V=ss$infV,
    incidence_P=ss$infP,
    cases=ss$cases_s+ss$cases_v
  )
}) %>%
  bind_rows(.id="sim")

# simulate with holiday 1 only
holiday1 <- lapply(1:npost, function(x) {
  ss <- simulate_seirv_final(
    nday=length(iowa$date),
    N=20496,
    sigma=1/17,
    gamma=1/7,
    drate=1/7,
    nu=iowa$vax_doses/20496,
    holiday1=ifelse(iowa$date >= "2015-11-22" & iowa$date <= "2015-11-29", 1, 0), ## thanksgiving
    holiday2=ifelse(iowa$date >= "2015-12-11" & iowa$date <= "2015-12-28", 1, 0), ## winter break
    holiday3=ifelse(iowa$date >= "2016-01-15" & iowa$date <= "2016-01-19", 1, 0), ## between-semester
    holiday4=ifelse(iowa$date >= "2016-03-13" & iowa$date <= "2016-03-20", 1, 0), ## spring break
    beta0=ee$beta0[x],
    reduction1=ee$reduction1[x],
    reduction2=0,
    reduction3=0,
    reduction4=0,
    S0=ee$S0[x],
    E0=ee$E0[x],
    I0=ee$I0[x],
    reporting=ee$reporting[x],
    efficacy=0,
    efficacyD=ee$efficacyD[x],
    phi=ee$phi[x]
  )
  
  data.frame(
    date=iowa$date,
    incidence_S=ss$inf,
    incidence_V=ss$infV,
    incidence_P=ss$infP,
    cases=ss$cases_s+ss$cases_v
  )
}) %>%
  bind_rows(.id="sim")

# simulate with holiday 2 only
holiday2 <- lapply(1:npost, function(x) {
  ss <- simulate_seirv_final(
    nday=length(iowa$date),
    N=20496,
    sigma=1/17,
    gamma=1/7,
    drate=1/7,
    nu=iowa$vax_doses/20496,
    holiday1=ifelse(iowa$date >= "2015-11-22" & iowa$date <= "2015-11-29", 1, 0), ## thanksgiving
    holiday2=ifelse(iowa$date >= "2015-12-11" & iowa$date <= "2015-12-28", 1, 0), ## winter break
    holiday3=ifelse(iowa$date >= "2016-01-15" & iowa$date <= "2016-01-19", 1, 0), ## between-semester
    holiday4=ifelse(iowa$date >= "2016-03-13" & iowa$date <= "2016-03-20", 1, 0), ## spring break
    beta0=ee$beta0[x],
    reduction1=0,
    reduction2=ee$reduction2[x],
    reduction3=0,
    reduction4=0,
    S0=ee$S0[x],
    E0=ee$E0[x],
    I0=ee$I0[x],
    reporting=ee$reporting[x],
    efficacy=0,
    efficacyD=ee$efficacyD[x],
    phi=ee$phi[x]
  )
  
  data.frame(
    date=iowa$date,
    incidence_S=ss$inf,
    incidence_V=ss$infV,
    incidence_P=ss$infP,
    cases=ss$cases_s+ss$cases_v
  )
}) %>%
  bind_rows(.id="sim")

# simulate with holiday 3 only
holiday3 <- lapply(1:npost, function(x) {
  ss <- simulate_seirv_final(
    nday=length(iowa$date),
    N=20496,
    sigma=1/17,
    gamma=1/7,
    drate=1/7,
    nu=iowa$vax_doses/20496,
    holiday1=ifelse(iowa$date >= "2015-11-22" & iowa$date <= "2015-11-29", 1, 0), ## thanksgiving
    holiday2=ifelse(iowa$date >= "2015-12-11" & iowa$date <= "2015-12-28", 1, 0), ## winter break
    holiday3=ifelse(iowa$date >= "2016-01-15" & iowa$date <= "2016-01-19", 1, 0), ## between-semester
    holiday4=ifelse(iowa$date >= "2016-03-13" & iowa$date <= "2016-03-20", 1, 0), ## spring break
    beta0=ee$beta0[x],
    reduction1=0,
    reduction2=0,
    reduction3=ee$reduction3[x],
    reduction4=0,
    S0=ee$S0[x],
    E0=ee$E0[x],
    I0=ee$I0[x],
    reporting=ee$reporting[x],
    efficacy=0,
    efficacyD=ee$efficacyD[x],
    phi=ee$phi[x]
  )
  
  data.frame(
    date=iowa$date,
    incidence_S=ss$inf,
    incidence_V=ss$infV,
    incidence_P=ss$infP,
    cases=ss$cases_s+ss$cases_v
  )
}) %>%
  bind_rows(.id="sim")

# simulate with holiday 4 only
holiday4 <- lapply(1:npost, function(x) {
  ss <- simulate_seirv_final(
    nday=length(iowa$date),
    N=20496,
    sigma=1/17,
    gamma=1/7,
    drate=1/7,
    nu=iowa$vax_doses/20496,
    holiday1=ifelse(iowa$date >= "2015-11-22" & iowa$date <= "2015-11-29", 1, 0), ## thanksgiving
    holiday2=ifelse(iowa$date >= "2015-12-11" & iowa$date <= "2015-12-28", 1, 0), ## winter break
    holiday3=ifelse(iowa$date >= "2016-01-15" & iowa$date <= "2016-01-19", 1, 0), ## between-semester
    holiday4=ifelse(iowa$date >= "2016-03-13" & iowa$date <= "2016-03-20", 1, 0), ## spring break
    beta0=ee$beta0[x],
    reduction1=0,
    reduction2=0,
    reduction3=0,
    reduction4=ee$reduction4[x],
    S0=ee$S0[x],
    E0=ee$E0[x],
    I0=ee$I0[x],
    reporting=ee$reporting[x],
    efficacy=0,
    efficacyD=ee$efficacyD[x],
    phi=ee$phi[x]
  )
  
  data.frame(
    date=iowa$date,
    incidence_S=ss$inf,
    incidence_V=ss$infV,
    incidence_P=ss$infP,
    cases=ss$cases_s+ss$cases_v
  )
}) %>%
  bind_rows(.id="sim")

# evaluate effects on cases
effects_inclusion_cases <- bind_rows(
  none %>% 
    group_by(date) %>% 
    summarize(
      median=median(cases), 
      lwr=quantile(cases, 0.025), 
      upr=quantile(cases, 0.975),
      type="Baseline (excluding all interventions)"
    ),
  vacc %>% 
    group_by(date) %>% 
    summarize(
      median=median(cases), 
      lwr=quantile(cases, 0.025), 
      upr=quantile(cases, 0.975),
      type="Including vaccination"
    ),
  holiday1 %>% 
    group_by(date) %>% 
    summarize(
      median=median(cases), 
      lwr=quantile(cases, 0.025), 
      upr=quantile(cases, 0.975),
      type="Including Thanksgiving"
    ),
  holiday2 %>% 
    group_by(date) %>% 
    summarize(
      median=median(cases), 
      lwr=quantile(cases, 0.025), 
      upr=quantile(cases, 0.975),
      type="Including winter break"
    ),
  holiday3 %>% 
    group_by(date) %>% 
    summarize(
      median=median(cases), 
      lwr=quantile(cases, 0.025), 
      upr=quantile(cases, 0.975),
      type="Including between-term break"
    ),
  holiday4 %>% 
    group_by(date) %>% 
    summarize(
      median=median(cases), 
      lwr=quantile(cases, 0.025), 
      upr=quantile(cases, 0.975),
      type="Including spring break"
    )
)

# evaluate effects on final size
effects_inclusion_finalsize <- bind_rows(
  none %>% 
    group_by(sim) %>% 
    summarize(
      finalsize=sum(incidence_S+incidence_V+incidence_P),
      type="Baseline (excluding all interventions)"
    ),
  vacc %>% 
    group_by(sim) %>% 
    summarize(
      finalsize=sum(incidence_S+incidence_V+incidence_P),
      type="Including vaccination"
    ),
  holiday1 %>% 
    group_by(sim) %>% 
    summarize(
      finalsize=sum(incidence_S+incidence_V+incidence_P),
      type="Including Thanksgiving"
    ),
  holiday2 %>% 
    group_by(sim) %>% 
    summarize(
      finalsize=sum(incidence_S+incidence_V+incidence_P),
      type="Including winter break"
    ),
  holiday3 %>% 
    group_by(sim) %>% 
    summarize(
      finalsize=sum(incidence_S+incidence_V+incidence_P),
      type="Including between-term break"
    ),
  holiday4 %>% 
    group_by(sim) %>% 
    summarize(
      finalsize=sum(incidence_S+incidence_V+incidence_P),
      type="Including spring break"
    )
)

save(effects_inclusion_cases, effects_inclusion_finalsize,
     file="effects_inclusion.rda")
