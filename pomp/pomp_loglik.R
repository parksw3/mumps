library(dplyr)
library(pomp)
library(rstan)
source("pomp_model.R")

load("../data_processed/iowa.rda")
load("../stanfit/stanfit_seirv_final.rda")

post <- extract(stanfit_seirv_final)

nsamp <- 8000
set.seed(101)
which_post <- sample(8000, nsamp)

cases <- iowa$onset
cases[1] <- NA

fitdata <- data.frame(
  time=1:nrow(iowa),
  nu=iowa$vax_doses/20496,
  holiday1=ifelse(iowa$date >= "2015-11-22" & iowa$date <= "2015-11-29", 1, 0), ## thanksgiving
  holiday2=ifelse(iowa$date >= "2015-12-11" & iowa$date <= "2015-12-28", 1, 0), ## winter break
  holiday3=ifelse(iowa$date >= "2016-01-15" & iowa$date <= "2016-01-19", 1, 0), ## between-semester
  holiday4=ifelse(iowa$date >= "2016-03-13" & iowa$date <= "2016-03-20", 1, 0), ## spring break
  cases=cases
)

pomp_arg2 <- append(pomp_arg,
                   list(data=fitdata[,c("time", "cases")],
                        t0=1,
                        covar=covariate_table(fitdata[,c("time", "nu", "holiday1", "holiday2", "holiday3", "holiday4")],
                                              times="time")))

stoch_model <- do.call(pomp, pomp_arg2)

reslist <- vector('list', nsamp)
for (i in 1:nsamp) {
  print(i)
  ww <- which_post[i]
  
  beta0 <- post$beta0[ww]
  reduction1 <- post$reduction1[ww]
  reduction2 <- post$reduction2[ww]
  reduction3 <- post$reduction3[ww]
  reduction4 <- post$reduction4[ww]
  S0 <- post$S0[ww]
  E0 <- post$E0[ww]
  I0 <- post$I0[ww]
  efficacy <- post$efficacy[ww]
  efficacyD <- post$efficacyD[ww]
  reporting <- post$reporting[ww]
  phi <- post$phi[ww]
  
  params <- c(beta0=beta0,
              reduction1=reduction1,
              reduction2=reduction2,
              reduction3=reduction3,
              reduction4=reduction4,
              S0=S0,
              E0=E0,
              I0=I0,
              efficacy=efficacy,
              N=20496,
              sigma=1/17,
              gamma=1/7,
              drate=1/7,
              efficacyD=efficacyD,
              reporting=reporting,
              phi=phi)
  
  ss <- simulate(stoch_model,
                 params=params)
  
  pp <- pfilter(stoch_model,
                Np=2000,params=params)
  
  reslist[[i]] <- data.frame(
    logLik=logLik(pp),
    beta0=beta0,
    reduction1=reduction1,
    reduction2=reduction2,
    reduction3=reduction3,
    reduction4=reduction4,
    S0=S0,
    E0=E0,
    I0=I0,
    efficacy=efficacy,
    efficacyD=efficacyD,
    reporting=reporting,
    phi=phi
  )
}

pomp_loglik <- reslist %>%
  bind_rows

save("pomp_loglik", file="pomp_loglik.rda")
