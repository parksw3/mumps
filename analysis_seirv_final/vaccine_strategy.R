library(rstan)
library(dplyr)
load("../data_processed/iowa.rda")
load("../stanfit/stanfit_seirv_final.rda")
source("../R/simulate_seirv_final.R")

ee <- rstan::extract(stanfit_seirv_final)

npost <- length(ee$beta0)
post <- seq(from=1, by=10, length.out=800)

efficacy <- c(20, 40, 60, 80)/100
coverage <- c(20, 40, 60)/100
timing <- c(0, 40, 80)
delay <- c(7, 14, 21)

strategy <- expand.grid(efficacy, coverage, timing, delay)

vaccine_strategy <- vector('list', nrow(strategy))

for (i in 1:nrow(strategy)) {
  print(i)
  eff <- strategy[i,1]
  cc <- strategy[i,2]
  tt <- strategy[i,3]
  delay <- strategy[i,4]
  
  simlist <- vector('list', npost)
  
  for (j in 1:length(post)) {
    x <- post[j]
    if (tt==0) {
      S0 <- ee$S0[x] * (1-cc)
      nu <- rep(0, nrow(iowa))
      V0 <- ee$S0[x] * cc
    } else {
      S0 <- ee$S0[x]
      nu <- rep(0, nrow(iowa))
      nu[tt] <- -log(1-cc)
      V0 <- 0
    }
    
    ss <- simulate_seirv_final(
      nday=length(iowa$date),
      N=20496,
      sigma=1/17,
      gamma=1/7,
      drate=1/delay,
      nu=nu,
      holiday1=ifelse(iowa$date >= "2015-11-22" & iowa$date <= "2015-11-29", 1, 0), ## thanksgiving
      holiday2=ifelse(iowa$date >= "2015-12-11" & iowa$date <= "2015-12-28", 1, 0), ## winter break
      holiday3=ifelse(iowa$date >= "2016-01-15" & iowa$date <= "2016-01-19", 1, 0), ## between-semester
      holiday4=ifelse(iowa$date >= "2016-03-13" & iowa$date <= "2016-03-20", 1, 0), ## spring break
      beta0=ee$beta0[x],
      reduction1=ee$reduction1[x],
      reduction2=ee$reduction2[x],
      reduction3=ee$reduction3[x],
      reduction4=ee$reduction4[x],
      S0=S0,
      E0=ee$E0[x],
      I0=ee$I0[x],
      V0=V0,
      reporting=ee$reporting[x],
      efficacy=eff,
      efficacyD=ee$efficacyD[x],
      phi=ee$phi[x]
    )
    
    simlist[[j]] <- data.frame(
      date=iowa$date,
      incidence=ss$inf+ss$infP+ss$infV,
      cincidence=cumsum(ss$inf+ss$infP+ss$infV),
      cases=ss$cases_s+ss$cases_v,
      efficacy=eff,
      coverage=cc,
      timing=tt,
      delay=delay,
      sim=x
    )
  }
  
  vaccine_strategy[[i]] <- simlist %>%
    bind_rows
}

vaccine_strategy <- vaccine_strategy %>%
  bind_rows %>%
  as_tibble

save("vaccine_strategy", file="vaccine_strategy.rda")
