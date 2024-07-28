simulate_seirv_final <- function(nday, # number of days simulated
                                 N, # population size
                                 sigma, # 1/(exposed period)
                                 gamma, # 1/(infectious period)
                                 drate, # vaccination rate
                                 nu, # 1/(time from vaccination to protection)
                                 holiday1, # indicator variable for holiday 1
                                 holiday2, # indicator variable for holiday 2
                                 holiday3, # indicator variable for holiday 3
                                 holiday4, # indicator variable for holiday 4
                                 beta0, # transmission rate
                                 reduction1, # reduction in transmission caused by holiday 1
                                 reduction2, # reduction in transmission caused by holiday 2
                                 reduction3, # reduction in transmission caused by holiday 3
                                 reduction4, # reduction in transmission caused by holiday 4
                                 S0, # initial proportion susceptible due to waned immunity
                                 E0, # initial proportion exposed
                                 I0, # initial proportion infectious
                                 V0=0, # initial proportion vaccinated but unprotected
                                 P0=0, # initial proportion protected
                                 reporting, # reporting probability
                                 efficacy, # VE against infection
                                 efficacyD, # VE against disease
                                 phi) { # negative binomial dispersion
  S <- Es <- Is <- Ev <- Iv <- Ep <- Ip <- R <- V <- P <- betat <- onsets <- recoverys <- onsetv <- recoveryv <- onsetp <- recoveryp <- foi <- inf <- vacc <- infV <- prot <- infP <- rep(0, nday)
  
  S[1] <- N * S0
  Es[1] <- N * E0
  Is[1] <- N * I0
  Ev[1] <- 0
  Iv[1] <- 0
  Ep[1] <- 0
  Ip[1] <- 0
  R[1] <- N * (1- S0 - E0 - I0)
  V[1] <- N * V0
  P[1] <- N * P0
  betat[1] <- beta0
  onsets[1] <- 0;
  recoverys[1] <- 0;
  onsetv[1] <- 0;
  recoveryv[1] <- 0;
  onsetp[1] <- 0;
  recoveryp[1] <- 0;
  foi[1] <- 0
  inf[1] <- 0
  vacc[1] <- 0
  infV[1] <- 0
  prot[1] <- 0
  infP[1] <- 0
  
  for (t in 2:nday) {
    betat[t] = beta0 * (1 - reduction1 * holiday1[t] - reduction2 * holiday2[t] - reduction3 * holiday3[t] - reduction4 * holiday4[t])
    
    foi[t] = betat[t] * (Is[t-1]+Iv[t-1]+Ip[t-1]+1e-301)/N
    inf[t] = foi[t]/(foi[t] + nu[t]) * (1 - exp(-(foi[t]+nu[t]))) * S[t-1]
    vacc[t] = nu[t]/(foi[t] + nu[t]) * (1 - exp(-(foi[t]+nu[t]))) * S[t-1]
    
    infV[t] = foi[t]/(foi[t]+drate) * (1 - exp(-(foi[t]+drate))) * V[t-1]
    prot[t] = drate/(foi[t]+drate) * (1 - exp(-(foi[t]+drate))) * V[t-1]
    
    infP[t] = (1 - exp(-((1-efficacy)*foi[t]))) * P[t-1]
    
    onsets[t] = (1 - exp(-sigma)) * Es[t-1]
    recoverys[t] = (1 - exp(-gamma)) * Is[t-1]
    onsetv[t] = (1 - exp(-sigma)) * Ev[t-1]
    recoveryv[t] = (1 - exp(-gamma)) * Iv[t-1]
    onsetp[t] = (1 - exp(-sigma)) * Ep[t-1]
    recoveryp[t] = (1 - exp(-gamma)) * Ip[t-1]
    
    S[t] = S[t-1] - inf[t] - vacc[t]
    V[t] = V[t-1] + vacc[t] - infV[t] - prot[t]
    P[t] = P[t-1] + prot[t] - infP[t]
    Es[t] = Es[t-1] - onsets[t] + inf[t]
    Is[t] = Is[t-1] + onsets[t] - recoverys[t]
    Ev[t] = Ev[t-1] - onsetv[t] + infV[t]
    Iv[t] = Iv[t-1] + onsetv[t] - recoveryv[t]
    Ep[t] = Ep[t-1] - onsetp[t] + infP[t]
    Ip[t] = Ip[t-1] + onsetp[t] - recoveryp[t]
    R[t] = R[t-1] + recoverys[t] + recoveryv[t] + recoveryp[t]
  }
  
  data.frame(
    day=1:nday,
    S=S,
    Es=Es,
    Is=Is,
    Ev=Ev,
    Iv=Iv,
    Ep=Ep,
    Ip=Ip,
    R=R,
    V=V,
    P=P,
    inf=inf,
    infV=infV,
    infP=infP,
    prot=prot,
    onsets=onsets,
    onsetv=onsetv,
    onsetp=onsetp,
    cases_s=rnbinom(length(onsets), mu=onsets*reporting, size=phi),
    cases_v=rnbinom(length(onsets), mu=onsetv*reporting+onsetp*reporting*(1-efficacyD), size=phi),
    vacc=vacc
  )
}
