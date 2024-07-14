data {
  int nday;
  real N;
  real sigma;
  real gamma;
  real drate;
  vector[nday] nu;
  vector<lower=0, upper=1>[nday] holiday1;
  vector<lower=0, upper=1>[nday] holiday2;
  vector<lower=0, upper=1>[nday] holiday3;
  vector<lower=0, upper=1>[nday] holiday4;
  int<lower=0> cases[nday];
  
  int<lower=0> twodose;
  int<lower=0> threedose;
  
  int<lower=0, upper=1> likelihood;
}
parameters {
  real<lower=0> beta0;
  real<lower=0, upper=1> reduction;
  real<lower=0, upper=1> S0;
  real<lower=0, upper=1> E0;
  real<lower=0, upper=1> I0;
  real<lower=0, upper=1> reporting;
  real<lower=0, upper=1> efficacy;
  real<lower=0, upper=1> efficacyD;
  real<lower=0> phi_inv;
}

transformed parameters{
  vector<lower=0>[nday] S;
  vector<lower=0>[nday] Es;
  vector<lower=0>[nday] Is;
  vector<lower=0>[nday] Ev;
  vector<lower=0>[nday] Iv;
  vector<lower=0>[nday] Ep;
  vector<lower=0>[nday] Ip;
  vector<lower=0>[nday] R;
  vector<lower=0>[nday] V;
  vector<lower=0>[nday] P;
  vector<lower=0>[nday] betat;
  vector<lower=0>[nday] onsets;
  vector<lower=0>[nday] recoverys;
  vector<lower=0>[nday] onsetv;
  vector<lower=0>[nday] recoveryv;
  vector<lower=0>[nday] onsetp;
  vector<lower=0>[nday] recoveryp;
  vector<lower=0>[nday] foi;
  vector<lower=0>[nday] inf;
  vector<lower=0>[nday] vacc;
  vector<lower=0>[nday] infV;
  vector<lower=0>[nday] prot;
  vector<lower=0>[nday] infP;
  real<lower=0> phi = 1/phi_inv;
  
  S[1] = N * S0;
  Es[1] = N * E0;
  Is[1] = N * I0;
  Ev[1] = 0;
  Iv[1] = 0;
  Ep[1] = 0;
  Ip[1] = 0;
  R[1] = N * (1- S0 - E0 - I0);
  V[1] = 0;
  P[1] = 0;
  
  betat[1] = beta0;
  onsets[1] = 0;
  recoverys[1] = 0;
  onsetv[1] = 0;
  recoveryv[1] = 0;
  onsetp[1] = 0;
  recoveryp[1] = 0;
  foi[1] = 0;
  inf[1] = 0;
  vacc[1] = 0;
  
  infV[1] = 0;
  prot[1] = 0;
  
  infP[1] = 0;
  
  for (t in 2:nday) {
    betat[t] = beta0 * (1 - reduction * holiday1[t] - reduction * holiday2[t] - reduction * holiday3[t] - reduction * holiday4[t]);
    
    foi[t] = betat[t] * (Is[t-1]+Iv[t-1]+Ip[t-1]+1e-301)/N;
    inf[t] = foi[t]/(foi[t] + nu[t]) * (1 - exp(-(foi[t]+nu[t]))) * S[t-1];
    vacc[t] = nu[t]/(foi[t] + nu[t]) * (1 - exp(-(foi[t]+nu[t]))) * S[t-1];
    
    infV[t] = foi[t]/(foi[t]+drate) * (1 - exp(-(foi[t]+drate))) * V[t-1];
    prot[t] = drate/(foi[t]+drate) * (1 - exp(-(foi[t]+drate))) * V[t-1];
    
    infP[t] = (1 - exp(-((1-efficacy)*foi[t]))) * P[t-1];
    
    onsets[t] = (1 - exp(-sigma)) * Es[t-1];
    recoverys[t] = (1 - exp(-gamma)) * Is[t-1];
    onsetv[t] = (1 - exp(-sigma)) * Ev[t-1];
    recoveryv[t] = (1 - exp(-gamma)) * Iv[t-1];
    onsetp[t] = (1 - exp(-sigma)) * Ep[t-1];
    recoveryp[t] = (1 - exp(-gamma)) * Ip[t-1];
    
    S[t] = S[t-1] - inf[t] - vacc[t];
    V[t] = V[t-1] + vacc[t] - infV[t] - prot[t];
    P[t] = P[t-1] + prot[t] - infP[t];
    Es[t] = Es[t-1] - onsets[t] + inf[t];
    Is[t] = Is[t-1] + onsets[t] - recoverys[t];
    Ev[t] = Ev[t-1] - onsetv[t] + infV[t];
    Iv[t] = Iv[t-1] + onsetv[t] - recoveryv[t];
    Ep[t] = Ep[t-1] - onsetp[t] + infP[t];
    Ip[t] = Ip[t-1] + onsetp[t] - recoveryp[t];
    R[t] = R[t-1] + recoverys[t] + recoveryv[t] + recoveryp[t];
  }
}

model {
  S0 ~ beta(2, 2);
  E0 ~ beta(1, 999);
  I0 ~ beta(1, 999);
  beta0 ~ gamma(15, 21);
  reduction ~ beta(1, 1);
  reporting ~ beta(1, 1);
  efficacy ~ beta(1, 1);
  efficacyD ~ beta(1, 1);
  
  phi_inv ~ exponential(5);
  
  if (likelihood==1) {
    for (t in 2:nday) {
      cases[t] ~ neg_binomial_2(reporting * (onsets[t]+onsetv[t]) + reporting * (1-efficacyD) * onsetp[t] + 1e-301, phi);
    }
  }
  
  twodose ~ neg_binomial_2(reporting * sum(onsets) + 1e-301, phi);
  threedose ~ neg_binomial_2(reporting * sum(onsetv) + reporting * (1-efficacyD) * sum(onsetp) + 1e-301, phi);
}

generated quantities {
  real R0 = beta0 / gamma;
  real pred_cases_s[nday];
  real pred_cases_v[nday];
  pred_cases_s = neg_binomial_2_rng(reporting * onsets + 1e-301, phi);
  pred_cases_v = neg_binomial_2_rng(reporting * onsetv + reporting * (1-efficacyD) * onsetp + 1e-301, phi);
}
