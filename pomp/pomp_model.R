rprocess <- pomp::Csnippet("
  double betat;
  double foi;
  double rate[11], trans[11];
	
	betat = beta0 * (1 - reduction1 * holiday1 - reduction2 * holiday2 - reduction3 * holiday3 - reduction4 * holiday4);
	foi = betat * (Is + Iv + Ip)/N;
	
	rate[0] = foi;
	rate[1] = nu;
	rate[2] = foi;
	rate[3] = drate;
	rate[4] = (1-efficacy) * foi;
	rate[5] = sigma;
	rate[6] = gamma;
	rate[7] = sigma;
	rate[8] = gamma;
	rate[9] = sigma;
	rate[10] = gamma;

	// transitions between classes
  reulermultinom(2,S,&rate[0],dt,&trans[0]);
	reulermultinom(2,V,&rate[2],dt,&trans[2]);
	reulermultinom(1,P,&rate[4],dt,&trans[4]);
	reulermultinom(1,Es,&rate[5],dt,&trans[5]);
	reulermultinom(1,Is,&rate[6],dt,&trans[6]);
	reulermultinom(1,Ev,&rate[7],dt,&trans[7]);
	reulermultinom(1,Iv,&rate[8],dt,&trans[8]);
	reulermultinom(1,Ep,&rate[9],dt,&trans[9]);
	reulermultinom(1,Ip,&rate[10],dt,&trans[10]);

	S += - trans[0] - trans[1];
	V += trans[1] - trans[2] - trans[3];
	P += trans[3] - trans[4];
	Es += trans[0] - trans[5];
	Is += trans[5] - trans[6];
	Ev += trans[2] - trans[7];
	Iv += trans[7] - trans[8];
	Ep += trans[4] - trans[9];
	Ip += trans[9] - trans[10];
	R += trans[6] + trans[8] + trans[10];
	
	onsets = trans[5];
	onsetv = trans[7];
	onsetp = trans[9];
")

initlz <- pomp::Csnippet("
	S = nearbyint(N*S0);
	V = 0;
	P = 0;
	Es = nearbyint(N*E0);
	Is = nearbyint(N*I0);
	Ev = 0;
	Iv = 0;
	Ep = 0;
	Ip = 0;
	R = nearbyint(N*(1-E0-I0-S0));
	onsets=0;
	onsetv=0;
	onsetp=0;
")

dmeas <- pomp::Csnippet("
	if (ISNA(cases)) {
    	lik = (give_log) ? 0 : 1;
  	} else {
    	lik = dnbinom_mu(cases, phi, reporting * (onsets + onsetv) + reporting * (1-efficacyD) * onsetp + 1e-301, give_log);
  	}
")

rmeas <- pomp::Csnippet("
	cases = rnbinom_mu(phi, reporting * (onsets + onsetv) + reporting * (1-efficacyD) * onsetp + 1e-301);
")

pomp_arg <- list(
  times="time",
  rprocess=pomp::euler(rprocess, delta.t=1),
  rinit = initlz,
  dmeasure=dmeas,
  rmeasure=rmeas,
  statenames=c("S", "V", "P", "Es", "Is", "Ev", "Iv", "Ep", "Ip", "R",
               "onsets", "onsetv", "onsetp"),
  paramnames=c("beta0", "reduction1", "reduction2", "reduction3", "reduction4", "S0", "E0", "I0",
               "efficacy", "N", "sigma", "gamma", "drate", "reporting", "efficacyD", "phi")
)
