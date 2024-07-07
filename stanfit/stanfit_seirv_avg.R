library(rstan)
load("../data_processed/iowa.rda")

model <- stan_model("../stanmodel/seirv_avg.stan")

standata <- list(
  nday=nrow(iowa),
  N=20496,
  sigma=1/17,
  gamma=1/7,
  drate=1/7,
  nu=iowa$vax_doses/20496,
  holiday1=ifelse(iowa$date >= "2015-11-22" & iowa$date <= "2015-11-29", 1, 0), ## thanksgiving
  holiday2=ifelse(iowa$date >= "2015-12-11" & iowa$date <= "2015-12-28", 1, 0), ## winter break
  holiday3=ifelse(iowa$date >= "2016-01-15" & iowa$date <= "2016-01-19", 1, 0), ## between-semester
  holiday4=ifelse(iowa$date >= "2016-03-13" & iowa$date <= "2016-03-20", 1, 0), ## spring break
  likelihood=1L,
  cases=iowa$onset,
  twodose=221,
  threedose=34
)

stanfit_seirv_avg <- sampling(model,
                          data = standata,
                          iter = 4000,
                          chains = 4,
                          cores=4,
                          seed=101,
                          control=list(adapt_delta=0.9))

save("stanfit_seirv_avg", file="stanfit_seirv_avg.rda")
