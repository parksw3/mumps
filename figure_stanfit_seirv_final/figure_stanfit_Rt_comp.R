library(rstan)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times", base_size=12))
library(egg)
library(EpiEstim)
load("../data_processed/iowa.rda")
load("../stanfit/stanfit_seirv_final.rda")

standata <- list(
  nday=nrow(iowa),
  N=20496,
  sigma=1/17,
  gamma=1/5,
  drate=1/7,
  nu=iowa$vax_doses/20496,
  holiday1=ifelse(iowa$date >= "2015-11-22" & iowa$date <= "2015-11-29", 1, 0), ## thanksgiving
  holiday2=ifelse(iowa$date >= "2015-12-11" & iowa$date <= "2015-12-28", 1, 0), ## winter break
  holiday3=ifelse(iowa$date >= "2016-01-15" & iowa$date <= "2016-01-19", 1, 0), ## between-semester
  holiday4=ifelse(iowa$date >= "2016-03-13" & iowa$date <= "2016-03-20", 1, 0), ## spring break
  likelihood=1L,
  cases=iowa$onset
)

ee <- rstan::extract(stanfit_seirv_final)

npost <- length(ee$beta0)

onsetdata <- data.frame(
  date=iowa$date,
  median=apply(ee$pred_cases_s+ee$pred_cases_v, 2, median),
  lwr=apply(ee$pred_cases_s+ee$pred_cases_v, 2, quantile, 0.025),
  upr=apply(ee$pred_cases_s+ee$pred_cases_v, 2, quantile, 0.975)
)

Smat <- sapply(1:npost, function(x) {
  (ee$S[x,] + ee$V[x,] + (1-ee$efficacy[x]) * ee$P[x,])/20496
})

Sdata <- data.frame(
  date=iowa$date,
  median=apply(Smat, 1, median),
  lwr=apply(Smat, 1, quantile, 0.025),
  upr=apply(Smat, 1, quantile, 0.975)
)

Reffmat <- sapply(1:npost, function(x) {
  ee$beta0[x] *
    (1 - ee$reduction1[x] * standata$holiday1  - 
       ee$reduction2[x] * standata$holiday2  - 
       ee$reduction3[x] * standata$holiday3  - 
       ee$reduction4[x] * standata$holiday4) * (ee$S[x,] + ee$V[x,] + (1-ee$efficacy[x]) * ee$P[x,])/20496*5
})

Reffdata <- data.frame(
  date=iowa$date,
  median=apply(Reffmat, 1, median),
  lwr=apply(Reffmat, 1, quantile, 0.025),
  upr=apply(Reffmat, 1, quantile, 0.975),
  cvx=cumsum(iowa$vax_doses)
)

sidata <- data.frame(
  si=9:32,
  prob=c(0, 1.4, 0, 2.8, 2.1, 14.1, 4.9, 10.6, 9.9, 9.2, 8.5, 7.0, 7.0, 5.7, 0.7, 4.2, 
         4.2, 2.1, 2.1, 2.1, 0.7, 0, 0, 0.7)/100
)

## mean of 18.58 days
simean <- sum(sidata$si*sidata$prob)

## sd of 4.28 days
sqrt(sum((sidata$si-simean)^2*sidata$prob))

rt7 <- wallinga_teunis(iowa$onset, method="parametric_si",
                      config = list(t_start = seq(2, 258), t_end = seq(8, 264),
                                    mean_si = 18.6, std_si = 4.3,
                                    n_sim = 100))

rt14 <- wallinga_teunis(iowa$onset, method="parametric_si",
                      config = list(t_start = seq(2, 251), t_end = seq(15, 264),
                                    mean_si = 18.6, std_si = 4.3,
                                    n_sim = 100))

rt21 <- wallinga_teunis(iowa$onset, method="parametric_si",
                       config = list(t_start = seq(2, 244), t_end = seq(22, 264),
                                     mean_si = 18.6, std_si = 4.3,
                                     n_sim = 100))

rtdata7 <- data.frame(
  date=iowa$date[rt7$R$t_end],
  est=rt7$R$`Mean(R)`,
  lwr=rt7$R$`Quantile.0.025(R)`,
  upr=rt7$R$`Quantile.0.975(R)`,
  window="7-day sliding window"
)

rtdata14 <- data.frame(
  date=iowa$date[rt14$R$t_end],
  est=rt14$R$`Mean(R)`,
  lwr=rt14$R$`Quantile.0.025(R)`,
  upr=rt14$R$`Quantile.0.975(R)`,
  window="14-day sliding window"
)

rtdata21 <- data.frame(
  date=iowa$date[rt21$R$t_end],
  est=rt21$R$`Mean(R)`,
  lwr=rt21$R$`Quantile.0.025(R)`,
  upr=rt21$R$`Quantile.0.975(R)`,
  window="21-day sliding window"
)

rtdataall <- bind_rows(rtdata7, rtdata14, rtdata21) %>%
  mutate(
    window=factor(window, levels=c("7-day sliding window", "14-day sliding window", "21-day sliding window"))
  )

g1 <- ggplot(Reffdata) +
  geom_hline(yintercept=1, lty=2) +
  geom_line(data=rtdataall, aes(date, est)) +
  geom_ribbon(data=rtdataall, aes(date, ymin=lwr, ymax=upr), alpha=0.2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.2, fill="#EF6351") +
  geom_line(aes(date, median), col="#EF6351", lwd=1) +
  geom_vline(xintercept=as.Date("2015-11-10"), lty=3) +
  scale_x_date("Date", expand=c(0, 0)) +
  scale_y_continuous("Effective reproduction number", expand=c(0, 0)) +
  coord_cartesian(ylim=c(0, 4)) +
  facet_wrap(~window, nrow=3) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(size=0.9)
  )

ggsave("figure_stanfit_Rt_comp.pdf", g1, width=8, height=6)
