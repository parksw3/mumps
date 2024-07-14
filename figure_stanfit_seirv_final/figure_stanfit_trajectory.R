library(rstan)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times", base_size=12))
library(egg)
load("../data_processed/iowa.rda")
load("../stanfit/stanfit_seirv_final.rda")

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
       ee$reduction4[x] * standata$holiday4) * (ee$S[x,] + ee$V[x,] + (1-ee$efficacy[x]) * ee$P[x,])/20496*7
})

Reffdata <- data.frame(
  date=iowa$date,
  median=apply(Reffmat, 1, median),
  lwr=apply(Reffmat, 1, quantile, 0.025),
  upr=apply(Reffmat, 1, quantile, 0.975)
)

g1 <- ggplot(iowa) +
  geom_bar(aes(date, onset), stat="identity", fill = "#CCECF8", width=1, col="black") +
  geom_ribbon(data=onsetdata, aes(date, ymin=lwr, ymax=upr), alpha=0.2, fill="#EF6351") +
  geom_line(data=onsetdata, aes(date, median), col="#EF6351", lwd=1) +
  geom_vline(xintercept=as.Date("2015-11-10"), lty=3) +
  scale_x_date("Date", expand=c(0, 0)) +
  scale_y_continuous("Number of symptomatic cases", expand=c(0, 0), limits=c(0, 11),
                     breaks=c(0, 2, 4, 6, 8, 10)) +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(size=0.9)
  )

g2 <- ggplot(Sdata) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.2, fill="#EF6351") +
  geom_line(aes(date, median), col="#EF6351", lwd=1) +
  geom_vline(xintercept=as.Date("2015-11-10"), lty=3) +
  scale_x_date("Date", expand=c(0, 0)) +
  scale_y_continuous("Effective susceptible proportion", limits=c(0, 0.5), expand=c(0, 0)) +
  ggtitle("B") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(size=0.9)
  )

g3 <- ggplot(Reffdata) +
  geom_hline(yintercept=1, lty=2) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr), alpha=0.2, fill="#EF6351") +
  geom_line(aes(date, median), col="#EF6351", lwd=1) +
  geom_vline(xintercept=as.Date("2015-11-10"), lty=3) +
  scale_x_date("Date", expand=c(0, 0)) +
  scale_y_continuous("Effective reproduction number", limits=c(0, 2.2), expand=c(0, 0)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(size=0.9)
  )

gtot <- ggarrange(g1, g2, g3, ncol=1)

ggsave("figure_stanfit_trajectory.pdf", gtot, width=8, height=8)
