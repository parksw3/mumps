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

twototal <- data.frame(
  total=apply(ee$pred_cases_s, 1, sum)
)

threetotal <- data.frame(
  total=apply(ee$pred_cases_v, 1, sum)
)

g1 <- ggplot(twototal) +
  geom_density(aes(total), fill="#EF6351", alpha=0.8) +
  geom_vline(xintercept=221, lty=2) +
  scale_x_continuous("Total number of reported cases among second dose recipients",
                     expand=c(0, 0)) +
  scale_y_continuous("Density", expand=c(0, 0), limits=c(0, 0.015)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

g2 <- ggplot(threetotal) +
  geom_density(aes(total), fill="#CCECF8") +
  geom_vline(xintercept=34, lty=2) +
  scale_x_continuous("Total number of reported cases among third dose recipients",
                     limits=c(0, 40),
                     expand=c(0, 0)) +
  scale_y_continuous("Density", expand=c(0, 0), limits=c(0, 0.09)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

gtot <- ggarrange(g1, g2, nrow=1)

ggsave("figure_stanfit_total.pdf", gtot, width=10, height=5)
