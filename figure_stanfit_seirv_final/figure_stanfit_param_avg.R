library(rstan)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times", base_size=12))
library(egg)
load("../data_processed/iowa.rda")
load("../stanfit/stanfit_seirv_avg.rda")

tt <- theme(
  panel.grid = element_blank(),
  panel.border = element_blank(),
  axis.line = element_line(),
  legend.position = "none"
)

ee1 <- rstan::extract(stanfit_seirv_avg, permuted=FALSE)

ss1 <- rstan::summary(stanfit_seirv_avg)

beta0 <- data.frame(
  value=c(ee1[,,"beta0"])
)

holiday1 <- data.frame(
  value=c(ee1[,,"reduction"]*100),
  type="Thanksgiving"
)

reporting <- data.frame(
  value=c(ee1[,,"reporting"]*100)
)

vaccine <- data.frame(
  value=c(ee1[,,"efficacy"]*100)
)

vaccineD <- data.frame(
  value=c(ee1[,,"efficacyD"]*100)
)

vaccineComb <- data.frame(
  value=c(
    (1-c(ee1[,,"efficacy"]))*(1-c(ee1[,,"efficacyD"]))*100
  )
)

S0 <- data.frame(
  value=c(ee1[,,"S0"]*100)
)

E0 <- data.frame(
  value=c(ee1[,,"E0"]*100)
)

I0 <- data.frame(
  value=c(ee1[,,"I0"]*100)
)

phi <- data.frame(
  value=c(ee1[,,"phi"]*100)
)

g1 <- ggplot(beta0) +
  stat_function(fun=function(x) dgamma(x/5, 5, 5)/5, col="black", lwd=1) +
  geom_density(aes(value*5), lwd=1, col=2) +
  scale_x_continuous("Basic reproduction number", limits=c(0, 25)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.25)) +
  ggtitle("A") +
  tt

g2 <- ggplot(holiday1) +
  stat_function(fun=function(x) dbeta(x/100, 1, 1)/100, col="black", lwd=1) +
  geom_density(aes(value), lwd=1, col=2) +
  scale_x_continuous("Holiday effect (%)", limits=c(0, 100)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.16)) +
  ggtitle("B") +
  tt

g3 <- ggplot(reporting) +
  stat_function(fun=function(x) dbeta(x/100, 1, 1)/100, col="black", lwd=1) +
  geom_density(aes(value), lwd=1, col=2) +
  scale_x_continuous("Reporting probability (%)", limits=c(0, 100)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.12)) +
  ggtitle("C") +
  tt

g4 <- ggplot(vaccineComb) +
  stat_function(fun=function(x) -log(x/100)/100, col="black", n=1001, lwd=1) +
  geom_density(aes(value), lwd=1, col=2) +
  scale_x_continuous("Combinaed VE (%)", limits=c(0, 100)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.11)) +
  ggtitle("D") +
  tt

g5 <- ggplot(vaccine) +
  stat_function(fun=function(x) dbeta(x/100, 1, 1)/100, col="black", lwd=1) +
  geom_density(aes(value), lwd=1, col=2) +
  scale_x_continuous("VE against infection (%)", limits=c(0, 100)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.11)) +
  ggtitle("E") +
  tt

g6 <- ggplot(vaccineD) +
  stat_function(fun=function(x) dbeta(x/100, 1, 1)/100, col="black", lwd=1) +
  geom_density(aes(value), lwd=1, col=2) +
  scale_x_continuous("VE against disease (%)", limits=c(0, 100)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.11)) +
  ggtitle("F") +
  tt

g7 <- ggplot(S0) +
  stat_function(fun=function(x) dbeta(x/100, 2, 2)/100, col="black", lwd=1) +
  geom_density(aes(value), lwd=1, col=2) +
  scale_x_continuous("Initial proportion susceptible (%)", limits=c(0, 100)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.11)) +
  ggtitle("G") +
  tt

g8 <- ggplot(E0) +
  stat_function(fun=function(x) dbeta(x/100, 1, 999)/100, col="black", lwd=1) +
  geom_density(aes(value), lwd=1, col=2) +
  scale_x_continuous("Initial proportion exposed (%)", limits=c(0, 0.8)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 20)) +
  ggtitle("H") +
  tt

g9 <- ggplot(I0) +
  stat_function(fun=function(x) dbeta(x/100, 1, 999)/100, col="black", lwd=1) +
  geom_density(aes(value), lwd=1, col=2) +
  scale_x_continuous("Initial proportion infected (%)", limits=c(0, 0.8)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 20)) +
  ggtitle("I") +
  tt

g10 <- ggplot(phi) +
  stat_function(fun=function(x) dexp(1/x, 5) * 1/x^2, col="black", lwd=1) +
  geom_density(aes(value), lwd=1, col=2) +
  scale_x_continuous("Negative binomial dispersion", limits=c(0, 17)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 1)) +
  ggtitle("J") +
  tt

gtot <- ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10,
          nrow=3,
          draw=FALSE)

ggsave("figure_stanfit_param_avg.pdf", gtot, width=12, height = 6)
