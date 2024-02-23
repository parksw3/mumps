library(rstan)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times", base_size=12))
library(egg)
load("../data_processed/iowa.rda")
load("../stanfit/stanfit_seirv_final.rda")
load("../stanfit/stanfit_seirv_final_14.rda")
load("../stanfit/stanfit_seirv_final_21.rda")

tt <- theme(
  panel.grid = element_blank(),
  panel.border = element_blank(),
  axis.line = element_line(),
  legend.position = "none"
)

ee1 <- rstan::extract(stanfit_seirv_final, permuted=FALSE)
ee2 <- rstan::extract(stanfit_seirv_final_14, permuted=FALSE)
ee3 <- rstan::extract(stanfit_seirv_final_21, permuted=FALSE)

ss1 <- rstan::summary(stanfit_seirv_final)
ss2 <- rstan::summary(stanfit_seirv_final_14)
ss3 <- rstan::summary(stanfit_seirv_final_21)

beta0 <- data.frame(
  value=c(ee1[,,"beta0"], ee2[,,"beta0"], ee3[,,"beta0"]),
  delay=rep(c(7, 14, 21), each=8000)
)

holiday1 <- data.frame(
  value=c(ee1[,,"reduction1"]*100, ee2[,,"reduction1"]*100, ee3[,,"reduction1"]*100),
  delay=rep(c(7, 14, 21), each=8000),
  type="Thanksgiving"
)

holiday2 <- data.frame(
  value=c(ee1[,,"reduction2"]*100, ee2[,,"reduction2"]*100, ee3[,,"reduction2"]*100),
  delay=rep(c(7, 14, 21), each=8000),
  type="Winter"
)

holiday3 <- data.frame(
  value=c(ee1[,,"reduction3"]*100, ee2[,,"reduction3"]*100, ee3[,,"reduction3"]*100),
  delay=rep(c(7, 14, 21), each=8000),
  type="Between-term"
)

holiday4 <- data.frame(
  value=c(ee1[,,"reduction4"]*100, ee2[,,"reduction4"]*100, ee3[,,"reduction4"]*100),
  delay=rep(c(7, 14, 21), each=8000),
  type="Spring"
)

reporting <- data.frame(
  value=c(ee1[,,"reporting"]*100, ee2[,,"reporting"]*100, ee3[,,"reporting"]*100),
  delay=rep(c(7, 14, 21), each=8000)
)

vaccine <- data.frame(
  value=c(ee1[,,"efficacy"]*100, ee2[,,"efficacy"]*100, ee3[,,"efficacy"]*100),
  delay=rep(c(7, 14, 21), each=8000)
)

vaccineD <- data.frame(
  value=c(ee1[,,"efficacyD"]*100, ee2[,,"efficacyD"]*100, ee3[,,"efficacyD"]*100),
  delay=rep(c(7, 14, 21), each=8000)
)

vaccineComb <- data.frame(
  value=c(
    (1-c(ee1[,,"efficacy"]))*(1-c(ee1[,,"efficacyD"]))*100,
    (1-c(ee2[,,"efficacy"]))*(1-c(ee2[,,"efficacyD"]))*100,
    (1-c(ee3[,,"efficacy"]))*(1-c(ee3[,,"efficacyD"]))*100
  ),
  delay=rep(c(7, 14, 21), each=8000)
)

S0 <- data.frame(
  value=c(ee1[,,"S0"]*100, ee2[,,"S0"]*100, ee3[,,"S0"]*100),
  delay=rep(c(7, 14, 21), each=8000)
)

E0 <- data.frame(
  value=c(ee1[,,"E0"]*100, ee2[,,"E0"]*100, ee3[,,"E0"]*100),
  delay=rep(c(7, 14, 21), each=8000)
)

I0 <- data.frame(
  value=c(ee1[,,"I0"]*100, ee2[,,"I0"]*100, ee3[,,"I0"]*100),
  delay=rep(c(7, 14, 21), each=8000)
)

phi <- data.frame(
  value=c(ee1[,,"phi"]*100, ee2[,,"phi"]*100, ee3[,,"phi"]*100),
  delay=rep(c(7, 14, 21), each=8000)
)

g1 <- ggplot(beta0) +
  stat_function(fun=function(x) dgamma(x/5, 5, 5)/5, col="black", lwd=1) +
  geom_density(aes(value*5, col=as.factor(delay)), lwd=1) +
  scale_x_continuous("Basic reproduction number", limits=c(0, 25)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.25)) +
  ggtitle("A") +
  tt

g2 <- ggplot(holiday1) +
  stat_function(fun=function(x) dbeta(x/100, 1, 1)/100, col="black", lwd=1) +
  geom_density(aes(value, col=as.factor(delay)), lwd=1) +
  scale_x_continuous("Thanksgiving effect (%)", limits=c(0, 100)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.11)) +
  ggtitle("B") +
  tt

g3 <- ggplot(holiday2) +
  stat_function(fun=function(x) dbeta(x/100, 1, 1)/100, col="black", lwd=1) +
  geom_density(aes(value, col=as.factor(delay)), lwd=1) +
  scale_x_continuous("Winter break effect (%)", limits=c(0, 100)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.11)) +
  ggtitle("C") +
  tt

g4 <- ggplot(holiday3) +
  stat_function(fun=function(x) dbeta(x/100, 1, 1)/100, col="black", lwd=1) +
  geom_density(aes(value, col=as.factor(delay)), lwd=1) +
  scale_x_continuous("Between-term effect (%)", limits=c(0, 100)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.11)) +
  ggtitle("D") +
  tt

g5 <- ggplot(holiday4) +
  stat_function(fun=function(x) dbeta(x/100, 1, 1)/100, col="black", lwd=1) +
  geom_density(aes(value, col=as.factor(delay)), lwd=1) +
  scale_x_continuous("Spring break effect (%)", limits=c(0, 100)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.11)) +
  ggtitle("E") +
  tt

g6 <- ggplot(reporting) +
  stat_function(fun=function(x) dbeta(x/100, 1, 1)/100, col="black", lwd=1) +
  geom_density(aes(value, col=as.factor(delay)), lwd=1) +
  scale_x_continuous("Reporting probability (%)", limits=c(0, 100)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.12)) +
  ggtitle("F") +
  tt

g7 <- ggplot(vaccineComb) +
  stat_function(fun=function(x) -log(x/100)/100, col="black", n=1001, lwd=1) +
  geom_density(aes(value, col=as.factor(delay)), lwd=1) +
  scale_x_continuous("Combinaed VE (%)", limits=c(0, 100)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.11)) +
  ggtitle("G") +
  tt

g8 <- ggplot(vaccine) +
  stat_function(fun=function(x) dbeta(x/100, 1, 1)/100, col="black", lwd=1) +
  geom_density(aes(value, col=as.factor(delay)), lwd=1) +
  scale_x_continuous("VE against infection (%)", limits=c(0, 100)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.11)) +
  ggtitle("H") +
  tt

g9 <- ggplot(vaccineD) +
  stat_function(fun=function(x) dbeta(x/100, 1, 1)/100, col="black", lwd=1) +
  geom_density(aes(value, col=as.factor(delay)), lwd=1) +
  scale_x_continuous("VE against disease (%)", limits=c(0, 100)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.11)) +
  ggtitle("I") +
  tt

g10 <- ggplot(S0) +
  stat_function(fun=function(x) dbeta(x/100, 2, 2)/100, col="black", lwd=1) +
  geom_density(aes(value, col=as.factor(delay)), lwd=1) +
  scale_x_continuous("Initial proportion susceptible (%)", limits=c(0, 100)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 0.11)) +
  ggtitle("J") +
  tt

g11 <- ggplot(E0) +
  stat_function(fun=function(x) dbeta(x/100, 1, 999)/100, col="black", lwd=1) +
  geom_density(aes(value, col=as.factor(delay)), lwd=1) +
  scale_x_continuous("Initial proportion exposed (%)", limits=c(0, 0.8)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 20)) +
  ggtitle("K") +
  tt

g12 <- ggplot(I0) +
  stat_function(fun=function(x) dbeta(x/100, 1, 999)/100, col="black", lwd=1) +
  geom_density(aes(value, col=as.factor(delay)), lwd=1) +
  scale_x_continuous("Initial proportion infected (%)", limits=c(0, 0.8)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 20)) +
  ggtitle("L") +
  tt

g13 <- ggplot(phi) +
  stat_function(fun=function(x) dexp(1/x, 5) * 1/x^2, col="black", lwd=1) +
  geom_density(aes(value, col=as.factor(delay)), lwd=1) +
  scale_x_continuous("Negative binomial dispersion", limits=c(0, 17)) +
  scale_y_continuous("Probability density", expand=c(0, 0), limits=c(0, 1)) +
  ggtitle("M") +
  tt

gtot <- ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12,
          nrow=3,
          draw=FALSE)

ggsave("figure_stanfit_param_all.pdf", gtot, width=12, height = 6)
