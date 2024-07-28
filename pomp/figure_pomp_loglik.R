library(dplyr)
library(rstan)
library(ggplot2); theme_set(theme_bw(base_family="Times"))
library(egg)

load("pomp_loglik.rda")
load("../stanfit/stanfit_seirv_final.rda")

allparam <- pomp_loglik[,-1]

llvec <- pomp_loglik$logLik

# find maximum log likelihood for each bin
ll_max <- apply(allparam, 2, function(x) {
  rr <- range(x)
  
  ss <- seq(from=rr[1], to=rr[2], length.out=200)
  
  ff <- cut(x, ss)
  
  dd <- data.frame(
    value=x,
    group=ff,
    ll=llvec
  )
  
  dd %>%
    group_by(group) %>%
    filter(ll==max(ll))
}) %>%
  bind_rows(.id="key")

# calculate profile likelihood
pf <- lapply(split(ll_max, ll_max$key), function(dd) {
  # filter ones that are too low
  dd <- dd %>%
    filter(ll > -320)
  
  # fit LOESS curve
  lfit <- loess(ll~value, data=dd)
  
  # get range of parameter values
  rr <- range(dd$value)
  
  # predict profile likelihood across the parameter range
  pred <- data.frame(
    value=seq(rr[1], rr[2], length.out=101),
    ll=predict(lfit, newdata=seq(rr[1], rr[2], length.out=101))
  )
  
  # estimate maximum likelihood from the smooth profile 
  mm <- pred$value[which.max(pred$ll)]
  
  # get lower CI bound
  lwr <- try(with(filter(pred, value <= mm), 
                  approx(x=ll, y=value, xout=max(llvec, na.rm=TRUE)-2))$y)
  
  if (inherits(lwr, "try-error")) lwr <- NA
  
  # get upper CI bound
  upr <- try(with(filter(pred, value >= mm), 
                  approx(x=ll, y=value, xout=max(llvec, na.rm=TRUE)-2))$y)
  
  if (inherits(upr, "try-error")) upr <- NA
  
  data.frame(
    est=mm,
    lwr=lwr,
    upr=upr
  )
}) %>%
  bind_rows(.id="key")

# get posterior from deterministic model for comparison
ss <- summary(stanfit_seirv_final)

stan_summ <- data.frame(
  key=rownames(ss$summary[1:12,]),
  est=ss$summary[1:12,6],
  lwr=ss$summary[1:12,4],
  upr=ss$summary[1:12,8]
)

tt <- theme(
  panel.grid = element_blank(),
  panel.border = element_rect(linewidth=1),
  legend.position = "none"
)

# set up plots
g1 <- ggplot(ll_max %>% filter(key=="beta0")) +
  geom_point(aes(value * 7, ll), col=2, shape=1) +
  geom_smooth(aes(value * 7, ll), method="loess", se=FALSE, col=2) +
  geom_vline(data=pf %>% filter(key=="beta0"), aes(xintercept=lwr*7), lty=2, col=2) +
  geom_vline(data=pf %>% filter(key=="beta0"), aes(xintercept=upr*7), lty=2, col=2) +
  geom_vline(data=stan_summ %>% filter(key=="beta0"), aes(xintercept=lwr*7), lty=2, col=1) +
  geom_vline(data=stan_summ %>% filter(key=="beta0"), aes(xintercept=upr*7), lty=2, col=1) +
  scale_x_continuous("Basic reproduction number") +
  scale_y_continuous("Profile log-likelihood", limits=c(-320, -313.8)) +
  tt

g2 <- ggplot(ll_max %>% filter(key=="reduction1")) +
  geom_point(aes(value*100, ll), col=2, shape=1) +
  geom_smooth(aes(value*100, ll), method="loess", se=FALSE, col=2) +
  geom_vline(data=pf %>% filter(key=="reduction1"), aes(xintercept=lwr*100), lty=2, col=2) +
  geom_vline(data=pf %>% filter(key=="reduction1"), aes(xintercept=upr*100), lty=2, col=2) +
  geom_vline(data=stan_summ %>% filter(key=="reduction1"), aes(xintercept=lwr*100), lty=2, col=1) +
  geom_vline(data=stan_summ %>% filter(key=="reduction1"), aes(xintercept=upr*100), lty=2, col=1) +
  scale_x_continuous("Thanksgiving effect (%)") +
  scale_y_continuous("Profile log-likelihood", limits=c(-320, -313.8)) +
  tt

g3 <- ggplot(ll_max %>% filter(key=="reduction2")) +
  geom_point(aes(value*100, ll), col=2, shape=1) +
  geom_smooth(aes(value*100, ll), method="loess", se=FALSE, col=2) +
  geom_vline(data=pf %>% filter(key=="reduction2"), aes(xintercept=lwr*100), lty=2, col=2) +
  geom_vline(data=pf %>% filter(key=="reduction2"), aes(xintercept=upr*100), lty=2, col=2) +
  geom_vline(data=stan_summ %>% filter(key=="reduction2"), aes(xintercept=lwr*100), lty=2, col=1) +
  geom_vline(data=stan_summ %>% filter(key=="reduction2"), aes(xintercept=upr*100), lty=2, col=1) +
  scale_x_continuous("Winter break effect (%)") +
  scale_y_continuous("Profile log-likelihood", limits=c(-320, -313.8)) +
  tt

g4 <- ggplot(ll_max %>% filter(key=="reduction3")) +
  geom_point(aes(value*100, ll), col=2, shape=1) +
  geom_smooth(aes(value*100, ll), method="loess", se=FALSE, col=2) +
  geom_vline(data=pf %>% filter(key=="reduction3"), aes(xintercept=lwr*100), lty=2, col=2) +
  geom_vline(data=pf %>% filter(key=="reduction3"), aes(xintercept=upr*100), lty=2, col=2) +
  geom_vline(data=stan_summ %>% filter(key=="reduction3"), aes(xintercept=lwr*100), lty=2, col=1) +
  geom_vline(data=stan_summ %>% filter(key=="reduction3"), aes(xintercept=upr*100), lty=2, col=1) +
  scale_x_continuous("Between-term effect (%)") +
  scale_y_continuous("Profile log-likelihood", limits=c(-320, -313.8)) +
  tt

g5 <- ggplot(ll_max %>% filter(key=="reduction4")) +
  geom_point(aes(value*100, ll), col=2, shape=1) +
  geom_smooth(aes(value*100, ll), method="loess", se=FALSE, col=2) +
  geom_vline(data=pf %>% filter(key=="reduction4"), aes(xintercept=lwr*100), lty=2, col=2) +
  geom_vline(data=pf %>% filter(key=="reduction4"), aes(xintercept=upr*100), lty=2, col=2) +
  geom_vline(data=stan_summ %>% filter(key=="reduction4"), aes(xintercept=lwr*100), lty=2, col=1) +
  geom_vline(data=stan_summ %>% filter(key=="reduction4"), aes(xintercept=upr*100), lty=2, col=1) +
  scale_x_continuous("Spring break effect (%)") +
  scale_y_continuous("Profile log-likelihood", limits=c(-320, -313.8)) +
  tt

g6 <- ggplot(ll_max %>% filter(key=="reporting")) +
  geom_point(aes(value*100, ll), col=2, shape=1) +
  geom_smooth(aes(value*100, ll), method="loess", se=FALSE, col=2) +
  geom_vline(data=pf %>% filter(key=="reporting"), aes(xintercept=lwr*100), lty=2, col=2) +
  geom_vline(data=pf %>% filter(key=="reporting"), aes(xintercept=upr*100), lty=2, col=2) +
  geom_vline(data=stan_summ %>% filter(key=="reporting"), aes(xintercept=lwr*100), lty=2, col=1) +
  geom_vline(data=stan_summ %>% filter(key=="reporting"), aes(xintercept=upr*100), lty=2, col=1) +
  scale_x_continuous("Reporting probability (%)") +
  scale_y_continuous("Profile log-likelihood", limits=c(-320, -313.8)) +
  tt

g7 <- ggplot(ll_max %>% filter(key=="efficacy")) +
  geom_point(aes(value*100, ll), col=2, shape=1) +
  geom_smooth(aes(value*100, ll), method="loess", se=FALSE, col=2) +
  geom_vline(data=pf %>% filter(key=="efficacy"), aes(xintercept=lwr*100), lty=2, col=2) +
  geom_vline(data=pf %>% filter(key=="efficacy"), aes(xintercept=upr*100), lty=2, col=2) +
  geom_vline(data=stan_summ %>% filter(key=="efficacy"), aes(xintercept=lwr*100), lty=2, col=1) +
  geom_vline(data=stan_summ %>% filter(key=="efficacy"), aes(xintercept=upr*100), lty=2, col=1) +
  scale_x_continuous("VE against infection (%)") +
  scale_y_continuous("Profile log-likelihood", limits=c(-320, -313.8)) +
  tt

g8 <- ggplot(ll_max %>% filter(key=="efficacyD")) +
  geom_point(aes(value*100, ll), col=2, shape=1) +
  geom_smooth(aes(value*100, ll), method="loess", se=FALSE, col=2) +
  geom_vline(data=pf %>% filter(key=="efficacyD"), aes(xintercept=lwr*100), lty=2, col=2) +
  geom_vline(data=pf %>% filter(key=="efficacyD"), aes(xintercept=upr*100), lty=2, col=2) +
  geom_vline(data=stan_summ %>% filter(key=="efficacyD"), aes(xintercept=lwr*100), lty=2, col=1) +
  geom_vline(data=stan_summ %>% filter(key=="efficacyD"), aes(xintercept=upr*100), lty=2, col=1) +
  scale_x_continuous("VE against disease (%)") +
  scale_y_continuous("Profile log-likelihood", limits=c(-320, -313.8)) +
  tt

g9 <- ggplot(ll_max %>% filter(key=="S0")) +
  geom_point(aes(value*100, ll), col=2, shape=1) +
  geom_smooth(aes(value*100, ll), method="loess", se=FALSE, col=2) +
  geom_vline(data=pf %>% filter(key=="S0"), aes(xintercept=lwr*100), lty=2, col=2) +
  geom_vline(data=pf %>% filter(key=="S0"), aes(xintercept=upr*100), lty=2, col=2) +
  geom_vline(data=stan_summ %>% filter(key=="S0"), aes(xintercept=lwr*100), lty=2, col=1) +
  geom_vline(data=stan_summ %>% filter(key=="S0"), aes(xintercept=upr*100), lty=2, col=1) +
  scale_x_continuous("Initial proportion susceptible (%)") +
  scale_y_continuous("Profile log-likelihood", limits=c(-320, -313.8)) +
  tt

g10 <- ggplot(ll_max %>% filter(key=="E0")) +
  geom_point(aes(value*100, ll), col=2, shape=1) +
  geom_smooth(aes(value*100, ll), method="loess", se=FALSE, col=2) +
  geom_vline(data=pf %>% filter(key=="E0"), aes(xintercept=lwr*100), lty=2, col=2) +
  geom_vline(data=pf %>% filter(key=="E0"), aes(xintercept=upr*100), lty=2, col=2) +
  geom_vline(data=stan_summ %>% filter(key=="E0"), aes(xintercept=lwr*100), lty=2, col=1) +
  geom_vline(data=stan_summ %>% filter(key=="E0"), aes(xintercept=upr*100), lty=2, col=1) +
  scale_x_continuous("Initial proportion exposed (%)") +
  scale_y_continuous("Profile log-likelihood", limits=c(-320, -313.8)) +
  tt

g11 <- ggplot(ll_max %>% filter(key=="I0")) +
  geom_point(aes(value*100, ll), col=2, shape=1) +
  geom_smooth(aes(value*100, ll), method="loess", se=FALSE, col=2) +
  geom_vline(data=pf %>% filter(key=="I0"), aes(xintercept=lwr*100), lty=2, col=2) +
  geom_vline(data=pf %>% filter(key=="I0"), aes(xintercept=upr*100), lty=2, col=2) +
  geom_vline(data=stan_summ %>% filter(key=="I0"), aes(xintercept=lwr*100), lty=2, col=1) +
  geom_vline(data=stan_summ %>% filter(key=="I0"), aes(xintercept=upr*100), lty=2, col=1) +
  scale_x_continuous("Initial proportion infected (%)") +
  scale_y_continuous("Profile log-likelihood", limits=c(-320, -313.8)) +
  tt

gcomb <- ggarrange(g1, g2, g3, g4,
          g5, g6, g7, g8,
          g9, g10, g11, nrow=3, draw=FALSE)

ggsave("figure_pomp_loglik.pdf", gcomb, width=12, height=6)
