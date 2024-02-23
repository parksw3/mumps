library(rstan)
library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times", base_size=12))
library(ggridges)
library(egg)
load("../data_processed/iowa.rda")
load("../analysis_seirv_final/effects_exclusion.rda")
load("../analysis_seirv_final/effects_inclusion.rda")

effects_exclusion_finalsize_base <- effects_exclusion_finalsize %>%
  filter(type=="Baseline (including all interventions)") %>%
  select(sim, finalsize) %>%
  rename(
    base=finalsize
  )

effects_exclusion_finalsize2 <- effects_exclusion_finalsize %>%
  filter(type!="Baseline (including all interventions)") %>%
  merge(effects_exclusion_finalsize_base) %>%
  mutate(
    diff=(finalsize-base)/base*100
  ) %>%
  mutate(
    type=factor(type, levels=c("Excluding spring break",
                               "Excluding between-term break",
                               "Excluding vaccination",
                               "Excluding Thanksgiving",
                               "Excluding winter break"),
                labels=c("Excluding spring break",
                         "Excluding between-term break",
                         "Excluding vaccination",
                         "Excluding Thanksgiving break",
                         "Excluding winter break"))
  )

effects_exclusion_finalsize3 <- effects_exclusion_finalsize %>%
  filter(type!="Baseline (including all interventions)") %>%
  merge(effects_exclusion_finalsize_base) %>%
  mutate(
    diff=(finalsize-base)/base*100
  ) %>%
  group_by(sim) %>%
  mutate(
    rank=rank(diff)
  ) %>%
  mutate(
    type=factor(type, levels=c("Excluding spring break",
                               "Excluding between-term break",
                               "Excluding vaccination",
                               "Excluding Thanksgiving",
                               "Excluding winter break"),
                labels=c("Excluding spring break",
                         "Excluding between-term break",
                         "Excluding vaccination",
                         "Excluding Thanksgiving break",
                         "Excluding winter break"))
  )

effects_inclusion_finalsize_base <- effects_inclusion_finalsize %>%
  filter(type=="Baseline (excluding all interventions)") %>%
  select(sim, finalsize) %>%
  rename(
    base=finalsize
  )

effects_inclusion_finalsize2 <- effects_inclusion_finalsize %>%
  filter(type!="Baseline (excluding all interventions)") %>%
  merge(effects_inclusion_finalsize_base) %>%
  mutate(
    diff=(base-finalsize)/base*100
  ) %>%
  mutate(
    type=factor(type, levels=c("Including spring break",
                               "Including between-term break",
                               "Including vaccination",
                               "Including Thanksgiving",
                               "Including winter break"),
                labels=c("Including spring break",
                         "Including between-term break",
                         "Including vaccination",
                         "Including Thanksgiving break",
                         "Including winter break"))
  )

effects_inclusion_finalsize3 <- effects_inclusion_finalsize %>%
  filter(type!="Baseline (excluding all interventions)") %>%
  merge(effects_inclusion_finalsize_base) %>%
  mutate(
    diff=(base-finalsize)/base*100
  ) %>%
  group_by(sim) %>%
  mutate(
    rank=rank(diff)
  ) %>%
  mutate(
    type=factor(type, levels=c("Including spring break",
                               "Including between-term break",
                               "Including vaccination",
                               "Including Thanksgiving",
                               "Including winter break"),
                labels=c("Including spring break",
                         "Including between-term break",
                         "Including vaccination",
                         "Including Thanksgiving break",
                         "Including winter break"))
  )

g1 <- ggplot(effects_exclusion_finalsize2) +
  geom_boxplot(aes(type, diff, fill=type), alpha=0.8, outlier.size = 0.7, outlier.shape=1, lwd=0.7) +
  scale_y_continuous("Relative increase in final size (%)", limits=c(0, 50), expand=c(0, 0)) +
  scale_fill_viridis_d() +
  ggtitle("A") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(hjust=1, angle=45),
    plot.margin = margin(0.1,0.1,0.1,0.5, "cm"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(size=0.7),
    legend.position = "none"
  )

effects_exclusion_finalsize2 %>%
  group_by(type) %>%
  summarize(
    median=median(diff),
    lwr=quantile(diff, 0.025),
    upr=quantile(diff, 0.975)
  )

g2 <- ggplot(effects_exclusion_finalsize3) +
  geom_bar(aes(type, fill=as.factor(rank))) +
  scale_y_continuous("Number of simulations", limits=c(0, 8000), expand=c(0, 0)) +
  scale_fill_viridis_d("Ranking") +
  ggtitle("B") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(hjust=1, angle=45),
    plot.margin = margin(0.1,0.1,0.1,0.5, "cm"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "right"
  )

effects_exclusion_finalsize3 %>%
  filter(type=="Excluding winter break") %>%
  ungroup %>%
  summarize(
    mean(rank==5)
  )

g3 <- ggplot(effects_inclusion_finalsize2) +
  geom_boxplot(aes(type, diff, fill=type), alpha=0.8, outlier.size = 0.7, outlier.shape=1, lwd=0.7) +
  scale_fill_viridis_d() +
  scale_y_continuous("Relative decrease in final size (%)", limits=c(0, 50), expand=c(0, 0)) +
  ggtitle("B") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(hjust=1, angle=45),
    plot.margin = margin(0.1,0.1,0.1,0.1, "cm"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(size=0.7),
    legend.position = "none"
  )

g4 <- ggplot(effects_inclusion_finalsize3) +
  geom_bar(aes(type, fill=as.factor(rank))) +
  scale_y_continuous("Number of simulations", limits=c(0, 8000), expand=c(0, 0)) +
  scale_fill_viridis_d("Ranking") +
  ggtitle("D") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(hjust=1, angle=45),
    plot.margin = margin(0.1,0.1,0.1,0.5, "cm"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none"
  )

gtot <- egg::ggarrange(g1, g3, nrow=1)

ggsave("figure_stanfit_effects.pdf", gtot, width=8, height=4)
