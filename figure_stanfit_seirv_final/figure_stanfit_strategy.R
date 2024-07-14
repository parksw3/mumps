library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family="Times", base_size=12))

load("../data_processed/iowa.rda")
load("../analysis_seirv_final/vaccine_strategy.rda")
load("../analysis_seirv_final/effects_exclusion.rda")

strategy_baseline <- effects_exclusion_finalsize %>%
  filter(type=="Excluding vaccination") %>%
  rename(
    base=finalsize
  )

effects_exclusion_base <- effects_exclusion_finalsize %>%
  filter(type=="Baseline (including all interventions)") %>%
  select(-type) %>%
  mutate(
    timing="Baseline",
    coverage="Baseline",
    sim=as.integer(as.character(sim))
  )

vaccine_strategy2 <- vaccine_strategy %>%
  group_by(sim, coverage, timing, delay, efficacy) %>%
  summarize(
    finalsize=sum(incidence)
  )

vaccine_strategy3 <- vaccine_strategy2 %>%
  merge(strategy_baseline) %>%
  mutate(
    reduction=(base-finalsize)/base*100
  )

vaccine_summ <- vaccine_strategy3 %>%
  group_by(timing, coverage, delay, efficacy) %>%
  summarize(
    median=median(reduction),
    lwr=quantile(reduction, 0.025),
    upr=quantile(reduction, 0.975)
  ) %>%
  mutate(
    efficacy=paste0(100*(efficacy), "% VE"),
    delay=factor(paste0(delay, " day delay"), levels=c("7 day delay", "14 day delay", "21 day delay")),
    timing=factor(timing),
    coverage=paste0(coverage*100, "%")
  )

vaccine_summ %>%
  filter(coverage=="20%")

g1 <- ggplot(vaccine_summ) +
  geom_point(aes(timing, median, col=coverage), position = position_dodge(width=0.5), size=2) +
  geom_errorbar(aes(timing, ymin=lwr, ymax=upr, col=coverage), width=0, position = position_dodge(width=0.5), lwd=0.8) +
  scale_x_discrete("Vaccination timing (days since outbreak)") +
  scale_y_continuous("Relative decrease in final size") +
  scale_color_viridis_d("Vaccination\ncoverage", end=0.9) +
  facet_grid(efficacy~delay) +
  theme(
    strip.background = element_blank(),
    # panel.spacing = unit(0, "cm"),
    panel.border = element_rect(size=1)
  )
  
ggsave("figure_stanfit_strategy.pdf", g1, width=8, height=6)

vaccine_summ2 <- vaccine_summ %>%
  filter(delay=="7 day delay", 
         efficacy=="60% VE")

g2 <- ggplot(vaccine_summ2) +
  geom_point(aes(timing, median, col=coverage), position = position_dodge(width=0.5), size=2) +
  geom_errorbar(aes(timing, ymin=lwr, ymax=upr, col=coverage), width=0, position = position_dodge(width=0.5), lwd=0.8) +
  scale_x_discrete("Vaccination timing (days since outbreak)") +
  scale_y_continuous("Relative decrease in final size") +
  scale_color_viridis_d("Vaccination\ncoverage", end=0.9) +
  theme(
    strip.background = element_blank(),
    # panel.spacing = unit(0, "cm"),
    panel.border = element_rect(size=1)
  )

ggsave("figure_stanfit_strategy_summ.pdf", g2, width=6, height=4)
