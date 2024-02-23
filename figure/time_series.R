library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times", base_size=12))
library(lubridate)
library(egg)
source("../R/combine_axes.R")

load("../data_processed/iowa.rda")

g1 <- ggplot(iowa) +
  geom_ribbon(aes(date, ymax=cumsum(vax_doses)/500, ymin=0), fill="#EF6351", alpha=0.2) +
  annotate("rect", xmin=as.Date("2015-11-22"), xmax=as.Date("2015-11-29"), ymin=-Inf, ymax=Inf, fill="gray", alpha=0.6) +
  annotate("rect", xmin=as.Date("2015-12-11"), xmax=as.Date("2015-12-28"), ymin=-Inf, ymax=Inf, fill="gray", alpha=0.6) +
  annotate("rect", xmin=as.Date("2016-01-15"), xmax=as.Date("2016-01-19"), ymin=-Inf, ymax=Inf, fill="gray", alpha=0.6) +
  annotate("rect", xmin=as.Date("2016-03-13"), xmax=as.Date("2016-03-20"), ymin=-Inf, ymax=Inf, fill="gray", alpha=0.6) +
  geom_line(aes(date, cumsum(vax_doses)/500), col="#EF6351", lwd=1) +
  geom_bar(aes(date, onset), stat="identity", fill = "#CCECF8", width=1, col="black") +
  annotate("text", x=as.Date("2015-11-30"), y=9, label="Thanksgiving break", angle=90, hjust=1, vjust=1, family="Times") +
  annotate("text", x=as.Date("2015-12-29"), y=9, label="Winter break", angle=90, hjust=1, vjust=1, family="Times") +
  annotate("text", x=as.Date("2016-01-15"), y=9, label="Between-term break", angle=90, hjust=1, vjust=-0.2, family="Times") +
  annotate("text", x=as.Date("2016-03-13"), y=9, label="Spring break", angle=90, hjust=1, vjust=-0.4, family="Times") +
  scale_x_date("Date of illness onset", limits=c(as.Date("2015-08-22"), as.Date("2016-05-15")),
               expand=c(0, 0)) +
  scale_y_continuous("Number of symptomatic cases", limits=c(0, 10), expand=c(0, 0),
                     breaks=0:6*2,
                     sec.axis = sec_axis(~.*500, name="Cumulative number of third dose recipients")) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title.y.right = element_text(color="#EF6351"),
    axis.text.y.right = element_text(color="#EF6351"),
    axis.ticks.y.right = element_line(color="#EF6351"),
    axis.line.y.right = element_line(color="#EF6351")
  )

g1B <- (ggplot(iowa)
        + labs(x="")
        + scale_x_discrete(breaks=NULL)
        + scale_y_continuous("Cumulative proportion of third dose recipients", expand=c(0, 0), limits=c(0, 5000/20496), position = "right")
        + geom_blank(aes(NA, cumsum(vax_doses)/20496))
        + theme(
          axis.line.y = element_line(color="#EF6351")
          , axis.ticks.y = element_line(color="#EF6351")
          , axis.text.y = element_text(color="#EF6351")
          , axis.title.y = element_text(color="#EF6351")
        )
)

gtot <- combine_axes(g1,g1B,pattern="(axis-r|ylab-r|panel)",add_pos="r", pad_inner = 0)

ggsave("time_series.pdf", gtot, width=12, height=4)
