library(tidyverse)
library(ggplot2)
library(survival)
library(survminer)

###########       chaoborus           ###########

#df with sac id
head(diameter.df)

surv_sac <- diameter.df %>%
  ungroup() %>%
  select(larva, species, instar, ant.post, side, atm) %>%
  mutate(sac = seq(1, nrow(diameter.df), by = 1)) %>%
  filter(species != "punctipennis") %>%
  filter(instar == "4")


head(surv_sac)

sac_survival <- Surv(time = surv_sac$atm)
sac_surv_fit <- survfit(sac_survival ~ species, data = surv_sac, type="kaplan-meier") 

ggsurvplot(sac_surv_fit, conf.int = TRUE, pval = FALSE, risk.table = FALSE,
           legend.labs=c("americanus", "trivittatus"), 
           legend = c(0.15, 0.2),
           legend.title = "species",
           censor.shape = "|", censor.size = 4,
           palette=c("firebrick", "goldenrod1"), 
           xlim = c(1, 3),
           xlab = "applied pressure (atm)",
           ylab = "Proportion remaining")

sacDiff <- survdiff(sac_survival ~ species, data = surv_sac)
print(sacDiff, digits = 4)

D1 <- sacDiff$obs[1]
D2 <- sacDiff$obs[2]
E1 <- sacDiff$exp[1]
E2 <- sacDiff$exp[2]
HR <- (D1/D2)/(E1/E2)
HR

E2 + E1
