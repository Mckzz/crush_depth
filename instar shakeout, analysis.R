library(tidyverse)
library(ggplot2)
library(readr)
install.packages("visreg")
library(visreg)

# crush depth by species and instar
ggplot(data = diameter.df,
       aes(x = species, 
           y = atm,
           colour = species,
           shape = ant.post)) +
  geom_jitter(alpha = 0.4) +
  geom_point(aes(y = spp_inst_mean, size = 3), shape = 1, stroke = 1) +
  geom_errorbar(data = diameter.df,
                aes(x= species, 
                    ymin= spp_inst_mean - spp_inst_sd, 
                    ymax= spp_inst_mean + spp_inst_sd, 
                    colour = species), 
                width= 0.1) +
  facet_wrap(~instar) +
  theme_minimal() 
  #theme(legend.position = "none")

instar.mod <- lm(atm ~ species + instar + ant.post, data = diameter.df)
summary(instar.mod)
par(mfrow = c(2, 2))
plot(instar.mod)
anova(instar.mod)

par(mfrow = c(1, 1))
visreg(instar.mod, "ant.post")

# I think mixed not needed because no repeat individuals across species or instar

inst.spp.compare <- emmeans(instar.mod, ~ species * instar + ant.post)
pairs(inst.spp.compare)

emmeans(instar.mod, pairwise ~ species * instar) #gives many, pull out the ones needed



# ggsave("crush depth with air-sac diameter.pdf", 
#        units = c("cm"), 
#        width = 12, height = 12,
#        device = "pdf",
#        path = NULL)

