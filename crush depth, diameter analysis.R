library(tidyverse)
library(ggplot2)
library(ggpubr)
library(emmeans)

print(diameter.df)

# simply plot crush depth with over all means for each species
ggplot(data = diameter.df,
       aes(x = species, 
           y = atm,
           colour = species)) +
  geom_point(alpha = 0.3) +
  #geom_point(aes(y = spp_mean, size = 3, colour = instar), shape = 1) +
  geom_point(aes(y = spp_mean, size = 3)) +
  theme_minimal()

# plot with reg lines by species
ggplot(data = diameter.df,
       aes(x = mm, 
           y = atm,
           colour = species)) +
  geom_point(#aes(shape = factor(ant.post)),
             size = 3,
             alpha = 0.3) +
  geom_smooth(method = "lm", se = T) +
  stat_regline_equation(label.x.npc = 0,
                        label.y.npc = 1,
                        aes(label = ..eq.label..),
                        show.legend = F) +
  xlab("Diameter (mm)") +
  annotate("rect", xmin = 0.3071954 - 0.016285646,
           xmax = 0.3071954 + 0.016285646,
           ymin = 1.9, ymax = 2.35, fill = "green3", alpha=0.4) +
  annotate("rect", xmin = 0.2985990 - 0.009361905,
           xmax = 0.2985990 + 0.009361905,
           ymin = 1.25, ymax = 1.4, fill = "firebrick", alpha=0.4) +
  theme_classic() +
  theme(legend.position = c(0.35, 0.9))


# export to combine with other species intact crush plot
# write_csv(filter(diameter.df, species == "americanus" | species == "trivittatus"), 
#           file = "C:\\Users\\evanm\\Documents\\student_documents\\UBC\\Research\\Malawi\\americanus comparison/americanus_instar_crush.csv")

# df to pull numbers for combined vitro/vivo crush depth plot
# am_4th_intact <- diameter.df %>%
#   filter(species == "americanus" & instar == "4") %>%
#   select(species, depth.m) %>%
#   write_csv(file = "C:\\Users\\evanm\\Documents\\student_documents\\UBC\\Research\\Malawi\\data/am_4th_intact.csv")
#   


###########     use Fedorenko and Swift 1972 to plot max pressures experienced over top?    ###########


######################     diameter   model?           #####################

library(lmerTest)
library(effects)
library(emmeans)

## check loging to test emmeans
diameter.df <- diameter.df %>%
  mutate(log10_atm = log10(atm), .after = atm) %>%
  print()

#######  linear NOT mixed model   #####
# diameter.model <- lm(atm ~ species * mm, data = diameter.df) # this one's output makes sense. idk why the lmer with random larva doesn't
diameter.model.log <- lm(log10_atm ~ species * mm, data = diameter.df) # play with log to test emmeans
summary(diameter.model.log)
#summary(diameter.model)

plot(diameter.model.log) #log fixes the qq residuals
# plot(diameter.model)
#hist(resid(diameter.model)) # histogram of residuals
hist(resid(diameter.model.log)) # histogram of residuals
anova(diameter.model.log)
# 
# emmeans(diameter.model, ~ species) # just gives means of crush depth
# emtrends(diameter.model, ~ species, var="mm") #gives slopes, or atm gained per mm diameter

emmeans(diameter.model.log, ~ species) # just gives means of crush depth
emtrends(diameter.model.log, ~ species, var="mm") #gives slopes, or atm gained per mm diameter
###   the trend is the slope of log10(atm) over mm

# Get slopes of x within each group
crush_slope_trends <- emtrends(diameter.model.log, ~ species, var = "mm") %>% 
  as.data.frame() %>%
  mutate(t_value = mm.trend / SE, 
         p_value = 2 * pt(-abs(t_value), df),
         mm.trend_unlog = 10^mm.trend) %>%
  print()




####    mixed model, estimates suddenly don't make sense    ####
diameter.model.mix <- lmer(atm ~ species * mm + (1|larva), data = diameter.df)
diameter.model.mix <- lmer(atm ~ species * mm + (mm|larva), data = diameter.df)

summary(diameter.model.mix)

anova(diameter.model.mix)

emtrends(diameter.model.mix, ~ species, var="mm") #gives slopes, or atm gained per mm diameter

####    trying to get model outputs onto a data frame
diameter.df$atm.fit <- predict(diameter.model) # error row number, but makes no sense why

plot(diameter.model)
coef(diameter.model)

atm.fit <- predict(diameter.model) %>%
  as.data.frame(atm.fit, row.names = "fit")

print(atm.fit)
nrow(atm.fit)
nrow(diameter.df)


# species separated apriori: now can add other explanatory attributes (ant.post, instar etc.)
t <- lm(atm ~ value, data = subset(diameter.df, species == 'trivittatus'))

summary(t)
plot(t)
coef(t)

a <- lm(atm ~ instar, data = subset(diameter.df, species == 'americanus'))

summary(a)
plot(a)
coef(a)
anova(a)

p <- lm(atm ~ value, data = subset(diameter.df, species == 'punctipenis'))

summary(p)
plot(p)
coef(p)


rm(z)
