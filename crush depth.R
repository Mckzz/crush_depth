library(tidyverse)
library(ggplot2)
library(ggpubr)


setwd("~/student_documents/UBC/Research/Chaoborus crush depth")


crush_raw <- read_csv("sac_fail.csv")
print(crush_raw)

ggplot(data = crush_raw,
       aes(x = larva, 
           y = head_cap_len, 
           colour = species)) +
  geom_point() #+
  #theme_classic()


##################    asigning instar based on head_cap_len and species    #################

crush_raw <- crush_raw %>% 
  mutate(instar = replace(instar, 
                          head_cap_len > 1.5 & 
                            head_cap_len < 2 & 
                            species == "trivittatus", # americanus and trivittatus based on Fedorenko and Swift 1972
                          4)) %>%
  mutate(instar = replace(instar, 
                           head_cap_len > 0.75 & 
                             head_cap_len < 1 & 
                             species == "trivittatus", 
                          3)) %>%
  mutate(instar = replace(instar, 
                          head_cap_len > 0.5 & 
                            head_cap_len < 0.75 & 
                            species == "trivittatus", 
                          2)) %>%
  mutate(instar = replace(instar, 
                          head_cap_len > 1.35 & 
                            head_cap_len < 2 & 
                            species == "americanus",
                          4)) %>%
  mutate(instar = replace(instar, 
                          head_cap_len > 1 & 
                            head_cap_len < 1.35 & 
                            species == "americanus", 
                          3)) %>% 
  mutate(instar = replace(instar,
                          head_cap_len > 0.5 &
                            head_cap_len < 1 &
                            species == "americanus",
                          2)) %>%
  mutate(instar = replace(instar,
                          head_cap_len > 0.5 &
                            head_cap_len < 1 &
                            species == "punctipenis",
                          3)) %>%
  mutate(instar = replace(instar,
                          head_cap_len > 0 &
                            head_cap_len < 0.5 &
                            species == "punctipenis",
                          2)) %>%
  mutate(species = as_factor(species))

print(crush_raw)

###########     simple scatter of dimensions with crush depth    ###############

# make separate data frames for crush-pressure and dimensions 
# so they can be pivoted separately and then recombined

crush_raw.atm <- crush_raw %>% ###    Maybe still need to make ant.post a vector...
  select(1:9) %>% # take only crush atm values
  pivot_longer(cols=c(`antR`, `antL`, `postR`, `postL`), 
               names_to = "sac", values_to = "atm") %>%
  mutate(ant.post = "NA") %>% 
  mutate(ant.post = replace(ant.post,
                            startsWith(sac, "a"),
                            'ant')) %>%
  mutate(ant.post = replace(ant.post,
                            startsWith(sac, "p"),
                            'post')) %>%
  mutate(side = "NA") %>%
  mutate(side = replace(side,
                            endsWith(sac, "R"),
                            'right')) %>%
  mutate(side = replace(side,
                            endsWith(sac, "L"),
                            'left')) %>%
  slice(rep(1:n(), each = 2)) %>% # so that an ant or post sac on a certain side can have both an area and diameter
  select(larva, site, species, head_cap_len, instar, ant.post, side, atm) %>%
  arrange(larva, side, by_group = T)
  
crush_raw.atm$sac <- NULL
  
print(crush_raw.atm, n = 20)


crush_raw.dimen <- crush_raw %>% 
  select(1:5, 10:13) %>% # take only dimension values
  pivot_longer(cols=c(`ant_area`, `ant_diameter`, `post_area`, `post_diameter`), 
               names_to = "dimension", values_to = "value") %>%
  mutate(ant.post = "NA") %>% 
  mutate(ant.post = replace(ant.post,
                            startsWith(dimension, "a"),
                            'ant')) %>%
  mutate(ant.post = replace(ant.post,
                            startsWith(dimension, "p"),
                            'post')) %>%
  mutate(dimension = replace(dimension,
                        endsWith(dimension, "a"),
                        'area')) %>%
  mutate(dimension = replace(dimension,
                        endsWith(dimension, "r"),
                        'diameter')) %>%
  slice(rep(1:n(), times = 2)) %>% # times instead of each, to so that area and diameter alternate as they should to combine with atm df
  select(larva, site, species, head_cap_len, instar, ant.post, dimension, value) %>%
  arrange(larva)

print(crush_raw.dimen, n = 20)

# recombine for single data frame
crush_raw.long <- crush_raw.atm %>% 
  mutate(dimension = crush_raw.dimen$dimension) %>% # assumed to be the same for left and right airsacs
  mutate(value = crush_raw.dimen$value)
  

print(crush_raw.long)

# for plotting using area
area.df <- crush_raw.long %>%
  filter(dimension == "area") %>%
  group_by(species)
print(area.df)

# for plotting using diameter
diameter.df <- crush_raw.long %>%
  filter(dimension == "diameter") %>%
  group_by(species)
print(diameter.df)

# plot with reg lines by species
ggplot(data = diameter.df,
       aes(x = value, 
           y = atm,
           colour = species, group = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = T) +
  stat_regline_equation(label.x.npc = 0, 
                        label.y.npc = 1, 
                        aes(label = ..eq.label..)) +
  theme_classic()


# with no regard to species
diameter.model <- lm(atm ~ value, data = diameter.df)

summary(diameter.model)
plot(diameter.model)
coef(diameter.model)

# species separated apriori: now can add other explanatory attributes (ant.post, instar etc.)
z <- lm(atm ~ value, data = subset(diameter.df, species == 'trivittatus'))

summary(z)
plot(z)
coef(z)





