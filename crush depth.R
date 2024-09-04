library(tidyverse)
library(ggplot2)
library(ggpubr)

# fresh water density value
FWD <- 997.0474
# gravity acceleration
g <- 9.80665

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
                          head_cap_len > 1.7 & 
                            head_cap_len < 2 & 
                            species == "trivittatus", # americanus and trivittatus based on Fedorenko and Swift 1972
                          4)) %>%
  mutate(instar = replace(instar, 
                           head_cap_len > 1.2 & 
                             head_cap_len < 1.7 & 
                             species == "trivittatus", 
                          3)) %>%
  mutate(instar = replace(instar, 
                          head_cap_len > 0.5 & 
                            head_cap_len < 1.2 & 
                            species == "trivittatus", 
                          2)) %>%
  mutate(instar = replace(instar, 
                          head_cap_len < 0.5 & 
                            species == "trivittatus", 
                          1)) %>%
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
  
print(crush_raw.atm, n = 10)


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
  slice(rep(1:n(), times = 2)) %>% # times instead of each, so that area and diameter alternate as they should to combine with atm df
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
  group_by(species) %>%
  mutate(instar = as.factor(instar)) %>%
  rename(sqr_mm = value)

print(area.df)

# for plotting using diameter
diameter.df <- crush_raw.long %>%
  filter(dimension == "diameter") %>%
  group_by(species) %>%
  mutate(instar = as.factor(instar)) %>%
  mutate(depth.m = ((atm*101.325)*1000)/(FWD*g)) %>%
  rename(mm = value)

print(diameter.df)

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

ggsave("crush depth with air-sac diameter.pdf", 
       units = c("cm"), 
       width = 12, height = 12,
       device = "pdf",
       path = NULL)

# plot americanus crush depth with instar
ggplot(data = filter(diameter.df, species == "americanus"),
       aes(x = instar, 
           y = depth.m,
           colour = species)) +
  geom_point(#aes(shape = factor(ant.post)),
    size = 3,
    alpha = 0.3) +
  ylab("max depth (m)") +
  theme_classic() +
  theme(legend.position = c(0.35, 0.9))

# df to pull numbers for combined vitro/vivo crush depth plot
am_4th_intact <- diameter.df %>%
  filter(species == "americanus" & instar == "4") %>%
  select(species, depth.m) %>%
  write_csv(file = "C:\\Users\\evanm\\Documents\\student_documents\\UBC\\Research\\Malawi\\data/am_4th_intact.csv")
  


###########     use Fedorenko and Swift 1972 to plot max pressures experienced over top?    ###########

##########      survival curve
trivit <- diameter.df %>%
  filter(species == "trivittatus")

amer <- diameter.df %>%
  filter(species == "americanus")

punct <- diameter.df %>%
  filter(species == "punctipennis")

# a df starting just as the pressure sequence that goes to max pressure applied
press_seq <- data.frame(atm_axis = seq(0, 
                           max(diameter.df$atm, na.rm = T), 
                           by = 0.1))

# check an individual value
trivit$atm [trivit$atm > 2]
(sum((trivit$atm > 2) / length(trivit$atm)))


###############     Sarah P's solution to making a curve     ###############

triv_collapse_pressure <- trivit$atm

# Get the frequencies of collapse for each pressure
curve <- plyr::count(triv_collapse_pressure)

# Rename the first column
names(curve)[1] <- "pressure_atm"

# insert a row at the top that is the last pressure to have no collapse (100% survivourship): 1atm, 0freq
curve <- add_row(curve, pressure_atm = 1, freq = 0, .before = 1)


# Get the count of collapsed sacs per pressure measurement
curve$cumulative_collapse <- cumsum(curve$freq)



# Calculate the total number of sacs
total_sacs <- length(triv_collapse_pressure)


# Calculate survivorship
curve$pct.survivorship <- ((total_sacs - curve$cumulative_collapse) / total_sacs) * 100

# Plot it
ggplot(curve, aes(x = pressure_atm, y = pct.survivorship)) +
  geom_point() +
  geom_smooth()






# number of trivittatus observations where collapse pressure is greater than the given pressure step, / total

totals <- count(diameter.df, species)
         
print(survivor)

nrow(diameter.df)


ggplot(diameter.df, aes(x = atm, 
                        colour = species,
                        linetype = instar)) +
  geom_density()



######################        model?           #####################
# with no regard to species

summary(diameter.model)
plot(diameter.model)
coef(diameter.model)

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


