library(tidyverse)
library(ggplot2)


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
                          2))




  