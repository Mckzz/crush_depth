library(tidyverse)
library(ggplot2)


setwd("~/student_documents/UBC/Research/Chaoborus crush depth")


crush_raw <- read_csv("sac_fail.csv")
print(crush_raw)

ggplot(data = crush_raw,
       aes(x = larva, 
           y = head_cap_len, 
           colour = species)) +
  geom_point()
  theme_classic()


##################    asigning instar based on head_cap_len and species    #################


  crush_raw <- crush_raw %>% 
    mutate(instar = replace(instar, 
                            head_cap_len > 1.5 & 
                              head_cap_len < 2 & 
                              species == "trivittatus", 
                            4)) %>%
    mutate(instar = replace(instar, 
                            head_cap_len > 1.35 & 
                              head_cap_len < 2 & 
                              species == "americanus", 
                            4))
    
  