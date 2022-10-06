library(tidyverse)
library(ggplot2)
library(readr)



instar <- read_csv("~/student_documents/UBC/Research/Chaoborus crush depth/instar_head_capsule.csv")



ggplot(data = instar, 
       aes(x= larva, 
           group = species, 
           colour= species)) +
  geom_point(aes(y= head_length))
