library(tidyverse)

tp = read.csv("turnparse.csv")



agrees = tp %>%
  filter(!is.na(group)) %>%
  group_by(id) %>% 
  slice(n()) 

items= tp %>%
  filter(!is.na(group)) %>%
  group_by(id) %>% 
  slice(-n())  %>%
  slice(n())

as.data.frame(tp[1:16,])

as.data.frame(m[1:16,])