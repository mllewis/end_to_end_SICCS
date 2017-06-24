library(tidyverse)
library(stringr)

# read in raw data
raw.data <- readLines("src/data/negotiate/data.txt")

### parse group-level data
split.data = raw.data %>%
  data.frame() %>%
  setNames(., c("raw")) %>%
  rowwise() %>%
  mutate(split = list(str_split(as.character(raw), " "))) %>%
  select(-raw) %>%
  mutate(item1 = unlist(split[1])[1],
         item2 = unlist(split[1])[2],
         item3 = unlist(split[1])[3]) %>%
  mutate(value1 = unlist(split[1])[4],
         value2 = unlist(split[1])[5],
         value3 = unlist(split[1])[6]) %>%
  mutate(part_item1 = tail(unlist(split[1]),6)[1],
         part_item2 = tail(unlist(split[1]),5)[1],
         part_item3 = tail(unlist(split[1]),4)[1]) %>%
  mutate(part_value1 = tail(unlist(split[1]),3)[1],
         part_value2 = tail(unlist(split[1]),2)[1],
         part_value3 = tail(unlist(split[1]),1)[1])  %>%
  mutate(reward = tail(unlist(split[1]),8)[1],
         reward = unlist(str_split(reward, "="))[2],
         agreement = tail(unlist(split[1]), 7)[1])

split.data$text = lapply(split.data$split, function(x){xx = unlist(x) 
                                      paste(xx[7:(length(xx)-12)], collapse = " ")}) # dplyr crashes

group.data = split.data %>%
              select(-split) %>%
              ungroup() %>%
              mutate(convo.id = 1:n(),
                     text = unlist(text)) 
                   #  group =  rep(1:(n()/2), each = 2)) # we can't straight-forwardly do this

write_csv(group.data,"group_df_new.csv")

### parse turn-level text
turn.data = group.data %>%
  select(convo.id, text) %>%
  rowwise() %>%
  mutate(turn.text = list(str_split(text, "<eos>"))) %>%
  select(-text) %>%
  unnest() %>%
  filter(!str_detect(turn.text, "<selection>")) %>%
  separate(turn.text, c("speaker", "turn.text"), ":")

write_csv(turn.data,"turn_df_new.csv")
