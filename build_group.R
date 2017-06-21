library(tidyverse)

## get text data without outcomes
tp = read.csv("turnparse.csv")  

turntext = tp  %>%
  filter(!is.na(group)) %>%
  group_by(id) %>% 
  slice(-n()) %>%
  slice(-n()) 

write_csv(turntext,"turntext.csv")

# get group levels outcomes
agrees = tp %>%
  filter(!is.na(group)) %>%
  group_by(id) %>% 
  slice(n()) 

items = tp %>%
  filter(!is.na(group)) %>%
  group_by(id) %>% 
  slice(-n())  %>%
  slice(n())

groups = inner_join(agrees, items, by = c("id", "group")) %>%
        slice(1:100) %>%
        select(-starts_with("you"), -starts_with("turn")) %>%
        mutate(text.x = as.character(text.x),
               reward = unlist(lapply(str_split(text.x, "=| "), 
                             function(x) {x[3]})),
               agreement = unlist(lapply(str_split(text.x, " "), 
                                      function(x) {x[3]}))) %>%
        select(-text.x) %>%
        mutate(text.y = as.character(text.y),
               num_items = str_split(text.y, "=| "),
               item1 = unlist(lapply(num_items, 
                                     function(x) {x[5]})),
               
               item2 = unlist(lapply(num_items, 
                                     function(x) {x[7]})),
               
               item3 = unlist(lapply(num_items, 
                                     function(x) {x[9]}))) %>%
          select(-text.y, -num_items)

write_csv(groups,"group_outcomes.csv")

