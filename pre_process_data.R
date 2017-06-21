library(tidyverse)

# read in raw data
dataset<-readLines("src/data/negotiate/data.txt")

dataparse<-data.frame(item1=NA,
                      item2=NA,
                      item3=NA,
                      value1=NA,
                      value2=NA,
                      value3=NA,
                      part_item1=NA,
                      part_item2=NA,
                      part_item3=NA,
                      part_value1=NA,
                      part_value2=NA,
                      part_value3=NA,
                      text=NA,
                      out1=NA,
                      out2=NA,
                      out3=NA)

for(x in 1:length(dataset)){
  splitted<-strsplit(dataset[x]," ")[[1]]
  dataparse[x,c(paste0("item",1:3),paste0("value",1:3))]<-splitted[1:6]
  dataparse[x,"text"]<-paste(splitted[7:(length(splitted)-6)], collapse=" ")
  dataparse[x,c(paste0("part_item",1:3),paste0("part_value",1:3))]<-splitted[(length(splitted)-5):length(splitted)]
  print(x)
}
dataparse<-dataparse[!is.na(dataparse),]

dataparse$id<-1:nrow(dataparse)
dataparse$group<-(1:nrow(dataparse)+1)%/% 2


### get turn-level text

turnparse<-data.frame(id=NA,
                      group=NA,
                      turn=NA,
                      youthem=NA,
                      text=NA)

tpb<-txtProgressBar(0,nrow(dataparse))
for(x in 1:nrow(dataparse)){
  person.turns<-strsplit(dataparse[x,"text"],"<eos>")[[1]]
  row.set<-(nrow(turnparse)+1):(nrow(turnparse)+length(person.turns))
  turnparse[row.set,c("id","group")]<-dataparse[x,c("id","group")]
  turnparse[row.set,"text"]<-person.turns
  turnparse[row.set,"turn"]<-1:length(person.turns)
  setTxtProgressBar(tpb,x)
}
turnparse <- turnparse[!(is.na(turnparse$group)),]

turntext = turnparse %>%
  filter(!is.na(group)) %>%
  group_by(id) %>% 
  slice(-n()) %>%
  slice(-n()) 

write_csv(turntext,"turn_tf.csv")

### get group-level outcomes
agrees = turnparse %>%
  select(-cleantext) %>%
  filter(!is.na(group)) %>%
  group_by(id) %>% 
  slice(n()) 

items = turnparse %>%
  select(-cleantext) %>%
  
  filter(!is.na(group)) %>%
  group_by(id) %>% 
  slice(-n())  %>%
  slice(n())

groups = inner_join(agrees, items, by = c("id", "group")) %>%
  slice(1:100) %>%
  select(-starts_with("you"), -starts_with("turn")) %>%
  mutate(text.x = as.character(text.x),
         agreement = unlist(lapply(str_split(text.x, "=| "), 
                                function(x) {x[3]})),
         reward = unlist(lapply(str_split(text.x, "=| "), 
                                   function(x) {x[2]}))) %>%
  select(-text.x) %>%
  mutate(text.y = as.character(text.y),
         num_items = str_split(text.y, "=| "),
         item1outcome = unlist(lapply(num_items, 
                               function(x) {x[3]})),
         
         item2outcome = unlist(lapply(num_items, 
                               function(x) {x[5]})),
         
         item3outcome = unlist(lapply(num_items, 
                               function(x) {x[7]}))) %>%
  select(-text.y, -num_items)

dp = dataparse %>%
  left_join(groups, by = c("id", "group"))

write_csv(dp,"group_df.csv")

