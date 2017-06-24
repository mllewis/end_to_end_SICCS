library(tidyverse)
library(stringr)

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
message("group data")
tpb<-txtProgressBar(0,length(dataset))
for(x in 1:length(dataset)){
  splitted<-strsplit(dataset[x]," ")[[1]]
  dataparse[x,c(paste0("item",1:3),paste0("value",1:3))]<-splitted[1:6]
  dataparse[x,"text"]<-paste(splitted[7:(length(splitted)-6)], collapse=" ")
  dataparse[x,c(paste0("part_item",1:3),paste0("part_value",1:3))]<-splitted[(length(splitted)-5):length(splitted)]
  setTxtProgressBar(tpb,x)
}
dataparse<-dataparse[!is.na(dataparse),]

dataparse$id<-1:nrow(dataparse)
dataparse$group<-(1:nrow(dataparse)+1)%/% 2

############################################################

### get turn-level text

turnparse<-data.frame(id=NA,
                      group=NA,
                      turn=NA,
                      text=NA)
message("turn data")
tpb<-txtProgressBar(0,nrow(dataparse))
for(x in 1:nrow(dataparse)){
  person.turns<-data.frame(id=dataparse[x,"id"],
                           group=dataparse[x,"group"],
                           text=strsplit(dataparse[x,"text"],"<eos>")[[1]])
  person.turns$turn<-1:nrow(person.turns)
  turnparse<-rbind(turnparse,person.turns)
  setTxtProgressBar(tpb,x)
}
turnparse <- turnparse[!(is.na(turnparse$group)),]

rm(tpb,x,person.turns,dataset,row.set,splitted)
############################################################
turntext = turnparse %>%
  filter(!is.na(group))
turntext<-turntext[!grepl("<selection>",turntext$text,fixed=T),]
turntext<-turntext[!grepl("reward=",turntext$text,fixed=T),]

write_csv(turntext,"turn_tf.csv")

### get group-level outcomes
agrees = turnparse[grepl("reward=",turnparse$text,fixed=T),]
items = turnparse[grepl("<selection>",turnparse$text,fixed=T),]

groups = inner_join(agrees, items, by = c("id", "group")) %>%
  select(-starts_with("you"), -starts_with("turn")) %>%
  mutate(text.x = as.character(text.x),
         agreement = unlist(lapply(str_split(text.x, "=| "), 
                                function(x) {x[4]})),
         reward = unlist(lapply(str_split(text.x, "=| "), 
                                   function(x) {x[3]}))) %>%
  select(-text.x) %>%
  mutate(text.y = as.character(text.y),
         num_items = str_split(text.y, "=| "),
         item1outcome = unlist(lapply(num_items, 
                               function(x) {x[5]})),
         
         item2outcome = unlist(lapply(num_items, 
                               function(x) {x[7]})),
         
         item3outcome = unlist(lapply(num_items, 
                               function(x) {x[9]}))) %>%
  select(-text.y, -num_items)

groups[groups$agreement=="disagree",c("reward",paste0("item",1:3,"outcome"))]<-c(0,NA,NA,NA)

dp = dataparse %>%
  left_join(groups, by = c("id", "group"))

groups[groups$reward=="no",]$reward<-0

write_csv(dp,"group_df.csv")
rm(agrees,dataparse,)
