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
turnparse$youthem<-1*grepl("YOU:",turnparse$text,fixed=T)

write.csv(turnparse,"turnparse.csv",row.names=F)
write.csv(dataparse,"dataparse.csv",row.names=F)