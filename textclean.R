turnparse$youthem<-1*grepl("YOU:",turnparse$text,fixed=T)

turnparse$text<-gsub("YOU:","",turnparse$text, fixed=T)
turnparse$text<-gsub("THEM:","",turnparse$text, fixed=T)

turnparse$cleantext<-cleantext(cleanpunct(turnparse$text),stop.words=F)

xxxx<-paste(turnparse$cleantext[1:50],collapse=" ")

politeness(xxxx)
turnparse$qmarks<-1*grepl("qmark",turnparse$cleantext,fixed=T)


max.turns<-sapply(1:max(turnparse$group,na.rm=T), function(x) max(turnparse[turnparse$group==x,"turn"]))
hist(max.turns)
