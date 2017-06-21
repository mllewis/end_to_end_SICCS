turnparse$youthem<-1*grepl("YOU:",turnparse$text,fixed=T)

turnparse$text<-gsub("YOU:","",turnparse$text, fixed=T)
turnparse$text<-gsub("THEM:","",turnparse$text, fixed=T)
turnparse$text<-tm::stripWhitespace(turnparse$text)


turnparse$text<-gsub("|",". ",turnparse$text,fixed=T)
for (x in 1:10){
  turnparse$text<-gsub(".?","?",turnparse$text,fixed=T)
  turnparse$text<-gsub("?.","?",turnparse$text,fixed=T)
  turnparse$text<-gsub("!?","?",turnparse$text,fixed=T)
  turnparse$text<-gsub("?!","?",turnparse$text,fixed=T)
  turnparse$text<-gsub("??","?",turnparse$text,fixed=T)
  turnparse$text<-gsub("!!","!",turnparse$text,fixed=T)
}

turnparse$text<-gsub("ha ha"," haha ",turnparse$text,fixed=T)
turnparse$text<-gsub("lol"," haha ",turnparse$text,fixed=T)
turnparse$text<-gsub("LOL"," haha ",turnparse$text,fixed=T)
turnparse$text<-gsub("LOl"," haha ",turnparse$text,fixed=T)
turnparse$text<-gsub("Lol"," haha ",turnparse$text,fixed=T)
turnparse$text<-gsub("!"," xmark.",turnparse$text,fixed=T)
for (x in 1:10){
  turnparse$text<-gsub("?"," qmark.",turnparse$text,fixed=T)
}