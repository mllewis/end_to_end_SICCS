turnparse$youthem<-1*grepl("YOU:",turnparse$text,fixed=T)

turnparse$text<-gsub("YOU:","",turnparse$text, fixed=T)
turnparse$text<-gsub("THEM:","",turnparse$text, fixed=T)

turnparse$cleantext<-cleantext(turnparse$text,stop.words=F)


max.turns<-sapply(1:max(turnparse$group,na.rm=T), function(x) max(turnparse[turnparse$group==x,"turn"]))
library(dplyr)
turnparse %>%
  group_by(group)
plot(turnparse$turn)


cleantext<-function (ex, language = "english", stop.words = TRUE) {
  for (x in 1:10){
    ex<-gsub(".?","?",ex,fixed=T)
    ex<-gsub("?.","?",ex,fixed=T)
    ex<-gsub("!?","?",ex,fixed=T)
    ex<-gsub("?!","?",ex,fixed=T)
    ex<-gsub("??","?",ex,fixed=T)
    ex<-gsub("!!","!",ex,fixed=T)
  }
  
  ex<-gsub("ha ha"," haha ",ex,fixed=T)
  ex<-gsub("lol"," haha ",ex,fixed=T)
  ex<-gsub("LOL"," haha ",ex,fixed=T)
  ex<-gsub("LOl"," haha ",ex,fixed=T)
  ex<-gsub("Lol"," haha ",ex,fixed=T)
  ex<-gsub("!"," xmark.",ex,fixed=T)
  for (x in 1:10){
    ex<-gsub("?"," qmark.",ex,fixed=T)
  }
  ex <- tolower(ex)
  if (language == "english") {
    ex <- ctxpand(ex)
  }
  ex <- gsub("[[:punct:]]", " ", ex)
  ex <- gsub("[[:cntrl:]]", " ", ex)
  if (length(stop.words) > 1) {
    ex <- tm::removeWords(ex, stop.words)
  }
  else if (stop.words) {
    ex <- tm::removeWords(ex, tm::stopwords(language))
  }
  ex <- tm::removeNumbers(ex)
  ex <- tm::stripWhitespace(ex)
  return(as.character(ex))
}

ctxpand<-function(text){
  text<-sapply(text, function(x) gsub("let's", "let us", x, fixed=T))
  text<-sapply(text, function(x) gsub("i'm", "i am", x, fixed=T))
  text<-sapply(text, function(x) gsub("won't", "will not", x, fixed=T))
  text<-sapply(text, function(x) gsub("can't", "cannot", x, fixed=T))
  text<-sapply(text, function(x) gsub("shan't", "shall not", x, fixed=T))
  text<-sapply(text, function(x) gsub("'d", " would", x, fixed=T))
  text<-sapply(text, function(x) gsub("'ve", " have", x, fixed=T))
  text<-sapply(text, function(x) gsub("'s", " is", x, fixed=T))
  text<-sapply(text, function(x) gsub("'ll", " will", x, fixed=T))
  text<-sapply(text, function(x) gsub("'re", " are", x, fixed=T))
  text<-sapply(text, function(x) gsub("n't", " not", x, fixed=T))
  text<-sapply(text, function(x) gsub("u.s.a.", "usa", x, fixed=T))
  text<-sapply(text, function(x) gsub("u.s.", "usa", x, fixed=T))
  text<-sapply(text, function(x) gsub("e.g.", "eg", x, fixed=T))
  text<-sapply(text, function(x) gsub("i.e.", "ie", x, fixed=T))
  return(text)}
