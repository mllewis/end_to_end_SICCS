#source("politenessTools.R")

politeness<-function(text, set="short"){
  features<-list()
  c.text<-cleantext(text, stop.words=FALSE)
  c.words<-strsplit(c.text, " ")[[1]]
  p.words<-tolower(core.parser(text)$all.parses)
  p.nonum<-gsub("-[0-99]","",p.words)
  c.nums<-substr(p.words, sapply(p.words, function(x) gregexpr(",",x,fixed=T)[[1]][1])+2, nchar(p.words)-1)
  ########################################################
  features[["hedges"]]<-sum(textcounter(hedge.list,c.words,words=T))
  features[["positive"]]<-sum(textcounter(positive.list,c.words,words=T))
  features[["negative"]]<-sum(textcounter(negative.list,c.words,words=T))
  features[["subjunctive"]]<-textcounter(c("could you","would you"),c.text)
  features[["indicative"]]<-textcounter(c("can you","will you"),c.text)
  features[["bytheway"]]<-textcounter(c("by the way"),c.text)
  features[["hello"]]<-sum(textcounter(c("hi","hello","hey"),c.words,words=T))
  features[["gratitude"]]<-sum(c(startsWith(c.words,"thank"),grepl("(appreciate, i)",p.nonum,fixed=T)))
  
  features[["groupidentity"]]<-sum(textcounter(c("we", "our", "ours", "us", "ourselves"),c.words,words=T))
  features[["questions"]]<-sum(textcounter(c("who","what","where","when","why","how","which"),c.words,words=T))
  #for(q in c("who","what","where","when","why","how","which")) features[[q]]<-sum(q%in%c.words) #getleftpos(p) in (1,2)
  
  features[["apologize"]]<-(sum(textcounter(c("sorry"," woops","oops","whoops"),c.words,words=T))
                            +sum(textcounter(c("dobj(excuse, me)","nsubj(apologize, i)","dobj(forgive, me)"),p.nonum, words=T)))
  features[["infact"]]<-(sum(textcounter(c("really", "actually", "honestly", "surely"),c.words,words=T))
                         +sum(textcounter(c("det(point, the)","det(reality, the)","det(truth, the)","case(fact, in)"),p.nonum,words=T)))
  features[["deference"]]<-sum(textcounter(paste0(c("great","good","nice","interesting","cool","excellent","awesome"),"-1"),c.nums,words=T))
  features[["conj"]]<-sum(textcounter(paste0(c("so","then","and","but","or"),"-1"),c.nums,words=T))
  
  features[["please_start"]]<-sum(c.nums=="please-1")
  features[["please"]]<-sum((!grepl("-1",p.words,fixed=T))&grepl("please",p.words,fixed=T))
  
  features[["firstperson_start"]]<-sum(textcounter(paste0(c("i","my","mine","myself"),"-1"),c.nums,words=T))
  features[["firstperson"]]<-sum(textcounter(c("i","my","mine","myself"),c.words,words=T))-features[["firstperson_start"]]
  
  features[["secondperson_start"]]<-sum(textcounter(paste0(c("you","your","yours","yourself"),"-1"),c.nums,words=T))
  features[["secondperson"]]<-sum(textcounter(c("you","your","yours","yourself"),c.words,words=T))-features[["secondperson_start"]]
  
  return(features)
}

options(java.parameters = "-Xmx8g")

require(qdap)
require(coreNLP)
require(tm)
require(rJava)
require(quanteda)

initCoreNLP("/stanford-corenlp/", mem="8g")
################################################################

hedge.list<-readLines("modeldata/hedges.txt")
positive.list<-readLines("modeldata/positive-words.txt")
negative.list<-readLines("modeldata/negative-words.txt")
liwc.lists<-quanteda::dictionary(file="modeldata/LQ.dic")
wordlists<-liwc.lists[c("ipron@Impersonal","swear@Swear","negate@Negations")]
names(wordlists)<-c("ImperPronoun","Swear","Negation")

wordlists[["FilledPause"]]<-c("er","sigh","hm*","uh*","um*")
wordlists[["InformalTitle"]]<-c("dude*", "bro*", "boss", "bud", "buddy", "champ", "man", "guy*", "guy", "brotha", "sista", "son", "sonny", "chief")
wordlists[["FormalTitle"]]<-c("sir", "ma'am", "maam", "mister", "mr*", "ms*", "madam", "miss", "gentleman", "lady")


wordlists[["Reasoning"]]<-c("reason", "why i", "why we", "explain", "you understand","because")
wordlists[["Reassurance"]]<-c(c("'s okay", "n't worry", "no big deal", "not a big deal", "no problem", 
                                "no worries", "'s fine", "you 're good", "is fine", "is okay"))
wordlists[["AskAgency"]]<-c("do me a favor", "let me", "allow me", "can i", "should i", "may i", "might i", "could i")

wordlists[["GiveAgency"]]<-c("let you", "allow you", "you can", "you may", "you could")
wordlists[["Goodbye"]]<-c("goodbye", "bye", "see you later")
wordlists[["ForMe"]]<-c("for me","for us")
wordlists[["ForYou"]]<-"for you"
################################################################
row.to.char<-function(deps){
  return(paste0(deps$type,"(",
                deps$governor,"-",
                deps$governorIdx,", ",
                deps$dependent,"-",
                deps$depIndex,")"))
}

core.parser<-function(text){
  sentences<-as.list(qdap::sent_detect(text))
  parses<-list()
  for (s in 1:length(sentences)){
    a.s<-coreNLP::annotateString(sentences[[s]])
    dep.table<-coreNLP::getDependency(a.s, type="collapsed")
    dep.table<-dep.table[dep.table$sentence==1,]
    dep.char<-c()
    for (x in 1:nrow(dep.table)){
      dep.char<-c(dep.char,row.to.char(dep.table[dep.table$depIndex==x,]))
    }
    parses[[s]]<-dep.char
  }
  all.parses=do.call(c, parses)
  return(list(sentences=sentences,
              parses=parses,
              all.parses=all.parses))
}


textcounter<-function (counted, texts, words=F, fixed = T) {
  texts
  counts <- rep(0, length(texts))
  if(words){
    for (x in counted) counts <- counts + (texts==x)
  }else {
    for (x in counted) {
      counts <- counts + sapply(gregexpr(x, texts, fixed = fixed), 
                                function(z) ifelse(z[1] == (-1), 0, length(z)))
    }
  }
  return(counts)
}

cleantext<-function (ex, language = "english", stop.words = TRUE) {
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

# 
# LIWCwrap<-function (text, dict = LQ, binary = F, ...) {
#   CTB <- as.matrix(array(0, c(length(text), length(dict))))
#   WC <- qdap::word_count(text)
#   wc1 <- (!is.na(WC))
#   CTD <- as.matrix(quanteda::dfm(text[wc1], dictionary = dict, 
#                                  verbose = F, ...))[, 1:length(dict)]
#   if (is.null(nrow(CTD))) 
#     CTD <- CTD/WC[wc1]
#   if (!is.null(nrow(CTD))) 
#     CTD <- apply(CTD, 2, function(x) x/WC[wc1])
#   CTB[wc1, ] <- CTD
#   colnames(CTB) <- substr(names(dict), 0, unlist(gregexpr("@", 
#                                                           names(dict))) - 1)
#   if (binary) 
#     CTB <- 1 * (CTB == 0)
#   return(CTB)
# }

# Model for word lists
# 
# load("data/MorFo.RData")
# MorFoFeatures<-function(text){
#   CTD<-as.matrix(quanteda::dfm(text, dictionary=MorFo))[,1:11]
#   WC<-qdap::word_count(text)
#   CTD<-apply(CTD, 2, function(x) x/WC)
#   return(CTD)
# }
