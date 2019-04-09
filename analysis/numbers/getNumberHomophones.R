try(setwd("~/Documents/Bristol/word2vec/number_alignment_piecewise/"))

d = read.csv("../getListOfWords/wordsForNumberAnalysis.csv",stringsAsFactors = F,header = F,encoding = "UTF-8",fileEncoding = "UTF-8")
names(d) = c("l",'w1','ipa','concept')

numberConcepts = data.frame(
  matrix(c("eins::NUM",1,
           "zwei::NUM",2,
           "drei::NUM",3,
           "vier::NUM",4,
           "fünf::NUM",5,
           "sechs::NUM",6,
           "sieben::NUM",7,
           "acht::NUM",8,
           "neun::NUM",9,
           "zehn::NUM",10,
           "elf::NUM",11,
           "zwölf::NUM",12 ,
           "dreißig::NUM",13,
           "vierzig::NUM",14,
           "fünfzig::NUM",15,
           "sechzig::NUM",16 ,
           "siebzig::NUM",17 ,
           "achtzig::NUM",18,
           "neunzig::NUM",19,
           "zwanzig::NUM",20,
           "hundert::NUM",100,
           "tausend::NUM",1000),ncol=2,byrow = T),
  stringsAsFactors = F
)
names(numberConcepts) = c("code","number")

dNum = d[d$concept %in% numberConcepts$code,]
d = d[!d$concept %in% numberConcepts$code,]
d = d[!duplicated(d),]
dNum = dNum[!duplicated(dNum),]

homophones = data.frame()
for(l in unique(d$l)){
  h = data.frame(
    l = l,
    word = dNum[dNum$l==l,]$w1,
    ipa = dNum[dNum$l==l,]$ipa,
    concept = dNum[dNum$l==l,]$concept,
    homophone = dNum[dNum$l==l,]$ipa %in% d[d$l==l,]$ipa
  )
  h = h[!duplicated(h),]
  h$otherMeanings = sapply(h$ipa,function(X){
    paste(d[d$l==l & d$ipa==X,]$concept,collapse=";")
  })
  homophones = rbind(homophones,h)
}

homophones$number = numberConcepts[match(homophones$concept,numberConcepts$code),]$number

hFinal = homophones[homophones$homophone,]
write.csv(hFinal,"../number_alignment_piecewise/data/numbers/NumberHomophones.csv",row.names = F,fileEncoding = "UTF-8")