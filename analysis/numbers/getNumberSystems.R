try(setwd("~/Documents/Bristol/word2vec/number_alignment_piecewise/"))

PATH <- "data/numbers"
numbers <- read.csv(file.path(PATH,"number-alignments.csv")) %>%
  left_join(read.csv(file.path(PATH,"word_to_number.csv")),by=c("Number"="number"))
data.table::setnames(numbers,tolower(names(numbers))) #lowercase column names for consistency
PATH2 <- "data"
language_info <- read.csv(file.path(PATH2,"distances/FAIR_languages_glotto_xdid.csv"))
numbers <- left_join(numbers,select(language_info,Language,family,iso2),by=c("l1"="iso2"))
numbers <- numbers %>% rename(name_l1=Language,family_l1=family)
numbers <- left_join(numbers,select(language_info,Language,family,iso2),by=c("l2"="iso2"))
numbers <- numbers %>% rename(name_l2=Language,family_l2=family)

nLangs = unique(c(as.character(numbers$name_l1),as.character(numbers$name_l2)))

cnv = read.table("data/numbers/Calude_Verkerk_NumberData_tab_file.txt",sep="\t",header=T,
                 stringsAsFactors = F)

cnv$nLang = cnv$language

nLangs[!nLangs %in% cnv$nLang]

cnv$nLang[cnv$nLang=="German_ST "] = "German"
cnv$nLang[cnv$nLang=="Dutch_List"] = "Dutch"
cnv$nLang[cnv$nLang=="English_ST"] = "English"
cnv$nLang[cnv$nLang=="Portuguese_ST"] = "Portuguese"
cnv$nLang[cnv$nLang=="Swedish_List"] = "Swedish"
cnv$nLang[cnv$nLang=="Riksmal"] = "Norwegian (Bokmål)"

# Fix for Ukrainian
cnv[cnv$nLang=="Ukrainian",]$X16 = "6 + 10"

nLangs[!nLangs %in% cnv$nLang]

cnv = cnv[cnv$nLang %in% nLangs,]

findNType = function(l,n){
  if(n<=10){
    return("atom")
  }
  t = cnv[cnv$nLang==l,paste0("X",n)]
  t = gsub(" ","",t)
  return(t)
}

numbers$sameNumeralTypology = NA
for(i in 1:nrow(numbers)){
  l1 = as.character(numbers[i,]$name_l1)
  l2 = as.character(numbers[i,]$name_l2)
  if(l1 %in% cnv$nLang && l2 %in% cnv$nLang){
    n = numbers[i,]$number_numeric
    nl1 = findNType(l1,n)
    nl2 = findNType(l2,n)
    numbers$l1_typology[i] = nl1
    numbers$l2_typology[i] = nl2
    
    nl1x =  strsplit(nl1,"/")[[1]]
    nl2x =  strsplit(nl2,"/")[[1]]
    if(length(nl1x)==1 && length(nl2x==1)){
      numbers$sameNumeralTypology[i] = nl1==nl2
    } else{
      numbers$sameNumeralTypology[i] = length(intersect(nl1x,nl2x))>0
    }
    
  }
}

write.csv(numbers[,c("l1",'l2','number_numeric','l1_typology','l2_typology','sameNumeralTypology')],
          file="data/numbers/Calude_Verkerk_NumberData.csv",row.names = F)