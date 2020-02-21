try(setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/analysis/"))
library(dplyr)
d = read.csv("../data/FAIR/nel-wiki-k100-alignments-merged-long.csv",
             stringsAsFactors = F,
             encoding = "UTF-8",fileEncoding = "UTF-8")

m = read.csv("../../offline/CLICS_numMeanings.csv",stringsAsFactors = F)

d = left_join(d,m,by = c("Glottocode_l1"="glottocode","Word_Form_l1"="sourceForm"))
names(m) = paste0(names(m),".l2")
d = left_join(d,m,by = c("Glottocode_l2"="glottocode.l2","Word_Form_l2"="sourceForm.l2"))

d = d[complete.cases(d[,c("NumMeanings","NumMeanings.l2")]),]


summary(lm(local_alignment ~ NumMeanings, data=d))
