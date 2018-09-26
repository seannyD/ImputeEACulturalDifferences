# Find the variables that most highly correlate with the cultural distances measure.

library(cluster)
library(reshape2)
library(mclust)
library(ggplot)
library(ggfortify)

try(setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/processing/"))

# Load language data
l = read.csv("../data/FAIR_langauges_glotto_xdid.csv", stringsAsFactors = F,encoding = "UTF-8",fileEncoding = "UTF-8")

eav = read.csv("../data/dplace-data-1.0/csv/EAVariableList.csv", stringsAsFactors = F)
eav$VarID = paste0("X",eav$VarID)

files = list.files("../data/EA_imputed/completeDataframes/","*.csv")
dists = list()
#for(i in 1:length(files)){
i = 1
  print(files[i])
  
  filename = paste0("../data/EA_imputed/completeDataframes/",files[i])
  eadx = read.csv(filename, stringsAsFactors = F)
  
  # Keep only FAIR langauges
  eadx = eadx[eadx$soc_id %in% l$soc.id,]
  # Remove family and area data
  eadx = eadx[,!names(eadx) %in% 
                c("Family","autotyp.area","X","soc_id")]
 
  names(eadx) = eav[match(names(eadx),eav$VarID),]$VarTitle
  for(x in 1:ncol(eadx)){
    eadx[,x] = factor(eadx[,x])
  }
  dist = daisy(eadx, metric = "gower")
  res = data.frame()
  for(v in names(eadx)){
    mtr = NA
    try(mtr <- ecodist::mantel(as.dist(dist)~
                    daisy(data.frame(eadx[,v]),metric = 'gower')))
    if(!is.na(mtr)){
      mtr = data.frame(t((mtr)))
      mtr$V = v
      res = rbind(res,mtr)
    }
  }

   
#}
  
res[,1] = as.numeric(as.character(res[,1]))
res[res[,1]==max(res[,1]),]
hist(res[,1])

res[order(abs(res$mantelr)),]


sum(res$pval3<0.05)/nrow(res)
# 88%

# Subsistence economy: dominant activity
# Descent: major type
# Domestic animals: type
# Domestic animals: plow cultivation
# Mean size of local communities