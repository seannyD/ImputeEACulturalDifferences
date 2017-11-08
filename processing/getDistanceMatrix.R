# Take a cultural database and make a distance matrix between societies

library(cluster)

setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/processing/")


# Load society data
eas = read.csv("../data/dplace-data-1.0/csv/EA_societies.csv", stringsAsFactors = F)

# Load data about variables
eav = read.csv("../data/dplace-data-1.0/csv/EAVariableList.csv", stringsAsFactors = F)

# Load language data
l = read.csv("../data/FAIR_langauges_glotto_xdid.csv", stringsAsFactors = F)

makeDistanceMatrix = function(filename){
# Load imputed Ethnogrpahic Atlas data
eadx = read.csv(filename, stringsAsFactors = F)

# Remove one variable that still has missing data
eadx = eadx[,apply(eadx,2,function(X){sum(is.na(X))==0})]
#eadx = eadx[,names(eadx)!="X69"]

# Convert to factor
for(i in 1:ncol(eadx)){
  eadx[,i] = as.factor(eadx[,i])
}
# Convert soc_id back to character
eadx$soc_id = as.character(eadx$soc_id)
eadx = eadx[,-which(names(eadx)=="Family")]



eadx = eadx[eadx$soc_id %in% l$soc.id,]

# names of languages according to Facebook
nx = l[match(eadx$soc_id,l$soc.id),]$Language
rownames(eadx) = nx

# Make distance matrix from factors
#dist = dist(eadx[,2:ncol(eadx)])
dist = daisy(eadx[,-which(names(eadx)=="soc_id")], metric = "gower")

# Convert to regular matrix
dist.m = as.matrix(dist)
rownames(dist.m) = nx
colnames(dist.m) = nx
return(dist.m)
}

dists = list()
for(i in 1:5){
  dists[[i]] = makeDistanceMatrix(paste0("../data/EA_imputed/EADX_Imputed_Complete_",i,".csv"))
}

# Check corelation
plot(dists[[1]],dists[[2]])
plot(dists[[2]],dists[[3]])
plot(dists[[3]],dists[[4]])
plot(dists[[4]],dists[[5]])

dist.m = Reduce('+', dists)
dist.m = dist.m / length(dists)

# Write distance matrix
write.csv(dist.m, "../results/EA_distances/CulturalDistances.csv")

# convert to long form
library(reshape2)
dist.long = melt(dist.m)
write.csv(dist.long, file="../results/EA_distances/CulturalDistances_Long.csv", row.names = F)

# Visualise the distances
library(mclust)

hc = hclust(dist)
pdf("../results/CulturalDistance.pdf", width=20, height=10)
plot(hc)
dev.off()


# Make sub-domain distance matrices:
# Load mappings between concepticon and Ethnographic Atlas
c2ea = read.csv("../data/Concepticon_to_EA.csv", stringsAsFactors = F)

# Remove domains that don't have matches
c2ea = c2ea[!is.na(c2ea$ea),]
c2ea = c2ea[c2ea$ea!="",]

# For each sub-domain, make a distance matrix and visualisation
for(i in 1:nrow(c2ea)){
  cats = strsplit(c2ea[i,]$ea,"-")[[1]]
  eavars = c()
  for(cx in cats){
    eavars = c(eavars,eav[grepl(cx,eav$IndexCategory),]$VarID)
  }
  eavars = unique(eavars)
  #distx = dist(eadx[,names(eadx) %in% paste0("X",eavars)])
  distx = daisy(eadx[,names(eadx) %in% paste0("X",eavars)],
               metric = "gower")
  distx.m = as.matrix(distx)
  rownames(distx.m) = nx
  colnames(distx.m) = nx
  filenamex = gsub(' ',"_",c2ea[i,]$concepticon)
  write.csv(
    distx.m,
    file = paste0("../results/EA_distances/",filenamex,'.csv')
  )
  
  distx.long = melt(distx.m)
  write.csv(distx.long, 
            file=paste0("../results/EA_distances/",filenamex,'_long.csv'), row.names = F)
  
  
  hcx = hclust(distx)
  pdf(
    paste0("../results/CulturalDistance_",filenamex,".pdf"),
    width=20, height=10)
  plot(hcx, main = c2ea[i,]$concepticon)
  dev.off()
  
}



fs = list.files("../results/EA_distances/","*_long.csv")
longlong = data.frame()
for(f in fs){
  fx = read.csv(paste0("../results/EA_distances/",f))
  namex = gsub("_long.csv","",f)
  namex = gsub("_"," ",namex)
  fx$semantic_domain = namex
  longlong = rbind(longlong,fx)
}

write.csv(longlong, "../results/EA_distances/All_Domains.csv", row.names = F)


####
# Kinship full (all categorical variables)
eadx.full = read.csv(filename, stringsAsFactors = F)
eadx.full = as.data.frame(eadx.full)
kinship.vars = eav$VarID[grepl("Kinship",eav$IndexCategory)]
eadx.kinship = eadx.full[,names(eadx.full) %in% paste0("X",kinship.vars)]

eadx.kinship =as.data.frame(apply(eadx.kinship,2,as.factor))
distx = daisy(eadx.kinship,
              metric = "gower")
distx.m = as.matrix(distx)
rownames(distx.m) = eadx.full$soc_id
colnames(distx.m)= eadx.full$soc_id
