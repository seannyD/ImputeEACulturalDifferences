# Take a cultural database and make a distance matrix between societies


library(cluster)

try(setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/processing/"))


# Load society data
eas = read.csv("../data/dplace-data-1.0/csv/EA_societies.csv", stringsAsFactors = F)

# Load data about variables
eav = read.csv("../data/dplace-data-1.0/csv/EAVariableList.csv", stringsAsFactors = F)

# Load language data
l = read.csv("../data/FAIR_langauges_glotto_xdid.csv", stringsAsFactors = F)

makeDistanceMatrix = function(filename, variables=c()){
  # Load imputed Ethnogrpahic Atlas data
  eadx = read.csv(filename, stringsAsFactors = F)
  
  # Keep only FAIR langauges
  eadx = eadx[eadx$soc_id %in% l$soc.id,]
  
  # Remove family and area data
  eadx = eadx[,!names(eadx) %in% c("Family","autotyp.area")]
  
  # Optionally only keep some variables
  if(length(variables)>0){
    eadx = eadx[,names(eadx) %in% variables]
  }
  
  # Remove any variables that still have missing data
  eadx = eadx[,apply(eadx,2,function(X){sum(is.na(X))==0})]
  #eadx = eadx[,names(eadx)!="X69"]
  
  eadx = eadx[names(eadx)!="X",]
  
  # Convert to factor
  for(i in 1:ncol(eadx)){
    eadx[,i] = as.factor(eadx[,i])
  }
  # Convert soc_id back to character
  eadx$soc_id = as.character(eadx$soc_id)
  
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

files = list.files("../data/EA_imputed/completeDataframes/","*.csv")
dists = list()
for(i in 1:length(files)){
  print(files[i])
  dists[[i]] = makeDistanceMatrix(paste0("../data/EA_imputed/completeDataframes/",files[i]))
}

# Check corelation
plot(dists[[1]],dists[[2]])
plot(dists[[2]],dists[[3]])
plot(dists[[3]],dists[[4]])
plot(dists[[4]],dists[[5]])

# Mean correlation between sets
cbx = combn(1:50,2)
cor.x = c()
upperx = upper.tri(dists[[1]],diag = F)
for(cx in 1:ncol(cbx)){
  cor.x = c(cor.x,
    cor(dists[[cbx[1,cx]]][upperx],
        dists[[cbx[2,cx]]][upperx]))
}
mean(cor.x)

# Get mean values of each cell across imputations
dist.m = Reduce('+', dists)
dist.m = dist.m / length(dists)

# get variation
var = apply(simplify2array(dists), 1:2, sd)
hist(var[upper.tri(var,diag = F)])

# variation over mean distance
hist((var/dist.m)[upper.tri(var,diag=F)])
quantile((var/dist.m)[upper.tri(var,diag=F)], probs = c(0.95))


# tests of normality
norm.test = apply(simplify2array(dists), 1:2, 
  function(X){
    if(length(unique(as.vector(X)))>3){
      return(shapiro.test(X)$p.value)}
    else{
      return(NA)
    }
  })

# Write distance matrix
write.csv(dist.m, "../results/EA_distances/CulturalDistances.csv")

# convert to long form
library(reshape2)
dist.long = melt(dist.m)
write.csv(dist.long, file="../results/EA_distances/CulturalDistances_Long.csv", row.names = F)

# Visualise the distances
library(mclust)

hc = hclust(dist(dist.m))
pdf("../results/CulturalDistanceTrees/CulturalDistance.pdf", width=20, height=10)
plot(hc)
dev.off()

#####################################
# Make sub-domain distance matrices:
# Load mappings between concepticon and Ethnographic Atlas
c2ea = read.csv("../data/Concepticon_to_EA.csv", stringsAsFactors = F)

# Remove domains that don't have matches
c2ea = c2ea[!is.na(c2ea$ea),]
c2ea = c2ea[c2ea$ea!="",]

c2ea.for.printing = data.frame()

# For each sub-domain, make a distance matrix and visualisation
for(i in 1:nrow(c2ea)){
  print(c2ea$concepticon[i])
  cats = strsplit(c2ea[i,]$ea,"-")[[1]]
  eavars = c()
  for(cx in cats){
    eavars = c(eavars,eav[grepl(cx,eav$IndexCategory),]$VarID)
  }
  eavars = unique(eavars)
  #distx = dist(eadx[,names(eadx) %in% paste0("X",eavars)])
  
  c2ea.for.printing = rbind(c2ea.for.printing,
      data.frame(
        Concepticon.Domain =c2ea$concepticon[i],
        EA.VarID = eavars,
        EA.IndexCategory = eav[match(eavars,eav$VarID),]$IndexCategory,
        EA.VarTitle = eav[match(eavars,eav$VarID),]$VarTitle,
        stringsAsFactors = F
      ))
  
  distsC = list()
  for(j in 1:length(files)){
    print(files[j])
    distsC[[j]] = makeDistanceMatrix(
      paste0("../data/EA_imputed/completeDataframes/",files[j]),
      variables=c(paste0("X",eavars),"soc_id"))
  }
  
  distx.m = Reduce('+', distsC)
  distx.m = distx.m / length(distsC)
    
  filenamex = gsub(' ',"_",c2ea[i,]$concepticon)
  write.csv(
    distx.m,
    file = paste0("../results/EA_distances/",filenamex,'.csv')
  )
  
  distx.long = melt(distx.m)
  write.csv(distx.long, 
            file=paste0("../results/EA_distances/",filenamex,'_long.csv'), row.names = F)
  
  
  hcx = hclust(as.dist(distx.m))
  pdf(
    paste0("../results/CulturalDistanceTrees/CulturalDistance_",filenamex,".pdf"),
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


write.csv(c2ea.for.printing,
          "../data/Concepticon_to_EA_FullVariableList.csv",row.names = F)

####
# Kinship full (all categorical variables)
# eadx.full = read.csv(filename, stringsAsFactors = F)
# eadx.full = as.data.frame(eadx.full)
# kinship.vars = eav$VarID[grepl("Kinship",eav$IndexCategory)]
# eadx.kinship = eadx.full[,names(eadx.full) %in% paste0("X",kinship.vars)]
# 
# eadx.kinship =as.data.frame(apply(eadx.kinship,2,as.factor))
# distx = daisy(eadx.kinship,
#               metric = "gower")
# distx.m = as.matrix(distx)
# rownames(distx.m) = eadx.full$soc_id
# colnames(distx.m)= eadx.full$soc_id
# 
# write.csv(distx.m, "../results/EA_distances/Kinship_AllSocieties.csv")
