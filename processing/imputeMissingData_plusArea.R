# USE the mice package to do multiple imputation.
# The method used is classification trees.
#  Language family is added as a variable with which the tree can make guesses about the missing data, allowing some control for language-family-specific tendencies.

library(tidyr)
library(mice)
setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/processing/")

l = read.csv("../data/FAIR_langauges_glotto_xdid.csv", stringsAsFactors = F)

eav = read.csv("../data/dplace-data-1.0/csv/EAVariableList.csv", stringsAsFactors = F)

ead = read.csv("../data/dplace-data-1.0/csv/EA_data.csv", stringsAsFactors = F)

# Load society data
eag = read.csv("../data/xd_id_to_language_and_area.csv", stringsAsFactors = F)
eas = read.csv("../data/dplace-data-1.0/csv/EA_societies.csv")

# Proportion of missing data
sum(is.na(ead$Code))/nrow(ead)

# restrict societies to those in the facebook sample
#ead = ead[ead$soc_id %in% l$soc.id,]

#ead = ead[,!names(ead) %in% c("Dataset","SubCase",'Year','VarID_Code',"Comment","EthnoReferences","SourceCodedData","AdminComment")]
ead = ead[,c("soc_id", "VarID",  "Code")]

eadx = spread(data = ead, VarID, Code)

xid = eas[match(eadx$soc_id, eas$soc_id),]$xd_id
eadx$Family = eag[match(xid,eag$xd_id),]$FamilyGlottocode
eadx$autotyp.area = eag[match(xid,eag$xd_id),]$autotyp.area

# remove pop size
eadx = eadx[,names(eadx)!="202"]

numObs = apply(eadx,2,function(X){sum(!is.na(X))})

eadx = eadx[,names(eadx) %in% names(numObs[numObs>60])]

numLangs = apply(eadx,1,function(X){sum(is.na(X))})
eadx = eadx[numLangs<31,]

in.final.and.ea = eadx$soc_id %in% l[l$in.final.analysis & !is.na(l$soc.id),]$soc.id

# missing data total
sum(is.na(eadx[,2:(ncol(eadx)-2)])) / prod(dim(eadx[,2:(ncol(eadx)-2)]))

# Missing data FAIR
sum(is.na(eadx[in.final.and.ea,2:(ncol(eadx)-2)])) / prod(dim(eadx[in.final.and.ea,2:(ncol(eadx)-2)]))

# Convert to factor
for(i in 2:ncol(eadx)){
  isOrdered = F
  ox = eav[match(names(eadx)[i],eav$VarID),]$VarType
  if(!is.na(ox)){
    if(ox=="Ordinal"){
      isOrdered = T
    }
  }
  if(isOrdered){
   eadx[,i] = as.ordered(eadx[,i]) 
  } else{
    eadx[,i] = as.factor(eadx[,i])
  }
}

# eadx = eadx[sample(1:nrow(eadx),100),c(2,3,4,ncol(eadx))]
save(eadx, file="../data/EA_imputed/preImputed.Rdat")

# impute 5 datasets
#  (missing soc id column)
eadx.imputed = mice(data = eadx[,2:ncol(eadx)],
                    m=5, method='cart')

eadx.imputed$soc_id = eadx$soc_id

save(eadx.imputed,file = "../data/EA_imputed/edax_imputed_multipleB.RDat")

####
# load("../data/EA_imputed/edax_imputed_multiple.RDat")
# Add imputed values back into dataframe
# (could use `complete`, but we need the soc_ids too)

for(set in 1:eadx.imputed$m){
  eadx2 = complete(eadx.imputed, set)
  eadx2$soc_id = eadx$soc_id
#   eadx2 = eadx
#    for(n in names(eadx.imputed$imp)){
#      if(length(eadx.imputed$imp[n][[set]])>0){
#        print(n)
#        dx = eadx.imputed$imp[n][[set]]
#        eadx2[match(rownames(dx),rownames(eadx2)),n] = dx[,1]
#      }
#    }
#   
    
  write.csv(eadx2, paste0("../data/EA_imputed/EADX_Imputed_Complete_",set,".csv"),row.names = F)
}