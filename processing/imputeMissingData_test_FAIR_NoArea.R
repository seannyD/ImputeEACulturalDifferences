# Includes family and area
library(mice)

prop.missing = 0.025

load("../data/EA_imputed/preImputed.Rdat")

# remove area data
eadx = eadx[,names(eadx)!="autotyp.area"]

fairlangs = read.csv("../data/FAIR_langauges_glotto_xdid.csv",stringsAsFactors = F)

numFairLangs = sum(eadx$soc_id %in% fairlangs$soc.id)

non.fair.langs = which(!eadx$soc_id %in% fairlangs$soc.id)
eadx.soc.id = eadx[,1]
# take out socid
eadx = eadx[,-1]

non.fair.langs.cells = matrix(T,nrow=nrow(eadx),ncol=ncol(eadx))
non.fair.langs.cells[non.fair.langs,] = F

n.test = round(prop.missing * numFairLangs * (ncol(eadx)-1))

# potential cells to use for testing
idx = 1:prod(dim(eadx))
# exclude cells that are already NA, or in the 
#  family column
nas = which(is.na(eadx),arr.ind = F)
# Last ONE column
familyCells = (prod(dim(eadx))-(1*nrow(eadx))):prod(dim(eadx))

idx = idx[!idx %in% nas]
idx = idx[!idx %in% familyCells]
idx = idx[idx %in% which(non.fair.langs.cells)]

test = sample(idx, n.test)

test.arrInd = cbind(
      test %% nrow(eadx),
      ceiling(test/(nrow(eadx))))

# create new NAs
for(i in 1:nrow(test.arrInd)){
  eadx[test.arrInd[i,1], test.arrInd[i,2]] = NA
}

eadx.imputed = mice(data = eadx,
                    m=1, method='cart')
eadx2 = complete(eadx.imputed, 1)

save(eadx2,test.arrInd, file=paste0("../results/imputationTests/imputeTest_FAIR_RF_NoArea_",sample(1:9999999,1),".rDat"))
