library(mice)

prop.missing = 0.025

load("../data/EA_imputed/preImputed.Rdat")

# take out socid
eadx = eadx[,-1]

n.test = round(prop.missing * prod(dim(eadx)))

# exclude cells that are already NA, or in the 
#  family column
idx = 1:prod(dim(eadx))
nas = which(is.na(eadx),arr.ind = F)
familyCells = (prod(dim(eadx))-(nrow(eadx))):prod(dim(eadx))

idx = idx[!idx %in% nas]
idx = idx[!idx %in% familyCells]

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

save(eadx2,test.arrInd, file=paste0("../results/imputationTests/imputeTest_",sample(1:9999999,1),".rDat"))
