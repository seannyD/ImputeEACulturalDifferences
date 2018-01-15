library(reshape2)
library(ggplot2)
setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/analysis/")

load("../data/EA_imputed/preImputed.Rdat")
soc.id = eadx[,1]
# take out socid
eadx = eadx[,-1]

getAccuracy = function(f){
  #print(f)
  load(f)
  if(nrow(eadx2)==nrow(eadx)){
  test.arrInd = test.arrInd[test.arrInd[,1]!=0,]
  accuracy = sum(eadx2[test.arrInd] == eadx[test.arrInd])/nrow(test.arrInd)
  # baseline
  baseline = replicate(100,getRandomBaseline(test.arrInd,eadx,eadx2))
  z = (accuracy - mean(baseline))/sd(baseline)
  
  baseline2 = replicate(100,getRandomBaseline_totallyRandom(test.arrInd,eadx,eadx2))
  z2 = (accuracy - mean(baseline2))/sd(baseline2)
  
  return(c(accuracy=accuracy, baseline=mean(baseline), z=z,
         baseline2 = mean(baseline2), z2 = z2))
  } else{
    print("Row numbers don't match")
  }
}

getRandomBaseline = function(test.arrInd, eadx, eadx2){
  # imputation by random sampling
  randomBaseline = 
      sapply(test.arrInd[,2], function(X){
      sx = eadx2[,X]
      sx = sx[!is.na(sx)]
      sample(sx,1)
    })
  sum(randomBaseline == eadx[test.arrInd])/nrow(test.arrInd)
}

getRandomBaseline_totallyRandom = function(test.arrInd, eadx, eadx2){
  # imputation by random sampling of set (no frequencies)
  randomBaseline = 
    sapply(test.arrInd[,2], function(X){
      sx = eadx2[,X]
      sx = sx[!is.na(sx)]
      sample(unique(sx),1)
    })
  sum(randomBaseline == eadx[test.arrInd])/nrow(test.arrInd)
}


getFolderAccuracy = function(folder,prefix){
  accuracy.orig = sapply( 
    paste0(folder,
           list.files(folder,prefix)),
    getAccuracy)
  
  accuracy.orig = as.data.frame(t(accuracy.orig))
  return(c(mean(accuracy.orig$accuracy,na.rm=T),
          (mean(accuracy.orig$baseline,na.rm=T)),
          (mean(accuracy.orig$z,na.rm=T)),
          (mean(accuracy.orig$baseline2,na.rm=T)),
          (mean(accuracy.orig$z2,na.rm=T))))
}

########################




getFolderAccuracy("../results/imputationTests/test1/","imputeTest_[0-9]*.rDat")

getFolderAccuracy("../results/imputationTests/test2/","imputeTest_[0-9]*.rDat")

getFolderAccuracy("../results/imputationTests/testFAIR/","imputeTest_*")
getFolderAccuracy("../results/imputationTests/testFAIR_Area/","imputeTest_*")
# Area: not much difference in z, but a bit higher on absolute accuracy


getFolderAccuracy("../results/imputationTests/testFAIR_Area_RF/","imputeTest_*")

# RF with bigger number of trees (same as above with 100 trees)
getFolderAccuracy("../results/imputationTests/test_FAIR_plus/","imputeTest_*")


getFolderAccuracy("../results/imputationTests/testFAIR_RF_NoArea/","imputeTest_*")

# single tree with tuned parameters
getFolderAccuracy("../results/imputationTests/testFAIR_tuned/","imputeTest_*")


# Test on final 15 languages that can actually be analysed
getFolderAccuracy("../results/imputationTests/testFAIR_15/","imputeTest_FAIR_15_*")
# 100 reps
# 0.7397059 0.3676176 5.5958611 0.1856618 8.6267473
