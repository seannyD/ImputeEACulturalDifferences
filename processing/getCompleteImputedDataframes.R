# Take the output of mice and generate full dataframes for each imputation

library(mice)
setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/processing/")

files = list.files("../data/EA_imputed/","FullImputation*")
icount = 1
for(f in files){
  load(paste0("../data/EA_imputed/",f))
  for(i in 1:eadx.imputed$m){
    dx = complete(eadx.imputed, action = i)
    write.csv(dx, paste0("../data/EA_imputed/completeDataframes/FullImputation_",icount,".csv"))
    icount = icount +1
  }
  
  
}