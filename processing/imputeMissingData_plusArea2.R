# Includes family and area
library(mice)

load("../data/EA_imputed/preImputed.Rdat")

eadx.imputed = mice(data = eadx,
                    m=5, method='cart')

save(eadx.imputed, file=paste0("../results/FullImputation_",sample(1:9999999,1),".rDat"))
