library(dplyr)
setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/processing/")
#d = read.csv("../data/FAIR/alignment_merged.csv",stringsAsFactors = F)
#saveRDS(d,"../data/FAIR/alignment_merged.RDS")

d = readRDS("../data/FAIR/alignment_merged.RDS")

# Compare
dOutWiki <- d %>% group_by(l1,l2) %>% summarise(
  local_alignment = mean(local_alignment),
  comparison_count = n()
)

write.csv(dOutWiki,file="../data/FAIR/nel-wiki-k100-alignments-by-language-pair_Filtered.csv",row.names = F)

dOutWiki.noKinship <- d[d$semanticfield=="Kinship",] %>% group_by(l1,l2) %>% summarise(
  local_alignment = mean(local_alignment),
  comparison_count = n()
)

write.csv(dOutWiki.noKinship,file="../data/FAIR/nel-wiki-k100-alignments-by-language-pair-without-kinship_Filtered.csv",row.names = F)

dOutWiki.domain <- d %>% group_by(l1,l2,semanticfield) %>% summarise(
  local_alignment = mean(local_alignment,na.rm=T),
  comparison_count = sum(!is.na(local_alignment))
)
names(dOutWiki.domain)[names(dOutWiki.domain)=="semanticfield"] = "IDS_SEMANTICFIELD_l1"

write.csv(dOutWiki.domain,file="../data/FAIR/nel-wiki-k100-alignments-by-language-pair-and-domain_Filtered.csv",row.names = F)

