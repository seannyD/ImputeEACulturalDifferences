library(dplyr)
setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/processing/")
#d = read.csv("../data/FAIR/alignment_merged.csv",stringsAsFactors = F)
#saveRDS(d,"../data/FAIR/alignment_merged.RDS")

#d = readRDS("../data/FAIR/alignment_merged.RDS")
d = read.csv("../data/FAIR/nel-wiki-k100-alignments-merged-long.csv",stringsAsFactors = F,encoding = "UTF-8",fileEncoding = "UTF-8")

l = read.csv("../data/FAIR_langauges_glotto_xdid.csv",stringsAsFactors = F)

dOutWiki <- d[d$PASSES.WIKI.FILTER=="True",] %>% group_by(l1,l2) %>%
  summarise(rho = mean(local_alignment),
            family1=head(Language_Family_l1,n=1),
            family2=head(Language_Family_l2,n=1),
            comparison_count = n()
  )
names(dOutWiki)[names(dOutWiki)=="l1"] = "iso2_l1"
names(dOutWiki)[names(dOutWiki)=="l2"] = "iso2_l2"

dOutWiki$name_l1 = l[match(dOutWiki$iso2_l1,l$iso2),]$Language2
dOutWiki$name_l2 = l[match(dOutWiki$iso2_l2,l$iso2),]$Language2

write.csv(dOutWiki,file="../data/FAIR/nel-wiki-k100-alignments-by-language-pair_Filtered.csv",row.names = F)


###############

#dOutWiki.noKinship <- d[d$semanticfield=="Kinship",] %>% group_by(l1,l2) %>% summarise(
#  local_alignment = mean(local_alignment),
#  comparison_count = n()
#)

#write.csv(dOutWiki.noKinship,file="../data/FAIR/nel-wiki-k100-alignments-by-language-pair-without-kinship_Filtered.csv",row.names = F)

###############

# dOutWiki.domain <- d[d$PASSES.WIKI.FILTER=="True",] %>% group_by(l1,l2,IDS_SEMANTICFIELD_l1) %>% summarise(
#   local_alignment = mean(local_alignment,na.rm=T),
#   comparison_count = sum(!is.na(local_alignment))
# )
# #names(dOutWiki.domain)[names(dOutWiki.domain)=="semanticfield"] = "IDS_SEMANTICFIELD_l1"
# 
# write.csv(dOutWiki.domain,file="../data/FAIR/nel-wiki-k100-alignments-by-language-pair-and-domain_Filtered.csv",row.names = F)

###############
# Make dataset of only semantically filtered data

dSemFilter <- d[d$PASSES.CONCEPT.FILTER=="True",] %>% group_by(l1,l2) %>%
  summarise(rho = mean(local_alignment),
            family1=head(Language_Family_l1,n=1),
            family2=head(Language_Family_l2,n=1),
            comparison_count = n()
  )
names(dSemFilter)[names(dSemFilter)=="l1"] = "iso2_l1"
names(dSemFilter)[names(dSemFilter)=="l2"] = "iso2_l2"

dSemFilter$name_l1 = l[match(dSemFilter$iso2_l1,l$iso2),]$Language2
dSemFilter$name_l2 = l[match(dSemFilter$iso2_l2,l$iso2),]$Language2

write.csv(dSemFilter,file="../data/FAIR/nel-wiki-k100-alignments-by-language-pair_SemanticFiltered.csv",row.names = F)

dBothFilter <- d[d$PASSES.CONCEPT.FILTER=="True" & d$PASSES.WIKI.FILTER=="True",] %>% group_by(l1,l2) %>%
  summarise(rho = mean(local_alignment),
            family1=head(Language_Family_l1,n=1),
            family2=head(Language_Family_l2,n=1),
            comparison_count = n()
  )
names(dBothFilter)[names(dBothFilter)=="l1"] = "iso2_l1"
names(dBothFilter)[names(dBothFilter)=="l2"] = "iso2_l2"

dBothFilter$name_l1 = l[match(dBothFilter$iso2_l1,l$iso2),]$Language2
dBothFilter$name_l2 = l[match(dBothFilter$iso2_l2,l$iso2),]$Language2

write.csv(dBothFilter,file="../data/FAIR/nel-wiki-k100-alignments-by-language-pair_BothFiltered.csv",row.names = F)