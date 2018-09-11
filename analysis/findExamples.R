try(setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/analysis/"))

l = read.csv("../data/FAIR_langauges_glotto_xdid.csv", stringsAsFactors = F)

ling =read.csv("../results/EA_distances_DPlaceDomains/CultDistancesByDPlaceMainDomain.csv",stringsAsFactors = F)

ling$family1 = l[match(ling$l1, l$Language),]$family
ling$family2 = l[match(ling$l2, l$Language),]$family

k = ling[ling$domain=="Marriage and Kinship",]
k = k[k$family1=="Indo-European" & k$family2=="Indo-European",]

k[k$cult.dist==max(k$cult.dist),]
