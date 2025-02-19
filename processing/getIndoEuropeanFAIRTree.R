# Get historical distances between langauges from phylogentic trees, obtained from D-Place
# Then match up the taxa names to FAIR langauge names

setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/analysis/")

library(ape)
library(caper)
library(phylobase)

# fair language data
f = read.csv("../data/FAIR_langauges_glotto_xdid.csv", stringsAsFactors = F)

# Indo-European
t = read.nexus("../data/trees/bouckaert_et_al2012-d-place_2.NEXUS")
treenames = read.csv("../data/trees/taxa.csv", stringsAsFactors = F)
# These are not necessarily the right glottocode, but they do link the right data
treenames[treenames$taxon=="Albanian_G",]$glottocode = "gheg1238"
treenames[treenames$taxon=="Greek_Mod",]$glottocode = "mode1248"

# convert tip labels to glotto codes
t$tip.label = treenames[match(t$tip.label,treenames$taxon),]$glottocode


f[f$xd.id %in% t$tip.label,]

keepTip = f[f$glotto %in% t$tip.label,]$glotto
t = drop.tip(t,t$tip.label[!t$tip.label %in% keepTip])

t$tip.label = f[match(t$tip.label,f$glotto),]$Language2

write.tree(t, "../data/trees/FAIR_tree_IndoEuropean.nwk")
t.dist = cophenetic(t)
write.csv(t.dist, file="../data/trees/IndoEuropean_historical_distances.csv", row.names = T)

library(reshape2)
t.dist.long = melt(t.dist)
write.csv(t.dist.long, file="../data/trees/IndoEuropean_historical_distances_long.csv", row.names = F)


# ASJP distances

asjp.langs = read.table("../data/ASJP/listss18_formatted.tab",stringsAsFactors = F,sep="\t",header=T,quote=c())
fiso = f$iso3
fiso = fiso[!is.na(fiso)]
fiso = fiso[fiso!=""]
fiso = fiso[fiso!=" "]
asjpIDS = asjp.langs[asjp.langs$iso %in% fiso,]$names
write.table(asjpIDS,file = "../data/ASJP/ASJPIDs_FAIR.txt",quote=F,row.names = F,col.names = F)

# At this point, you should run /processing/whittleASJPData.py
# to create ../data/ASJP/levenshteinLanguageDistances_FAIR.csv

asjp = read.csv("../data/ASJP/levenshteinLanguageDistances_FAIR.csv",stringsAsFactors = F,header=F)
names(asjp) = c("l1","l2","dist")
asjp$l1 = asjp.langs[match(asjp$l1,asjp.langs$names),]$iso
asjp$l2 = asjp.langs[match(asjp$l2,asjp.langs$names),]$iso
asjp$l1 = f[match(asjp$l1,f$iso3),]$glotto
asjp$l2 = f[match(asjp$l2,f$iso3),]$glotto

# To matrix
library(igraph)
graph <- graph.data.frame(asjp, directed=FALSE)
asjp = get.adjacency(graph, attr="dist", sparse=FALSE)

saveRDS(asjp,file="../data/ASJP/asjp17-dists_FAIR.RData")

# # Afro-Asiatic
# t2 = read.nexus("../data/trees/Grollemund_summary.trees")
# treenames2 = read.csv("../data/trees/Grollemund_taxa.csv", stringsAsFactors = F)
# 
# t2$tip.label = treenames2[match(t2$tip.label,treenames2$taxon),]$glottocode
# keepTip2 = f[f$glotto %in% t2$tip.label,]$glotto
# t2 = drop.tip(t2,t2$tip.label[!t2$tip.label %in% keepTip2])
# 
# t2$tip.label = f[match(t2$tip.label,f$glotto),]$Language2
# 
# write.tree(t2, "../data/trees/FAIR_tree_AfroAsiatic.nwk")
# t2.dist = cophenetic(t2)
# write.csv(t2.dist, file="../data/trees/AfroAsiatic_historical_distances.csv", row.names = T)
# 
# # Austronesian
# t3 = read.nexus("../data/trees/Gray_summary.trees")
# treenames3 = read.csv("../data/trees/Gray_taxa.csv", stringsAsFactors = F)
# 
# t3$tip.label = treenames3[match(t3$tip.label,treenames3$taxon),]$glottocode
# keepTip3 = f[f$glotto %in% t3$tip.label,]$glotto
# t3 = drop.tip(t3,t3$tip.label[!t3$tip.label %in% keepTip3])
# 
# t3$tip.label = f[match(t3$tip.label,f$glotto),]$Language2
# 
# write.tree(t3, "../data/trees/FAIR_tree_Austronesian.nwk")
# t3.dist = cophenetic(t3)
# write.csv(t3.dist, file="../data/trees/Austronesian_historical_distances.csv", row.names = T)