setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/analysis/")

library(ape)
library(caper)
library(phylobase)

t = read.nexus("../data/trees/bouckaert_et_al2012-d-place_2.NEXUS")

treenames = read.csv("../data/trees/treeNameToDPlaceName.tab", stringsAsFactors = F)
treenames = treenames[treenames$Tree!="",]

treenames$Tree %in% t$tip.label
t = drop.tip(t,t$tip.label[!t$tip.label %in% treenames$Tree])

t$tip.label = treenames[match(t$tip.label, treenames$Tree),]$FAIR

write.tree(t, "../data/trees/FAIR_tree.nwk")

# Match to society ID
f = read.csv("../data/FAIR_langauges_glotto_xdid.csv", stringsAsFactors = F)
t$tip.label = f[match(t$tip.label, f$Language2),]$soc.id
