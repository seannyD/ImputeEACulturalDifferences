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
# convert tip labels to glotto codes
t$tip.label = treenames[match(t$tip.label,treenames$taxon),]$glottocode
keepTip = f[f$glotto %in% t$tip.label,]$glotto
t = drop.tip(t,t$tip.label[!t$tip.label %in% keepTip])

t$tip.label = f[match(t$tip.label,f$glotto),]$Language2

write.tree(t, "../data/trees/FAIR_tree_IndoEuropean.nwk")
t.dist = cophenetic(t)
write.csv(t.dist, file="../data/trees/IndoEuropean_historical_distances.csv", row.names = T)

# Afro-Asiatic
t2 = read.nexus("../data/trees/Grollemund_summary.trees")
treenames2 = read.csv("../data/trees/Grollemund_taxa.csv", stringsAsFactors = F)

t2$tip.label = treenames2[match(t2$tip.label,treenames2$taxon),]$glottocode
keepTip2 = f[f$glotto %in% t2$tip.label,]$glotto
t2 = drop.tip(t2,t2$tip.label[!t2$tip.label %in% keepTip2])

t2$tip.label = f[match(t2$tip.label,f$glotto),]$Language2

write.tree(t2, "../data/trees/FAIR_tree_AfroAsiatic.nwk")
t2.dist = cophenetic(t2)
write.csv(t2.dist, file="../data/trees/AfroAsiatic_historical_distances.csv", row.names = T)

# Austronesian
t3 = read.nexus("../data/trees/Gray_summary.trees")
treenames3 = read.csv("../data/trees/Gray_taxa.csv", stringsAsFactors = F)

t3$tip.label = treenames3[match(t3$tip.label,treenames3$taxon),]$glottocode
keepTip3 = f[f$glotto %in% t3$tip.label,]$glotto
t3 = drop.tip(t3,t3$tip.label[!t3$tip.label %in% keepTip3])

t3$tip.label = f[match(t3$tip.label,f$glotto),]$Language2

write.tree(t3, "../data/trees/FAIR_tree_Austronesian.nwk")
t3.dist = cophenetic(t3)
write.csv(t3.dist, file="../data/trees/Austronesian_historical_distances.csv", row.names = T)