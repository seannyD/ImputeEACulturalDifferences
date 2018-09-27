setwd("/Users/sgroberts/Documents/Bristol/word2vec/word2vec_DPLACE/processing")

makeSplitstree = function(dists, filename){
  header = paste("#nexus\n\nBEGIN Taxa;\nDIMENSIONS ntax=",nrow(dists),";\nTAXLABELS\n",collapse="")
  
  taxlabels= paste(paste("[",1:nrow(dists),"] '",rownames(dists),"'",sep=''),collapse='\n')
  
  header2 = paste("\n;\nEND;  [TAXA]\n\nBEGIN DISTANCES;\n        DIMENSIONS NTAX=" , nrow(dists),";  FORMAT  TRIANGLE=BOTH DIAGONAL LABELS=LEFT;\nMATRIX\n", collapse='')
  
  rnames = paste("'",rownames(dists),"'",sep='')
  
  mat = paste(paste(rnames,apply(dists,1,paste,collapse=' ')),collapse='\n')
  
  header3 = "\n;\nEND;\n"
  
  nexus = paste(header, taxlabels, header2, mat, header3, collapse='')
  
  cat(nexus,file = filename)
}

# All FAIR langauges:

dists = read.csv("../results/EA_distances/CulturalDistances.csv", stringsAsFactors = F)
dists = dists[,2:ncol(dists)]
rownames(dists) = colnames(dists)
dists = as.matrix(dists)

makeSplitstree(dists, "../results/splitstree/CulturalDistances.nex")

### Just for langs in final analysis

cult = read.csv("../results/EA_distances/CulturalDistances_Long.csv", stringsAsFactors = F)
names(cult) = c("l1","l2","cult.dist")
cultLangs = unique(c(cult$Var1,cult$Var2))

l = read.csv("../data/FAIR_langauges_glotto_xdid.csv", stringsAsFactors = F)
g = read.csv("../data/glottolog-languoid.csv/languoid.csv", stringsAsFactors = F)
l$family = g[match(l$glotto,g$id),]$family_pk
l$family = g[match(l$family,g$pk),]$name

lingDistancesFile = "../data/FAIR/nel-wiki-k100-alignments-by-language-pair.csv"
ling = read.csv(lingDistancesFile, stringsAsFactors = F)

ling = ling[!(ling$l1=="se" || ling$l2 == "se"),]
ling = ling[!(ling$l1=="sl" || ling$l2 == "sl"),]

cult$l1.iso2 = l[match(cult$l1,l$Language2),]$iso2
cult$l2.iso2 = l[match(cult$l2,l$Language2),]$iso2

fairisos = unique(c(ling$l1,ling$l2))
cultisos = unique(c(cult$l1.iso2, cult$l2.iso2))

cult = cult[(cult$l1.iso2 %in% fairisos) & (cult$l2.iso2 %in% fairisos),]
ling = ling[(ling$l1 %in% cultisos) & (ling$l2 %in% cultisos),]

final.langs = l[l$iso2 %in% unique(c(ling$l1,ling$l2)),]$Language2

final.langs = final.langs[final.langs %in% rownames(dists)]

dists15 = dists[final.langs,final.langs]

makeSplitstree(dists15, "../results/splitstree/CulturalDistances_Final15.nex")


# Add colours:
nodeColours =read.csv("../results/splitstree/nodeColours.txt",sep="\t",stringsAsFactors = F)
areas = l[match(gsub("'","",nodeColours$L),l$Language),]$family

library(RColorBrewer)
colours = brewer.pal(length(unique(areas)),"Set1")
names(colours) = unique(areas)
areas.colours = colours[areas]
areas.colours.text = apply(col2rgb(areas.colours),2,paste,collapse=" ")

vlabels = data.frame(
  num = 1:ncol(dists15),
  L= paste0("'",nodeColours$L,"'"),
  lc = paste0("lc=",areas.colours.text)
)
vlables.text = apply(vlabels,1,paste,collapse=" ")

vlables.text = paste0(
  "\nVLABELS\n",
  paste(vlables.text,collapse=",\n"),
  "\n;"
)

plot(1:2,type='n',xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
legend(1,2,legend=names(colours),text.col=colours)

cat(vlables.text,file="../results/splitstree/nodeColours2.txt",append = F)


####
# Splitstree for linguistic distances
library(igraph)

grph <- graph.data.frame(ling[,c("l1",'l2','local_alignment')], directed=FALSE)
# add value as a weight attribute
ling.m = get.adjacency(grph, attr="local_alignment", sparse=FALSE)
rownames(ling.m) = l[match(rownames(ling.m),l$iso2),]$Language2
colnames(ling.m) = l[match(colnames(ling.m),l$iso2),]$Language2
ling.m = ling.m[final.langs,final.langs]
# flip to distance
ling.m = max(ling.m)-ling.m
ling.m = ling.m/max(ling.m)
makeSplitstree(ling.m, "../results/splitstree/LinguisticDistances.nex")


####
# Kinship, all cultures and variables

k = read.csv("../results/EA_distances/Kinship_AllSocieties.csv", stringsAsFactors = F)
k = k[,2:ncol(k)]
nx = names(k)
dpn = read.csv("../data/dplace-data-1.0/csv/EA_societies.csv")
nx = as.character(dpn[match(nx,dpn$soc_id),]$pref_name_for_society)
nx = iconv(nx,to='ASCII//TRANSLIT')
nx = gsub("['\":~,!]","",nx)
nx = gsub("[\\(\\)\\-]"," ",nx)
nx = gsub(" +",".",nx)

k = as.matrix(k)
rownames(k) = nx
colnames(k) = nx

makeSplitstree(k, "../results/splitstree/CulturalDistances_Kinship_AllSocieties.nex")


library(mclust)
hc = hclust(as.dist(k), method = "ward.D2")
pdf("../results/splitstree/CulturalDistances_Kinship_AllSocieties_Hclust.pdf",
    width = 90, height= 15)
plot(hc)
dev.off()
