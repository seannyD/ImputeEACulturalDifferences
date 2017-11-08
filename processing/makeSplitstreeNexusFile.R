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

dists = read.csv("../results/EA_distances/CulturalDistances.csv", stringsAsFactors = F)
dists = dists[,2:ncol(dists)]
rownames(dists) = colnames(dists)
dists = as.matrix(dists)

makeSplitstree(dists, "../results/splitstree/CulturalDistances.nex")

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
