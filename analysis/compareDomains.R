# Part 1: Compare each linguistic domain to the overall cultural similarity
# Part 2: Compare each linguistic domain to the cultural similarity of each original D-PLACE domain


library(lme4)
library(gplots)

try(setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/analysis/"))

# Parameters

lingDistancesByDomainFile = "../results/EA_distances/nel-wiki-k100_with_ling.csv"
outputFolder = "../results/stats/wikipedia-main/"

# Load language details
l = read.csv("../data/FAIR_langauges_glotto_xdid.csv", stringsAsFactors = F)
g = read.csv("../data/glottolog-languoid.csv/languoid.csv", stringsAsFactors = F)

# Load cultural distances
cult = read.csv("../results/EA_distances/CulturalDistances_Long.csv", stringsAsFactors = F)
names(cult) = c("l1","l2","cult.dist")
cult$l1.iso2 = l[match(cult$l1,l$Language2),]$iso2
cult$l2.iso2 = l[match(cult$l2,l$Language2),]$iso2
cultisos = unique(c(cult$l1.iso2, cult$l2.iso2))

# Load linguistic distances
ling.dom = read.csv(
  lingDistancesByDomainFile, 
  stringsAsFactors = F)

ling.dom = ling.dom[(ling.dom$l1 %in% cultisos) & 
                      (ling.dom$l2 %in% cultisos),]
#There are very few possible comparisons for Slovenian and Northern Sami, so we'll remove these:

ling.dom = ling.dom[!(ling.dom$l1=="se" || ling.dom$l2 == "se"),]
ling.dom = ling.dom[!(ling.dom$l1=="sl" || ling.dom$l2 == "sl"),]

ling.dom$cult.similarity = 1-ling.dom$cult.dist

# Match overall distances to domain-specific distances
matches = sapply(1:nrow(ling.dom), function(i){
  which(cult$l1.iso2==ling.dom$l1[i] & cult$l2.iso2==ling.dom$l2[i])
})

ling.dom$cult.similarity.overall = 1- cult[matches,]$cult.dist

# Find language family

ling.dom$family1 = l[match(ling.dom$l1, l$iso2),]$family
ling.dom$family2 = l[match(ling.dom$l2, l$iso2),]$family
ling.dom$area1 = l[match(ling.dom$l1, l$iso2),]$autotyp.area
ling.dom$area2 = l[match(ling.dom$l2, l$iso2),]$autotyp.area


# Paste language family names together,
# but order shouldn't matter, so sort first
fgroup = cbind(ling.dom$family1,ling.dom$family2)
fgroup = apply(fgroup,1,sort)
ling.dom$family.group = apply(fgroup,2,paste,collapse=":")

agroup = cbind(ling.dom$area1,ling.dom$area2)
agroup = apply(agroup,1,sort)
ling.dom$area.group = apply(agroup,2,paste,collapse=":")

# Scale variables 
ling.dom$cult.similarity.overall.center = scale(ling.dom$cult.similarity.overall)
ling.dom$rho.center = scale(ling.dom$local_alignment)
ling.dom$comparison_count.center = scale(ling.dom$comparison_count)
####

# Part 1: Compare each linguistic domain to the overall cultural similarity

res = data.frame(
  domain = NA,
  raw.r = NA,
  lmeModel.CultSimilarity.Beta = NA,
  lmeModel.CultSimilarity.logLikelihoodDifference = NA,
  lmeModel.CultSimilarity.ChiSquared = NA,
  lmeModel.CultSimilarity.p = NA
)

set.seed(218932)

for(dom in unique(ling.dom$imputed_semantic_domain)){
  dx = ling.dom[ling.dom$imputed_semantic_domain==dom,]
  m0 = lmer(
    rho.center ~ 1 +
      comparison_count.center +
      (1 + cult.similarity.overall.center | family.group) +
      (1 + cult.similarity.overall.center | area.group),
    data = dx
  )
  m1 =  lmer(
    rho.center ~ 1 +
      comparison_count.center +
      cult.similarity.overall.center +
      (1 + cult.similarity.overall.center | family.group) +
      (1 + cult.similarity.overall.center | area.group),
    data = dx
  ) 
  modelComparison = anova(m0,m1)
  r = cor(dx$cult.similarity.overall,
          dx$rho.center)
  loglikDiff = signif(diff(modelComparison$logLik),2)
  chi = modelComparison$Chisq[2]
  df = modelComparison$`Chi Df`[2]
  p = modelComparison$`Pr(>Chisq)`[2]
  beta = summary(m1)$coef["cult.similarity.overall.center",1]
  
  res = rbind(res,
              c(dom,r,beta,loglikDiff,chi,p))
}

res = res[!is.na(res$r),]

write.csv(res,file=paste0(outputFolder,"Cor_LingAlignmentByDomains_vs_OverallCulturalSimilarity.csv"))




# Part 2: Compare each linguistic domain to the cultural similarity of each original D-PLACE domain



# Load data
cult.DP = read.csv("../results/EA_distances_DPlaceDomains/CultDistancesByDPlaceMainDomain.csv",stringsAsFactors = F)
cult.DP$l1.iso2 = l[match(cult.DP$l1,l$Language2),]$iso2
cult.DP$l2.iso2 = l[match(cult.DP$l2,l$Language2),]$iso2


# Data frame for results
res2 = data.frame(
  lingDomain = NA,
  cultDomainDP = NA,
  convergenceWarnings = NA,
  numCultVars = NA,
  raw.r = NA,
  lmeModel.CultSimilarity.Beta = NA,
  lmeModel.CultSimilarity.logLikelihoodDifference = NA,
  lmeModel.CultSimilarity.ChiSquared = NA,
  lmeModel.CultSimilarity.p = NA
)

# Loop through each pair of domains

lingDomains = unique(ling.dom$imputed_semantic_domain)
cultDomains = unique(cult.DP$domain)
for(lingDom in lingDomains){
  for(cultDom in cultDomains){
    print(c(lingDom,cultDom))
  # Find relevant data
  d.ling = ling.dom[ling.dom$imputed_semantic_domain==lingDom,]
  d.cult = cult.DP[cult.DP$domain==cultDom,]
  
  # Add cultural distances to ling data
  matches = sapply(1:nrow(d.ling), function(i){
    which(d.cult$l1.iso2==d.ling$l1[i] & d.cult$l2.iso2==d.ling$l2[i])
  })
  d.ling$cult.distance.DP = d.cult[matches,]$cult.dist
  
  # Center data and flip to similarity
  d.ling$cult.similarity.DP = scale(1-d.ling$cult.distance.DP)
  
  # Run models
  m0 = lmer(
    rho.center ~ 1 +
      comparison_count.center +
      (1 + cult.similarity.DP | family.group) +
      (1 + cult.similarity.DP | area.group),
    data = d.ling
  )
  m1 =  update(m0, ~.+cult.similarity.DP)
  convergenceWarnings = paste(m1@optinfo$conv$lme4$messages,m0@optinfo$conv$lme4$messages,sep=";",collapse=";")
  
  if(nchar(convergenceWarnings)>0){
    # Attempt to fix convergence warnings:
    # Change optimiser
    m0 = lmer(
      rho.center ~ 1 +
        comparison_count.center +
        (1 + cult.similarity.DP | family.group) +
        (1 + cult.similarity.DP | area.group),
      data = d.ling, 
      control=lmerControl(optimizer = "Nelder_Mead"))
    m1 =  update(m0, ~.+cult.similarity.DP,
                 control=lmerControl(optimizer = "Nelder_Mead"))
    convergenceWarnings = paste(m1@optinfo$conv$lme4$messages,m0@optinfo$conv$lme4$messages,sep=";",collapse=";")
  }
  
  if(nchar(convergenceWarnings)>0){
    # 2nd attempt to fix convergence warnings
    # Remove correlation between random intercept and slope
    m0 = lmer(
      rho.center ~ 1 +
        comparison_count.center +
        (1 + cult.similarity.DP || family.group) +
        (1 + cult.similarity.DP || area.group),
      data = d.ling)
    m1 =  update(m0, ~.+cult.similarity.DP)
    convergenceWarnings = paste(m1@optinfo$conv$lme4$messages,m0@optinfo$conv$lme4$messages,sep=";",collapse=";")
  }
  
  modelComparison = anova(m0,m1)
  r = cor(d.ling$cult.distance.DP,
          d.ling$rho.center)
  loglikDiff = signif(diff(modelComparison$logLik),2)
  chi = modelComparison$Chisq[2]
  df = modelComparison$`Chi Df`[2]
  p = modelComparison$`Pr(>Chisq)`[2]
  beta = summary(m1)$coef["cult.similarity.DP",1]
  
  
  res2 = rbind(res2,
              c(lingDom, cultDom,convergenceWarnings,
                d.cult[matches,]$numCultVars[1],
                r,beta,loglikDiff,chi,p))

  
  }
  
  
}

res2 = res2[!is.na(res2$lingDomain),]
for(i in 4:ncol(res2)){
  res2[,i] = as.numeric(res2[,i])
}

write.csv(res2,file=paste0(outputFolder,"Cor_LingAlignmentByDomains_vs_DPlaceCulturalDomains.csv"))


makeMat = function(stat){
  statx = res2[,stat]
  statx[nchar(res2$convergenceWarnings)>1] = NA
  mat = matrix(statx,ncol=length(lingDomains),nrow=length(cultDomains))
  rownames(mat) = cultDomains
  colnames(mat) = lingDomains
  return(mat)
}

mat.beta = makeMat("lmeModel.CultSimilarity.Beta")
hm.beta = heatmap.2(mat.beta,col=cm.colors(20),
                    margins=c(14,14),srtCol=45,trace="none",
                    key.title="",key.par = list(mar=c(3,2,1,2)),
                    key.xlab = "Beta",na.color = "gray",
                    xlab = "Concepticon domains",
                    ylab = "D-Place domains")



mat.p = makeMat("lmeModel.CultSimilarity.p")
signifColours = c("dark red","red",rep("blue",40))
pdf(paste0(outputFolder,"LingAlignmentByDomains_vs_DPlaceCulturalDomains_ModelComparisonP.pdf"), width = 8.5, height = 8)
hm.p = heatmap.2(mat.p,col = signifColours,
          margins=c(14,14),srtCol=45,trace="none",
          key.title="",key.par = list(mar=c(3,2,1,2)),
          key.xlab = "Model comparison p",
          xlab = "Concepticon domains",
          ylab = "D-Place domains",
          Rowv = hm.beta$rowDendrogram,
          Colv = hm.beta$colDendrogram)
dev.off()


pdf(paste0(outputFolder,"LingAlignmentByDomains_vs_DPlaceCulturalDomains_Beta.pdf"), 
    width = 8.5, height = 8)
heatmap.2(mat.beta,col=cm.colors(20),
          margins=c(14,14),srtCol=45,trace="none",
          key.title="",key.par = list(mar=c(3,2,1,2)),
          key.xlab = "Beta",na.color = "gray",
          xlab = "Concepticon domains",
          ylab = "D-Place domains")
x = expand.grid(
  seq(0.25,0.74, length.out=length(cultDomains)),
  seq(0.22,0.725,length.out=length(lingDomains)))
nonSig = t(hm.p$carpet)>0.05
points(x[,2],x[,1],
       pch=c(NA,4)[as.numeric(nonSig)+1])
dev.off()

mat.beta2 = mat.beta[hm.beta$rowInd,]
mat.beta2 = mat.beta2[,hm.beta$colInd]
mat.p2 = mat.p[hm.beta$rowInd,]
mat.p2 = mat.p2[,hm.beta$colInd]

library(ellipse)
library(RColorBrewer)

# Build a Pannel of 100 colors with Rcolor Brewer
my_colors <- brewer.pal(4, "Spectral")

redCols = brewer.pal(9,"YlOrRd")[9:5]
blueCols = brewer.pal(9,"YlGnBu")[2:9]
blueCols = colorRampPalette(blueCols)(95)
my_colors = c(redCols,blueCols)

# TODO: The colours aren't mapping properly?
plotcorr(mat.beta2/0.5, col=my_colors[mat.p2*100] , mar=c(1,1,1,1)  )
