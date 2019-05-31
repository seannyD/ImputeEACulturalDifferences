library(reshape2)
library(dplyr)
library(lme4)
library(ggplot2)
library(sjPlot)

# Load data
data = read.csv("~/Documents/Bristol/word2vec/word2vec_DPLACE/data/FAIR/alignment_merged.csv", stringsAsFactors = F)  

data = data[complete.cases(data[,c("local_alignment","local.density.diff","freq.diff.wf"),]),]

# Family pair
data$family_pair = apply(data[,c("language_family_l1",'language_family_l2')],
                         1, function(X){
                           paste(sort(X),collapse="#")
                         })

m0 = lmer(local_alignment ~
            local.density.diff+
            freq.diff.wf+
            posx+
            (1|concept_id)+
            (1|lang_pair),
          data=data)

m1 = lmer(local_alignment ~
            local.density.diff+
            freq.diff.wf+
            posx+
            (1|concept_id)+
            (1|lang_pair)+
            (1|family_pair),
          data=data)

#anova(m0,m1)

MuMIn::r.squaredGLMM(m0)
MuMIn::r.squaredGLMM(m1)

summary(m0)
plot_model(m0,"std")
plot_model(m0,"re",terms = "family_pair")

##############
# GAM
# Look at relationship between local alignment and predictors:

ggplot(data,aes(x=local_alignment,y=freq.diff.wf)) +
  stat_smooth()

ggplot(data,aes(x=local_alignment,y=local.density.diff)) +
  stat_smooth()

# relationship with density seems non-linear

library(mgcv)

data$posxf = factor(data$posx)
data$posxf = relevel(data$posxf,"Nouns")
data$concept_id = factor(data$concept_id)
data$lang_pair = factor(data$lang_pair)

data$freq.diff.wf.scaled = scale(data$freq.diff.wf)
data$local.density.diff.scaled = scale(data$local.density.diff)
data$local_alignment.scaled = scale(data$local_alignment)

b0 = bam(local_alignment.scaled ~
           s(local.density.diff.scaled)+
           s(freq.diff.wf.scaled)+
           posxf+
           s(concept_id,bs="re") +
           s(lang_pair,bs="re"),
         data=data)
summary(b0)
plot.gam(b0)

# Model with only random effects
# (su)
b1 = bam(local_alignment ~
           s(concept_id,bs="re") +
           s(lang_pair,bs="re"),
         data=data)
summary(b1)

library(lmtest)
lrtest(b0,b1)


library(gdm)

# Normalise alginment between 0 and 1, flip to distance
data$local_alignment = data$local_alignment-min(data$local_alignment)
data$local_alignment = data$local_alignment/max(data$local_alignment)

data$local_alignment = 1-data$local_alignment

#data$freql1.wf = data$local_alignment + (0.1*rnorm(length(data$local_alignment)))
#data$freql2.wf = data$local_alignment + (0.1*rnorm(length(data$local_alignment)))

minx = min(c(data$freql1.wf,data$freql2.wf),na.rm = T)
maxx = max(c(data$freql1.wf,data$freql2.wf),na.rm=T)
data$freql1.wf = (data$freql1.wf-minx)/(maxx-minx)
data$freql2.wf = (data$freql2.wf-minx)/(maxx-minx)

minx = min(c(data$local_density_l1,data$local_density_l2),na.rm = T)
maxx = max(c(data$local_density_l1,data$local_density_l2),na.rm=T)
data$local_density_l1 = (data$local_density_l1-minx)/(maxx-minx)
data$local_density_l2 = (data$local_density_l2-minx)/(maxx-minx)

sitePair = data.frame(
  distance = data$local_alignment,
  weights = 1,
  s1.xCoord = as.numeric(as.factor(data$l1)),
  s1.yCoord = as.numeric(as.factor(data$l1)),
  s2.xCoord = as.numeric(as.factor(data$l2)),
  s2.yCoord = as.numeric(as.factor(data$l2)),
  s1.freq = data$freql1.wf,
  s1.density = data$local_density_l1,
  s2.freq = data$freql2.wf,
  s2.density = data$local_density_l2
)
sitePair = sitePair[complete.cases(sitePair),]

sitePair = sitePair[order(sitePair$s1.xCoord,sitePair$s2.xCoord),]

gdm1 = gdm(sitePair, geo = F)

plot(gdm1)

####

# Some observations have values for freq.diff.wf, even if one of the languages has a freq.wf of NA
