library(fields)
setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/processing/")

l = read.delim("../data/FAIR_languages.tab", stringsAsFactors = F)
l$Language2 = gsub("_"," ",l$Language)

g = read.csv("../data/glottolog-languoid.csv/languoid.csv", stringsAsFactors = F)
g$family = g[match(g$family_pk,g$pk),]$name

l$glotto = g[match(l$Language2,g$name),]$id
sum(is.na(l$glotto))

l2 = l[is.na(l$glotto),]

# Write missing langauges
write.table(l2[,1], "../data/FAIR_languages_missing.tab", quote = F, row.names = F)

# Manually coded mappings
l3 = read.delim("../data/FAIR_languages_missing_edited.tab", sep='\t', quote = "", stringsAsFactors = F)

l[is.na(l$glotto),]$glotto = l3[match(l[is.na(l$glotto),]$Language, l3$Lang),]$glotto

l$family = g[match(l$glotto,g$id),]$family
# Isolates:
l$family[is.na(l$family)] = l$Language2[is.na(l$family)]

#l = l[!is.na(l$glotto),]
#l = l[l$glotto!="",]

dpid = read.csv("../data/dplace-data-1.0/csv/xd_id_to_language.csv", stringsAsFactors = F)

l$xd.id = dpid[match(l$glotto,dpid$DialectLanguageGlottocode),]$xd_id
#l$soc.id = dpid[match(l$glotto,dpid$DialectLanguageGlottocode),]$soc

#l = l[!is.na(l$xd.id),]

# SCCS
#sccs = read.csv("../data/dplace_SCCS/societies.csv", stringsAsFactors = F)
#sum(sccs$xd_id %in% l$xd.id)

# Ethnographic atlas
ea = read.csv("../data/dplace-data-1.0/csv/EA_societies.csv", stringsAsFactors = F)

sum(ea$xd_id %in% l$xd.id)

l$soc.id = ea[match(l$xd.id,ea$xd_id),]$soc_id

l[l$Language2=="Georgian",]$soc.id = "Ci8"# Georgians
l[l$Language2=="Georgian",]$soc.id = "Ce2"# Portuguese




g2 = read.csv("../data/languages-and-dialects-geo.csv", stringsAsFactors = F)

l$iso3 = g2[match(l$glotto,g2$glottocode),]$isocodes

l[l$Language=="Khmer",]$iso3 = "khm"

l[l$Language=="Inupiak",]$soc.id = "Na12"


# Get geographic area:

load("~/Documents/MPI/Neandertals_Collab/fossil/autotyp.data/autotyp.backbone.rda")
autotyp.backbone$glottolog_LID.2014 =as.character(autotyp.backbone$glottolog_LID.2014)
autotyp.backbone$area =as.character(autotyp.backbone$area)

# Find actual matches by glottoID
l$autotyp.area = autotyp.backbone[match(l$glotto,autotyp.backbone$glottolog_LID.2014),]$area

l[l$glotto %in% c("tumb1250","afri1274"),]$autotyp.area = "S Africa"
l[l$glotto %in% c("neap1235",'serb1264','wall1255'),]$autotyp.area = "Europe"
l[l$glotto =="tahi1242",]$autotyp.area = "Oceania"
l[l$glotto %in% c("sout2688",'cent1989'),]$autotyp.area = "Southeast Asia"
l[l$glotto %in% c("mang1399"),]$autotyp.area ="African Savannah"

# Get fair iso2 codes:
library(stringr)
htx = readLines("../data/FAIR/FAIR_langs2iso.html")

res =str_match_all(htx,'<td>([^:]+):.+wiki\\.([a-z]+)\\.zip')

res = sapply(res, function(X){
  if(nrow(X)>0){
    return(X[1,2:3])
  }})

res = unlist(res)
res = as.data.frame(matrix(res,ncol=2,byrow = T),
                    stringsAsFactors = F)
names(res) = c("name","iso2")

l$iso2 = res[match(l$Language2, res$name),]$iso2

l[l$Language2=="Banyumasan",]$iso2 = "bms"
l[l$Language2=="Serbo Croatian",]$iso2 = "sh"

# Load final linguistic distance measures to see which langauges will go into the analysis

ling = read.csv("../data/FAIR/semantic_distances_FAIR.csv", stringsAsFactors = F)

ling.langs = unique(c(ling$l1, ling$l2))

l$in.final.analysis = l$iso2 %in% ling.langs

l[l$in.final.analysis,]

sum(l$in.final.analysis & !is.na(l$soc.id) & l$soc.id %in% ea$soc_id)
l[l$in.final.analysis & !is.na(l$soc.id),]$family

# Write list of languages
write.csv(l,"../data/FAIR_langauges_glotto_xdid.csv", row.names = F)

####
# Get autotyp area for all EA languages

exid = read.csv("../data/dplace-data-1.0/csv/ALL_soc_ids_to_xd_ids_24Feb2016.csv", stringsAsFactors = F)

eag = read.csv("../data/dplace-data-1.0/csv/xd_id_to_language.csv", stringsAsFactors = F)

eag$soc_id = exid[match(eag$xd_id, exid$xd_id),]$soc_id

eag$autotyp.area = autotyp.backbone[match(eag$DialectLanguageGlottocode,autotyp.backbone$glottolog_LID.2014),]$area

eall =read.csv("../data/dplace-data-1.0/csv/EA_Binford_Lat_Long.csv", stringsAsFactors = F)
eag$latitude = eall[match(eag$soc_id, eall$soc_id),]$Latitude
eag$longitude = eall[match(eag$soc_id, eall$soc_id),]$Longitude

eag$latitude[is.na(eag$latitude)] = g[match(eag$DialectLanguageGlottocode[is.na(eag$latitude)], g$id),]$latitude
eag$longitude[is.na(eag$longitude)] = g[match(eag$DialectLanguageGlottocode[is.na(eag$longitude)], g$id),]$longitude


atMissing = as.matrix(eag[is.na(eag$autotyp.area),c("longitude","latitude")])
allAutotyp = as.matrix(autotyp.backbone[!is.na(autotyp.backbone$latitude),c("longitude",'latitude')])

# For missing, find area of closest langauge
atDist = rdist.earth(allAutotyp,atMissing)
closestArea = autotyp.backbone[!is.na(autotyp.backbone$latitude),]$area[apply(atDist,2,function(X){which(X==min(X,na.rm=T),arr.ind=T)[1]})]

eag[is.na(eag$autotyp.area),]$autotyp.area = closestArea

#eag[is.na(eag$autotyp.area),]

fixes = 
list(c("colu1250",'Basin and Plains'),
     c("ipai1239","Mesoamerica"),
     c("oowe1239","Alaska-Oregon"),
     c("musq1240","Alaska-Oregon"),
     c("saan1244","Alaska-Oregon"),
     c("sanj1276","Basin and Plains"),
     c("namb1296","Basin and Plains"),
     c("klik1240","Alaska-Oregon"),
     c("sout2955","California"))

for(i in 1:length(fixes)){
  eag[eag$DialectLanguageGlottocode==fixes[[i]][1],]$autotyp.area = fixes[[i]][2]
}

write.csv(eag,'../data/xd_id_to_language_and_area.csv')
