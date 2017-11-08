setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/processing/")

l = read.delim("../data/FAIR_languages.tab", stringsAsFactors = F)
l$Language2 = gsub("_"," ",l$Language)

g = read.csv("../data/glottolog-languoid.csv/languoid.csv", stringsAsFactors = F)

l$glotto = g[match(l$Language2,g$name),]$id
sum(is.na(l$glotto))

l2 = l[is.na(l$glotto),]

write.table(l2[,1], "../data/FAIR_languages_missing.tab", quote = F, row.names = F)

l3 = read.delim("../data/FAIR_languages_missing_edited.tab", sep='\t', quote = "", stringsAsFactors = F)

l[is.na(l$glotto),]$glotto = l3[match(l[is.na(l$glotto),]$Language, l3$Lang),]$glotto

l = l[!is.na(l$glotto),]
l = l[l$glotto!="",]

dpid = read.csv("../data/dplace-data-1.0/csv/xd_id_to_language.csv", stringsAsFactors = F)

l$xd.id = dpid[match(l$glotto,dpid$DialectLanguageGlottocode),]$xd_id
#l$soc.id = dpid[match(l$glotto,dpid$DialectLanguageGlottocode),]$soc

l = l[!is.na(l$xd.id),]

# SCCS
#sccs = read.csv("../data/dplace_SCCS/societies.csv", stringsAsFactors = F)
#sum(sccs$xd_id %in% l$xd.id)

# Ethnographic atlas
ea = read.csv("../data/dplace-data-1.0/csv/EA_societies.csv", stringsAsFactors = F)

sum(ea$xd_id %in% l$xd.id)

l$soc.id = ea[match(l$xd.id,ea$xd_id),]$soc_id


g2 = read.csv("../data/languages-and-dialects-geo.csv", stringsAsFactors = F)

l$iso3 = g2[match(l$glotto,g2$glottocode),]$isocodes

l[l$Language=="Khmer",]$iso3 = "khm"

l[l$Language=="Inupiak",]$soc.id = "Na12"


write.csv(l,"../data/FAIR_langauges_glotto_xdid.csv", row.names = F)


