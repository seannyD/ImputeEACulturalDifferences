# combine the distances for linguistic and cultural
# features for each domain

try(setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/processing/"))

l = read.csv("../data/FAIR_langauges_glotto_xdid.csv", 
             stringsAsFactors = F)

combineCultAndLingDistances = function(inputFile,outputFile){

  ling.domain = read.csv(inputFile, stringsAsFactors = F)
  ling.domain$cult.dist = NA
  
  ling.langs = unique(c(ling.domain$l1,
                        ling.domain$l2))
  names(ling.domain)[names(ling.domain)=="IDS_SEMANTICFIELD_l1"] = "imputed_semantic_domain"
  domains = unique(ling.domain$imputed_semantic_domain)
  
  for(dom in domains){
    dom.filename = paste0('../results/EA_distances/',
                          gsub(" ","_",dom),"_long.csv")
    if(file.exists(dom.filename)){
      print(dom)
      dx = read.csv(dom.filename, stringsAsFactors = F)
      dx$l1 = l[match(dx$Var1,l$Language2),]$iso2
      dx$l2 = l[match(dx$Var2,l$Language2),]$iso2
      dx = dx[dx$l1 %in% ling.langs & dx$l2 %in% ling.langs,]
      
      lx = ling.domain$imputed_semantic_domain==dom
      ling.domain[lx,]$cult.dist = dx[match(
        paste(ling.domain[lx,]$l1,ling.domain[lx,]$l2),
        paste(dx$l1,dx$l2)
      ),]$value
    }
  }
  
  ling.domain = ling.domain[,names(ling.domain)!="X"]
  
  write.csv(ling.domain,outputFile, row.names = F)
}

#combineCultAndLingDistances(
#"../data/FAIR/semantic_distances_by_domain_extended.csv",
#"../results/EA_distances/All_Domains_with_ling.csv")

combineCultAndLingDistances(
"../data/FAIR/nel-wiki-k100-alignments-clean-by-language-pair-and-domain.csv",
"../results/EA_distances/nel-wiki-k100-clean_with_ling.csv")

combineCultAndLingDistances(
  "../data/FAIR/nel-wiki-k100-alignments-by-language-pair-and-domain.csv",
  "../results/EA_distances/nel-wiki-k100_with_ling.csv")