library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)

library(dplyr)

setwd("~/Documents/Bristol/word2vec/word2vec_DPLACE/processing")

# http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CAMS_OPI/.v0208/.mean/.prcp/T/%28Jan%201981%29%28Dec%202010%29RANGE/T/12/splitstreamgrid%5BT2%5Daverage/T/%28days%20since%201960-01-01%29streamgridunitconvert/T/differential_mul/T/%28months%20since%201960-01-01%29streamgridunitconvert//units/%28mm/month%29def/T/12/makeperiodic/X/Y/1/SM121//long_name/%28Precipitation%29def/DATA/0/50/100/150/200/250/300/350/400/450/500/VALUES/datatables.html
p = read.csv("../data/geography/NOAA-NCEP_CPC_CAMS_OPI_v0208_mean_prcp.csv",stringsAsFactors = F)
p2 = p %>% group_by(Longitude,Latitude) %>% summarise(avg=mean(Precipitation))
p2$Latitude[grepl("S",p2$Latitude)] = paste0("-",p2$Latitude[grepl("S",p2$Latitude)])
p2$Latitude = gsub("[SN]","",p2$Latitude)
p2$Latitude = as.numeric(p2$Latitude)
p2$Longitude[grepl("W",p2$Longitude)] = paste0("-",p2$Longitude[grepl("W",p2$Longitude)])
p2$Longitude = gsub("[EW]","",p2$Longitude)
p2$Longitude = as.numeric(p2$Longitude)


d = read.csv("../data/geography/geographic-terms-nel.csv",stringsAsFactors = F, fileEncoding = "UTF-8",encoding = "UTF-8")

g = read.csv("../data/glottolog-languoid.csv/languoid.csv",stringsAsFactors = F, fileEncoding = "UTF-8",encoding = "UTF-8")

d$latl1 = g[match(d$Glottocode_l1,g$id),]$latitude
d$latl2 = g[match(d$Glottocode_l2,g$id),]$latitude
d$longl1 = g[match(d$Glottocode_l1,g$id),]$longitude
d$longl2 = g[match(d$Glottocode_l2,g$id),]$longitude

library(fields)
dist = rdist.earth(d[,c("longl1",'latl1')],p2[,c("Longitude","Latitude")])
dMatch = unlist(apply(dist,1,function(X){which(X==min(X))[1]}))
d$MeanPrecipitationL1 = p2[dMatch,]$avg


dist = rdist.earth(d[,c("longl2",'latl2')],p2[,c("Longitude","Latitude")])
dMatch = unlist(apply(dist,1,function(X){which(X==min(X))[1]}))
d$MeanPrecipitationL2 = p2[dMatch,]$avg


colx = gray(d$MeanPrecipitationL1/max(d$MeanPrecipitationL1))
plot(d$longl1,d$latl1,col=colx,pch=16)

plot(abs(d[d$Concept_ID=="Regen::N",]$MeanPrecipitationL1-
               d[d$Concept_ID=="Regen::N",]$MeanPrecipitationL2),
         d[d$Concept_ID=="Regen::N",]$local_alignment)


#####
#https://www.ipcc-data.org/observ/clim/cru_climatologies.html
# f = read.delim("../data/geography/ippc-data_observ_clim_10yrMeans_cfrsall/cfrs8190.dat",stringsAsFactors = F,skip=2,header = F,sep="\t")
# xmin = 0.25
# ymin = -89.75
# xmax = 359.75
# ymax = 89.75
# grd_sz = 0.50
# 
# y = seq(ymin,ymax,by=grd_sz)
# x = seq(xmin,xmax,by=grd_sz)
# 
# m = seq()
# 
# image(matrix(f[1:360,1:720]))


#ISLSCP II Climate Research Unit CRU05 Monthly Climate Data
#https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1015

# One file for each month
files= list.files("../data/geography/cru5_frost_frq_hd_1961-90/",pattern="*.asc")

frost = NA

for(filename in files){

  f = read.delim("../data/geography/cru5_frost_frq_hd_1961-90/cru5_frost_frq_hd_61-9001.asc",stringsAsFactors = F,skip=6,header=F,sep=" ")
  # remove missing data
  # (all the points we want have no missing data)
  f[f<0] = 0
  
  if(is.na(frost)){
    frost = as.matrix(f)
  } else{
    frost = frost + as.matrix(f)
  }
  
}

ncols = 720
nrows =360
xllcorner = -180
yllcorner =-90
cellsize = .5
y = rev(seq(yllcorner,by=cellsize,length.out = 360))
x = seq(xllcorner,by=cellsize,length.out=720)

# columns first
f2 = data.frame(
  f = as.vector(frost),
  long = rep(x,each=length(y)),
  lat = rep(y,length.out=length(as.vector(frost)))
)

#col = gray((f2$f+100)/(100+max(f2$f)))
#plot(f2$long,f2$lat,col=col,pch=16)

dist = rdist.earth(d[,c("longl1",'latl1')],f2[,c("long","lat")])
dMatch = unlist(apply(dist,1,function(X){which(X==min(X))[1]}))
d$FrostL1 = f2[dMatch,]$f
dist = rdist.earth(d[,c("longl2",'latl2')],f2[,c("long","lat")])
dMatch = unlist(apply(dist,1,function(X){which(X==min(X))[1]}))
d$FrostL2 = f2[dMatch,]$f


sel = d$Concept_ID %in% c("Frost::N")
plot(abs(d[sel,]$FrostL1-
           d[sel,]$FrostL2),
     d[sel,]$local_alignment)


write.csv(d[,c("l1","l2","MeanPrecipitationL1","MeanPrecipitationL2","FrostL1","FrostL2")],"../data/geography/MeanPrecipitation_FrostDays_Processed.csv")