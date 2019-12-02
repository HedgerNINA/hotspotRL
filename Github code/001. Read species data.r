
##############
# THIS READS SPECIES DATA (CSV FORMAT), 
# CLEANS THEM,
# SAVES THEM AS SHAPEFILES,
# SAVES THEM AS POINT PATTERNS IN .RData
##############


rm(list=ls())

library(tidyr) # addcoords
library(sp)
library(raster)
library(spatstat)
library(rgdal)


addcoords<-function(D){
  DD<-data.frame(D$WKT)
  g<-separate(DD,D.WKT, into = c("POINT", "E","N"), sep = " ")
  E<-g$E
  N<-g$N
  E<-gsub("\\(","",E)
  N<-gsub("\\)","",N)
  print(paste(sum(is.na(E)),sum(is.na(N))))
  D$E<-as.numeric(E)
  D$N<-as.numeric(N)
  return(D)
}

fixspecies<-function(D){
  D$Species<-D$Vitenskapelig.navn
  DD<-data.frame(D$Species)
  g<-separate(DD,D.Species, into = c("N1", "N2","N3"), sep = " ")
  D$Genus<-g$N1
  D$FullSpecName<-"Y"
  tf<-D$Species==D$Genus; D$FullSpecName[tf]<-"N"
  return(D)
}

filternames<-function(D){
  Names<-c("Institusjon","Kategori","Vitenskapelig.navn","Autor","Norsk.navn","Artsgruppe","Finner.Samler","Funndato","Presisjon","Kommune","Fylke","Antall","Funnegenskaper","Katalognummer","Breddegrad","Lengdegrad","Geometri","Art.rang","Endringsdato","Dybde","Institusjonskode","E","N","Species","Genus","FullSpecName")
  tf<-colnames(D)%in%Names
  D<-D[,tf]
  return(D)
}

convert2ppp<-function(D,CoRS){
  D.sp<-SpatialPointsDataFrame(D[,c("E", "N")], data.frame(D))
  crs(D.sp)<-CoRS
  #D.sp = spTransform(D.sp,NewCoRS)
  D.jit<-jittercoords(D.sp,1) 
  D.ppp<-ppp(x=D.jit$E,y=D.jit$N,window=Dil )
  #D.ppp<-as.ppp(D.ppp)
}


jittercoords<-function(input,dist=1){
  temp <- as.data.frame(input)
  L<-nrow(temp)
  temp$E<-temp$E+runif(L, min = 0 - dist, max = 0 + dist)
  temp$N<-temp$N+runif(L, min = 0 - dist, max = 0 + dist)
  return(temp)
}

write.obs.f<-function(D,Name)
{
  CoRS<-"+init=epsg:25833 +proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  write.csv(file=paste0("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/Filtered_observations/csv/",Name,".csv"),D)
  coordinates(D)=~E+N
  proj4string(D)<- CRS(CoRS)
  setwd("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/Filtered_observations/shp/")
  writeOGR(D, ".", Name, driver="ESRI Shapefile")
}

##################################################

setwd("C:/GIS")
NB <- readOGR(dsn = "Basic_data\\Boundaries\\Norway_terrestrialBoundaries.shp")

setwd("C:/GIS")
load("Rdata\\NorWin.1km.Rdata")
Dil<-dilation(NorWin.1km,5000)

setwd("C:\\15801000_geografisk_utbredelse_av_trua_arter_i_norge\\benno\\final")


A.ai<-read.csv(file="alle_arachnids+insecta.csv",stringsAsFactors=FALSE,encoding = "UTF-8")
A.b<-read.csv(file="alle_bryophytes.csv",stringsAsFactors=FALSE,encoding = "UTF-8")
A.f<-read.csv(file="alle_fungi.csv",stringsAsFactors=FALSE,encoding = "UTF-8")
A.l<-read.csv(file="alle_lichens.csv",stringsAsFactors=FALSE,encoding = "UTF-8")

R.ai<-read.csv(file="resp-list_arachnids+insecta.csv",stringsAsFactors=FALSE,encoding = "UTF-8")
R.b<-read.csv(file="resp-list_bryophytes.csv",stringsAsFactors=FALSE,encoding = "UTF-8")
R.f<-read.csv(file="resp-list_fungi.csv",stringsAsFactors=FALSE,encoding = "UTF-8")
R.l<-read.csv(file="resp-list_lichens.csv",stringsAsFactors=FALSE,encoding = "UTF-8")

T.ai<-read.csv(file="VU-EN-CR_arachnids+insecta.csv",stringsAsFactors=FALSE,encoding = "UTF-8")
T.b<-read.csv(file="VU-EN-CR_bryophytes.csv",stringsAsFactors=FALSE,encoding = "UTF-8")
T.f<-read.csv(file="VU-EN-CR_fungi.csv",stringsAsFactors=FALSE,encoding = "UTF-8")
T.l<-read.csv(file="VU-EN-CR_lichens.csv",stringsAsFactors=FALSE,encoding = "UTF-8")


# JOIN ALL CATEGROIES TOGETHER

A.ai$WKT.1<-NULL
data.frame(names(A.ai),names(A.b),names(A.f),names(A.l))

A<-rbind(A.ai,A.b,A.f,A.l)


# ADD COORDINATES

A<-addcoords(A)
A.ai<-addcoords(A.ai)
A.b<-addcoords(A.b)
A.f<-addcoords(A.f)
A.l<-addcoords(A.l)

R.ai<-addcoords(R.ai)
R.b<-addcoords(R.b)
R.f<-addcoords(R.f)
R.l<-addcoords(R.l)

T.ai<-addcoords(T.ai)
T.b<-addcoords(T.b)
T.f<-addcoords(T.f)
T.l<-addcoords(T.l)

# FIX SPECIES

###############


length(unique(R.ai$Species))
length(unique(T.ai$Species))


A<-fixspecies(A)
A.ai<-fixspecies(A.ai)
A.b<-fixspecies(A.b)
A.f<-fixspecies(A.f)
A.l<-fixspecies(A.l)

R.ai<-fixspecies(R.ai)
R.b<-fixspecies(R.b)
R.f<-fixspecies(R.f)
R.l<-fixspecies(R.l)

T.ai<-fixspecies(T.ai)
T.b<-fixspecies(T.b)
T.f<-fixspecies(T.f)
T.l<-fixspecies(T.l)

# FILTERNAMES

A.ai<-filternames(A.ai)
A.b<-filternames(A.b)
A.f<-filternames(A.f)
A.l<-filternames(A.l)

R.ai<-filternames(R.ai)
R.b<-filternames(R.b)
R.f<-filternames(R.f)
R.l<-filternames(R.l)

T.ai<-filternames(T.ai)
T.b<-filternames(T.b)
T.f<-filternames(T.f)
T.l<-filternames(T.l)

###




write.obs.f(A.ai,"A.ai")
write.obs.f(A.b,"A.b")
write.obs.f(A.f,"A.f")
write.obs.f(A.l,"A.l")
write.obs.f(R.ai,"R.ai")
write.obs.f(R.b,"R.b")
write.obs.f(R.f,"R.f")
write.obs.f(R.l,"R.l")
write.obs.f(T.ai,"T.ai")
write.obs.f(T.b,"T.b")
write.obs.f(T.f,"T.f")
write.obs.f(T.l,"T.l")



CoRS<-"+init=epsg:25833 +proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

A.ai.old<-A.ai

A<-convert2ppp(A,CoRS)
A.ai<-convert2ppp(A.ai,CoRS)
A.b<-convert2ppp(A.b,CoRS)
A.f<-convert2ppp(A.f,CoRS)
A.l<-convert2ppp(A.l,CoRS)

R.ai<-convert2ppp(R.ai,CoRS)
R.b<-convert2ppp(R.b,CoRS)
R.f<-convert2ppp(R.f,CoRS)
R.l<-convert2ppp(R.l,CoRS)

T.ai<-convert2ppp(T.ai,CoRS)
T.b<-convert2ppp(T.b,CoRS)
T.f<-convert2ppp(T.f,CoRS)
T.l<-convert2ppp(T.l,CoRS)

# SAVE RESULTS

setwd("C:\\15801000_geografisk_utbredelse_av_trua_arter_i_norge\\")

save(A, file="Data\\Rdata_ppp\\A.1km.Rdata")
save(A.ai, file="Data\\Rdata_ppp\\A.ai.1km.Rdata")
save(A.b, file="Data\\Rdata_ppp\\A.b.1km.Rdata")
save(A.f, file="Data\\Rdata_ppp\\A.f.1km.Rdata")
save(A.l, file="Data\\Rdata_ppp\\A.l.1km.Rdata")

save(T.ai, file="Data\\Rdata_ppp\\T.ai.1km.Rdata")
save(T.b, file="Data\\Rdata_ppp\\T.b.1km.Rdata")
save(T.f, file="Data\\Rdata_ppp\\T.f.1km.Rdata")
save(T.l, file="Data\\Rdata_ppp\\T.l.1km.Rdata")

save(R.ai, file="Data\\Rdata_ppp\\R.ai.1km.Rdata")
save(R.b, file="Data\\Rdata_ppp\\R.b.1km.Rdata")
save(R.f, file="Data\\Rdata_ppp\\R.f.1km.Rdata")
save(R.l, file="Data\\Rdata_ppp\\R.l.1km.Rdata")

