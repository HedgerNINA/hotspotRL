
rm(list=ls())

library(sp)
library(raster)
library(spatstat)
library(maptools)
library(shapefiles)
library(rgdal)
library(rgeos)


####


HSNames<-c("A.ai.10","A.ai.05","A.ai.01",   "A.b.10","A.b.05","A.b.01",   "A.f.10","A.f.05","A.f.01",   "A.l.10","A.l.05","A.l.01",   "A.kar.10","A.kar.05","A.kar.01",
           "T.ai.10","T.ai.05","T.ai.01",   "T.b.10","T.b.05","T.b.01",   "T.f.10","T.f.05","T.f.01",   "T.l.10","T.l.05","T.l.01",   "T.kar.10","T.kar.05","T.kar.01",
           "R.ai.10","R.ai.05","R.ai.01",   "R.b.10","R.b.05","R.b.01",   "R.f.10","R.f.05","R.f.01",   "R.l.10","R.l.05","R.l.01",   "R.kar.10","R.kar.05","R.kar.01")
HSNamesL<-length(HSNames)
SubGroup<-substring(HSNames,1,1)
SpecGroup<-substring(HSNames,3,3)

setwd("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/")


MiS.name<-paste0("Zones/MiS.shp")
MiS.all<-readOGR(dsn = MiS.name)
Mis.area<-(sum(gArea(MiS.all,byid=TRUE))/1E06)
MiS.Hotspot.int.area<-rep(NA,HSNamesL)
for (i in 1:HSNamesL)
{
  MiS.name<-paste0("Intersection/MiS/MiS.",HSNames[i],".shp")
  MiS<-readOGR(dsn = MiS.name)
  MiS.Hotspot.int.area[i]<-(sum(gArea(MiS,byid=TRUE))/1E06)
}
Mis.PerInHS<-MiS.Hotspot.int.area*100/Mis.area


NIN.name<-paste0("Zones/NIN_25833.shp")
NIN.all<-readOGR(dsn = NIN.name)
NIN.area<-(sum(gArea(NIN.all,byid=TRUE))/1E06)
NIN.Hotspot.int.area<-rep(NA,HSNamesL)
for (i in 1:HSNamesL)
{
  NIN.name<-paste0("Intersection/NIN/NIN.",HSNames[i],".shp")
  NIN<-readOGR(dsn = NIN.name)
  NIN.Hotspot.int.area[i]<-(sum(gArea(NIN,byid=TRUE))/1E06)
}
NIN.PerInHS<-NIN.Hotspot.int.area*100/NIN.area

setwd("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/")

NT.name<-paste0("Zones/Naturtype_25833_GeoCheck.shp")
NT.all<-readOGR(dsn = NT.name)
NT.area<-(sum(gArea(NT.all,byid=TRUE))/1E06)
NT.Hotspot.int.area<-rep(NA,HSNamesL)
for (i in 1:HSNamesL)
{
  NT.name<-paste0("Intersection/Naturtyper/NT.",HSNames[i],".shp")
  NT<-readOGR(dsn = NT.name)
  NT.Hotspot.int.area[i]<-(sum(gArea(NT,byid=TRUE))/1E06)
}
NT.PerInHS<-NT.Hotspot.int.area*100/NT.area

###############

setwd("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/Tables")

write.csv(file="Mis.PerInHS.csv",MiS.PerInHS.csv)
write.csv(file="NIN.PerInHS.csv",NIN.PerInHS.csv)
write.csv(file="NT.PerInHS.csv",NT.PerInHS.csv)

