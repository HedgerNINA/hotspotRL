
######
# THIS READS (1) HOTSPOTS AND CALCULATES THE TOTAL AREA,
# THEN READS (2) SHAPEFILES OF AREAS OF ZONES THAT INTERESECT WITH HOTSPOT AREAS
# (FOR EACH OF NATURVERNOMRÅDER, NATURTYPE, NIN, MIS, INON)
# TO DETERMINE PERCENTATE OF HOTSPOT AREA WITHIN ZONE

rm(list=ls())

library(sp)
library(raster)
library(spatstat)
library(maptools)
library(shapefiles)
library(rgdal)
library(rgeos)


HSNames<-c("A.ai.10","A.ai.05","A.ai.01",   "A.b.10","A.b.05","A.b.01",   "A.f.10","A.f.05","A.f.01",   "A.l.10","A.l.05","A.l.01",   "A.kar.10","A.kar.05","A.kar.01",
           "T.ai.10","T.ai.05","T.ai.01",   "T.b.10","T.b.05","T.b.01",   "T.f.10","T.f.05","T.f.01",   "T.l.10","T.l.05","T.l.01",   "T.kar.10","T.kar.05","T.kar.01",
           "R.ai.10","R.ai.05","R.ai.01",   "R.b.10","R.b.05","R.b.01",   "R.f.10","R.f.05","R.f.01",   "R.l.10","R.l.05","R.l.01",   "R.kar.10","R.kar.05","R.kar.01")
HSNamesL<-length(HSNames)

setwd("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/")

### HOTSPOT AREA

HS.area<-rep(NA,HSNamesL)
for (i in 1:HSNamesL)
  {
  HS.name<-paste0("Hotspots/Shape/",HSNames[i],".shp")
  HS<-readOGR(dsn = HS.name)
  HS.area[i]<-length(HS$layer)
  }

### NATURVERNOMRÅDER

FTG<-read.csv(file="C:\\Users\\richard.hedger\\OneDrive - NINA\\NINA projects\\TruaArter\\Data\\Forslag til gruppering av verneformer.v2.csv",stringsAsFactors=FALSE)
ClassName<-c("Biotopvern","Landskapsvernområde","Marint verneområde","Nasjonalpark","Naturreservat")
NVO.Hotspot.int.area.array<-array(dim=c(length(ClassName),HSNamesL))
rownames(NVO.Hotspot.int.area.array)<-ClassName
colnames(NVO.Hotspot.int.area.array)<-HSNames
NVO.Hotspot.area<-rep(NA,HSNamesL)
for (i in 1:HSNamesL)
{
  NVO.name<-paste0("Intersection/Naturvernområder/NVO.",HSNames[i],".shp")
  NVO<-readOGR(dsn = NVO.name)
  Area<-gArea(NVO,byid=TRUE)/1E06
  
  VF<-as.character(NVO$VERNEFORM)
  Class<-FTG$Class[match(VF,FTG$Verneform)]
  Class<-factor(Class,levels=ClassName)
  
  NVO.Hotspot.int.area<-tapply(Area,Class,sum)
  NVO.Hotspot.int.area.array[,i]<-NVO.Hotspot.int.area
}

### NATURTYPE

NT.LETTER<-c("A","B","C","D","E","F","G","H","I")                            
NT.ClassName<-c("Myr og kilde","Rasmark, berg og kantkratt","Fjell","Kulturlandskap","Ferskvann/våtmark","Skog","Havstrand/kyst","Andre viktige forekomster","Marin")

NT.Hotspot.int.area.array<-array(dim=c(length(NT.ClassName),HSNamesL))
rownames(NT.Hotspot.int.area.array)<-NT.ClassName
colnames(NT.Hotspot.int.area.array)<-HSNames
NT.Hotspot.area<-rep(NA,HSNamesL)
for (i in 1:HSNamesL)
{
  NT.name<-paste0("Intersection/Naturtyper/NT.",HSNames[i],".shp")
  NT<-readOGR(dsn = NT.name)
  Area<-gArea(NT,byid=TRUE)/1E06
  nt.letter<-as.character(NT$naturtype)
  nt.letter<-substring(nt.letter,1,1)

  Class<-NT.ClassName[match(nt.letter,NT.LETTER)]
  Class<-factor(Class,levels=NT.ClassName)
  
  NT.Hotspot.int.area<-tapply(Area,Class,sum)
  NT.Hotspot.int.area.array[,i]<-NT.Hotspot.int.area
}

### NIN

NIN.Hotspot.int.area<-rep(NA,HSNamesL)
for (i in 1:HSNamesL)
{
  NIN.name<-paste0("Intersection/NIN/NIN.",HSNames[i],".shp")
  NIN<-readOGR(dsn = NIN.name)
  NIN.Hotspot.int.area[i]<-(sum(gArea(NIN,byid=TRUE))/1E06)
}

### MiS

MiS.Hotspot.int.area<-rep(NA,HSNamesL)
for (i in 1:HSNamesL)
  {
  MiS.name<-paste0("Intersection/MiS/MiS.",HSNames[i],".shp")
  MiS<-readOGR(dsn = MiS.name)
  MiS.Hotspot.int.area[i]<-(sum(gArea(MiS,byid=TRUE))/1E06)
}

### INON

INON.Hotspot.int.area<-rep(NA,HSNamesL)
for (i in 1:HSNamesL)
{
  INON.name<-paste0("Intersection/INON/INON.",HSNames[i],".shp")
  INON<-readOGR(dsn = INON.name)
  INON.Hotspot.int.area[i]<-(sum(gArea(INON,byid=TRUE))/1E06)
}

##############################################

colnames(NT.Hotspot.int.area.array)<-HSNames

### PERCENTAGE

Per.NVO.Hotspot.int.area.array<-NVO.Hotspot.int.area.array
L<-nrow(NVO.Hotspot.int.area.array)
for (i in 1:L){
  Per.NVO.Hotspot.int.area.array[i,]<-NVO.Hotspot.int.area.array[i,]*100/HS.area
}
tf<-is.na(Per.NVO.Hotspot.int.area.array); Per.NVO.Hotspot.int.area.array[tf]<-0


Per.NT.Hotspot.int.area.array<-NT.Hotspot.int.area.array
L<-nrow(NT.Hotspot.int.area.array)
for (i in 1:L){
  Per.NT.Hotspot.int.area.array[i,]<-NT.Hotspot.int.area.array[i,]*100/HS.area
}
tf<-is.na(Per.NT.Hotspot.int.area.array); Per.NT.Hotspot.int.area.array[tf]<-0

Per.INON.Hotspot.int.area<-INON.Hotspot.int.area*100/HS.area
Per.NIN.Hotspot.int.area<-NIN.Hotspot.int.area*100/HS.area
Per.MiS.Hotspot.int.area<-MiS.Hotspot.int.area*100/HS.area

### ADD NAMES

names(Per.INON.Hotspot.int.area)<-HSNames
names(Per.NIN.Hotspot.int.area)<-HSNames
names(Per.MiS.Hotspot.int.area)<-HSNames

setwd("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/Tables")

write.csv(file="Per.NVO.Hotspot.int.area.array.csv",Per.NVO.Hotspot.int.area.array)
write.csv(file="Per.NT.Hotspot.int.area.array.csv",Per.NT.Hotspot.int.area.array)
write.csv(file="Per.INON.Hotspot.int.area.csv",Per.INON.Hotspot.int.area)
write.csv(file="Per.NIN.Hotspot.int.area.csv",Per.NIN.Hotspot.int.area)
write.csv(file="Per.MiS.Hotspot.int.area.csv",Per.MiS.Hotspot.int.area)

