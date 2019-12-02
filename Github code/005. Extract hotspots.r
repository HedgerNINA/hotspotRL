
rm(list=ls())

SF<-0

library(sf)
library(sp)
library(raster)
library(spatstat)
library(maptools)
library(shapefiles)
library(rgdal)
library(rasterVis)
library(corrplot)
library(colorRamps)
library(reshape)
library(rgeos)

CoRS<-"+init=epsg:25833 +proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

union.f<-function(g){
  d<-gUnaryUnion(g)
  did <- sapply(slot(d, "polygons"), function(x) slot(x, "ID"))
  d.df <- data.frame( ID=1:length(d), row.names = did)    
  d <- SpatialPolygonsDataFrame(d, d.df)
  return(d)
}

Name<-c("A.ai","A.b","A.f","A.l","A.kar","T.ai","T.b","T.f","T.l","T.kar","R.ai","R.b","R.f","R.l","R.kar")
NameL<-length(Name)

NB <- readOGR(dsn = "C:/GIS/Basic_data/Boundaries/Norway_terrestrialBoundaries.shp")

### WRITE CSV FILES

for (i in 1:NameL)
{
setwd("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/")
load(paste0("Data/Rdata_PSIM/",Name[i],".pred.1km.Rdata"))

if(substring(Name[i],1,1)=="A"){p1<-g.pred}
if(substring(Name[i],1,1)%in%c("T","R")){p1<-go.pred}

pdf<-as.data.frame(p1)
qcrit<-quantile(pdf$value,probs=0.90); tf<-pdf$value>qcrit; pdf.90<-pdf[tf,]
qcrit<-quantile(pdf$value,probs=0.95); tf<-pdf$value>qcrit; pdf.95<-pdf[tf,]
qcrit<-quantile(pdf$value,probs=0.99); tf<-pdf$value>qcrit; pdf.99<-pdf[tf,]

write.csv(file=paste0("Data/Hotspots/csv/",Name[i],".10.new.csv"),pdf.90)
write.csv(file=paste0("Data/Hotspots/csv/",Name[i],".05.new.csv"),pdf.95)
write.csv(file=paste0("Data/Hotspots/csv/",Name[i],".01.new.csv"),pdf.99)
}

### WRITE RASTER AND VECTOR

Name<-c("A.ai","A.b","A.f","A.l","A.kar","T.ai","T.b","T.f","T.l","T.kar","R.ai","R.b","R.f","R.l","R.kar")
NameNor<-c("Alle: insekta og edderkoppdyr","Alle: moser","Alle: sopp","Alle: lav","Alle: karplanter","Truede: insekta og edderkoppdyr","Truede: moser","Truede: sopp","Truede: lav","Truede: karplanter","Ansvar: insekta og edderkoppdyr","Ansvar: moser","Ansvar: sopp","Ansvar: lav","Ansvar: karplanter")
NameOut<-c("Alle_insekterEdderkoppdyr","Alle_moser","Alle_sopp","Alle_lav","Alle_karplanter","Truede_insekterEdderkoppdyr","Truede_moser","Truede_sopp","Truede_lav","Truede_karplanter","Ansvar_insekterEdderkoppdyr","Ansvar_moser","Ansvar_sopp","Ansvar_lav","Ansvar_karplanter")
NameL<-length(Name)

for (i in 1:NameL)
{
  print(i)
  setwd("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/")
  load(paste0("Data/Rdata_PSIM/",Name[i],".pred.1km.Rdata"))

if(substring(Name[i],1,1)=="A"){D<-raster(g.pred)}
if(substring(Name[i],1,1)%in%c("T","R")){D<-raster(go.pred)}

crs(D)<-CoRS
  
Dmin<-0
D10<-quantile(D,probs=0.90)
D5<-quantile(D,probs=0.95)
D1<-quantile(D,probs=0.99)
Dmax<-quantile(D,probs=1)

Min<-c(Dmin,D10,D5,D1)
Max<-c(D10,D5,D1,Dmax)
Level<-c(100,10,5,1)
D.LUT<-cbind(Min,Max,Level)

D.class<-reclassify(D,as.matrix(D.LUT))

tmp<-"Data/Hotspots/Raster/"
writeRaster(D.class, filename=file.path(tmp, paste0(Name[i],".hotspot.tif")), format="GTiff", overwrite=TRUE)

setwd("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/")

r <- D.class %in% c(10,5,1)
writeRaster(r, filename=file.path(tmp, paste0("r.tif")), format="GTiff", overwrite=TRUE)
pol.union <- rasterToPolygons(r, fun=function(x){x==1})
Name.out<-paste0("Data/Hotspots/Shape/",Name[i],".10")
writeOGR(pol.union, ".", Name.out, driver="ESRI Shapefile",overwrite_layer=TRUE)
d<-union.f(pol.union)
Name.out<-paste0("Data/Hotspots/Shape/Dissolved/",Name[i],".10.dis")
writeOGR(d, ".", Name.out, driver="ESRI Shapefile",overwrite_layer=TRUE)

r <- D.class %in% c(5,1)
pol.union <- rasterToPolygons(r, fun=function(x){x==1})
Name.out<-paste0("Data/Hotspots/Shape/",Name[i],".05")
writeOGR(pol.union, ".", Name.out, driver="ESRI Shapefile",overwrite_layer=TRUE)
Name.out<-paste0("Data/Hotspots/Shape/Dissolved/",Name[i],".05.dis")
writeOGR(d, ".", Name.out, driver="ESRI Shapefile",overwrite_layer=TRUE)

r <- D.class %in% c(1)
pol.union <- rasterToPolygons(r, fun=function(x){x==1})
Name.out<-paste0("Data/Hotspots/Shape/",Name[i],".01")
writeOGR(pol.union, ".", Name.out, driver="ESRI Shapefile",overwrite_layer=TRUE)
d<-union.f(pol.union)
Name.out<-paste0("Data/Hotspots/Shape/Dissolved/",Name[i],".01.dis")
writeOGR(d, ".", Name.out, driver="ESRI Shapefile",overwrite_layer=TRUE)

}


