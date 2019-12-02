
######
# THIS READS HOTSPOTS FROM THE FIVE SPECIES GROUPS
# AND DETERMINES THE TOTAL NUMBER OF OVERLAPPING HOTSPOTS (0-5)
# IN EACH 1 km PIXEL
######

rm(list=ls())
library(raster)
library(rasterVis)
library(rgdal)


setwd("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/Hotspots/Raster/")
NB <- readOGR(dsn = "C:/13704100_kartlegging_vandringshinder_for_sjoorret_gis_met/Data/Boundaries/Norway_terrestrialBoundaries.shp")

A.ai<-raster("A.ai.hotspot.tif")
A.b<-raster("A.b.hotspot.tif")
A.f<-raster("A.f.hotspot.tif")
A.l<-raster("A.l.hotspot.tif")
A.kar<-raster("A.kar.hotspot.tif")

T.ai<-raster("T.ai.hotspot.tif")
T.b<-raster("T.b.hotspot.tif")
T.f<-raster("T.f.hotspot.tif")
T.l<-raster("T.l.hotspot.tif")
T.kar<-raster("T.kar.hotspot.tif")

R.ai<-raster("R.ai.hotspot.tif")
R.b<-raster("R.b.hotspot.tif")
R.f<-raster("R.f.hotspot.tif")
R.l<-raster("R.l.hotspot.tif")
R.kar<-raster("R.kar.hotspot.tif")

overlap<-function(ai,b,f,l,kar){
tot.ls<-list()
Level<-c(10,5,1)
for (i in 1:3)
{
  print(i)
  ai.lev<-ai%in%Level[i:3]
  b.lev<-b%in%Level[i:3]
  f.lev<-f%in%Level[i:3]
  l.lev<-l%in%Level[i:3]
  kar.lev<-kar%in%Level[i:3]
  tot<-ai.lev + b.lev + f.lev + l.lev + kar.lev
  tf<-is.na(ai); tot[tf]<-NA
  tot.ls[[i]]<-tot
}
return(tot.ls)
}

A<-overlap(A.ai, A.b, A.f, A.l,A.kar)
T<-overlap(T.ai, T.b, T.f, T.l,T.kar)
R<-overlap(R.ai, R.b, R.f, R.l,R.kar)

plot(A[[1]])
plot(A[[2]])
plot(A[[3]])

plot(T[[1]])
plot(T[[2]])
plot(T[[3]])

plot(R[[1]])
plot(R[[2]])
plot(R[[3]])


########
# PLOT OVERLAPS - TIFF
########

plot.overlap<-function(D.class,VAL,NAMEOUT){
D.class<-mask(D.class,NB)
D.fac<-ratify(D.class)
rat <- levels(D.fac)[[1]]
rat$landcover<-c("0","1","2","3","4","5")
levels(D.fac)<-rat
myColors <- c("lightgray","blue", "cyan", "green","orange","red")
print(VAL)
tiff(file=paste0("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Figures/Hotspots_overlap.",NAMEOUT,".tiff"),units="in",res=600,width=5.5,height=6,compression="lzw")
if(VAL=="A") {print(levelplot(D.fac, col.regions = myColors,scales=list(y=list(rot=90))) + layer(panel.text(0, 7850000, "A")) + layer(sp.polygons(NB)) )}
if(VAL=="B") {print(levelplot(D.fac, col.regions = myColors,scales=list(y=list(rot=90))) + layer(panel.text(0, 7850000, "B")) + layer(sp.polygons(NB)) )}
if(VAL=="C") {print(levelplot(D.fac, col.regions = myColors,scales=list(y=list(rot=90))) + layer(panel.text(0, 7850000, "C")) + layer(sp.polygons(NB)) )}
dev.off()
}

plot.overlap(A[[3]],"A","Alle.01")
plot.overlap(A[[2]],"B","Alle.05")
plot.overlap(A[[1]],"C","Alle.10")

plot.overlap(T[[3]],"A","Truede.01")
plot.overlap(T[[2]],"B","Truede.05")
plot.overlap(T[[1]],"C","Truede.10")

plot.overlap(R[[3]],"A","Ansvar.01")
plot.overlap(R[[2]],"B","Ansvar.05")
plot.overlap(R[[1]],"C","Ansvar.10")


########
# PLOT OVERLAPS - EPS
########

plot.overlap.eps<-function(D.class,TITLE,NAMEOUT){
  D.fac<-ratify(D.class)
  rat <- levels(D.fac)[[1]]
  rat$landcover<-c("0","1","2","3","4","5")
  levels(D.fac)<-rat
  myColors <- c("white","blue", "cyan", "green","orange","red")
  postscript(file=paste0("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Figures/Hotspots_overlap.",NAMEOUT,".eps"),horizontal=FALSE)
  print(levelplot(D.fac,scales=list(y=list(rot=90)), col.regions = myColors,main=TITLE) + layer(sp.polygons(NB)))
  dev.off()
}

plot.overlap.eps(A[[1]],"Alle (10%)","Alle.10")
plot.overlap.eps(A[[2]],"Alle (5%)","Alle.05")
plot.overlap.eps(A[[3]],"Alle (1%)","Alle.01")

plot.overlap.eps(T[[1]],"Truede (10%)","Truede.10")
plot.overlap.eps(T[[2]],"Truede (5%)","Truede.05")
plot.overlap.eps(T[[3]],"Truede (1%)","Truede.01")

plot.overlap.eps(R[[1]],"Ansvar (10%)","Ansvar.10")
plot.overlap.eps(R[[2]],"Ansvar (5%)","Ansvar.05")
plot.overlap.eps(R[[3]],"Ansvar (1%)","Ansvar.01")


