
rm(list=ls())

library(sp)
library(raster)
library(spatstat)
library(maptools)
library(shapefiles)
library(rgdal)

source("C:/Users/richard.hedger/OneDrive - NINA/NINA projects/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Code/extract_species.r")

setwd("C:\\Users\\richard.hedger\\OneDrive - NINA\\")
source("NINA projects\\TruaArter\\Code\\Functions\\lso.r")
source("NINA projects\\TruaArter\\Code\\Functions\\myPseudoR2.r")
source("NINA projects\\TruaArter\\Code\\Functions\\gbifJitter.r")
source("NINA projects\\TruaArter\\Code\\Functions\\predPlot.r")
source("NINA projects\\TruaArter\\Code\\Functions\\multiCoefPlot.r")

####

write.obs.f<-function(D,Name)
{
  tf<-!is.na(D$Species); D<-D[tf,]
  D<-data.frame(D)
#  D$E<-D$x
#  D$N<-D$y
  
  CoRS<-"+init=epsg:25833 +proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  write.csv(file=paste0("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/Filtered_observations/csv/",Name,".csv"),D)
  coordinates(D)=~E+N
  proj4string(D)<- CRS(CoRS)
  setwd("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/Filtered_observations/shp/")
  writeOGR(D, ".", Name, driver="ESRI Shapefile")
}

####


setwd("C:/GIS")
load("Rdata\\NorWin.1km.Rdata")

Dil<-dilation(NorWin.1km,5000)

setwd("C:/GIS")

CoRS<-   "+init=epsg:32633 +proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"
NewCoRS<-"+init=epsg:25833 +proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

##########
# THREATENED SPECIES
##########

setwd("C:/GIS")
R <- readOGR(dsn = "Basic_data\\Species\\Truendearter_after1998.shp",encoding="UTF-8",use_iconv=TRUE)
tf<-R$Phylum%in%c("Magnoliophyta","Pinophyta","Pteridophyta"); R<-R[tf,]
R$YearCollec<-as.numeric(as.character(R$YearCollec)); tf<-R$YearCollec>=1998; R<-R[tf,]
xy<-coordinates(R); R$x<-xy[,1]; R$y<-xy[,2]
R$Species<-as.character(R$VitNavn)
#write.obs.f(R,"T.kar")
R.old<-R
R.sp<-SpatialPointsDataFrame(R[,c("x", "y")], data.frame(R[,c(2,14,25)]))
crs(R.sp)<-CoRS

R.sp = spTransform(R.sp,NewCoRS)
R.jit<-gbifJitter(R.sp)
R.jit$E<-R.jit$coords.x1.1; R.jit$N<-R.jit$coords.x2.1; write.obs.f(R.jit,"T.kar")

R<-ppp(x=R.jit$coords.x1,y=R.jit$coords.x2,window=Dil )
T.kar<-as.ppp(R)
save(T.kar, file="C:\\15801000_geografisk_utbredelse_av_trua_arter_i_norge\\Data\\Rdata_ppp\\T.kar.1km.Rdata")


##########
# REDLIST DATA
##########

setwd("C:\\Users\\richard.hedger\\OneDrive - NINA\\")
RL<-read.csv(file="NINA projects\\TruaArter\\Data\\Rodlista2015_Criteria.csv",stringsAsFactors=FALSE)

R.old$VitNavn<-as.character(R.old$VitNavn)
tf<-R.old$VitNavn=="Atriplex longipes ssp. praecox"; R.old$VitNavn[tf]<-"Atriplex longipes"
tf<-R.old$VitNavn=="Odontites vernus ssp. serotinus"; R.old$VitNavn[tf]<-"Odontites vernus"
tf<-R.old$VitNavn=="Papaver dahlianum ssp. dahlianum"; R.old$VitNavn[tf]<-"Papaver dahlianum"      
tf<-R.old$VitNavn=="Salix daphnoides daphnoides"; R.old$VitNavn[tf]<-"Salix daphnoides"
tf<-R.old$VitNavn=="Trifolium montanum L."; R.old$VitNavn[tf]<-"Trifolium montanum"

R.old$Kat<-RL$Kriterium_kat[match(R.old$VitNavn,RL$Vitenskapelig.navn)]
tf<-is.na(R.old$Kat); table(R.old$VitNavn[tf])


tf<-R.old$VitNavn%in%RL$Vitenskapelig.navn
Sub<-R.old$VitNavn[tf==FALSE]
out<-sort(unique(Sub))
write.csv(file="Truede not in redlist.csv",data.frame(out))

setwd("C:/GIS")
KatU<-c("A","B","C","D")
KatUL<-length(KatU)

i<-1
tf<-R.old$Kat%in%KatU[i]
Sub<-R.old[tf,]; print(nrow(Sub))
Sub.sp<-SpatialPointsDataFrame(Sub[,c("x", "y")], data.frame(Sub[,c(2,25)]))
crs(Sub.sp)<-CoRS
Sub.sp = spTransform(Sub.sp,NewCoRS)
Sub.jit<-gbifJitter(Sub.sp) 
Sub.jit<-ppp(x=Sub.jit$coords.x1,y=Sub.jit$coords.x2,window=Dil )
Sub.jit<-as.ppp(Sub.jit)
RL.A<-Sub.jit
save(RL.A, file=paste("Rdata\\RedList.",KatU[i],".1km.Rdata",sep=""))

i<-2
tf<-R.old$Kat%in%KatU[i]
Sub<-R.old[tf,]; print(nrow(Sub))
Sub.sp<-SpatialPointsDataFrame(Sub[,c("x", "y")], data.frame(Sub[,c(2,25)]))
crs(Sub.sp)<-CoRS
Sub.sp = spTransform(Sub.sp,NewCoRS)
Sub.jit<-gbifJitter(Sub.sp) 
Sub.jit<-ppp(x=Sub.jit$coords.x1,y=Sub.jit$coords.x2,window=Dil )
Sub.jit<-as.ppp(Sub.jit)
RL.B<-Sub.jit
save(RL.B, file=paste("Rdata\\RedList.",KatU[i],".1km.Rdata",sep=""))

i<-3
tf<-R.old$Kat%in%KatU[i]
Sub<-R.old[tf,]; print(nrow(Sub))
Sub.sp<-SpatialPointsDataFrame(Sub[,c("x", "y")], data.frame(Sub[,c(2,25)]))
crs(Sub.sp)<-CoRS
Sub.sp = spTransform(Sub.sp,NewCoRS)
Sub.jit<-gbifJitter(Sub.sp) 
Sub.jit<-ppp(x=Sub.jit$coords.x1,y=Sub.jit$coords.x2,window=Dil )
Sub.jit<-as.ppp(Sub.jit)
RL.C<-Sub.jit
save(RL.C, file=paste("Rdata\\RedList.",KatU[i],".1km.Rdata",sep=""))

i<-4
tf<-R.old$Kat%in%KatU[i]
Sub<-R.old[tf,]; print(nrow(Sub))
Sub.sp<-SpatialPointsDataFrame(Sub[,c("x", "y")], data.frame(Sub[,c(2,25)]))
crs(Sub.sp)<-CoRS
Sub.sp = spTransform(Sub.sp,NewCoRS)
Sub.jit<-gbifJitter(Sub.sp) 
Sub.jit<-ppp(x=Sub.jit$coords.x1,y=Sub.jit$coords.x2,window=Dil )
Sub.jit<-as.ppp(Sub.jit)
RL.D<-Sub.jit
save(RL.D, file=paste("Rdata\\RedList.",KatU[i],".1km.Rdata",sep=""))

length(R$x)
length(RL.A$x)+length(RL.B$x)+length(RL.C$x)+length(RL.D$x)

###################
# RESPONSIBLE SPECIES
###################

setwd("C:/GIS")
A <- readOGR(dsn = "Basic_data\\Species\\AnsvarsarterAfter1998.shp")
A$x=as.numeric(as.character(A$UTM33ost))
A$y=as.numeric(as.character(A$UTM33nord))
A$Species<-A$VitNavn
#write.obs.f(A,"R.kar")
A.sp<-SpatialPointsDataFrame(A[,c("x", "y")], data.frame(A[,c(2,14,25)]))
crs(A.sp)<-CoRS
A.sp = spTransform(A.sp,NewCoRS)
A.sp$YearCollec<-as.numeric(as.character(A.sp$YearCollec))
tf<-A.sp$YearCollec>=1998
A.sp<-A.sp[tf,]
A.jit<-gbifJitter(A.sp) 
A.jit$E<-A.jit$coords.x1.1; A.jit$N<-A.jit$coords.x2.1; write.obs.f(A.jit,"R.kar")
A<-ppp(x=A.jit$coords.x1,y=A.jit$coords.x2,window=Dil )
R.kar<-as.ppp(A)
save(R.kar, file="C:\\15801000_geografisk_utbredelse_av_trua_arter_i_norge\\Data\\Rdata_ppp\\R.kar.1km.Rdata")



###################
# ALL SPECIES
###################

setwd("C:/GIS")
G <- readOGR(dsn = "Basic_data\\Species\\GeneralSpecies_accuracyOver100m_mainland_25833.shp")
Sel<-c("Tracheophyta","Magnoliophyta","Pinophyta","Pteridophyta")

tf<-G$kingdom%in%Sel | G$recordnumb%in%Sel | G$field_47%in%Sel | G$field_53%in%Sel
G<-G[tf,]

xy<-coordinates(G)
tf<-xy[,1]<=-2e05; tf[is.na(tf)]<-FALSE; xy[tf,1]<-NA
tf<-xy[,2]<=6e06; tf[is.na(tf)]<-FALSE; xy[tf,2]<-NA
plot(xy[,1],xy[,2],pch=".",col="RED")
G$x<-xy[,1]
G$y<-xy[,2]
G$Species<-extract_species.f(G)
#write.obs.f(G,"A.kar")
G.sp<-SpatialPointsDataFrame(G[,c("x", "y")], data.frame(G[,c(2,64)]))
crs(G.sp)<-CoRS
G.sp = spTransform(G.sp,NewCoRS)
G.jit<-gbifJitter(G.sp) 
G.jit$E<-G.jit$coords.x1.1; G.jit$N<-G.jit$coords.x2.1; write.obs.f(G.jit,"A.kar")

G<-ppp(x=G.jit$coords.x1,y=G.jit$coords.x2,window=Dil )
A.kar<-as.ppp(G)
save(A.kar, file="C:\\15801000_geografisk_utbredelse_av_trua_arter_i_norge\\Data\\Rdata_ppp\\A.kar.1km.Rdata")

###


##################
# SPECIES GROUPING
##################

R <- readOGR(dsn = "Basic_data\\Species\\Truendearter_after1998.shp")

xy<-coordinates(R); R$x<-xy[,1]; R$y<-xy[,2]
R.sp<-SpatialPointsDataFrame(R[,c("x", "y")], data.frame(R[,c(2,8,9,25)]))
crs(R.sp)<-CoRS
R.sp = spTransform(R.sp,NewCoRS)
R.sp$YearCollec<-as.numeric(as.character(R.sp$YearCollec))
tf<-R.sp$YearCollec>=1998
R.sp<-R.sp[tf,]
R.jit<-gbifJitter(R.sp)

R.jit$Phylum<-as.character(R.jit$Phylum)
PhylumU<-unique(R.jit$Phylum)
PhylumU<-PhylumU[c(1,2,3,4,5,6,8)]
PhylumUL<-length(PhylumU)
for (i in 1:PhylumUL)
{
tf<-R.jit$Phylum==PhylumU[i]
Sub.jit<-R.jit[tf,]
Sub.jit<-ppp(x=Sub.jit$coords.x1,y=Sub.jit$coords.x2,window=Dil )
Sub.jit<-as.ppp(Sub.jit)
save(Sub.jit, file=paste("Rdata\\",PhylumU[i],".1km.Rdata",sep=""))
}

