
######
# THIS FITS POINT PROCESS MODELS
######

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
library(car)

#setwd("C:/Users/richard.hedger/OneDrive - NINA/")
#source("NINA projects/TruaArter/Code/Functions/lso.r")
#source("NINA projects/TruaArter/Code/Functions/myPseudoR2.r")
#source("NINA projects/TruaArter/Code/Functions/gbifJitter.r")
#source("NINA projects/TruaArter/Code/Functions/predPlot.r")
#source("NINA projects/TruaArter/Code/Functions/predPlot.title.r")
#source("NINA projects/TruaArter/Code/Functions/multiCoefPlot.setrange.r")
#source("NINA projects/TruaArter/Code/Functions/SCP.Z.r")
#source("NINA projects/TruaArter/Code/Functions/sortedCoefPlot.r")
#source("NINA projects/TruaArter/Code/Functions/singleCoefPlot.r")
#source("NINA projects/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Code/Functions/multiCoefPlot.r")
#source("NINA projects/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Code/Functions/sortedCoefPlot.r")


#setwd("R:/Prosjekter/15801000_geografisk_utbredelse_av_trua_arter_i_norge/GIS")
setwd("C:/GIS")

load("Basic_data/Borders/NorwayCounty.Rdata")
NorwayBorder <- rgeos::gUnaryUnion(NorwayCounty)

load("Rdata/NorWin.1km.Rdata")
#Dil<-dilation(NorWin.1km,10000)

### LOAD IMAGE DATA

load("Rdata/Alt.1km.Rdata")
load("Rdata/Calc.1km.Rdata")
load("Rdata/NDVI.1km.Rdata")
load("Rdata/Spr.1km.Rdata")
load("Rdata/PPT.1km.Rdata")
load("Rdata/Snow.1km.Rdata")
load("Rdata/Sol.1km.Rdata")
load("Rdata/Temp.1km.Rdata")
load("Rdata/Land.1km.Rdata")
load("Rdata/HerbDist.1km.Rdata")
load("Rdata/Pop.1km.Rdata")
load("Rdata/PortDist.1km.Rdata")
load("Rdata/RailDist.1km.Rdata")
load("Rdata/RivDist.1km.Rdata")
load("Rdata/RoadDist.1km.Rdata")
load("Rdata/SettDist.1km.Rdata")


# ADD NAs TO Distance

Dom.1km<-Snow.1km
tf<-Dom.1km$v%in%c(0); Dom.1km$v[tf]<-NA


tf<-is.na(Dom.1km$v)
Alt.1km$v[tf]<-NA
Calc.1km$v[tf]<-NA
NDVI.1km$v[tf]<-NA
Spr.1km$v[tf]<-NA
PPT.1km$v[tf]<-NA
Snow.1km$v[tf]<-NA
Sol.1km$v[tf]<-NA
Temp.1km$v[tf]<-NA
Land.1km$v[tf]<-NA
HerbDist.1km$v[tf]<-NA
Pop.1km$v[tf]<-NA
PortDist.1km$v[tf]<-NA
RailDist.1km$v[tf]<-NA
RivDist.1km$v[tf]<-NA
RoadDist.1km$v[tf]<-NA
SettDist.1km$v[tf]<-NA
HerbDist.1km$v[tf]<-NA

# FIX ADDITIONAL PROBLEMS WITH DATA

tf<-Sol.1km$v==0; tf[is.na(tf)]<-FALSE; Sol.1km$v[tf]<-NA




# FIX CATEGORICAL VARIABLES 

tf<-Land.1km$v==128; Land.1km$v[tf]<-NA


tf<-Land.1km$v%in%c(22,70); Land.1km$v[tf]<-NA
breaks<-c(11,21,23,30,50,60,80,99)
breaks <- c(breaks - 0.01,max(breaks) + 0.01)
Land.1km <- cut.im(Land.1km, breaks, labels = paste0(c("Bebyggelse","Fulldyrka J.","Innmarksbeite","Skog","Åpen Fastm.","Myr","Vann","Ikke kartl.")))

breaks <- sort(unique(as.vector(Calc.1km$v))) ##remove the NA
breaks <- c(breaks - 0.01,max(breaks) + 0.01)
Calc.1km <- cut.im(Calc.1km, breaks,labels = c("Klasse 1","Klasse 2","Klasse 3"))



scaletomean0<-function(g)
{
  g.sd<-g
  tf<-!is.na(g$v)
  g.sd$v[tf]<-scale(g$v[tf])
  return(g.sd)
}

Alt.1km.s<-     scaletomean0(Alt.1km)
NDVI.1km.s<-    scaletomean0(NDVI.1km)
Spr.1km.s<-     scaletomean0(Spr.1km)
PPT.1km.s<-     scaletomean0(PPT.1km)
Snow.1km.s<-    scaletomean0(Snow.1km)
Sol.1km.s<-     scaletomean0(Sol.1km)
Temp.1km.s<-    scaletomean0(Temp.1km)
HerbDist.1km.s<-scaletomean0(HerbDist.1km)
Pop.1km.s<-     scaletomean0(Pop.1km)
PortDist.1km.s<-scaletomean0(PortDist.1km)
RailDist.1km.s<-scaletomean0(RailDist.1km)
RivDist.1km.s<- scaletomean0(RivDist.1km)
RoadDist.1km.s<-scaletomean0(RoadDist.1km)
SettDist.1km.s<-scaletomean0(SettDist.1km)



#### READ SPECIES DATA


setwd("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/")
load("Rdata_ppp/A.1km.Rdata")
load("Rdata_ppp/A.ai.1km.Rdata")
load("Rdata_ppp/A.b.1km.Rdata")
load("Rdata_ppp/A.f.1km.Rdata")
load("Rdata_ppp/A.l.1km.Rdata")
load("Rdata_ppp/A.kar.1km.Rdata")

load("Rdata_ppp/T.ai.1km.Rdata")
load("Rdata_ppp/T.b.1km.Rdata")
load("Rdata_ppp/T.f.1km.Rdata")
load("Rdata_ppp/T.l.1km.Rdata")
load("Rdata_ppp/T.kar.1km.Rdata")

load("Rdata_ppp/R.ai.1km.Rdata")
load("Rdata_ppp/R.b.1km.Rdata")
load("Rdata_ppp/R.f.1km.Rdata")
load("Rdata_ppp/R.l.1km.Rdata")
load("Rdata_ppp/R.kar.1km.Rdata")



### ADD UNITS

unitname(A) <- c("metre", "metres")
unitname(A.ai) <- c("metre", "metres")
unitname(A.b) <- c("metre", "metres")
unitname(A.f) <- c("metre", "metres")
unitname(A.l) <- c("metre", "metres")
unitname(A.kar) <- c("metre", "metres")

unitname(T.ai) <- c("metre", "metres")
unitname(T.b) <- c("metre", "metres")
unitname(T.f) <- c("metre", "metres")
unitname(T.l) <- c("metre", "metres")
unitname(T.kar) <- c("metre", "metres")

unitname(R.ai) <- c("metre", "metres")
unitname(R.b) <- c("metre", "metres")
unitname(R.f) <- c("metre", "metres")
unitname(R.l) <- c("metre", "metres")
unitname(R.kar) <- c("metre", "metres")

unitname(Alt.1km.s) <- c("metre", "metres")
unitname(Calc.1km) <- c("metre", "metres")
unitname(NDVI.1km.s) <- c("metre", "metres")
unitname(Spr.1km.s) <- c("metre", "metres")
unitname(PPT.1km.s) <- c("metre", "metres")
unitname(Snow.1km.s) <- c("metre", "metres")
unitname(Sol.1km.s) <- c("metre", "metres")
unitname(Temp.1km.s) <- c("metre", "metres")
unitname(Land.1km) <- c("metre", "metres")
unitname(HerbDist.1km.s) <- c("metre", "metres")
unitname(Pop.1km.s) <- c("metre", "metres")
unitname(PortDist.1km.s) <- c("metre", "metres")
unitname(RailDist.1km.s) <- c("metre", "metres")
unitname(RivDist.1km.s) <- c("metre", "metres")
unitname(RoadDist.1km.s) <- c("metre", "metres")
unitname(SettDist.1km.s) <- c("metre", "metres")

### PREPARE SAMPLE INTENSITY TO BE USED AS OFFSET

#density(pppFirstAlienPlants, eps = 1000, sigma = bw.diggle(pppFirstAlienPlants))

#Aint.1km.ppp<-density.ppp(A, eps=1000, sigma = bw.diggle(A))

#Aint.1km<-density(A, eps=1000)

#b1<-Sys.time()
#Aint.1km.cv<-density(A,eps=1000, sigma = bw.diggle(A))
#b2<-Sys.time()
#b2-b1

b1<-Sys.time(); Aint.ai.1km.cv<-density(A.ai,eps=1000, sigma = bw.diggle(A.ai)); b2<-Sys.time(); b2-b1
b1<-Sys.time(); Aint.b.1km.cv<-density(A.b,eps=1000, sigma = bw.diggle(A.b)); b2<-Sys.time(); b2-b1
b1<-Sys.time(); Aint.f.1km.cv<-density(A.f,eps=1000, sigma = bw.diggle(A.f)); b2<-Sys.time(); b2-b1
b1<-Sys.time(); Aint.l.1km.cv<-density(A.l,eps=1000, sigma = bw.diggle(A.l)); b2<-Sys.time(); b2-b1
b1<-Sys.time(); Aint.kar.1km.cv<-density(A.kar,eps=1000, sigma = bw.diggle(A.kar)); b2<-Sys.time(); b2-b1
b1<-Sys.time(); Aint.kar.1km<-density(A.kar,eps=1000); b2<-Sys.time(); b2-b1

#b1<-Sys.time(); Aint.ai.1km.cv<-density(A.ai,eps=1000); b2<-Sys.time(); b2-b1
#b1<-Sys.time(); Aint.b.1km.cv<-density(A.b,eps=1000); b2<-Sys.time(); b2-b1
#b1<-Sys.time(); Aint.f.1km.cv<-density(A.f,eps=1000); b2<-Sys.time(); b2-b1
#b1<-Sys.time(); Aint.l.1km.cv<-density(A.l,eps=1000); b2<-Sys.time(); b2-b1
#b1<-Sys.time(); Aint.kar.1km.cv<-density(A.kar,eps=1000); b2<-Sys.time(); b2-b1


#Aint.1km.log<-log(Aint.1km+0.0000001)
#Aint.ai.1km.cv.log<-log(Aint.ai.1km.cv+0.0000001)
#Aint.b.1km.cv.log<-log(Aint.b.1km.cv+0.0000001)
#Aint.f.1km.cv.log<-log(Aint.f.1km.cv+0.0000001)
#Aint.l.1km.cv.log<-log(Aint.l.1km.cv+0.0000001)
#Aint.kar.1km.cv.log<-log(Aint.kar.1km.cv+0.0000001)



#save(Aint.ai.1km.cv, file="Rdata_sample_intensity/Aint.ai.1km.cv.Rdata")
#save(Aint.b.1km.cv,  file="Rdata_sample_intensity/Aint.b.1km.cv.Rdata")
#save(Aint.f.1km.cv,  file="Rdata_sample_intensity/Aint.f.1km.cv.Rdata")
#save(Aint.l.1km.cv,  file="Rdata_sample_intensity/Aint.l.1km.cv.Rdata")
#save(Aint.kar.1km.cv,file="Rdata_sample_intensity/Aint.kar.1km.cv.Rdata")

#save(Aint.ai.1km.cv.log, file="Rdata_sample_intensity/Aint.ai.1km.cv.log.Rdata")
#save(Aint.b.1km.cv.log,  file="Rdata_sample_intensity/Aint.b.1km.cv.log.Rdata")
#save(Aint.f.1km.cv.log,  file="Rdata_sample_intensity/Aint.f.1km.cv.log.Rdata")
#save(Aint.l.1km.cv.log,  file="Rdata_sample_intensity/Aint.l.1km.cv.log.Rdata")
#save(Aint.kar.1km.cv.log,file="Rdata_sample_intensity/Aint.kar.1km.cv.log.Rdata")

#load("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/Rdata_sample_intensity/Aint.1km.cv.log.Rdata")
#load("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/Rdata_sample_intensity/Aint.1km.log.Rdata")

setwd("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/")

load("Rdata_sample_intensity/Aint.ai.1km.cv.log.Rdata")
load("Rdata_sample_intensity/Aint.b.1km.cv.log.Rdata")
load("Rdata_sample_intensity/Aint.f.1km.cv.log.Rdata")
load("Rdata_sample_intensity/Aint.l.1km.cv.log.Rdata")
load("Rdata_sample_intensity/Aint.kar.1km.cv.log.Rdata")

#Aint.kar.1km<-density(A.kar,eps=1000)
#Aint.kar.1km.log<-log(Aint.kar.1km+0.0000001)


###########
# POINT PROCESS MODELS
###########


library(car)

setwd("C:/GIS/")
NB <- readOGR(dsn = "Basic_data/Boundaries/Norway_terrestrialBoundaries.shp")

#setwd("C:/Users/richard.hedger/OneDrive - NINA/")

SF<-0

###############
# RENAME VARIABLES
###############

Nedbør<-PPT.1km.s
Temperatur<-Temp.1km.s
Snø<-Snow.1km.s
VårensAnkomst<-Spr.1km.s
H.o.h<-Alt.1km.s
Kalkinnhold<-Calc.1km
Solinnstråling<-Sol.1km.s
NDVI<-NDVI.1km.s
AvstandVei<-RoadDist.1km.s
AvstandJernbane<-RailDist.1km.s
AvstandHavn<-PortDist.1km.s
AvstandElv<-RivDist.1km.s	
AvstandBebyggelse<-SettDist.1km.s
Befolkning<-Pop.1km.s
Arealbruk<-Land.1km
#Intensitet<-Aint.1km.cv.log

#####

OutDir<-"C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/Rdata_pred/"


#### FOR THREATENNED AND RESPONSIBLE SPECIES


Name<-c("T.ai","T.b","T.f","T.l","T.kar","R.ai","R.b","R.f","R.l","R.kar")
NameL<-length(Name)

SpecResp<-list()
SpecResp[[1]]<-T.ai
SpecResp[[2]]<-T.b
SpecResp[[3]]<-T.f
SpecResp[[4]]<-T.l
SpecResp[[5]]<-T.kar
SpecResp[[6]]<-R.ai
SpecResp[[7]]<-R.b
SpecResp[[8]]<-R.f
SpecResp[[9]]<-R.l
SpecResp[[10]]<-R.kar
NameNor<-c("Truede: insekter og edderkoppdyr","Truede: moser","Truede: sopp","Truede: lav","Truede: karplanter","Ansvar: insekter og edderkoppdyr","Ansvar: moser","Ansvar: sopp","Ansvar: lav","Ansvar: karplanter")
NameOut<-c("Truede_insekterEdderkoppdyr","Truede_moser","Truede_sopp","Truede_lav","Truede_karplanter","Ansvar_insekterEdderkoppdyr","Ansvar_moser","Ansvar_sopp","Ansvar_lav","Ansvar_karplanter")



SpecInt<-list()
SpecInt[[1]]<-Aint.ai.1km.cv.log
SpecInt[[2]]<-Aint.b.1km.cv.log
SpecInt[[3]]<-Aint.f.1km.cv.log
SpecInt[[4]]<-Aint.l.1km.cv.log
SpecInt[[5]]<-Aint.kar.1km.cv.log
SpecInt[[6]]<-Aint.ai.1km.cv.log
SpecInt[[7]]<-Aint.b.1km.cv.log
SpecInt[[8]]<-Aint.f.1km.cv.log
SpecInt[[9]]<-Aint.l.1km.cv.log
SpecInt[[10]]<-Aint.kar.1km.cv.log

g.TrRe.R2<-go.TrRe.R2<-rep(NA,10)


#setwd("C:/Users/richard.hedger/OneDrive - NINA/NINA projects/15801000_geografisk_utbredelse_av_trua_arter_i_norge")

#LETTER<-c("B","B","B","B","B","C","C","C","C","C")
for (i in 1:10)
{
  print(i)
  
Resp<-SpecResp[[i]]
Intensitet<-SpecInt[[i]]

g <- ppm(Resp ~ Nedbør + Temperatur + Snø + VårensAnkomst +
           H.o.h + Kalkinnhold + Solinnstråling + NDVI + 
           AvstandVei + AvstandJernbane + AvstandHavn + AvstandElv + AvstandBebyggelse + 
           Befolkning + Arealbruk,
         Poisson(), gcontrol = list(maxit = 10000))

go <- ppm(Resp ~ Nedbør + Temperatur + Snø + VårensAnkomst +
            H.o.h + Kalkinnhold + Solinnstråling + NDVI + 
            AvstandVei + AvstandJernbane + AvstandHavn + AvstandElv + AvstandBebyggelse + 
            Befolkning + Arealbruk +
            offset(Intensitet),
          Poisson(), gcontrol = list(maxit = 10000))

print(Name[i])



if(Name[i]=="T.ai"){
  g <- update(g,.~.-Temperatur -Arealbruk)
  go <- update(go,.~.-Temperatur -Arealbruk)
}

if(Name[i]=="T.b"){
  g <- update(g,.~.-Arealbruk -Temperatur)
  go <- update(go,.~.-Arealbruk -Temperatur)
}

if(Name[i]=="T.f"){
  g <- update(g,.~.-Temperatur -Arealbruk)
  go <- update(go,.~.-Temperatur -Arealbruk)
  g <- update(g,.~.-AvstandElv)
  go <- update(go,.~.-AvstandElv)
}

if(Name[i]=="T.l"){
  g <- update(g,.~.-Temperatur -Arealbruk)
  go <- update(go,.~.-Temperatur -Arealbruk)
}

if(Name[i]=="T.kar"){
  g <- update(g,.~.-Temperatur -Arealbruk)
  go <- update(go,.~.-Temperatur -Arealbruk)
}

if(Name[i]=="R.ai"){
  g <- update(g,.~. -Arealbruk -Temperatur)
  go <- update(go,.~. -Arealbruk -Temperatur)
  g <- update(g,.~.-VårensAnkomst)
  go <- update(go,.~.-VårensAnkomst)
  }

if(Name[i]=="R.b"){
  g <- update(g,.~.-Temperatur -Arealbruk)
  go <- update(go,.~.-Temperatur -Arealbruk)
  g <- update(g,.~.-AvstandBebyggelse -Solinnstråling -Befolkning)
  go <- update(go,.~.-AvstandBebyggelse -Solinnstråling -Befolkning)
  }

if(Name[i]=="R.f"){
  g <- update(g,.~.-Temperatur -Arealbruk)
  go <- update(go,.~.-Temperatur -Arealbruk)
  g <- update(g,.~.-Solinnstråling -VårensAnkomst -AvstandHavn)
  go <- update(go,.~.-Solinnstråling -VårensAnkomst -AvstandHavn)
}

if(Name[i]=="R.f"){
  g <- update(g,.~.-Temperatur)
  go <- update(go,.~.-Temperatur)
  g <- update(g,.~.-Arealbruk -Nedbør)
  go <- update(go,.~.-Arealbruk -Nedbør)
}

if(Name[i]=="R.kar"){
  g <- update(g,.~.-Temperatur -Arealbruk -Snø)
  go <- update(go,.~.-Temperatur -Arealbruk -Snø)
  g <- update(g,.~.-AvstandVei)
  go <- update(go,.~.-AvstandVei)
}

g.Coefs <- summary(g)$coefs.SE.CI; g.Coefs <- g.Coefs[-1,]
go.Coefs <- summary(go)$coefs.SE.CI; go.Coefs <- go.Coefs[-1,]

go.pred<-predict(go, type = "cif", locations = NorWin.1km)
save(go.pred, file=paste0("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/Rdata_PSIM/",Name[i],".pred.1km.Rdata"))
go.pred.rast<-raster(go.pred)

tmp<-"C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/PSIM/"
writeRaster(go.pred.rast*1E06, filename=file.path(tmp, paste0(Name[i],".PSIM.tif")), format="GTiff", overwrite=TRUE)

g.TrRe.R2[i]<-pseudoR2(g)
go.TrRe.R2[i]<-pseudoR2(go)
myPseudoR2(go)
}

TrRe.R2<-data.frame(Name,round(g.TrRe.R2,3),round(go.TrRe.R2,3))
colnames(TrRe.R2)<-c("SpeciesGroup","NoOffset","Offset")
#write.csv(file="C:/Users/richard.hedger/OneDrive - NINA/NINA projects/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Results/TrRe.R2.csv",TrRe.R2,row.names=FALSE)


####################### FOR ALL (GENERAL SPECIES) DATA

Name<-c("A.ai","A.b","A.f","A.l","A.kar")
NameNor<-c("Alle: insekter og edderkoppdyr","Alle: moser","Alle: sopp","Alle: lav","Alle: karplanter")
NameOut<-c("Alle_insekterEdderkoppdyr","Alle_moser","Alle_sopp","Alle_lav","Alle_karplanter")

NameL<-length(Name)

SpecResp<-list()
SpecResp[[1]]<-A.ai
SpecResp[[2]]<-A.b
SpecResp[[3]]<-A.f
SpecResp[[4]]<-A.l
SpecResp[[5]]<-A.kar

g.All.R2<-rep(NA,5)

#setwd("C:/Users/richard.hedger/OneDrive - NINA/NINA projects/15801000_geografisk_utbredelse_av_trua_arter_i_norge")

for (i in 1:5)
{
  print(i)
  
  Resp<-SpecResp[[i]]

  g <- ppm(Resp ~ Nedbør + Temperatur + Snø + VårensAnkomst +
             H.o.h + Kalkinnhold + Solinnstråling + NDVI + 
             AvstandVei + AvstandJernbane + AvstandHavn + AvstandElv + AvstandBebyggelse + 
             Befolkning + Arealbruk,
           Poisson(), gcontrol = list(maxit = 10000))
 
  print(Name[i])
  
   
  if(Name[i]=="A.ai"){
    g <- update(g,.~.-Temperatur -Arealbruk)
  }

  if(Name[i]=="A.b"){
    g <- update(g,.~.-Temperatur -Arealbruk)
  }
  
  if(Name[i]=="A.f"){
    g <- update(g,.~.-Temperatur)
  }
  
  if(Name[i]=="A.f"){
    g <- update(g,.~.-Temperatur -Arealbruk)
    g <- update(g,.~.-Snø)
  } 
  
  if(Name[i]=="A.kar"){
    g <- update(g,.~.-Temperatur -Arealbruk)
    g <- update(g,.~.-Snø)
  } 
  
  g.Coefs <- summary(g)$coefs.SE.CI; g.Coefs <- g.Coefs[-1,]

  g.pred<-predict(g, type = "cif", locations = NorWin.1km)
  save(g.pred, file=paste0("C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/Rdata_PSIM/",Name[i],".pred.1km.Rdata"))
  g.pred.rast<-raster(g.pred)
  
  tmp<-"C:/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Data/PSIM/"
  writeRaster(g.pred.rast*1E06, filename=file.path(tmp, paste0(Name[i],".PSIM.tif")), format="GTiff", overwrite=TRUE)

  g.All.R2[i]<-pseudoR2(g)
  
  
}

All.R2<-data.frame(Name,round(g.All.R2,3))
colnames(All.R2)<-c("SpeciesGroup","NoOffset")
#write.csv(file="C:/Users/richard.hedger/OneDrive - NINA/NINA projects/15801000_geografisk_utbredelse_av_trua_arter_i_norge/Results/All.R2.csv",All.R2,row.names=FALSE)





