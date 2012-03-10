library (raster)

MA_path<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles"
img_files<-list.files(path=MA_path, pattern="*.img$")

#Create empty vectors to hold correct tiles to be mosaiced
JanTiles_MonthlyAvgs<- c(1:length(img_files))*NA
FebTiles_MonthlyAvgs<- c(1:length(img_files))*NA
MarTiles_MonthlyAvgs<- c(1:length(img_files))*NA
AprTiles_MonthlyAvgs<- c(1:length(img_files))*NA
MayTiles_MonthlyAvgs<- c(1:length(img_files))*NA
JunTiles_MonthlyAvgs<- c(1:length(img_files))*NA
JulTiles_MonthlyAvgs<- c(1:length(img_files))*NA
AugTiles_MonthlyAvgs<- c(1:length(img_files))*NA
SepTiles_MonthlyAvgs<- c(1:length(img_files))*NA
OctTiles_MonthlyAvgs<- c(1:length(img_files))*NA
NovTiles_MonthlyAvgs<- c(1:length(img_files))*NA
DecTiles_MonthlyAvgs<- c(1:length(img_files))*NA

#Separate pairs of tiles (h08v04 and h09v04) into vectors
for (i in 1:length(img_files)){
  if (length(grep("^Jan.h.",img_files[i]) !=0)){
      img_files[i]-> JanTiles_MonthlyAvgs[i]
  }else if (length(grep("^Feb.h.",img_files[i]) !=0)){
      img_files[i]-> FebTiles_MonthlyAvgs[i]
  }else if (length(grep("^Mar.h.",img_files[i]) !=0)){
      img_files[i]-> MarTiles_MonthlyAvgs[i]
  }else if (length(grep("^Apr.h.",img_files[i]) !=0)){
      img_files[i]-> AprTiles_MonthlyAvgs[i]
  }else if (length(grep("^May.h.",img_files[i]) !=0)){
      img_files[i]-> MayTiles_MonthlyAvgs[i]
  }else if (length(grep("^Jun.h.",img_files[i]) !=0)){
      img_files[i]-> JunTiles_MonthlyAvgs[i]
  }else if (length(grep("^Jul.h.",img_files[i]) !=0)){
      img_files[i]-> JulTiles_MonthlyAvgs[i]
  }else if (length(grep("^Aug.h.",img_files[i]) !=0)){
      img_files[i]-> AugTiles_MonthlyAvgs[i]
  }else if (length(grep("^Sep.h.",img_files[i]) !=0)){
      img_files[i]-> SepTiles_MonthlyAvgs[i]
  }else if (length(grep("^Oct.h.",img_files[i]) !=0)){
      img_files[i]-> OctTiles_MonthlyAvgs[i]
  }else if (length(grep("^Nov.h.",img_files[i]) !=0)){
      img_files[i]-> NovTiles_MonthlyAvgs[i]
  }else if (length(grep("^Dec.h.",img_files[i]) !=0)){
      img_files[i]-> DecTiles_MonthlyAvgs[i]
  }
}

#Get rid of NA's and add full path name to each file
JanTiles_MonthlyAvgs<-JanTiles_MonthlyAvgs[!is.na(JanTiles_MonthlyAvgs)]
JanTiles_MonthlyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/",JanTiles_MonthlyAvgs,sep="")
FebTiles_MonthlyAvgs<-FebTiles_MonthlyAvgs[!is.na(FebTiles_MonthlyAvgs)]
FebTiles_MonthlyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/",FebTiles_MonthlyAvgs,sep="")
MarTiles_MonthlyAvgs<-MarTiles_MonthlyAvgs[!is.na(MarTiles_MonthlyAvgs)]
MarTiles_MonthlyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/",MarTiles_MonthlyAvgs,sep="")
AprTiles_MonthlyAvgs<-AprTiles_MonthlyAvgs[!is.na(AprTiles_MonthlyAvgs)]
AprTiles_MonthlyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/",AprTiles_MonthlyAvgs,sep="")
MayTiles_MonthlyAvgs<-MayTiles_MonthlyAvgs[!is.na(MayTiles_MonthlyAvgs)]
MayTiles_MonthlyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/",MayTiles_MonthlyAvgs,sep="")
JunTiles_MonthlyAvgs<-JunTiles_MonthlyAvgs[!is.na(JunTiles_MonthlyAvgs)]
JunTiles_MonthlyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/",JunTiles_MonthlyAvgs,sep="")
JulTiles_MonthlyAvgs<-JulTiles_MonthlyAvgs[!is.na(JulTiles_MonthlyAvgs)]
JulTiles_MonthlyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/",JulTiles_MonthlyAvgs,sep="")
AugTiles_MonthlyAvgs<-AugTiles_MonthlyAvgs[!is.na(AugTiles_MonthlyAvgs)]
AugTiles_MonthlyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/",AugTiles_MonthlyAvgs,sep="")
SepTiles_MonthlyAvgs<-SepTiles_MonthlyAvgs[!is.na(SepTiles_MonthlyAvgs)]
SepTiles_MonthlyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/",SepTiles_MonthlyAvgs,sep="")
OctTiles_MonthlyAvgs<-OctTiles_MonthlyAvgs[!is.na(OctTiles_MonthlyAvgs)]
OctTiles_MonthlyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/",OctTiles_MonthlyAvgs,sep="")
NovTiles_MonthlyAvgs<-NovTiles_MonthlyAvgs[!is.na(NovTiles_MonthlyAvgs)]
NovTiles_MonthlyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/",NovTiles_MonthlyAvgs,sep="")
DecTiles_MonthlyAvgs<-DecTiles_MonthlyAvgs[!is.na(DecTiles_MonthlyAvgs)]
DecTiles_MonthlyAvgs<- paste("/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/",DecTiles_MonthlyAvgs,sep="")

#Create mosaics
Jan_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/JanMosaic_MonthlyAvg_Scaled.img"
mosaic(raster(JanTiles_MonthlyAvgs[1]),raster(JanTiles_MonthlyAvgs[2]), fun=mean,na.rm=TRUE, filename=Jan_name, datatype="HFA")
Feb_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/FebMosaic_MonthlyAvg_Scaled.img"
mosaic(raster(FebTiles_MonthlyAvgs[1]),raster(FebTiles_MonthlyAvgs[2]), fun=mean,na.rm=TRUE, filename=Feb_name, datatype="HFA")
Mar_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/MarMosaic_MonthlyAvg_Scaled.img"
mosaic(raster(MarTiles_MonthlyAvgs[1]),raster(MarTiles_MonthlyAvgs[2]), fun=mean,na.rm=TRUE, filename=Mar_name, datatype="HFA")
Apr_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/AprMosaic_MonthlyAvg_Scaled.img"
mosaic(raster(AprTiles_MonthlyAvgs[1]),raster(AprTiles_MonthlyAvgs[2]), fun=mean,na.rm=TRUE, filename=Apr_name, datatype="HFA")
May_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/MayMosaic_MonthlyAvg_Scaled.img"
mosaic(raster(MayTiles_MonthlyAvgs[1]),raster(MayTiles_MonthlyAvgs[2]), fun=mean,na.rm=TRUE, filename=May_name, datatype="HFA")
Jun_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/JunMosaic_MonthlyAvg_Scaled.img"
mosaic(raster(JunTiles_MonthlyAvgs[1]),raster(JunTiles_MonthlyAvgs[2]), fun=mean,na.rm=TRUE, filename=Jun_name, datatype="HFA")
Jul_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/JulMosaic_MonthlyAvg_Scaled.img"
mosaic(raster(JulTiles_MonthlyAvgs[1]),raster(JulTiles_MonthlyAvgs[2]), fun=mean,na.rm=TRUE, filename=Jul_name, datatype="HFA")
Aug_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/AugMosaic_MonthlyAvg_Scaled.img"
mosaic(raster(AugTiles_MonthlyAvgs[1]),raster(AugTiles_MonthlyAvgs[2]), fun=mean,na.rm=TRUE, filename=Aug_name, datatype="HFA")
Sep_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/SepMosaic_MonthlyAvg_Scaled.img"
mosaic(raster(SepTiles_MonthlyAvgs[1]),raster(SepTiles_MonthlyAvgs[2]), fun=mean,na.rm=TRUE, filename=Sep_name, datatype="HFA")
Oct_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/OctMosaic_MonthlyAvg_Scaled.img"
mosaic(raster(OctTiles_MonthlyAvgs[1]),raster(OctTiles_MonthlyAvgs[2]), fun=mean,na.rm=TRUE, filename=Oct_name, datatype="HFA")
Nov_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/NovMosaic_MonthlyAvg_Scaled.img"
mosaic(raster(NovTiles_MonthlyAvgs[1]),raster(NovTiles_MonthlyAvgs[2]), fun=mean,na.rm=TRUE, filename=Nov_name, datatype="HFA")
Dec_name<-"/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/DecMosaic_MonthlyAvg_Scaled.img"
mosaic(raster(DecTiles_MonthlyAvgs[1]),raster(DecTiles_MonthlyAvgs[2]), fun=mean,na.rm=TRUE, filename=Dec_name, datatype="HFA")