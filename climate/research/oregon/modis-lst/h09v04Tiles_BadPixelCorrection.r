#Correct 1st column-------------------------------------------------------------------------------------------------
library (raster)

path<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/ByDate"

#Get h09v04 tiles:
h09v04_infiles <- list.files(path=path, pattern="*.h09v04.005.ClearDay.img$")
for (i in 1: length(h09v04_infiles)){
  h09v04_infiles[i]<- paste (path,"/",h09v04_infiles[i],sep="")
}
h09v04_infiles

#raster(h09v04_infiles[311])[1200,1]
#raster(h09v04_infiles[311])[1200,2]

rastfunction<- function (h09v04_infiles){
    r<-raster(h09v04_infiles)
    val<-raster(h09v04_infiles)[,2]
    r[,1]<-val
    name1<-sub("_Extracts/ByDate","_Extracts/ByDate/BadPixelsCorrected_Tiles",h09v04_infiles)
    name<-sub(".ClearDay.img",".ClearDay_corrected.img",name1)
    writeRaster(r,filename=name,format="HFA")
}

for (i in 1:length (h09v04_infiles)){
  rastfunction(h09v04_infiles[i])
}

#Correct last row, overwrite corrected files-------------------------------------------------------------------------------------------
path1<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/ByDate/BadPixelsCorrected_Tiles"

#Get h09v04 tiles:
newh09v04_infiles <- list.files(path=path1, pattern="*.h09v04.005.ClearDay_corrected.img$")
for (i in 1: length(newh09v04_infiles)){
  newh09v04_infiles[i]<- paste (path1,"/",newh09v04_infiles[i],sep="")
}
newh09v04_infiles

#raster(h09v04_infiles[311])[1200,1]
#raster(h09v04_infiles[311])[1200,2]

newrastfunction<- function (newh09v04_infiles){
    r<-raster(newh09v04_infiles)
    val<-raster(newh09v04_infiles)[1199,]
    r[1200,]<-val
    writeRaster(r,filename=newh09v04_infiles,format="HFA",overwrite=TRUE)
}

for (i in 1:length (newh09v04_infiles)){
  newrastfunction(newh09v04_infiles[i])
}
