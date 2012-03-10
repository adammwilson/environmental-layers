#Correct last row -------------------------------------------------------------------------------------------
library (raster)

path<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/ByDate"

#Get h08v04 tiles:
h08v04_infiles <- list.files(path=path, pattern="*.h08v04.005.ClearDay.img$")
for (i in 1: length(h08v04_infiles)){
  h08v04_infiles[i]<- paste (path,"/",h08v04_infiles[i],sep="")
}
h08v04_infiles

#raster(h09v04_infiles[311])[1200,1]
#raster(h09v04_infiles[311])[1200,2]

rastfunction<- function (h08v04_infiles){
    r<-raster(h08v04_infiles)
    val<-raster(h08v04_infiles)[1199,]
    r[1200,]<-val
    name1<-sub("_Extracts/ByDate","_Extracts/ByDate/BadPixelsCorrected_Tiles",h08v04_infiles)
    name<-sub(".ClearDay.img",".ClearDay_corrected.img",name1)
    writeRaster(r,filename=name,format="HFA")
}

for (i in 1:length (h08v04_infiles)){
  rastfunction(h08v04_infiles[i])
}