library(raster)

#Get list of tile names and add full pathname to beginning
Tiles <- list.files("DEM/asterGdem/N59to81_W20toE19", pattern="^ASTGTM.*_dem.tif$")
Tiles<- paste("DEM/asterGdem/N59to81_W20toE19/", Tiles, sep="")

# datavalues: 0= water, -9999= nodata

#Create new vectors for name of tile and accompanying % of cells= ocean and non-landmass
#   IF % OCEAN AND % NON-LAND ARE VERY DIFFERENT USE % NON-LAND FOR QC CHECK, IF THEY
#   ARE THE SAME, % OCEAN IS FASTER TO CALCULATE SO USE THIS

l<- length (Tiles)
Names<- c(1:l)*NA
PctNotLand<- c(1:l) * NA
PctOcean<- c(1:l)*NA

#Fill vectors with % of each tiles where pixel value  = 0 (water) OR 0 & -9999 (water or nodata)
for (i in 1:l){
    Names[i]<-sapply(strsplit(Tiles[i], '[/]'), '[[', 4)
    PctOcean[i]<- count(raster(Tiles[i]),0)/ncell(raster(Tiles[i]))
    PctNotLand[i]<- (count(raster(Tiles[i]),0)+count(raster(Tiles[i]),-9999))/ncell(raster(Tiles[i]))
}

data.frame (Names, PctOcean,PctNotLand)

#Use the above calculation to calculate % landmass for each tile and compare total over all tiles
#  in folder to % landmass of USGS coverage for same area.

# Requires aggregating aster tiles to match pixel size of USGS coverage:

LandAgg<- c(1:l) * NA
Counts_Agg<- c(1:l)*NA
for (i in 1:l){
  Counts_Agg[i]<- count(aggregate(raster(Tiles[i]),30),0)+count(aggregate(raster(Tiles[i]),30),-9999)
  LandAgg[i]<-  ncell(aggregate(raster(Tiles[i]),30))-Counts_Agg[i]
}

a<-sum(LandAgg)



#Calculate % landmass in USGS coverage and compare:

AstRast<- raster("DEM/asterGdem/N59to81_W20toE19/w020n90/W020N90_clipped.dem")
b<-ncell(AstRast)-cellStats(AstRast, "countNA") #Total # cells- cells with no value
1-(b/a)  #Difference= 4.49%
#------------------------------------------------------------------
#after first try, anytime PctNotLand was >= 99%, so too was PctOcean and visa versa
#    All subsequent runs will only calc. PctOcean to save time

Tiles <- list.files("DEM/asterGdem/N59to81_E20to59", pattern="^ASTGTM.*_dem.tif$")
Tiles<- paste("DEM/asterGdem/N59to81_E20to59/", Tiles, sep="")

l<- length (Tiles)
Names<- c(1:l)*NA
PctNotLand<- c(1:l) * NA
PctOcean<- c(1:l)*NA
for (i in 1:l){
    Names[i]<-sapply(strsplit(Tiles[i], '[/]'), '[[', 4)
    PctOcean[i]<- count(raster(Tiles[i]),0)/ncell(raster(Tiles[i]))
}

data.frame (Names, PctOcean)

LandAgg<- c(1:l) * NA
Counts_Agg<- c(1:l)*NA
for (i in 1:l){
  Counts_Agg[i]<- count(aggregate(raster(Tiles[i]),30),0)+count(aggregate(raster(Tiles[i]),30),-9999)
  LandAgg[i]<-  ncell(aggregate(raster(Tiles[i]),30))-Counts_Agg[i]
    print( paste(round(100*i/l),"%",sep=""), quote=FALSE )
}

a<-sum(LandAgg)
AstRast<- raster("DEM/asterGdem/N59to81_E20to59/e020n90/E020N90_clipped.dem")
b<-ncell(AstRast)-cellStats(AstRast, "countNA") #Total # cells- cells with no value
1-(b/a) #Difference= 2.21%
#-----------------------------------------------------------------------
Tiles <- list.files("DEM/asterGdem/N59to81_E60to99", pattern="^ASTGTM.*_dem.tif$")
Tiles<- paste("DEM/asterGdem/N59to81_E60to99/", Tiles, sep="")

l<- length (Tiles)
Names<- c(1:l)*NA
PctNotLand<- c(1:l) * NA
PctOcean<- c(1:l)*NA
for (i in 1:l){
    Names[i]<-sapply(strsplit(Tiles[i], '[/]'), '[[', 4)
    PctOcean[i]<- count(raster(Tiles[i]),0)/ncell(raster(Tiles[i]))
}

data.frame (Names, PctOcean)

LandAgg<- c(1:l) * NA
Counts_Agg<- c(1:l)*NA
for (i in 1:l){
  Counts_Agg[i]<- count(aggregate(raster(Tiles[i]),30),0)+count(aggregate(raster(Tiles[i]),30),-9999)
  LandAgg[i]<-  ncell(aggregate(raster(Tiles[i]),30))-Counts_Agg[i]
}

a<-sum(LandAgg)
AstRast<- raster("DEM/asterGdem/N59to81_E60to99/USGS_ErosDEM_N59to81E60to99/E060N90_clipped.dem")
b<-ncell(AstRast)-cellStats(AstRast, "countNA") #Total # cells- cells with no value
1-(b/a)  # Datasets differ by 2.27%

cellStats(AstRast,max)
cellStats(AstRast,min)
count(AstRast, NA)
#-----------------------------------------------------------------------
Tiles <- list.files("DEM/asterGdem/N59to81_E100to139", pattern="^ASTGTM.*_dem.tif$")
Tiles<- paste("DEM/asterGdem/N59to81_E100to139/", Tiles, sep="")

l<- length (Tiles)
Names<- c(1:l)*NA
PctNotLand<- c(1:l) * NA
PctOcean<- c(1:l)*NA
for (i in 1:l){
    Names[i]<-sapply(strsplit(Tiles[i], '[/]'), '[[', 4)
    PctOcean[i]<- count(raster(Tiles[i]),0)/ncell(raster(Tiles[i]))
}

data.frame (Names, PctOcean)

LandAgg<- c(1:l) * NA
Counts_Agg<- c(1:l)*NA
for (i in 1:l){
  Counts_Agg[i]<- count(aggregate(raster(Tiles[i]),30),0)+count(aggregate(raster(Tiles[i]),30),-9999)
  LandAgg[i]<-  ncell(aggregate(raster(Tiles[i]),30))-Counts_Agg[i]
    print( paste(round(100*i/l),"%",sep=""), quote=FALSE )
}

a<-sum(LandAgg)
AstRast<- raster("DEM/asterGdem/N59to81_E100to139/e100n90/E100N90_clipped.dem")
b<-ncell(AstRast)-cellStats(AstRast, "countNA") #Total # cells- cells with no value
1-(b/a)  #Difference= 3.1%
#-----------------------------------------------------------------------
Tiles <- list.files("DEM/asterGdem/N59to81_E140to179", pattern="^ASTGTM.*_dem.tif$")
Tiles<- paste("DEM/asterGdem/N59to81_E140to179/", Tiles, sep="")

l<- length (Tiles)
Names<- c(1:l)*NA
PctNotLand<- c(1:l) * NA
PctOcean<- c(1:l)*NA
for (i in 1:l){
    Names[i]<-sapply(strsplit(Tiles[i], '[/]'), '[[', 4)
    PctOcean[i]<- count(raster(Tiles[i]),0)/ncell(raster(Tiles[i]))
}

data.frame (Names, PctOcean)

LandAgg<- c(1:l) * NA
Counts_Agg<- c(1:l)*NA
for (i in 1:l){
  Counts_Agg[i]<- count(aggregate(raster(Tiles[i]),30),0)+count(aggregate(raster(Tiles[i]),30),-9999)
  LandAgg[i]<-  ncell(aggregate(raster(Tiles[i]),30))-Counts_Agg[i]
    print( paste(round(100*i/l),"%",sep=""), quote=FALSE )
}

a<-sum(LandAgg)
AstRast<- raster("DEM/asterGdem/N59to81_E140to179/e140n90/E140N90_clipped.dem")
b<-ncell(AstRast)-cellStats(AstRast, "countNA") #Total # cells- cells with no value
1-(b/a)   #Difference= 3.4%
#-----------------------------------------------------------------------
Tiles <- list.files("DEM/asterGdem/N59to81_W60to21", pattern="^ASTGTM.*_dem.tif$")
Tiles<- paste("DEM/asterGdem/N59to81_W60to21/", Tiles, sep="")

l<- length (Tiles)
Names<- c(1:l)*NA
PctNotLand<- c(1:l) * NA
PctOcean<- c(1:l)*NA
for (i in 1:l){
    Names[i]<-sapply(strsplit(Tiles[i], '[/]'), '[[', 4)
    PctOcean[i]<- count(raster(Tiles[i]),0)/ncell(raster(Tiles[i]))
}

data.frame (Names, PctOcean)

LandAgg<- c(1:l) * NA
Counts_Agg<- c(1:l)*NA
for (i in 1:l){
  Counts_Agg[i]<- count(aggregate(raster(Tiles[i]),30),0)+count(aggregate(raster(Tiles[i]),30),-9999)
  LandAgg[i]<-  ncell(aggregate(raster(Tiles[i]),30))-Counts_Agg[i]
    print( paste(round(100*i/l),"%",sep=""), quote=FALSE )
}

a<-sum(LandAgg)
AstRast<- raster("DEM/asterGdem/N59to81_W60to21/w060n90/W060N90_clipped.dem")
b<-ncell(AstRast)-cellStats(AstRast, "countNA") #Total # cells- cells with no value
1-(b/a)  #Difference= 3.03%
#-----------------------------------------------------------------------
Tiles <- list.files("DEM/asterGdem/N59to81_W180to141", pattern="^ASTGTM.*_dem.tif$")
Tiles<- paste("DEM/asterGdem/N59to81_W180to141/", Tiles, sep="")

l<- length (Tiles)
Names<- c(1:l)*NA
PctNotLand<- c(1:l) * NA
PctOcean<- c(1:l)*NA
for (i in 1:l){
    Names[i]<-sapply(strsplit(Tiles[i], '[/]'), '[[', 4)
    PctOcean[i]<- count(raster(Tiles[i]),0)/ncell(raster(Tiles[i]))
}

data.frame (Names, PctOcean)

LandAgg<- c(1:l) * NA
Counts_Agg<- c(1:l)*NA
for (i in 1:l){
  Counts_Agg[i]<- count(aggregate(raster(Tiles[i]),30),0)+count(aggregate(raster(Tiles[i]),30),-9999)
  LandAgg[i]<-  ncell(aggregate(raster(Tiles[i]),30))-Counts_Agg[i]
     print( paste(round(100*i/l),"%",sep=""), quote=FALSE )
}

a<-sum(LandAgg)
AstRast<- raster("DEM/asterGdem/N59to81_W180to141/w180n90/W180N90_clipped.dem")
b<-ncell(AstRast)-cellStats(AstRast, "countNA") #Total # cells- cells with no value
1-(b/a) #Difference= 2.87%
#-----------------------------------------------------------------------
Tiles <- list.files("DEM/asterGdem/N59to81_W140to101", pattern="^ASTGTM.*_dem.tif$")
Tiles<- paste("DEM/asterGdem/N59to81_W140to101/", Tiles, sep="")

l<- length (Tiles)
Names<- c(1:l)*NA
PctNotLand<- c(1:l) * NA
PctOcean<- c(1:l)*NA
for (i in 1:l){
    Names[i]<-sapply(strsplit(Tiles[i], '[/]'), '[[', 4)
    PctOcean[i]<- count(raster(Tiles[i]),0)/ncell(raster(Tiles[i]))
}

data.frame (Names, PctOcean)

LandAgg<- c(1:l) * NA
Counts_Agg<- c(1:l)*NA
for (i in 1:l){
  Counts_Agg[i]<- count(aggregate(raster(Tiles[i]),30),0)+count(aggregate(raster(Tiles[i]),30),-9999)
  LandAgg[i]<-  ncell(aggregate(raster(Tiles[i]),30))-Counts_Agg[i]
    print( paste(round(100*i/l),"%",sep=""), quote=FALSE )
}

a<-sum(LandAgg)
AstRast<- raster("DEM/asterGdem/N59to81_W140to101/w140n90/W140N90_clipped.dem")
b<-ncell(AstRast)-cellStats(AstRast, "countNA") #Total # cells- cells with no value
1-(b/a) #Difference= 3.84%
#-----------------------------------------------------------------------
Tiles <- list.files("DEM/asterGdem/N59to81_W100to61", pattern="^ASTGTM.*_dem.tif$")
Tiles<- paste("DEM/asterGdem/N59to81_W100to61/", Tiles, sep="")

l<- length (Tiles)
Names<- c(1:l)*NA
PctNotLand<- c(1:l) * NA
PctOcean<- c(1:l)*NA
for (i in 1:l){
    Names[i]<-sapply(strsplit(Tiles[i], '[/]'), '[[', 4)
    PctOcean[i]<- count(raster(Tiles[i]),0)/ncell(raster(Tiles[i]))
}

data.frame (Names, PctOcean)

LandAgg<- c(1:l) * NA
Counts_Agg<- c(1:l)*NA
for (i in 1:l){
  Counts_Agg[i]<- count(aggregate(raster(Tiles[i]),30),0)+count(aggregate(raster(Tiles[i]),30),-9999)
  LandAgg[i]<-  ncell(aggregate(raster(Tiles[i]),30))-Counts_Agg[i]
    print( paste(round(100*i/l),"%",sep=""), quote=FALSE )
}

a<-sum(LandAgg)
AstRast<- raster("DEM/asterGdem/N59to81_W100to61/w100n90/W100N90_clipped.dem")
b<-ncell(AstRast)-cellStats(AstRast, "countNA") #Total # cells- cells with no value
1-(b/a) #Difference= 7.21% (lot's of coastline in this set of tiles!)
#-------------------------------------------------------------------
c<- raster("DEM/asterGdem/N59to81_E60to99/USGS_ErosDEM_N59to81E60to99/E060N90.DEM")
a<- raster("DEM/asterGdem/N59to81_E60to99/USGS_ErosDEM_N59to81E60to99/E060N90_clipped.dem")
b<- raster(nrow=82800, ncol=144000)
d<- raster("DEM/asterGdem/N59to81_E60to99/ASTGTM_N59E060_dem.tif")
s<- resample(a,b,method='ngb')
disaggregate(a,30)
e<-aggregate(d,30)

count(e, -9999)
count (e,0)
