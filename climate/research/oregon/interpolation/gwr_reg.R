####################GWR of Tmax for 10 dates.#####################
#This script generate station values for the Oregon case study. This program loads the station data from a shp file 
#and performs a GWR regression. 
#Script created by Benoit Parmentier on March 13, 2012. 

###Loading r library and packages
library(sp)
library(spdep)
library(rgdal)
library(spgwr)
library(gpclib)
library(PBSmapping)
library(maptools)
library(gstat)
###Parameters and arguments

infile1<-"ghcn_or_tmax_b_03032012_OR83M.shp" 
path<- "/data/computer/parmentier/Data/IPLANT_project/data_Oregon_stations/"
setwd(path)
#infile2<-"dates_interpolation_03012012.txt"  # list of 10 dates for the regression
infile2<-"dates_interpolation_03052012.txt"
prop<-0.3
out_prefix<-"_03272012_Res_fit"

###Reading the shapefile and raster image from the local directory

mean_LST<- readGDAL("mean_day244_rescaled.rst")  #This reads the whole raster in memory and provide a grid for kriging
ghcn<-readOGR(".", "ghcn_or_tmax_b_03032012_OR83M") 
proj4string(ghcn) #This retrieves the coordinate system for the SDF
CRS_ghcn<-proj4string(ghcn) #this can be assigned to mean_LST!!!
proj4string(mean_LST)<-CRS_ghcn #Assigning coordinates information

# Creating state outline from county

orcnty<-readOGR(".", "orcnty24_OR83M")
proj4string(orcnty) #This retrieves the coordinate system for the SDF
lps <-getSpPPolygonsLabptSlots(orcnty)  #Getting centroids county labels
IDOneBin <- cut(lps[,1], range(lps[,1]), include.lowest=TRUE)  #Creating one bin var
gpclibPermit() #Set the gpclib to True to allow union
OR_state <- unionSpatialPolygons(orcnty ,IDOneBin) #Dissolve based on bin var

# Adding variables for the regression

ghcn$Northness<- cos(ghcn$ASPECT) #Adding a variable to the dataframe
ghcn$Eastness <- sin(ghcn$ASPECT)  #adding variable to the dataframe.

ghcn$Northness_w <- sin(ghcn$slope)*cos(ghcn$ASPECT) #Adding a variable to the dataframe
ghcn$Eastness_w  <- sin(ghcn$slope)*sin(ghcn$ASPECT)  #adding variable to the dataframe.

set.seed(100)

dates <-readLines(paste(path,"/",infile2, sep=""))

results <- matrix(1,length(dates),3)            #This is a matrix containing the diagnostic measures from the GAM models.

#Screening for bad values
#tmax range: min max)
ghcn_test<-subset(ghcn,ghcn$tmax>-150 & ghcn$tmax<400)
ghcn_test2<-subset(ghcn_test,ghcn_test$ELEV_SRTM>0)
ghcn<-ghcn_test2

#ghcn.subsets <-lapply(dates, function(d) subset(ghcn, date==as.numeric(d)))#this creates a list of 10 subsets data
ghcn.subsets <-lapply(dates, function(d) subset(ghcn, ghcn$date==as.numeric(d)))

###Regression part 1: Creating a validation dataset by creating training and testing datasets
for(i in 1:length(dates)){            # start of the for loop #1
  
  ###Regression part 1: Creating a validation dataset by creating training and testing datasets
    
  n<-nrow(ghcn.subsets[[i]])
  ns<-n-round(n*prop)  #Create a sample from the data frame with 70% of the rows
  nv<-n-ns             #create a sample for validation with prop of the rows
  #ns<-n-round(n*prop)  #Create a sample from the data frame with 70% of the rows
  ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
  ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)
  data_s <- ghcn.subsets[[i]][ind.training, ]
  data_v <- ghcn.subsets[[i]][ind.testing, ]
  bwG <- gwr.sel(tmax~ lon + lat + ELEV_SRTM + Eastness + Northness + DISTOC,data=data_s,gweight=gwr.Gauss, verbose = FALSE)
  gwrG<- gwr(tmax~ lon + lat + ELEV_SRTM + Eastness + Northness + DISTOC, data=data_s, bandwidth=bwG, gweight=gwr.Gauss, hatmatrix=TRUE)

  Res_fit<-gwrG$lm$residuals
  RMSE_f<-sqrt(sum(Res_fit^2)/ns)
  t<- data_s$tmax-gwrG$lm$fitted.values #Checking output
  t2<-t-Res_fit #This should be zero
  data_s$residuals <- Res_fit #adding field to the data 
  
  #Saving the subset in a dataframe
  data_name<-paste("ghcn_v_",dates[[i]],sep="")
  assign(data_name,data_v)
  data_name<-paste("ghcn_s_",dates[[i]],sep="")
  assign(data_name,data_s)
  
  results[i,1]<- dates[i]  #storing the interpolation dates in the first column
  results[i,2]<- ns        #number of stations used in the training stage
  results[i,3]<- RMSE_f
  
  #Kriging residuals!!
  X11()
  hscat(residuals~1,data_s,(0:9)*20000) # 9 lag classes with 20,000m width
  v<-variogram(residuals~1, data_s)
  plot(v)
  tryCatch(v.fit<-fit.variogram(v,vgm(1,"Sph", 150000,1)),error=function()next)
  gwr_res_krige<-krige(residuals~1, data_s,mean_LST, v.fit)#mean_LST provides the data grid/raster image for the kriging locations.

  # GWR visualization of Residuals fit over space
  grays = gray.colors(5,0.45, 0.95)
  image(gwr_res_krige,col=grays) #needs to change to have a bipolar palette !!!
  
  #image(mean_LST, col=grays,breaks = c(185,245,255,275,315,325))

  plot(OR_state, axes = TRUE, add=TRUE)
  plot(data_s, pch=1, col="red", cex= abs(data_s$residuals)/10, add=TRUE) #Taking the absolute values because residuals are 
  LegVals<- c(0,20,40,80,110)
  legend("topleft", legend=LegVals,pch=1,col="red",pt.cex=LegVals/10,bty="n",title= "residuals")
  legend("left", legend=c("275-285","285-295","295-305", "305-315","315-325"),fill=grays, bty="n", title= "LST mean DOY=244")

  savePlot(paste(data_name,out_prefix,".png", sep=""), type="png")
  dev.off()
  }
  
## Plotting and saving diagnostic measures
results_num <-results
mode(results_num)<- "numeric"
# Make it numeric first
# Now turn it into a data.frame...

results_table<-as.data.frame(results_num)
colnames(results_table)<-c("dates","ns","RMSE_gwr1")

write.csv(results_table, file= paste(path,"/","results_GWR_Assessment",out_prefix,".txt",sep=""))


# End of script##########

# ###############################

  
  
#Compare the coefficients and residuals using both 30 and 100%
#coefficients are stored in gwrG$SDF$lon
#write out a new shapefile (including .prj component)
#writeOGR(data_s,".", "ghcn_1507_s", driver ="ESRI Shapefile")
#ogrInfo(".", "ghcn_1507_s") #This will check the file...
#plot(ghcn_1507, axes=TRUE, border="gray")

#library(foreign)
#dbfdata<-read.dbf("file.dbf", as.is=TRUE)
##Add new attribute data (just the numbers of 1 to the numbers of objects)
#dbfdata$new.att <- 1:nrow(shp)
##overwrite the file with this new copy
#write.dbf(dbfdata, "file.dbf")
