####################GWR of Tmax for one Date#####################
#This script generates predicted values from station values for the Oregon case study. This program loads the station data from a shp file 
#and performs Kriging and co-kriging on tmax regression.
#Script created by Benoit Parmentier on April 17, 2012. 

###Loading r library and packages
library(sp)
library(spdep)
library(rgdal)
library(spgwr)
library(gpclib)
library(maptools)
library(gstat)
library(graphics)
###Parameters and arguments

path<- "/data/computer/parmentier/Data/IPLANT_project/data_Oregon_stations/"         #Path to all datasets
setwd(path)
infile1<-"ghcn_or_tmax_b_04142012_OR83M.shp" #Weather station location in Oregon with input variables
infile2<-"dates_interpolation_03052012.txt"  # list of 10 dates for the regression, more thatn 10 dates may be used
infile3<-"mean_day244_rescaled.rst"          #This image serves as the reference grid for kriging
infile4<- "orcnty24_OR83M.shp"               #Vector file defining the study area: Oregon state and its counties.
prop<-0.3                                    #Propotion of weather stations retained for validation/testing
out_prefix<-"_04102012_RMSE"                 #output name used in the text file result

###STEP 1 DATA PREPARATION AND PROCESSING#####

###Reading the shapefile and raster image from the local directory

mean_LST<- readGDAL(infile3)                  #This reads the whole raster in memory and provide a grid for kriging in a SpatialGridDataFrame object
filename<-sub(".shp","",infile1)              #Removing the extension from file.
ghcn<-readOGR(".", filename)                  #Reading station locations from vector file using rgdal and creating a SpatialPointDataFrame
CRS_ghcn<-proj4string(ghcn)                   #This retrieves the coordinate system information for the SDF object (PROJ4 format)
proj4string(mean_LST)<-CRS_ghcn               #Assigning coordinates information to SpatialGridDataFrame object

# Creating state outline from county

orcnty<-readOGR(".", "orcnty24_OR83M")
proj4string(orcnty)                           #This retrieves the coordinate system for the SDF
lps <-getSpPPolygonsLabptSlots(orcnty)        #Getting centroids county labels
IDOneBin <- cut(lps[,1], range(lps[,1]), include.lowest=TRUE)  #Creating one bin var
gpclibPermit()                                #Set the gpclib to True to allow union
OR_state <- unionSpatialPolygons(orcnty ,IDOneBin) #Dissolve based on bin var

# Adding variables for the regressions

ghcn$Northness<- cos(ghcn$ASPECT)             #Adding a variable to the dataframe by calculating the cosine of Aspect
ghcn$Eastness <- sin(ghcn$ASPECT)             #Adding variable to the dataframe.
ghcn$Northness_w <- sin(ghcn$slope)*cos(ghcn$ASPECT)  #Adding a variable to the dataframe
ghcn$Eastness_w  <- sin(ghcn$slope)*sin(ghcn$ASPECT)  #Adding variable to the dataframe.

set.seed(100)                                 #This set a seed number for the random sampling to make results reproducible.

dates <-readLines(paste(path,"/",infile2, sep=""))  #Reading dates in a list from the textile.
results <- matrix(1,length(dates),4)            #This is a matrix containing the diagnostic measures from the GAM models.
results_mod_n<-matrix(1,length(dates),3)

#Screening for bad values and setting the valid range

ghcn_test<-subset(ghcn,ghcn$tmax>-150 & ghcn$tmax<400) #Values are in tenth of degrees C
ghcn_test2<-subset(ghcn_test,ghcn_test$ELEV_SRTM>0)    #No elevation below sea leve is allowed.
ghcn<-ghcn_test2

###CREATING SUBSETS BY INPUT DATES AND SAMPLING
ghcn.subsets <-lapply(dates, function(d) subset(ghcn, ghcn$date==as.numeric(d)))   #Producing a list of data frame, one data frame per date.

for(i in 1:length(dates)){            # start of the for loop #1
#i<-3                                           #Date 10 is used to test kriging
  n<-nrow(ghcn.subsets[[i]])
  ns<-n-round(n*prop)                             #Create a sample from the data frame with 70% of the rows
  nv<-n-ns                                        #create a sample for validation with prop of the rows
  ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE)  #This selects the index position for 70% of the rows taken randomly
  ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)         #This selects the index position for testing subset stations.
  data_s <- ghcn.subsets[[i]][ind.training, ]
  data_v <- ghcn.subsets[[i]][ind.testing, ]
  
  ###STEP 2 KRIGING###
  
  #Kriging tmax
  
  hscat(tmax~1,data_s,(0:9)*20000)                       # 9 lag classes with 20,000m width
  v<-variogram(tmax~1, data_s)                           # This plots a sample varigram for date 10 fir the testing dataset
  plot(v)
  v.fit<-fit.variogram(v,vgm(2000,"Sph", 150000,1000))   #Model variogram: sill is 2000, spherical, range 15000 and nugget 1000
  plot(v, v.fit)                                         #Compare model and sample variogram via a graphical plot
  tmax_krige<-krige(tmax~1, data_s,mean_LST, v.fit)      #mean_LST provides the data grid/raster image for the kriging locations to be predicted.
  
  #Cokriging tmax
  g<-gstat(NULL,"tmax", tmax~1, data_s)                   #This creates a gstat object "g" that acts as container for kriging specifications.
  g<-gstat(g, "SRTM_elev",ELEV_SRTM~1,data_s)            #Adding variables to gstat object g
  g<-gstat(g, "Eastness", Eastness~1,data_s)
  g<-gstat(g, "Northness", Northness~1, data_s)
  
  vm_g<-variogram(g)                                     #Visualizing multivariate sample variogram.
  vm_g.fit<-fit.lmc(vm_g,g,vgm(2000,"Sph", 100000,1000)) #Fitting variogram for all variables at once.
  plot(vm_g,vm_g.fit)                                    #Visualizing variogram fit and sample
  vm_g.fit$set <-list(nocheck=1)                         #Avoid checking and allow for different range in variogram
  co_kriged_surf<-predict(vm_g.fit,mean_LST) #Prediction using co-kriging with grid location defined from input raster image.
  #co_kriged_surf$tmax.pred                              #Results stored in SpatialGridDataFrame with tmax prediction accessible in dataframe.
  
  
  #spplot.vcov(co_kriged_surf)                           #Visualizing the covariance structure
  
  tmax_krig1_s <- overlay(tmax_krige,data_s)             #This overlays the kriged surface tmax and the location of weather stations
  tmax_cokrig1_s<- overlay(co_kriged_surf,data_s)        #This overalys the cokriged surface tmax and the location of weather stations
  tmax_krig1_v <- overlay(tmax_krige,data_v)             #This overlays the kriged surface tmax and the location of weather stations
  tmax_cokrig1_v<- overlay(co_kriged_surf,data_v)
  
  data_s$tmax_kr<-tmax_krig1_s$var1.pred                 #Adding the results back into the original dataframes.
  data_v$tmax_kr<-tmax_krig1_v$var1.pred  
  data_s$tmax_cokr<-tmax_cokrig1_s$tmax.pred    
  data_v$tmax_cokr<-tmax_cokrig1_v$tmax.pred
  
  #Co-kriging only on the validation sites for faster computing
  
  cokrig1_dv<-predict(vm_g.fit,data_v)
  cokrig1_ds<-predict(vm_g.fit,data_s)
  data_s$tmax_cokr<-cokrig1_ds$tmax.pred    
  data_v$tmax_cokr<-cokrig1_dv$tmax.pred
  
  #Calculate RMSE and then krig the residuals....!
  
  res_mod1<- data_v$tmax - data_v$tmax_kr              #Residuals from kriging.
  res_mod2<- data_v$tmax - data_v$tmax_cokr                #Residuals from cokriging.
  
  RMSE_mod1 <- sqrt(sum(res_mod1^2,na.rm=TRUE)/(nv-sum(is.na(res_mod1))))                  #RMSE from kriged surface.
  RMSE_mod2 <- sqrt(sum(res_mod2^2,na.rm=TRUE)/(nv-sum(is.na(res_mod2))))                  #RMSE from co-kriged surface.
  #(nv-sum(is.na(res_mod2)))       

  #Saving the subset in a dataframe
  data_name<-paste("ghcn_v_",dates[[i]],sep="")
  assign(data_name,data_v)
  data_name<-paste("ghcn_s_",dates[[i]],sep="")
  assign(data_name,data_s)
  
  krig_raster_name<-paste("coKriged_tmax_",data_name,out_prefix,".tif", sep="")
  writeGDAL(co_kriged_surf,fname=krig_raster_name, driver="GTiff", type="Float32",options ="INTERLEAVE=PIXEL")
  krig_raster_name<-paste("Kriged_tmax_",data_name,out_prefix,".tif", sep="")
  writeGDAL(tmax_krige,fname=krig_raster_name, driver="GTiff", type="Float32",options ="INTERLEAVE=PIXEL")
  X11()
  plot(raster(co_kriged_surf))
  title(paste("Tmax cokriging for date ",dates[[i]],sep=""))
  savePlot(paste("Cokriged_tmax",data_name,out_prefix,".png", sep=""), type="png")
  dev.off()
  X11()
  plot(raster(tmax_krige))
  title(paste("Tmax Kriging for date ",dates[[i]],sep=""))
  savePlot(paste("Kriged_res_",data_name,out_prefix,".png", sep=""), type="png")
  dev.off()
  
  results[i,1]<- dates[i]  #storing the interpolation dates in the first column
  results[i,2]<- ns     #number of stations in training
  results[i,3]<- RMSE_mod1
  results[i,4]<- RMSE_mod2  
  
  results_mod_n[i,1]<-dates[i]
  results_mod_n[i,2]<-(nv-sum(is.na(res_mod1)))
  results_mod_n[i,3]<-(nv-sum(is.na(res_mod2)))
  }

## Plotting and saving diagnostic measures
results_num <-results
mode(results_num)<- "numeric"
# Make it numeric first
# Now turn it into a data.frame...

results_table<-as.data.frame(results_num)
colnames(results_table)<-c("dates","ns","RMSE_gwr1")

write.csv(results_table, file= paste(path,"/","results_GWR_Assessment",out_prefix,".txt",sep=""))