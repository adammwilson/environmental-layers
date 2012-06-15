##################    CLIMATE INTERPOLATION FUSION METHOD   #######################################
############################ Merging LST and station data ##########################################
#This script interpolates tmax values using MODIS LST and GHCND station data                     #
#interpolation area. It requires the text file of stations and a shape file of the study area.   #       
#Note that the projection for both GHCND and study area is lonlat WGS84.                         #
#AUTHOR: Brian McGill                                                                            #
#DATE: 06/09/212                                                                                 #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#363--                                  #
###################################################################################################

###Loading R library and packages                                                      
library(gtools)                                         # loading some useful tools 
library(mgcv)                                           # GAM package by Simon Wood
library(sp)                                             # Spatial pacakge with class definition by Bivand et al.
library(spdep)                              # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(gstat)                               # Kriging and co-kriging by Pebesma et al.
library(fields)                              # Spatial Interpolation methods such as kriging, splines
library(raster)
### Parameters and argument

infile1<- "ghcn_or_tmax_b_04142012_OR83M.shp"             #GHCN shapefile containing variables for modeling 2010                 
infile2<-"list_10_dates_04212012.txt"                     #List of 10 dates for the regression
#infile2<-"list_365_dates_04212012.txt"
infile3<-"LST_dates_var_names.txt"                        #LST dates name
infile4<-"models_interpolation_05142012.txt"              #Interpolation model names
infile5<-"mean_day244_rescaled.rst"                       #Raster or grid for the locations of predictions

#path<-"/home/parmentier/Data/IPLANT_project/data_Oregon_stations"
path<-"M:/Data/IPLANT_project/data_Oregon_stations"   #Locations on Atlas

#Station location of the study area
stat_loc<-read.table(paste(path,"/","location_study_area_OR_0602012.txt",sep=""),sep=",", header=TRUE)
#GHCN Database for 1980-2010 for study area (OR) 
data3<-read.table(paste(path,"/","ghcn_data_TMAXy1980_2010_OR_0602012.txt",sep=""),sep=",", header=TRUE)

prop<-0.3                                                                           #Proportion of testing retained for validation   
seed_number<- 100                                                                   #Seed number for random sampling
out_prefix<-"_06142012_10d_fusion2"                                                   #User defined output prefix
setwd(path)
############ START OF THE SCRIPT ##################

#
### Step 0/Step 6 in Brian's code...preparing year 2010 data for modeling 
#


###Reading the station data and setting up for models' comparison
filename<-sub(".shp","",infile1)             #Removing the extension from file.
ghcn<-readOGR(".", filename)                 #reading shapefile 

CRS<-proj4string(ghcn)                       #Storing projection information (ellipsoid, datum,etc.)

mean_LST<- readGDAL(infile5)                 #Reading the whole raster in memory. This provides a grid for kriging
proj4string(mean_LST)<-CRS                   #Assigning coordinate information to prediction grid.

ghcn = transform(ghcn,Northness = cos(ASPECT)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness = sin(ASPECT))  #adding variable to the dataframe.
ghcn = transform(ghcn,Northness_w = sin(slope)*cos(ASPECT)) #Adding a variable to the dataframe
ghcn = transform(ghcn,Eastness_w = sin(slope)*sin(ASPECT))  #adding variable to the dataframe.

set.seed(seed_number)                        #Using a seed number allow results based on random number to be compared...

dates <-readLines(paste(path,"/",infile2, sep=""))
LST_dates <-readLines(paste(path,"/",infile3, sep=""))
models <-readLines(paste(path,"/",infile4, sep=""))

#Model assessment: specific diagnostic/metrics for GAM
# results_AIC<- matrix(1,length(dates),length(models)+3)  
# results_GCV<- matrix(1,length(dates),length(models)+3)
# results_DEV<- matrix(1,length(dates),length(models)+3)
# results_RMSE_f<- matrix(1,length(dates),length(models)+3)

#Model assessment: general diagnostic/metrics 
results_RMSE <- matrix(1,length(dates),length(models)+3)
results_MAE <- matrix(1,length(dates),length(models)+3)
results_ME <- matrix(1,length(dates),length(models)+3)
results_R2 <- matrix(1,length(dates),length(models)+3)       #Coef. of determination for the validation dataset
results_RMSE_f<- matrix(1,length(dates),length(models)+3)    #RMSE fit, RMSE for the training dataset
results_RMSE_f_kr<- matrix(1,length(dates),length(models)+3)

# #Tracking relationship between LST AND LC
# cor_LST_LC1<-matrix(1,10,1)      #correlation LST-LC1
# cor_LST_LC3<-matrix(1,10,1)      #correlation LST-LC3
# cor_LST_tmax<-matrix(1,10,1)     #correlation LST-tmax

#Screening for bad values
ghcn_all<-ghcn
ghcn_test<-subset(ghcn,ghcn$tmax>-150 & ghcn$tmax<400)
ghcn_test2<-subset(ghcn_test,ghcn_test$ELEV_SRTM>0)
ghcn<-ghcn_test2
#coords<- ghcn[,c('x_OR83M','y_OR83M')]

month_var<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09", "mm_10", "mm_11", "mm_12")
ghcn.subsets <-lapply(dates, function(d) subset(ghcn, date==d)) #this creates a list of 10 or 365 subsets dataset based on dates


#Start loop here...

## looping through the dates...this is the main part of the code
#i=1 #for debugging
#j=1 #for debugging
for(i in 1:length(dates)){            # start of the for loop #1
  
  date<-strptime(dates[i], "%Y%m%d")   # interpolation date being processed
  month<-strftime(date, "%m")          # current month of the date being processed
  LST_month<-paste("mm_",month,sep="") # name of LST month to be matched
  
  ###Regression part 1: Creating a validation dataset by creating training and testing datasets
  
  mod_LST <-ghcn.subsets[[i]][,match(LST_month, names(ghcn.subsets[[i]]))]  #Match interpolation date and monthly LST average
  ghcn.subsets[[i]] = transform(ghcn.subsets[[i]],LST = mod_LST)            #Add the variable LST to the subset dataset
  n<-nrow(ghcn.subsets[[i]])
  ns<-n-round(n*prop)   #Create a sample from the data frame with 70% of the rows
  nv<-n-ns              #create a sample for validation with prop of the rows
  ind.training <- sample(nrow(ghcn.subsets[[i]]), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
  ind.testing <- setdiff(1:nrow(ghcn.subsets[[i]]), ind.training)
  data_s <- ghcn.subsets[[i]][ind.training, ]   #Training dataset currently used in the modeling
  data_v <- ghcn.subsets[[i]][ind.testing, ]    #Testing/validation dataset
  
  #i=1
  date_proc<-dates[i]
  date_proc<-strptime(dates[i], "%Y%m%d")   # interpolation date being processed
  mo<-as.integer(strftime(date_proc, "%m"))          # current month of the date being processed
  day<-as.integer(strftime(date_proc, "%d"))
  year<-as.integer(strftime(date_proc, "%Y"))
  
  #setup
  
  #mo=9           #Commented out by Benoit on June 14
  #day=2
  #year=2010
  datelabel=format(ISOdate(year,mo,day),"%b %d, %Y")
  
  #
  #  Step 1 - 10 year monthly averages
  #
  
  library(raster)
  #old<-getwd()
  #setwd("c:/data/benoit/data_Oregon_stations_Brian_04242012")
  #l=list.files(pattern="mean_month.*rescaled.tif")
  l=list.files(pattern="mean_month.*rescaled.rst")
  molst<-stack(l)
  #setwd(old)
  molst=molst-273.16  #K->C
  idx <- seq(as.Date('2010-01-15'), as.Date('2010-12-15'), 'month')
  molst <- setZ(molst, idx)
  layerNames(molst) <- month.abb
  themolst<-raster(molst,mo)
  plot(themolst)
  #
  # Step 2 - Weather station means across same days
  #
  # ??? which years & what quality flags???
  #select ghcn.id, lat,lon, elev, month, avg(value/10) as "TMax", count(*) as "NumDays" from ghcn, stations where ghcn.id in (select id from stations where state=='OR') and ghcn.id==stations.id and value<>-9999 and year>=2000 and  element=='TMAX' group by stations.id, month;select ghcn.id, lat,lon, elev, month, avg(value/10) as "TMax", count(*) as "NumDays" from ghcn, stations where ghcn.id in (select id from stations where state=='OR') and ghcn.id==stations.id and value<>-9999 and year>=2000 and  element=='TMAX' group by stations.id, month;
  #below table from above SQL query
  #dst<-read.csv('/data/benoit/data_oregon_stations_brian_04242012/station_means.csv',h=T)
  
  
  ##Added by Benoit ######
  date1<-ISOdate(data3$year,data3$month,data3$day) #Creating a date object from 3 separate column
  date2<-as.POSIXlt(as.Date(date1))
  data3$date<-date2
  d<-subset(data3,year>=2000 & mflag=="0" )
  d1<-aggregate(value~station+month, data=d, mean)  #Calculate monthly mean for every station in OR
  id<-as.data.frame(unique(d1$station))
  
  dst<-merge(d1, stat_loc, by.x="station", by.y="STAT_ID")
  #This allows to change only one name of the data.frame
  names(dst)[3]<-c("TMax")
  dst$TMax<-dst$TMax/10
  #dstjan=dst[dst$month==9,]  #dst contains the monthly averages for tmax for every station over 2000-2010
  ##############
  
  modst=dst[dst$month==mo,]
  #
  # Step 3 - get LST at stations
  #
  sta_lola=modst[,c("lon","lat")]
  library(rgdal)
  proj_str="+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
  lookup<-function(r,lat,lon) {
    xy<-project(cbind(lon,lat),proj_str);
    cidx<-cellFromXY(r,xy);
    return(r[cidx])
  }
  sta_tmax_from_lst=lookup(themolst,sta_lola$lat,sta_lola$lon)
  #
  # Step 4 - bias at stations
  #
  sta_bias=sta_tmax_from_lst-modst$TMax;
  bias_xy=project(as.matrix(sta_lola),proj_str)
  # windows()
  plot(modst$TMax,sta_tmax_from_lst,xlab="Station mo Tmax",ylab="LST mo Tmax")
  abline(0,1)
  #
  # Step 5 - interpolate bias
  #
  # ?? include covariates like elev, distance to coast, cloud frequency, tree height
  library(fields)
  #windows()
  quilt.plot(sta_lola,sta_bias,main="Bias at stations",asp=1)
  US(add=T,col="magenta",lwd=2)
  #fitbias<-Tps(bias_xy,sta_bias) #use TPS or krige
  fitbias<-Krig(bias_xy,sta_bias,theta=1e5) #use TPS or krige
  # windows()
  surface(fitbias,col=rev(terrain.colors(100)),asp=1,main="Interpolated bias")
  #US(add=T,col="magenta",lwd=2)
  #
  # Step 6 - return to daily station data & calcualate delta=daily T-monthly T from stations
  #Commmented out by Benoit 06/14
  # library(RSQLite)
  # m<-dbDriver("SQLite")
  # con<-dbConnect(m,dbname='c:/data/ghcn_tmintmax.db')
  # querystr=paste("select ghcn.id, value as 'dailyTmax' from ghcn where ghcn.id in (select id from stations where state=='OR') and value<>-9999",
  #    "and year==",year,"and month==",mo,"and day==",day,
  #                "and element=='TMAX' ")
  # rs<-dbSendQuery(con,querystr)
  # d<-fetch(rs,n=-1)
  # dbClearResult(rs)
  # dbDisconnect(con)
  # 
  # d$dailyTmax=d$dailyTmax/10 #stored as 1/10 degree C to allow integer storage
  # dmoday=merge(modst,d,by="id")
  ##########################Commented out by Benoit
  
  #added by Benoit 
  #d<-ghcn.subsets[[i]]
  d<-data_s
  names(d)[8]<-c("dailyTmax")
  d$dailyTmax=d$dailyTmax/10 #stored as 1/10 degree C to allow integer storage
  names(d)[1]<-c("id")
  names(modst)[1]<-c("id")
  dmoday=merge(modst,d,by="id")  #LOOSING DATA HERE!!! from 162 t0 146
  names(dmoday)[4]<-c("lat")
  names(dmoday)[5]<-c("lon")
  ###
  
  # windows()
  plot(dailyTmax~TMax,data=dmoday,xlab="Mo Tmax",ylab=paste("Daily for",datelabel),main="across stations in OR")
  #
  # Step 7 - interpolate delta across space
  #
  daily_sta_lola=dmoday[,c("lon","lat")] #could be same as before but why assume merge does this - assume not
  daily_sta_xy=project(as.matrix(daily_sta_lola),proj_str)
  daily_delta=dmoday$dailyTmax-dmoday$TMax
  #windows()
  quilt.plot(daily_sta_lola,daily_delta,asp=1,main="Station delta for Jan 15")
  US(add=T,col="magenta",lwd=2)
  #fitdelta<-Tps(daily_sta_xy,daily_delta) #use TPS or krige
  fitdelta<-Krig(daily_sta_xy,daily_delta,theta=1e5) #use TPS or krige
  # windows()
  surface(fitdelta,col=rev(terrain.colors(100)),asp=1,main="Interpolated delta")
  #US(add=T,col="magenta",lwd=2)
  #
  # Step 8 - assemble final answer - T=LST+Bias(interpolated)+delta(interpolated)
  #
  bias_rast=interpolate(themolst,fitbias)
  plot(bias_rast,main="Raster bias")
  daily_delta_rast=interpolate(themolst,fitdelta)
  plot(daily_delta_rast,main="Raster Daily Delta")
  tmax_predicted=themolst+daily_delta_rast-bias_rast #Final surface??
  plot(tmax_predicted,main="Predicted daily")
  #
  # check
  #
  sta_pred=lookup(tmax_predicted,data_v$lat,data_v$lon)
  #sta_pred=lookup(tmax_predicted,daily_sta_lola$lat,daily_sta_lola$lon)
  RMSE<-function(x,y) {return(mean((x-y)^2)^0.5)}
  rmse=RMSE(sta_pred,dmoday$dailyTmax)
  #plot(sta_pred~dmoday$dailyTmax,xlab=paste("Actual daily for",datelabel),ylab="Pred daily",main=paste("RMSE=",rmse))
  tmax<-data_v$tmax/10
  plot(sta_pred~tmax,xlab=paste("Actual daily for",datelabel),ylab="Pred daily",main=paste("RMSE=",rmse))
  abline(0,1)
  resid=sta_pred-dmoday$dailyTmax
  quilt.plot(daily_sta_lola,resid)
  
  ### END OF BRIAN's code
  
  ### Added by benoit
  j=1
  results_RMSE[i,1]<- dates[i]    #storing the interpolation dates in the first column
  results_RMSE[i,2]<- ns          #number of stations used in the training stage
  results_RMSE[i,3]<- "RMSE"
  results_RMSE[i,j+3]<- rmse  #Storing RMSE for the model j
  
  # end of the for loop1
  
}


## Plotting and saving diagnostic measures

#Specific diagnostic measures related to the testing datasets

results_table_RMSE<-as.data.frame(results_RMSE)
results_table_MAE<-as.data.frame(results_MAE)
results_table_ME<-as.data.frame(results_ME)
results_table_R2<-as.data.frame(results_R2)

cname<-c("dates","ns","metric","mod1", "mod2","mod3", "mod4", "mod5", "mod6", "mod7","mod8")
colnames(results_table_RMSE)<-cname

#tb_diagnostic1<-rbind(results_table_RMSE,results_table_MAE, results_table_ME, results_table_R2)   #
tb_diagnostic1<-results_table_RMSE

write.table(tb_diagnostic1, file= paste(path,"/","results_fusion_Assessment_measure1",out_prefix,".txt",sep=""), sep=",")

#### END OF SCRIPT