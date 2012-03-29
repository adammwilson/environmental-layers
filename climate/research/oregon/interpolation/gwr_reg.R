####################GWR of Tmax for 10 dates.#####################
#This script generate station values for the Oregon case study. This program loads the station data from a shp file 
#and performs a GWR regression. 
#Script created by Benoit Parmentier on March 13, 2012. 

###Loading r library and packages
library(sp)
library(spdep)
library(rgdal)
library(spgwr)

###Parameters and arguments

infile1<-"ghcn_or_tmax_b_03032012_OR83M.shp" 
path<- "/data/computer/parmentier/Data/IPLANT_project/data_Oregon_stations/"
setwd(path)
#infile2<-"dates_interpolation_03012012.txt"  # list of 10 dates for the regression
infile2<-"dates_interpolation_03052012.txt"
prop<-0.3
out_prefix<-"_03132012_0"

###Reading the shapefile from the local directory
ghcn<-readOGR(".", "ghcn_or_tmax_b_03032012_OR83M") 

ghcn$Northness<- cos(ghcn$ASPECT) #Adding a variable to the dataframe
ghcn$Eastness <- sin(ghcn$ASPECT)  #adding variable to the dataframe.

ghcn$Northness_w <- sin(ghcn$slope)*cos(ghcn$ASPECT) #Adding a variable to the dataframe
ghcn$Eastness_w  <- sin(ghcn$slope)*sin(ghcn$ASPECT)  #adding variable to the dataframe.

set.seed(100)

dates <-readLines(paste(path,"/",infile2, sep=""))

results <- matrix(1,length(dates),3)            #This is a matrix containing the diagnostic measures from the GAM models.

#Screening for bad values
#tmax~ lon + lat + ELEV_SRTM + Eastness + Northness + DISTOC
#tmax range: min max)
ghcn_test<-subset(ghcn,ghcn$tmax>-150 & ghcn$tmax<400)
ghcn_test2<-subset(ghcn_test,ghcn_test$ELEV_SRTM>0)
ghcn<-ghcn_test2
#lon range
#lat range
#ELEV_SRTM
#Eastness
#Northness

#ghcn.subsets <-lapply(dates, function(d) subset(ghcn, date==as.numeric(d)))#this creates a list of 10 subsets data
ghcn.subsets <-lapply(dates, function(d) subset(ghcn, ghcn$date==as.numeric(d)))

#ghcn.subsets <-subset(ghcn,ghcn$date=="20100101")
#summary(lm(y~x,data=df,weights=(df$wght1)^(3/4))
#ggwr.sel: find the bandwith from the data



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
