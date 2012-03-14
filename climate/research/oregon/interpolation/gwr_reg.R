####################GWR of Tmax for 10 dates.#####################
#This script generate station values for the Oregon case study. This program loads the station data from a shp file 
#and performs a GWR regression. 
#Script created by Benoit Parmentier on March 13, 2012. 

###Loading r library and packages
library(sp)
library(spdep)
library(rgdal)
library(spgwr)

setwd("/data/computer/parmentier/Data/IPLANT_project/data_Oregon_stations/")
prop<-0

###Reading the shapefile from the local directory
ghcn<-readOGR(".", "ghcn_or_tmax_b_03032012_OR83M") 

ghcn$Northness<- cos(ghcn$ASPECT) #Adding a variable to the dataframe
ghcn$Eastness <- sin(ghcn$ASPECT)  #adding variable to the dataframe.

ghcn$Northness_w <- sin(ghcn$slope)*cos(ghcn$ASPECT) #Adding a variable to the dataframe
ghcn$Eastness_w  <- sin(ghcn$slope)*sin(ghcn$ASPECT)  #adding variable to the dataframe.
 
set.seed(100)

ghcn.subsets <-subset(ghcn,ghcn$date=="20100101")

#summary(lm(y~x,data=df,weights=(df$wght1)^(3/4))
#ggwr.sel: find the bandwith from the data

###Regression part 1: Creating a validation dataset by creating training and testing datasets

n<- nrow(ghcn.subsets)
ns<- n-round(n*prop)  #Create a sample from the data frame with 70% of the rows
nv<- n-ns             #create a sample for validation with prop of the rows
#ns<-n-round(n*prop)  #Create a sample from the data frame with 70% of the rows
ind.training <- sample(nrow(ghcn.subsets), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
ind.testing <- setdiff(1:nrow(ghcn.subsets), ind.training)
data_s <- ghcn.subsets[ind.training, ]
data_v <- ghcn.subsets[ind.testing, ]

bwG <- gwr.sel(tmax~ lon + lat + ELEV_SRTM + Eastness + Northness + DISTOC,data=data_s,gweight=gwr.Gauss, verbose = FALSE)
gwrG<- gwr(tmax~ lon + lat + ELEV_SRTM + Eastness + Northness + DISTOC, data=data_s, bandwidth=bwG, gweight=gwr.Gauss, hatmatrix=TRUE)

Res_fit<-gwrG$lm$residuals
RMSE_f<-sqrt(sum(Res_fit^2)/ns)

t<- data_s$tmax-gwrG$lm$fitted.values
t2<-t-Res_fit #This should be zero

#Compare the coefficients and residuals using both 30 and 100%

#coefficients are stored in gwrG$SDF$lon

#write out a new shapefile (including .prj component)

writeOGR(data_s,".", "ghcn_1507_s", driver ="ESRI Shapefile")

#ogrInfo(".", "ghcn_1507_s") #This will check the file...
#plot(ghcn_1507, axes=TRUE, border="gray")

#library(foreign)
#dbfdata<-read.dbf("file.dbf", as.is=TRUE)
##Add new attribute data (just the numbers of 1 to the numbers of objects)
#dbfdata$new.att <- 1:nrow(shp)
##overwrite the file with this new copy
#write.dbf(dbfdata, "file.dbf")
