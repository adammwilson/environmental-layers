##################    Covariates and stations analyses  #######################################
############################ Covariate production for a given tile/region ##########################################
#This script examines inputs and outputs from the interpolation step.                             
#Part 1: Script produces summary information about stations used in the interpolation
#Part 2: Script produces plots of input covariates for study region
#AUTHOR: Benoit Parmentier                                                                       
#DATE: 04/01/2013                                                                                 

#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#???--   

##Comments and TODO:
#Add interpolation analyses of number of stations used for every day of the year...  

## Input arguments
#The output is a list of four shapefile names produced by the function:
#1) loc_stations: locations of stations as shapefile in EPSG 4326
#2) loc_stations_ghcn: ghcn daily data for the year range of interpolation (locally projected)
#3) daily_query_ghcn_data: ghcn daily data from daily query before application of quality flag
#4) daily_covar_ghcn_data: ghcn daily data with covariates for the year range of interpolation (locally projected)
#5) monthly_query_ghcn_data: ghcn daily data from monthly query before application of quality flag
#6) monthly_covar_ghcn_data: ghcn monthly averaged data with covariates for the year range of interpolation (locally projected)
#7) var
#8) range_years
#9) range_years_clim
#8) infile_covariate: s_raster brick file
#9)  covar_names: variable mames
#10) raster_prediction
#11) world_countries
#12) region_outline
#13) out_prefix

## Functions used in the script

load_obj <- function(f) 
{
  env <- new.env()
  nm <- load(f, env)[1]  
  env[[nm]]
}

extract_number_obs<-function(list_param){
  #Function to extract the number of observations used in modeling
  
  method_mod_obj<-list_param$method_mod_obj
  
  #Change to results_mod_obj[[i]]$data_s to make it less specific!!!!
  
  #number of observations 
  list_nrow_data_s<-lapply(1:length(method_obj),function(k) nrow(method_mod_obj[[k]]$data_s))
  list_nrow_data_v<-lapply(1:length(method_obj),function(k) nrow(method_mod_obj[[k]]$data_v))
  #lapply(1:length(clim_obj),function(k) nrow(method_mod_obj[[k]]$data_month))
  list_nrow_data_month<-lapply(1:length(gamclim_fus_mod),function(k) nrow(gamclim_fus_mod[[k]]$data_month))
  
  #number of valid observations 
  list_nrow_valid_data_s<-lapply(1:length(method_obj),function(k) length(method_mod_obj[[k]]$data_s$[[y_var_name]])
  list_nrow_data_v<-lapply(1:length(method_obj),function(k) length(method_mod_obj[[k]]$data_v$[[y_var_name]])
  list_nrow_valid_data_month<-lapply(1:length(gamclim_fus_mod),function(k) length(gamclim_fus_mod[[k]]$data_month$y_var))
  
  c1<-do.call(rbind,list_nrow_data_s)
  c2<-do.call(rbind,list_nrow_data_v)
  c3<-do.call(rbind,list_nrow_data_month)
  
  c1v<-do.call(rbind,list_valid_nrow_data_s)
  c2v<-do.call(rbind,list_valid_nrow_data_v)
  c3v<-do.call(rbind,list_valid_nrow_data_month)
  
  n_data_s<-cbdind(c1,c1v)
  n_data_v<-cbdind(c2,c2v)
  n_data_month<-cbdind(c3,c3vv)
  list_observations<-list(n_data_s,n_data_v,n_data_month)                         
  return(list_observations)
}

###########################################################################
########################## BEGIN SCRIPT/FUNCTION ##########################


########################################
#### STEP 1: read in data

#Stations in the processing region/study area
stat_reg <- readOGR(dsn=dirname(list_outfiles$loc_stations),sub(".shp","",basename(list_outfiles$loc_stations)))
#Stations available before screening the data query: ghcn daily data for the year range of interpolation
data_d <- readOGR(dsn=dirname(list_outfiles$loc_stations_ghcn),sub(".shp","",basename(list_outfiles$loc_stations_ghcn)))
#Stations available after screening the data query: ghcn daily data for the year range of interpolation
data_reg <- readOGR(dsn=dirname(list_outfiles$daily_query_ghcn_data),sub(".shp","",basename(list_outfiles$daily_query_ghcn_data)))
#Covariates data available after screening:ghcn daily data with covariates for the year range of interpolation
data_RST_SDF <-readOGR(dsn=dirname(list_outfiles$daily_covar_ghcn_data),sub(".shp","",basename(list_outfiles$daily_covar_ghcn_data)))
#Stations before screening monthly_query_ghcn_data: ghcn daily data from monthly query before application of quality flag
data_m <- readOGR(dsn=dirname(list_outfiles$monthly_query_ghcn_data),sub(".shp","",basename(list_outfiles$monthly_query_ghcn_data)))
#Stations after screening monthly_query_ghcn_data, extraction of covariates and monthly averages
dst<-readOGR(dsn=dirname(list_outfiles$monthly_covar_ghcn_data),sub(".shp","",basename(list_outfiles$monthly_covar_ghcn_data)))

### Load data used in fitting the model at monthly scale...
data_month<-gamclim_fus_mod[[1]]$data_month
list_data_month<-lapply(1:length(gamclim_fus_mod),function(k) gamclim_fus_mod[[k]]$data_month)

#Loading covariates raster images
s_raster<-brick(infile_covariates)
names(s_raster)<-covar_names
rast_ref<-subset(s_raster,"mm_01") # mean month for January

names(raster_prediction_obj)
var<-list_param$var

raster_prediction_obj<-load_obj(list_param$raster_prediction_obj)
#method_mod_obj<-raster_prediction_obj$method_mod_obj
method_mod_obj<-raster_prediction_obj$gam_fus_mod #change later for any model type
#validation_obj<-raster_prediction_obj$validation_obj
validation_obj<-raster_prediction_obj$gam_fus_validation_mod #change later for any model type
#clim_obj<-raster_prediction_obj$clim_obj
clim_obj<-raster_prediction_obj$gamclim_fus_mod #change later for any model type

names(raster_prediction_obj$method_obj[[1]])
data_s<-validation_obj[[1]]$data_s
summary_data_v<-validation_obj[[1]]$summary_data_v
names(validation_obj[[1]])
###################################################################
######## PART I: Script produces summary information about stations used in the interpolation ########

### Figue 1: stations in the study area/processing tile  
png(paste("Total_number_of_stations_in_study_area_",out_prefix,".png", sep=""))
plot(rast_ref)
plot(stat_reg,add=TRUE)
nb_point1<-paste("n_station=",length(stat_reg$STAT_ID))
#Add the number of data points on the plot
title("Stations located in the study area")
legend("topleft",legend=c(nb_point1),bty="n",cex=1.2)
dev.off()

### Figue 2: stations in the study area/processing tile: from query without flag screening             
png(paste("Stations_for_range_",range_years[1],"_",range_years[2],"_no_screening",out_prefix,".png", sep=""))
plot(rast_ref)
plot(data_d, add=TRUE)
nb_point<-paste("nrow=",nrow(data_d),sep="")
nb_point2<-paste("ns_stations=",length(unique(data_d$station)),sep="")
#Add the number of data points on the plot
legend("topleft",legend=c(nb_point,nb_point2),bty="n",cex=1)
title(paste("Stations available for year ",range_years[1],sep=""))
dev.off()

### Figue 3: stations in the study area/processing tile: after screening     
png(paste("Stations_for_range_",range_years[1],"_",range_years[2],"_after_screening",out_prefix,".png", sep=""))
plot(rast_ref)
plot(data_reg,add=TRUE)
nb_point<-paste("nrow=",nrow(data_reg),sep="")
nb_point2<-paste("ns_stations=",length(unique(data_reg$station)),sep="")
#Add the number of data points on the plot
legend("topleft",legend=c(nb_point,nb_point2),bty="n",cex=1)
title(paste("Stations available for year ",range_years[1]," after screening",sep=""))
#Add the number of data points on the plot
dev.off()

### Figue 4: stations in the study area/processing tile: after screening and covar extraction
png(paste("Stations_for_range_",range_years[1],"_",range_years[2],"_after_screening",out_prefix,".png", sep=""))
plot(rast_ref)
plot(data_RST_SDF,add=TRUE)
nb_point<-paste("nrow=",nrow(data_RST_SDF),sep="")
nb_point2<-paste("ns_stations=",length(unique(data_RST_SDF$station)),sep="")
#Add the number of data points on the plot
legend("topleft",legend=c(nb_point,nb_point2),bty="n",cex=1)
title(paste("Stations year ",range_years[1]," after screening and covar extraction",sep=""))
#Add the number of data points on the plot
dev.off()

### Figue 5: stations in the study area/processing tile: monthly query for specified range of years before screening    
png(paste("Stations_monthly_for_range_",range_years[1],"_",range_years[2],"_before_screening",out_prefix,".png", sep=""))
plot(rast_ref)
plot(data_m,add=TRUE)
nb_point<-paste("nrow=",nrow(data_m),sep="")
nb_point2<-paste("ns_stations=",length(unique(data_m$station)),sep="")
#Add the number of data points on the plot
legend("topleft",legend=c(nb_point,nb_point2),bty="n",cex=1)
title(paste("Stations ",range_years[1],"-",range_years[2]," after screening and covar extraction",sep=""))
#Add the number of data points on the plot
dev.off()
             
### Figue 6: histogram            
    
#dst$nobs_station
png(paste("Stations_data_month_modeled_for_range_",range_years_clim[1],"_",range_years_clim[2],out_prefix,".png", sep=""))
dst$nobs_station<-dst$nbs_stt
hist(dst$nobs_station)
dev.off()

### Figure 7: data month

png(paste("Stations_data_month_modeled_for_range_",range_years_clim[1],"_",range_years_clim[2],out_prefix,".png", sep=""))
par(mfrow=c(3,4))
for (j in 1:12){

  data_month<-list_data_month[[j]]
  plot(rast_ref)
  plot(data_month,add=TRUE)
  nb_point<-paste("nrow=",nrow(data_month),sep="")
  nb_point2<-paste("ns_stations=",length(unique(data_month$station)),sep="")
  nb_point3<-paste("ns_non_na_stations=",length(unique(data_month$y_var)),sep="")
  #Add the number of data points on the plot
  legend("topleft",legend=c(nb_point,nb_point2,nb_point3),bty="n",cex=0.9)
  title(paste("Stations ",range_years_clim[1],"-",range_years_clim[2]," used for modeling on ",sep=""))
  #data_NA<-subset(data=data_month,is.na(data_month$y_var))
  data_NA<-data_month[is.na(data_month$y_var),]
  plot(data_NA,add=TRUE,pch=2,col=c("red"))
  legend("topright",legend=c("NA value"),pch=2,col=c("red"),bty="n",cex=0.9)
}
dev.off()

### Figue 8: LST and TMax

names_tmp<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12")
LST_s<-subset(s_raster,names_tmp)
names(LST_s)<-names_tmp
names_tmp<-c("nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
             "nobs_09","nobs_10","nobs_11","nobs_12")
LST_nobs<-subset(s_raster,names_tmp)
    
list_statistic_values_LST_s<-mclapply(c("min","max","mean","sd"),FUN=cellStats,x=LST_s,mc.preschedule=FALSE,mc.cores = 4)
list_statistic_values_LST_nobs<-mclapply(c("min","max","mean","sd"),FUN=cellStats,x=LST_nobs,mc.preschedule=FALSE,mc.cores = 4)

statistics_LST_s<-do.call(cbind,list_statistic_values_LST_s)
statistics_LST_nobs<-do.call(cbind,list_statistic_values_LST_nobs)
LST_stat_data<-as.data.frame(statistics_LST_s)
names(LST_stat_data)<-c("min","max","mean","sd")
statistics_LSTnobs_s<-cbind(min_values,max_values,mean_values,sd_values) #This shows that some values are extremes...especially in October
LSTnobs_stat_data<-as.data.frame(statistics_LSTnobs_s)
names(LSTnobs_stat_data)<-c("min","max","mean","sd")

png(paste("Stations_data_month_modeled_for_range_",range_years_clim[1],"_",range_years_clim[2],out_prefix,".png", sep=""))    
plot(1:12,LST_stat_data$mean,type="b",ylim=c(-15,70),col="black",xlab="month",ylab="tmax (degree C)")
lines(1:12,LST_stat_data$min,type="b",col="blue")
lines(1:12,LST_stat_data$max,type="b",col="red")
text(1:12,LST_stat_data$mean,rownames(LST_stat_data),cex=1,pos=2)
legend("topleft",legend=c("min","mean","max"), cex=0.8, col=c("blue","black","red"),
       lty=1,lwd=1.4,bty="n")
title(paste("LST statistics for Study area",sep=" "))
# savePlot("lst_statistics_OR.png",type="png")
dev.off()
    
#### Fig9...#####

# #Plot number of valid observations for LST
png(paste("Stations_data_month_modeled_for_range_",range_years_clim[1],"_",range_years_clim[2],out_prefix,".png", sep=""))    
plot(1:12,LSTnobs_stat_data$mean,type="b",ylim=c(0,280),col="black",xlab="month",ylab="tmax (degree C)")
lines(1:12,LSTnobs_stat_data$min,type="b",col="blue")
lines(1:12,LSTnobs_stat_data$max,type="b",col="red")
text(1:12,LSTnobs_stat_data$mean,rownames(LSTnobs_stat_data),cex=1,pos=2)
legend("topleft",legend=c("min","mean","max"), cex=1.5, col=c("blue","black","red"),lty=1)
title(paste("LST number of valid observations for Oregon", "2010",sep=" "))
# savePlot("lst_nobs_OR.png",type="png")
dev.off()
    
#### Fig 10...#####

#Use data_month!!!

d_month<-aggregate(TMax~month, data=dst, mean)  #Calculate monthly mean for every station in OR
d_month<-aggregate(TMax~month, data=dst, length)  #Calculate monthly mean for every station in OR

plot(d_month,type="l")
# plot(data_month$TMax,add=TRUE)

#data_month #-> plot for everymonth

#number of stations
## Summarize information for the day: write out textfile...

#Number of station per month
#Number of station per day (training, testing,NA)
#metrics_v,metrics_s
#

# ################
# #PART 2: Region Covariate analyses ###
# ################
# 
# # This should be in a separate script to analyze covariates from region.
# 
# #MAP1:Study area with LC mask and tiles/polygon outline
             
#LC_mask<-subset(s_raster,"LC12")
#LC_mask[LC_mask==100]<-NA
#LC_mask <- LC_mask < 100
#LC_mask_rec<-LC_mask
#LC_mask_rec[is.na(LC_mask_rec)]<-0
             
#Add proportion covered by study area+ total of image pixels
#tmp_tb<-freq(LC_mask_rec)
#tmp_tb[2,2]/sum(tmp_tb[,2])*100
#png(paste("Study_area_",
#         out_prefix,".png", sep=""))
#plot(LC_mask_rec,legend=FALSE,col=c("black","red"))
#legend("topright",legend=c("Outside","Inside"),title="Study area",
#          pt.cex=0.9,fill=c("black","red"),bty="n")
#           title("Study area")
#             dev.off()
             
# 
# #MAP 2: plotting land cover in the study region:
# 
# l1<-"LC1,Evergreen/deciduous needleleaf trees"
# l2<-"LC2,Evergreen broadleaf trees"
# l3<-"LC3,Deciduous broadleaf trees"
# l4<-"LC4,Mixed/other trees"
# l5<-"LC5,Shrubs"
# l6<-"LC6,Herbaceous vegetation"
# l7<-"LC7,Cultivated and managed vegetation"
# l8<-"LC8,Regularly flooded shrub/herbaceous vegetation"
# l9<-"LC9,Urban/built-up"
# l10<-"LC10,Snow/ice"
# l11<-"LC11,Barren lands/sparse vegetation"
# l12<-"LC12,Open water"
# lc_names_str<-c(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12)
# 
# names(lc_reg_s)<-lc_names_str
# 
# png(paste("LST_TMax_scatterplot_",sampling_dat$date[i],"_",sampling_dat$prop[i],"_",sampling_dat$run_samp[i], out_prefix,".png", sep=""))
# plot(modst$TMax,sta_tmax_from_lst,xlab="Station mo Tmax",ylab="LST mo Tmax",main=paste("LST vs TMax for",datelabel,sep=" "))
# abline(0,1)
# nb_point<-paste("n=",length(modst$TMax),sep="")
# mean_bias<-paste("LST bigrasas= ",format(mean(modst$LSTD_bias,na.rm=TRUE),digits=3),sep="")
# #Add the number of data points on the plot
# legend("topleft",legend=c(mean_bias,nb_point),bty="n")
# dev.off()
# 
#
### MAP3: Majority land cover for every pixels in the study region

#Add barplot of majority map...

###Map 4: Elevation and LST in January
# tmp_s<-stack(LST,elev_1)
# png(paste("LST_elev_",sampling_dat$date[i],"_",sampling_dat$prop[i],"_",sampling_dat$run_samp[i], out_prefix,".png", sep=""))
# plot(tmp_s)
#
###Histogram elevation?
#
# #Map 5: mean TMIN/TMAX: LST climatology per month
#         
# names_tmp<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12")
# LST_s<-subset(s_raster,names_tmp)
# names_tmp<-c("nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
#              "nobs_09","nobs_10","nobs_11","nobs_12")
# LST_nobs<-subset(s_raster,names_tmp)
# 
# #Map 6: number of obs- LST climatology per month
#         
# names_tmp<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12")
# LST_s<-subset(s_raster,names_tmp)
# names_tmp<-c("nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
#              "nobs_09","nobs_10","nobs_11","nobs_12")
# LST_nobs<-subset(s_raster,names_tmp)
# 

# LST_nobs<-mask(LST_nobs,LC_mask,filename="test2.tif")
# LST_s<-mask(LST_s,LC_mask,filename="test3.tif")
# c("Jan","Feb")
# plot(LST_s)
# plot(LST_nobs)
# 
# #Map 7: LST-TMAX/TMIN fit for all 12 months...
#
#
#
#### End of script ####