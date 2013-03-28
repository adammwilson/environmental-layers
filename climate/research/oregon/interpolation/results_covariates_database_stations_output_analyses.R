##################    Covariates and stations analyses  #######################################
############################ Covariate production for a given tile/region ##########################################
#This script examines inputs and outputs from the interpolation step.                             
#Part 1: Script produces summary information about stations used in the interpolation
#Part 2: Script produces plots of input covariates for study region
#AUTHOR: Benoit Parmentier                                                                       
#DATE: 03/27/2013                                                                                 

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
#8) infile_covariate: s_raster brick file
#9)  covar_names: variable mames
#10) raster_prediction
#11) world_countries
#12) region_outline
#13) out_prefix

#list_param <-list_outfiles$loc_stations
#list_param <-list_outfiles$loc_stations_ghcn
#list_param <-list_outfiles$loc_stations_ghcn_data
#list_param <-list_outfiles$loc_stations
#list_param <-list_outfiles$loc_stations

## Functions used in the script

load_obj <- function(f) 
{
  env <- new.env()
  nm <- load(f, env)[1]  
  env[[nm]]
}

extract_number_obs<-function(list_param){
  
  method_mod_obj<-list_param$method_mod_obj
  #Change to results_mod_obj[[i]]$data_s to make it less specific
  lapply(1:length(method_obj),function(k) nrow(method_mod_obj[[k]]$data_s))
  lapply(1:length(method_obj),function(k) nrow(method_mod_obj[[k]]$data_v))
  lapply(1:length(clim_obj),function(k) nrow(method_mod_obj[[k]]$data_month))
  #number of observations 
  
  return()
}

#### STEP 1: read in data

stat_reg <- readOGR(dsn=dirname(list_outfiles$loc_stations),sub(".shp","",basename(list_outfiles$loc_stations)))
data_reg <- readOGR(dsn=dirname(list_outfiles$loc_stations_ghcn),sub(".shp","",basename(list_outfiles$loc_stations_ghcn)))
data_RST_SDF <-readOGR(dsn=dirname(list_outfiles$daily_covar_ghcn_data),sub(".shp","",basename(list_outfiles$daily_covar_ghcn_data)))
data_m <- readOGR(dsn=dirname(list_outfiles$monthly_query_ghcn_data),sub(".shp","",basename(list_outfiles$monthly_query_ghcn_data)))
dst<-readOGR(dsn=dirname(list_outfiles$monthly_covar_ghcn_data),sub(".shp","",basename(list_outfiles$monthly_covar_ghcn_data)))

s_raster<-brick(infile_covariates)
names(s_raster)<-covar_names
rast_ref<-subset(s_raster,"mm_01") # mean month for January

######## PART I 
### Figue 1: stations in the study area/processing tile  
png(paste("Total_number_of_stations_in_study_area_",out_prefix,".png", sep=""))
plot(rast_ref)
plot(stat_reg,add=TRUE)
nb_point1<-paste("n_station=",length(stat_reg$STAT_ID))
#Add the number of data points on the plot
legend("topleft",legend=c(nb_point1),bty="n",cex=1.2)
dev.off()

#add number of stations+ name of region
#title()

### Figue 2: stations in the study area/processing tile: from query without flag screening             
png(paste("Studay_area_",out_prefix,".png", sep=""))
plot(rast_ref)
plot(data_d, add=TRUE)
nb_point<-paste("ns=",length(data_d$TMax),sep="")
nb_point2<-paste("ns_obs=",length(data_s$TMax)-sum(is.na(data_s[[y_var_name]])),sep="")
nb_point3<-paste("n_month=",length(data_month$TMax),sep="")
#Add the number of data points on the plot
legend("topleft",legend=c(nb_point,nb_point2,nb_point3),bty="n",cex=0.8)
dev.off()

### Figue 3: stations in the study area/processing tile: after screening             
plot(rast_ref)
plot(data_reg,add=TRUE)

### Figue 4: stations in the study area/processing tile: from monthly climatology query without flag screening             
plot(rast_ref)
plot(data_RST_SDF,add=TRUE)
             
### Figue 5: stations in the study area/processing tile: after screening             
plot(rast_ref)
plot(data_m,add=TRUE)
             
### Figue 6: histogram            
#plot(dst) 
    
#dst$nobs_station
dst$nobs_station<-dst$nbs_stt
hist(dst$nobs_station)

### Figue 7: LST and TMax

names_tmp<-c("mm_01","mm_02","mm_03","mm_04","mm_05","mm_06","mm_07","mm_08","mm_09","mm_10","mm_11","mm_12")
LST_s<-subset(s_raster,names_tmp)
names(LST_s)<-names_tmp
names_tmp<-c("nobs_01","nobs_02","nobs_03","nobs_04","nobs_05","nobs_06","nobs_07","nobs_08",
             "nobs_09","nobs_10","nobs_11","nobs_12")
LST_nobs<-subset(s_raster,names_tmp)
 
## Function...note differnces in patternin agricultural areas

list_statistic_values_LST_s<-mclapply(c("min","max","mean","sd"),FUN=cellStats,x=LST_s,mc.preschedule=FALSE,mc.cores = 4)
list_statistic_values_LST_nobs<-mclapply(c("min","max","mean","sd"),FUN=cellStats,x=LST_nobs,mc.preschedule=FALSE,mc.cores = 4)

statistics_LST_s<-do.call(cbind,list_statistic_values_LST_s)
statistics_LST_nobs<-do.call(cbind,list_statistic_values_LST_nobs)
LST_stat_data<-as.data.frame(statistics_LST_s)
names(LST_stat_data)<-c("min","max","mean","sd")
statistics_LSTnobs_s<-cbind(min_values,max_values,mean_values,sd_values) #This shows that some values are extremes...especially in October
LSTnobs_stat_data<-as.data.frame(statistics_LSTnobs_s)
names(LSTnobs_stat_data)<-c("min","max","mean","sd")

# X11(width=12,height=12)
# #Plot statiscs (mean,min,max) for monthly LST images
plot(1:12,LST_stat_data$mean,type="b",ylim=c(-15,70),col="black",xlab="month",ylab="tmax (degree C)")
lines(1:12,LST_stat_data$min,type="b",col="blue")
lines(1:12,LST_stat_data$max,type="b",col="red")
text(1:12,LST_stat_data$mean,rownames(LST_stat_data),cex=1,pos=2)
legend("topleft",legend=c("min","mean","max"), cex=0.8, col=c("blue","black","red"),
       lty=1,lwd=1.4,bty="n")
title(paste("LST statistics for Study area",sep=" "))
# savePlot("lst_statistics_OR.png",type="png")

#### Fig8...#####

# #Plot number of valid observations for LST
plot(1:12,LSTnobs_stat_data$mean,type="b",ylim=c(0,280),col="black",xlab="month",ylab="tmax (degree C)")
lines(1:12,LSTnobs_stat_data$min,type="b",col="blue")
lines(1:12,LSTnobs_stat_data$max,type="b",col="red")
text(1:12,LSTnobs_stat_data$mean,rownames(LSTnobs_stat_data),cex=1,pos=2)
# 
legend("topleft",legend=c("min","mean","max"), cex=1.5, col=c("blue","black","red"),lty=1)
title(paste("LST number of valid observations for Oregon", "2010",sep=" "))
# savePlot("lst_nobs_OR.png",type="png")
# 
#### Fig 9...#####

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