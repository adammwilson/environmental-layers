#####################################  METHOD COMPARISON ##########################################
#################################### Spatial Analysis ########################################
#This script utilizes the R ojbects created during the interpolation phase.                       #
#At this stage the script produces figures of various accuracy metrics and compare methods:       #
#- multisampling plots                                                                            #
#- spatial accuracy in terms of distance to closest station                                       #
#- spatial density of station network and accuracy metric                                         # 
#AUTHOR: Benoit Parmentier                                                                        #
#DATE: 09/25/2012                                                                                 #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#??--                                    #
###################################################################################################

###Loading R library and packages                                                      
#library(gtools)                                        # loading some useful tools 
library(mgcv)                                           # GAM package by Wood 2006 (version 2012)
library(sp)                                             # Spatial pacakge with class definition by Bivand et al. 2008
library(spdep)                                          # Spatial package with methods and spatial stat. by Bivand et al. 2012
library(rgdal)                                          # GDAL wrapper for R, spatial utilities (Keitt et al. 2012)
library(gstat)                                          # Kriging and co-kriging by Pebesma et al. 2004
library(automap)                                        # Automated Kriging based on gstat module by Hiemstra et al. 2008
library(spgwr)
library(gpclib)
library(maptools)
library(graphics)
library(parallel)                            # Urbanek S. and Ripley B., package for multi cores & parralel processing
library(raster)
library(rasterVis)
library(plotrix)   #Draw circle on graph
library(reshape)
## Functions
#loading R objects that might have similar names
load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

###Parameters and arguments

infile1<- "ghcn_or_tmax_covariates_06262012_OR83M.shp"    #GHCN shapefile containing variables for modeling 2010                 
#infile2<-"list_10_dates_04212012.txt"                    #List of 10 dates for the regression
infile2<-"list_365_dates_04212012.txt"                    #list of dates
infile3<-"LST_dates_var_names.txt"                        #LST dates name
infile4<-"models_interpolation_05142012.txt"              #Interpolation model names
infile5<-"mean_day244_rescaled.rst"                       #mean LST for day 244
inlistf<-"list_files_05032012.txt"                        #list of raster images containing the Covariates
infile6<-"OR83M_state_outline.shp"
#stat_loc<-read.table(paste(path,"/","location_study_area_OR_0602012.txt",sep=""),sep=",", header=TRUE)


obj_list<-"list_obj_08262012.txt"                                  #Results of fusion from the run on ATLAS
path<-"/home/parmentier/Data/IPLANT_project/methods_interpolation_comparison" #Jupiter LOCATION on Atlas for kriging                              #Jupiter Location on XANDERS
#path<-"/Users/benoitparmentier/Dropbox/Data/NCEAS/Oregon_covariates/"            #Local dropbox folder on Benoit's laptop
setwd(path) 
proj_str="+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
                                                                                #Number of kriging model
out_prefix<-"methods_09262012_"                                              #User defined output prefix

sampling_CAI<-load_obj("results2_CAI_sampling_obj_09132012_365d_GAM_CAI2_multisampling2.RData")
sampling_fus<-load_obj("results2_fusion_sampling_obj_10d_GAM_fusion_multisamp4_09192012.RData")
fus_CAI_mod<-load_obj("results2_CAI_Assessment_measure_all_09132012_365d_GAM_CAI2_multisampling2.RData")
gam_fus_mod1<-load_obj("results2_fusion_Assessment_measure_all_10d_GAM_fusion_multisamp4_09192012.RData")

filename<-sub(".shp","",infile1)             #Removing the extension from file.
ghcn<-readOGR(".", filename)                 #reading shapefile 

#CRS<-proj4string(ghcn)                       #Storing projection information (ellipsoid, datum,etc.)
lines<-read.table(paste(path,"/",inlistf,sep=""), sep="")                      #Column 1 contains the names of raster files
inlistvar<-lines[,1]
inlistvar<-paste(path,"/",as.character(inlistvar),sep="")
covar_names<-as.character(lines[,2])                                         #Column two contains short names for covaraites

s_raster<- stack(inlistvar)                                                  #Creating a stack of raster images from the list of variables.
layerNames(s_raster)<-covar_names                                            #Assigning names to the raster layers
projection(s_raster)<-proj_str

#Create mask
pos<-match("LC10",layerNames(s_raster))
LC10<-subset(s_raster,pos)
LC10[is.na(LC10)]<-0   #Since NA values are 0, we assign all zero to NA
mask_land<-LC10<100
mask_land_NA<-mask_land
mask_land_NA[mask_land_NA==0]<-NA

data_name<-"mask_land_OR"
raster_name<-paste(data_name,".rst", sep="")
writeRaster(mask_land, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)
#writeRaster(r2, filename=raster_name,overwrite=TRUE)  #Writing the data in a raster file format...(IDRISI)

pos<-match("mm_01",layerNames(s_raster))
mm_01<-subset(s_raster,pos)
mm_01<-mm_01-273.15
mm_01<-mask(mm_01,mask_land_NA)
#mention this is the last... files

### RESULTS COMPARISON

### CODE BEGIN #####

### PART I MULTISAMPLING COMPARISON ####

tb_diagnostic<-sampling_CAI$tb
tb_diagnostic2<-sampling_fus$tb

tb_diagnostic[["prop"]]<-as.factor(tb_diagnostic[["prop"]])
tb_diagnostic2[["prop"]]<-as.factor(tb_diagnostic2[["prop"]])

#Preparing the data for the plot
#fus data
t<-melt(tb_diagnostic,
        measure=c("mod1","mod2","mod3","mod4", "mod5", "mod6", "mod7", "mod8","mod9"), 
        id=c("dates","metric","prop"),
        na.rm=F)
avg_tb<-cast(t,metric+prop~variable,mean)
sd_tb<-cast(t,metric+prop~variable,sd)
n_tb<-cast(t,metric+prop~variable,length)
avg_tb[["prop"]]<-as.numeric(as.character(avg_tb[["prop"]]))
avg_RMSE<-subset(avg_tb,metric=="RMSE")

#CAI data
t2<-melt(tb_diagnostic2,
        measure=c("mod1","mod2","mod3","mod4", "mod5", "mod6", "mod7", "mod8","mod9"), 
        id=c("dates","metric","prop"),
        na.rm=F)
avg_tb2<-cast(t2,metric+prop~variable,mean)
sd_tb2<-cast(t2,metric+prop~variable,sd)
n_tb2<-cast(t2,metric+prop~variable,length)
avg_tb2[["prop"]]<-as.numeric(as.character(avg_tb2[["prop"]]))
avg_RMSE2<-subset(avg_tb2,metric=="RMSE")

#Select only information related to FUSION

x<-avg_RMSE[["prop"]]
i=9
mod_name<-paste("mod",i,sep="")
y<-avg_RMSE[[mod_name]]

sd_tb_RMSE <- subset(sd_tb, metric=="RMSE") 
x_sd<-sd_tb_RMSE[["prop"]]
i=9
mod_name<-paste("mod",i,sep="")
y_sd<-sd_tb_RMSE[[mod_name]]

#Select only information related to CAI

x2<-avg_RMSE2[["prop"]]
i=9
mod_name<-paste("mod",i,sep="")
y2<-avg_RMSE2[[mod_name]]

sd_tb_RMSE2 <- subset(sd_tb2, metric=="RMSE") 
x_sd2<-sd_tb_RMSE2[["prop"]]
i=9
mod_name<-paste("mod",i,sep="")
y_sd2<-sd_tb_RMSE2[[mod_name]]

n=150
ciw   <- qt(0.975, n) * y_sd / sqrt(n)
ciw2   <- qt(0.975, n) * y_sd2 / sqrt(n)

X11()
plotCI(y=y, x=x, uiw=ciw, col="black", main=" FUS: RMSE proportion of hold out", barcol="blue", lwd=1)
lines(x,y,col="grey")
plotCI(y=y2, x=x2, uiw=ciw2, col="black", main=" CAI: RMSE proportion of hold out", barcol="blue", lwd=1)
lines(x2,y2,col="grey")
plot(x,y,col="grey",type="b")
lines(x2,y2,col="blue")

savePlot(paste("fus_CAI_multisapling_",out_prefix,".png", sep=""), type="png")
dev.off()


### PART II SPATIAL PATTERN COMPARISON: TEMPORAL PROFILES ####


#gam_fus_mod1<-method_mod$gam_fus_mod1
#fus_CAI_mod<- method_mod$fus_CAI_mod
#gwr_mod<-method_mod$gwr_mod
l_f<-list.files(pattern="*tmax_predicted.*fusion5.rst$") #Search for files in relation to fusion
l_f2<-list.files(pattern="CAI_tmax_predicted.*_GAM_CAI2.rst$")
inlistpred<-paste(path,"/",as.character(l_f),sep="")
inlistpred2<-paste(path,"/",as.character(l_f2),sep="")

fus_rast<- stack(inlistpred)                                                  #Creating a stack of raster images from the list of variables.
cai_rast<- stack(inlistpred2)                                                  #Creating a stack of raster images from the list of variables.

id<-unique(ghcn$station)
ghcn_id<-as.data.frame(subset(ghcn,select=c("station","x_OR83M","y_OR83M")))

ghcn_melt<-melt(ghcn_id,
                measure=c("x_OR83M","y_OR83M"), 
                id=c("station"),
                na.rm=F)

ghcn_cast<-cast(ghcn_melt,station~variable,mean)
ghcn_locs<-as.data.frame(ghcn_cast)

coords<- ghcn_locs[,c('x_OR83M','y_OR83M')]
coordinates(ghcn_locs)<-coords
proj4string(ghcn_locs)<-proj_str  #Need to assign coordinates...

tmp<-extract(fus_rast,ghcn_locs)
tmp2<-extract(cai_rast,ghcn_locs)

tmp_names<-paste("fusd",seq(1,365),sep="")
colnames(tmp)<-tmp_names
tmp_names<-paste("caid",seq(1,365),sep="")
colnames(tmp2)<-tmp_names
ghcn_fus_pred<-cbind(as.data.frame(ghcn_locs),as.data.frame(tmp))
ghcn_cai_pred<-cbind(as.data.frame(ghcn_locs),as.data.frame(tmp2))

write.table(ghcn_fus_pred,file="extract3_fus_y2010.txt",sep=",")
write.table(ghcn_cai_pred,file="extract3_cai_y2010.txt",sep=",")

ghcn$value[ghcn$value< -150 | ghcn$value>400]<-NA #screenout values out of range
ghcn$value<-ghcn$value/10
ghcn_m<-melt(as.data.frame(ghcn),
                measure=c("value"), 
                id=c("station","date"),
                na.rm=F)

ghcn_mc<-cast(ghcn_m,station~date~variable,mean) #This creates an array of dimension 186,366,1

ghcn_value<-as.data.frame(ghcn_mc[,,1])
ghcn_value<-cbind(ghcn_locs,ghcn_value[,1:365])
write.table(ghcn_value,na="",file="extract3_dailyTmax_y2010.txt",sep=",")

id<-c("USW00094261","USW00004141","USC00356252","USC00357208")
m<-match(id,ghcn_locs$station)
dat_id<-ghcn_locs[m,]  #creating new subset
#dat_id<-subset(ghcn_locs[gj])

filename<-sub(".shp","",infile6)             #Removing the extension from file.
reg_outline<-readOGR(".", filename)                 #reading shapefile 
X11()
s.range <- c(min(minValue(mm_01)), max(maxValue(mm_01)))
col.breaks <- pretty(s.range, n=50)
lab.breaks <- pretty(s.range, n=5)
temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
plot(mm_01, breaks=col.breaks, col=temp.colors(length(col.breaks)-1),
     axis=list(at=lab.breaks, labels=lab.breaks))
plot(reg_outline, add=TRUE)
plot(dat_id,cex=1.5,add=TRUE)
title("Selected stations for comparison",line=3)
title("(Background: mean January LST)", cex=0.5, line=2)
coords<-coordinates(dat_id)
text(x=coords[,1],y=coords[,2],labels=id,cex=0.8, adj=c(0,1),offset=2) #c(0,1) for lower right corner!
savePlot(paste("temporal_profile_station_locations_map",out_prefix,".png", sep=""), type="png")
dev.off()

stat_list<-vector("list",3 )
stat_list[[1]]<-ghcn_fus_pred
stat_list[[2]]<-ghcn_cai_pred
stat_list[[3]]<-ghcn_value
ac_temp<-matrix(NA,length(id),2)

#id<-ghcn_value$station #if runinng on all the station...
for (i in 1:length(id)){
  m1<-match(id[i],ghcn_fus_pred$station)
  m2<-match(id[i],ghcn_cai_pred$station)
  m3<-match(id[i],ghcn_value$station)
  y1<-as.numeric(ghcn_fus_pred[m1,6:ncol(ghcn_fus_pred)])
  y2<-as.numeric(ghcn_cai_pred[m2,6:ncol(ghcn_cai_pred)])
  y3<-as.numeric(ghcn_value[m3,6:ncol(ghcn_value)])
  res2<-y2-y3
  res1<-y1-y3
  x<-1:365
  X11(6,15)
  plot(x,y1,type="l",col="black")
  lines(x,y2,col="blue")
  lines(x,y3,col="red")
  title(paste("temporal profile for station ", id[i],sep=""))
  # add a legend
  legend("topright",legend=c("fus","CAI","OBS"), cex=1.2, col=c("black","blue","red"),
         lty=1, title="tmax")
  savePlot(paste("Temporal_profile_",id[i],out_prefix,".png", sep=""), type="png")
  zero<-rep(0,365)
  plot(x,res2,type="l",col="black")
  lines(x,res1,col="blue")
  lines(x,zero,col="red")
  legend("topright",legend=c("fus","CAI"), cex=1.2, col=c("black","blue"),
         lty=1, title="tmax")
  savePlot(paste("Temporal_profile_res",id[i],out_prefix,".png", sep=""), type="png")
  
  ac_temp[i,1]<-mean(abs(res1),na.rm=T)
  ac_temp[i,2]<-mean(abs(res2),na.rm=T)
  dev.off()
}
ac_temp<-as.data.frame(ac_temp)
ac_temp$station<-id
names(ac_temp)<-c("fus","CAI","station")

id<-ghcn_value$station #if runinng on all the station...
ac_temp2<-matrix(NA,length(id),2)

for (i in 1:length(id)){
  m1<-match(id[i],ghcn_fus_pred$station)
  m2<-match(id[i],ghcn_cai_pred$station)
  m3<-match(id[i],ghcn_value$station)
  y1<-as.numeric(ghcn_fus_pred[m1,6:ncol(ghcn_fus_pred)])
  y2<-as.numeric(ghcn_cai_pred[m2,6:ncol(ghcn_cai_pred)])
  y3<-as.numeric(ghcn_value[m3,6:ncol(ghcn_value)])
  res2<-y2-y3
  res1<-y1-y3
  ac_temp2[i,1]<-mean(abs(res1),na.rm=T)
  ac_temp2[i,2]<-mean(abs(res2),na.rm=T)
}  

ac_temp2<-as.data.frame(ac_temp2)
ac_temp2$station<-id
names(ac_temp2)<-c("fus","CAI","station")

ac_temp2<-ac_temp2[order(ac_temp2$fus,ac_temp2$CAI), ]
ghcn_MAE<-merge(ghcn_locs,ac_temp2,by.x=station,by.y=station)
##### USING TEMPORAL IMAGES...

date_list<-vector("list", length(l_f))
for (k in 1:length(l_f)){
  tmp<-(unlist(strsplit(l_f[k],"_"))) #spliting file name to obtain the prediction date
  date_list[k]<-tmp[4] 
}



date_list2<-vector("list", length(l_f2))
for (k in 1:length(l_f2)){
  tmp<-(unlist(strsplit(l_f2[k],"_"))) #spliting file name to obtain the prediction date
  date_list2[k]<-tmp[4] 
}

setdiff(date_list,date_list2)
all.equal(date_list,date_list2) #This checks that both lists are equals

list_fus_data_s<-vector("list", 365)
list_cai_data_s<-vector("list", 365)
list_fus_data_v<-vector("list", 365)
list_cai_data_v<-vector("list", 365)

list_fus_data<-vector("list", 365)
list_cai_data<-vector("list", 365)

list_dstspat_er<-vector("list", 365)
k=1
for (k in 1:365){
  
  #Start loop over the full year!!!
  names(gam_fus_mod1[[k]])
  data_s<-gam_fus_mod1[[k]]$data_s
  data_v<-gam_fus_mod1[[k]]$data_v
  
  date_proc<-unique(data_s$date)
  index<-match(as.character(date_proc),unlist(date_list)) #find the correct date..
  #raster_pred<-raster(rp_raster,index)
  raster_pred<-raster(l_f[[index]])
  layerNames(raster_pred)<-"y_pred"
  projection(raster_pred)<-proj_str
  pred_sgdf<-as(raster_pred,"SpatialGridDataFrame") #Conversion to spatial grid data frame
  
  rpred_val_s <- overlay(pred_sgdf,data_s)             #This overlays the kriged surface tmax and the location of weather stations
  rpred_val_v <- overlay(pred_sgdf,data_v)             #This overlays the kriged surface tmax and the location of weather stations
  
  pred_mod<-"pred_fus" #Change for the name of the method
  #Adding the results back into the original dataframes.
  data_s[[pred_mod]]<-rpred_val_s$y_pred
  
  data_v[[pred_mod]]<-rpred_val_v$y_pred  
  res_mod_s<- data_s$dailyTmax - data_s[[pred_mod]]           #Residuals from kriging training
  res_mod_v<- data_v$dailyTmax - data_v[[pred_mod]]           #Residuals from kriging validation
  
  res_mod<-"res_fus"
  data_v[[res_mod]]<-res_mod_v
  data_s[[res_mod]]<-res_mod_s
  
  #####second series added
  data_v2<-fus_CAI_mod[[k]]$data_v
  data_s2<-fus_CAI_mod[[k]]$data_s
  
  date_proc<-unique(data_s$date)
  index<-match(as.character(date_proc),unlist(date_list)) #find the correct date..
  #raster_pred<-raster(rp_raster,index)
  raster_pred2<-raster(l_f2[[index]])
  layerNames(raster_pred2)<-"y_pred"
  projection(raster_pred2)<-proj_str
  pred_sgdf2<-as(raster_pred2,"SpatialGridDataFrame") #Conversion to spatial grid data frame
  
  rpred_val_s2 <- overlay(pred_sgdf2,data_s2)             #This overlays the kriged surface tmax and the location of weather stations
  rpred_val_v2 <- overlay(pred_sgdf2,data_v2)             #This overlays the kriged surface tmax and the location of weather stations
  
  pred_mod2<-"pred_CAI" #Change for the name of the method
  #Adding the results back into the original dataframes.
  data_s2[[pred_mod2]]<-rpred_val_s2$y_pred
  
  data_v2[[pred_mod2]]<-rpred_val_v2$y_pred  
  res_mod_s2<- data_s2$dailyTmax - data_s2[[pred_mod2]]           #Residuals from kriging training
  res_mod_v2<- data_v2$dailyTmax - data_v2[[pred_mod2]]           #Residuals from kriging validation
  
  res_mod2<-"res_CAI"
  data_v2[[res_mod2]]<-res_mod_v2
  data_s2[[res_mod2]]<-res_mod_s2
  
  ###Checking if training and validation have the same columns
  nd<-setdiff(names(data_s),names(data_v))
  nd2<-setdiff(names(data_s2),names(data_v2))
  
  data_v[[nd]]<-NA #daily_delta is not the same
  
  data_v$training<-rep(0,nrow(data_v))
  data_v2$training<-rep(0,nrow(data_v2))
  data_s$training<-rep(1,nrow(data_s))
  data_s2$training<-rep(1,nrow(data_s2))
  
  #if length(nd)!=0 {
  #  for (j in 1:length(nd))
  #  data_s
  #}
  
  list_fus_data_s[[k]]<-data_s
  list_cai_data_s[[k]]<-data_s2
  list_fus_data_v[[k]]<-data_v
  list_cai_data_v[[k]]<-data_v2
  list_fus_data[[k]]<-rbind(data_v,data_s)
  list_cai_data[[k]]<-rbind(data_v2,data_s2)
  
  d_s_v<-matrix(0,nrow(data_v),nrow(data_s))
  for(i in 1:nrow(data_s)){
    pt<-data_s[i,]
    d_pt<-(spDistsN1(data_v,pt,longlat=FALSE))/1000  #Distance to stataion i in km
    d_s_v[,i]<-d_pt
  }
  
  #Create data.frame with position, ID, dst and residuals...
  pos<-vector("numeric",nrow(data_v))
  y<-vector("numeric",nrow(data_v))
  dst<-vector("numeric",nrow(data_v))
  for (i in 1:nrow(data_v)){
    pos[i]<-match(min(d_s_v[i,]),d_s_v[i,])
    dst[i]<-min(d_s_v[i,]) 
  }
  
  #Check if 8 models exist in data_v, if it doesn't then add column with name and "NA"
  #mod_name<-paste(rep("res_mod",8),1:8,sep="")
  #t2<-match(names(data_v),mod_name)
  #dstspat_er<-as.data.frame(cbind(as.vector(data_v$id),as.vector(data_s$id[pos]),pos, dst,res_mod_v))
  dstspat_er<-as.data.frame(cbind(as.vector(data_v$id),as.vector(data_s$id[pos]),pos, data_v$lat, data_v$lon, data_v$x_OR83M,data_v$y_OR83M,
                                  dst,
                                  res_mod_v,
                                  data_v$res_mod1,
                                  data_v$res_mod2,
                                  data_v$res_mod3,
                                  data_v$res_mod4,
                                  data_v$res_mod5,
                                  res_mod_v2))
  
  names(dstspat_er)[1:7]<-c("v_id","s_id","pos","lat","lon","x_OR83M","y_OR83M")
  names(dstspat_er)[10:15]<-c("res_mod1","res_mod2","res_mod3","res_mod4","res_mod5","res_CAI")
  list_dstspat_er[[k]]<-dstspat_er

}  
save(list_dstspat_er,file="spat_ac5.RData")
#obj_tmp2<-load_obj("spat_ac4.RData")
save(list_fus_data,file="list_fus_data_combined.RData")
save(list_cai_data,file="list_cai_data_combined.RData")

save(list_fus_data_s,file="list_fus_data_s_combined.RData")
save(list_cai_data_s,file="list_cai_data_s_combined.RData")
save(list_fus_data_v,file="list_fus_data_v_combined.RData")
save(list_cai_data_v,file="list_cai_data_v_combined.RData")

for (k in 1:365){
  data_s<-as.data.frame(list_fus_data_s[[k]])
  data_v<-as.data.frame(list_fus_data_v[[k]])
  list_fus_data[[k]]<-rbind(data_s,data_v)
  data_s2<-as.data.frame(list_cai_data_s[[k]])
  data_v2<-as.data.frame(list_cai_data_v[[k]])
  list_cai_data[[k]]<-rbind(data_s2,data_v2)
}
data_fus<-do.call(rbind.fill,list_fus_data)
data_cai<-do.call(rbind.fill,list_cai_data)

data_fus_melt<-melt(data_fus,
                   measure=c("x_OR83M","y_OR83M","res_fus","res_mod1","res_mod2","res_mod3","res_mod4","res_mod5","pred_fus","dailyTmax","TMax","LST","training"), 
                   id=c("id","date"),
                   na.rm=F)
data_fus_cast<-cast(data_fus_melt,id+date~variable,mean)
id1="USC00350036"
id2="USW00004128"
dat_id1<-subset(data_fus_cast,id==id1)
dat_id2<-subset(data_fus_cast,id==id2)
write.table(dat_id1,file=paste("station_",id1,".txt",sep=""),sep=",")
#list_fus_data<-vector("list", 365)
#list_cai_data<-vector("list", 365)

test_dst<-list_dstspat_er
test<-do.call(rbind,list_dstspat_er)

for(i in 4:ncol(test)){            # start of the for loop #1
  test[,i]<-as.numeric(as.character(test[,i]))	
}

# Plot results
plot(test$dst,abs(test$res_mod_v))
limit<-seq(0,150, by=10)
tmp<-cut(test$dst,breaks=limit)
erd1<-tapply(test$res_mod_v,tmp, mean)
erd2<-as.numeric(tapply(abs(test$res_mod_v),tmp, mean))
plot(erd2)

erd1_mod1<-tapply(test$res_mod1,tmp, mean)
erd2_mod1<-tapply(abs(test$res_mod1),tmp, mean)
erd2_mod2<-tapply(abs(test$res_mod2),tmp, mean)
erd2_mod3<-tapply(abs(test$res_mod3),tmp, mean)
erd2_mod4<-tapply(abs(test$res_mod4),tmp, mean)
erd2_mod5<-tapply(abs(test$res_mod5),tmp, mean)
erd2_CAI<-tapply(abs(test$res_CAI),tmp, mean)
n<-tapply(abs(test$res_mod1),tmp, length)
distance<-seq(5,145,by=10)

X11()
plot(distance,erd2,ylim=c(1,4), type="b", col="red",ylab=" Average MAE", 
     xlab="distance to closest training station (km)")
lines(distance,erd2_mod1,col="black")
lines(distance,erd2_mod2,col="green")
lines(distance,erd2_mod3,col="blue")
lines(distance,erd2_mod4,col="yellow")
lines(distance,erd2_mod5,col="pink")
lines(distance,erd2_CAI,col="grey")

# add a title and subtitle 
title("MAE in terms of distance to closest station GAM and FUSION")

#colused<-
# add a legend 
#legend("bottomright",legend=1:(5), cex=1.2, col=colors,
       #pch=plotchar, lty=linetype, title="mod")
savePlot(paste("Comparison_models_er_spat",out_prefix,".png", sep=""), type="png")
dev.off()

means <- erd2_CAI
means2<- erd2
stdev <-tapply(abs(test$res_CAI),tmp, sd)
stdev2 <-tapply(abs(test$res_mod_v),tmp, sd)

ciw   <- qt(0.975, n) * stdev / sqrt(n)
ciw2   <- qt(0.975, n) * stdev2 / sqrt(n)

X11()
plotCI(y=means, x=distance, uiw=ciw, col="black", main=" CAI: MAE and distance to clostest training station", barcol="blue", lwd=1)
lines(distance,erd2_CAI,col="grey")
savePlot(paste("CI_CAI_er_spat_",out_prefix,".png", sep=""), type="png")
dev.off()

X11()
plotCI(y=means2, x=distance, uiw=ciw2, col="black", main=" FUSION: MAE and distance to clostest training station", barcol="blue", lwd=1)
lines(distance,erd2,col="black")
savePlot(paste("CI_fusion_er_spat_",out_prefix,".png", sep=""), type="png")
dev.off()

X11()
barplot(n,names.arg=as.character(distance))
savePlot(paste("Barplot_freq_er_spat_",out_prefix,".png", sep=""), type="png")
dev.off()

### Average MAE per station and coarse grid box (0.5 deg)

test$abs_res_fus<-abs(test$res_mod_v)
test$abs_res_CAI<-abs(test$res_CAI)

station_melt<-melt(test,
        measure=c("x_OR83M","y_OR83M","res_mod_v","res_mod1","res_mod2","res_mod3","res_mod4","res_mod5","abs_res_fus","abs_res_CAI"), 
        id=c("v_id"),
        na.rm=F)
station_v_er<-cast(station_melt,v_id~variable,mean)
#station_v_er2<-as.data.frame(station_v_er)
station_v_er<-as.data.frame(station_v_er)
oc<-vector("numeric",nrow(station_v_er))
oc<-oc+1
station_v_er$oc<-oc

unique(ghcn$id)

coords<- station_v_er[,c('x_OR83M','y_OR83M')]
coordinates(station_v_er)<-coords
proj4string(station_v_er)<-CRS  #Need to assign coordinates...

bubble(station_v_er,"abs_res_fus")

rast_agg<-aggregate(raster_pred,fact=50,fun=mean,na.rm=TRUE) #Changing the raster resolution by aggregation factor
rast_MAE_fus<-rasterize(station_v_er,rast_agg,"abs_res_fus",na.rm=TRUE,fun=mean)
rast_MAE_CAI<-rasterize(station_v_er,rast_agg,"abs_res_CAI",na.rm=TRUE,fun=mean)
rast_oc<-rasterize(station_v_er,rast_agg,"oc",na.rm=TRUE,fun=sum)
ac_agg50<-as.data.frame(values(rast_oc))
ac_agg50$MAE_fus<-as.numeric(values(rast_MAE_fus))
ac_agg50$MAE_CAI<-as.numeric(values(rast_MAE_CAI))
names(ac_agg50)<-c("oc","MAE_fus","MAE_CAI")

ghcn_sub<-as.data.frame(subset(ghcn, select=c("station","x_OR83M","y_OR83M")))
ghcn_sub_melt<-melt(ghcn_sub,
                   measure=c("x_OR83M","y_OR83M"), 
                   id=c("station"),
                   na.rm=F)
ghcn_stations<-as.data.frame(cast(ghcn_sub_melt,station~variable,mean))
coords<- ghcn_stations[,c('x_OR83M','y_OR83M')]
coordinates(ghcn_stations)<-coords
proj4string(ghcn_stations)<-CRS  #Need to assign coordinates...
oc_all<-vector("numeric",nrow(ghcn_stations))
oc_all<-oc_all+1

ghcn_stations$oc_all<-oc_all
rast_oc_all<-rasterize(ghcn_stations,rast_agg,"oc_all",na.rm=TRUE,fun=sum)
ac_agg50$oc_all<-values(rast_oc_all)

td1<-aggregate(MAE_fus~oc,data=ac_agg50,mean) 
td2<-aggregate(MAE_CAI~oc,data=ac_agg50,mean)
td<-merge(td1,td2,by="oc")

td1_all<-aggregate(MAE_fus~oc_all,data=ac_agg50,mean) 
td2_all<-aggregate(MAE_CAI~oc_all,data=ac_agg50,mean)
td_all<-merge(td1_all,td2_all,by="oc_all")

plot(MAE_fus~oc,data=td,type="b")
lines(td$oc,td$MAE_CAI, type="b", lwd=1.5,co="red")
plot(MAE_fus~oc_all,data=td_all,type="b")
lines(td_all$oc_all,td_all$MAE_CAI, type="b", lwd=1.5,co="red")

filename<-sub(".shp","",infile6)             #Removing the extension from file.
reg_outline<-readOGR(".", filename)                 #reading shapefile 
plot(rast_MAE_fus, main="Fusion MAE in coarsened 50km grid")
plot(reg_outline, add=TRUE)

plot(rast_MAE_CAI, main="CAI MAE in coarsened 50km grid")
plot(reg_outline, add=TRUE)

plot(rast_oc, main="Number of val stations in coarsened 50km grid")
plot(reg_outline, add=TRUE)
plot(rast_oc_all, main="Number of stations in coarsened 50km grid")
plot(reg_outline, add=TRUE)

list_var_stat<-vector("list", 365)
#list_var_stat<-vector("list", 2)
#k=2
for (k in 1:length(l_f)){
  
  raster_pred<-raster(l_f[[k]])
  layerNames(raster_pred)<-"fus"
  projection(raster_pred)<-proj_str
  
  raster_pred2<-raster(l_f2[[k]])
  layerNames(raster_pred2)<-"fus"
  projection(raster_pred2)<-proj_str
  
  tmp_rast<-mask(raster_pred2,raster_pred)
  
  t1<-cellStats(raster_pred,na.rm=TRUE,stat=sd)
  t2<-cellStats(raster_pred2,na.rm=TRUE,stat=sd)
  t2_b<-cellStats(tmp_rast,na.rm=TRUE,stat=sd)
  
  m1<-Moran(raster_pred,w=3)
  m2<-Moran(tmp_rast,w=3)
  stat<-as.data.frame(t(c(m1,m2,t1,t2)))
  names(stat)<-c("moran_fus","moran_CAI","sd_fus","sd_CAI")
  list_var_stat[[k]]<-stat
}

var_stat<-do.call(rbind,list_var_stat)


pos<-match("ELEV_SRTM",layerNames(s_raster)) #Find column with name "value"
elev<-raster(s_raster,layer=pos)             #Select layer from stack
elev<-mask(elev,raster_pred)
te<-cellStats(elev,na.rm=TRUE,stat=sd)

pos<-match("mm_12",layerNames(s_raster)) #Find column with name "value"
m_12<-raster(s_raster,layer=pos)             #Select layer from stack
m_LST<-Moran(m_12,w=3)
m_e<-Moran(elev,w=3)
m_12<-m_12-273.15
plot(MAE_fus~oc,data=td,type="b")
lines(td$oc,td$MAE_CAI, type="b", lwd=1.5,co="red")

data_dist<-as.data.frame(cbind(distance,erd2,erd2_mod1,erd2_mod2,erd2_mod3,erd2_mod4,erd2_mod5,erd2_CAI,n))
rownames(data_dist)<-NULL

#PLOTING CAI AND FUSION TO COMPARE

infile2<-"list_10_dates_04212012.txt"                             #List of 10 dates for the regression
dates2<-read.table(paste(path,"/",infile2,sep=""), sep="")         #Column 1 contains the names of raster files
date_list2<-as.list(as.character(dates2[,1]))


for (k in 1:length(date_list2)){
  
  date_proc2<-date_list2[[k]]
  #date_proc<-date_list[[k]]
  index<-match(as.character(date_proc2),unlist(date_list)) #find the correct date... in the 365 stack
  #raster_pred<-raster(rp_raster,index)
  raster_pred1<-raster(l_f[[index]])
  projection(raster_pred1)<-proj_str
  raster_pred1<-mask(raster_pred1,mask_land_NA)
  
  raster_pred2<-raster(l_f2[[index]])
  projection(raster_pred2)<-proj_str
  raster_pred2<-mask(raster_pred2,mask_land_NA)
  
  predictions <- stack(raster_pred1,raster_pred2)
  layerNames(predictions)<-c(paste('fusion',date_list2[[k]],sep=" "),paste('CAI',date_list2[[k]],sep=" "))
  # use overall min and max values to generate an nice, consistent set
  # of breaks for both colors (50 values) and legend labels (5 values)
  s.range <- c(min(minValue(predictions)), max(maxValue(predictions)))
  col.breaks <- pretty(s.range, n=50)
  lab.breaks <- pretty(s.range, n=5)
  temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
  
  # plot using these (common) breaks; note use of _reverse_ heat.colors,
  # making it so that larger numbers are redder
  X11(6,12)
  #plot(predictions, breaks=col.breaks, col=rev(heat.colors(length(col.breaks)-1)),
    #   axis=list(at=lab.breaks, labels=lab.breaks))
  plot(predictions, breaks=col.breaks, col=temp.colors(length(col.breaks)-1),
       axis=list(at=lab.breaks, labels=lab.breaks))
  
  savePlot(paste("comparison_raster1_CAI_fusion_tmax_prediction_",date_list2[[k]],out_prefix,".png", sep=""), type="png")
  diff<-raster_pred1-raster_pred2
  s.range <- c(min(minValue(dif)), max(maxValue(d)))
  
  plot(diff,col=temp.colors(50))
  savePlot(paste("comparison_raster1_diff_CAI_fusion_tmax_prediction_",date_list2[[k]],out_prefix,".png", sep=""), type="png")
  
  
  
  #hist(predictions, freq=FALSE,maxpixels=ncells(predictions))
  hist(predictions, breaks=col.breaks,freq=FALSE,maxpixels=ncells(predictions))
  savePlot(paste("comparison_histo_CAI_fusion_tmax_prediction_",date_list2[[k]],out_prefix,".png", sep=""), type="png")
  #plot(predictions)
  dev.off()
  
}  


write.table(data_dist,file=paste("data_dist_",out_prefix,".txt",sep=""),sep=",")
write.table(test,file=paste("ac_spat_dist",out_prefix,".txt",sep=""),sep=",")
write.table(var_stat,file=paste("moran_var_stat_",out_prefix,".txt",sep=""),sep=",")
write.table(td,file=paste("MAE_density_station_",out_prefix,".txt",sep=""),sep=",")
write.table(td_all,file=paste("MAE_density_station_all_",out_prefix,".txt",sep=""),sep=",")

# VISUALIZATION OF RESULTS PLOTS ACROSS MODELS FOR METHODS

date_selected<-"20100103"

lf_gwr<-list.files(pattern=paste("*",date_selected,".*08152012_1d_gwr4.rst$",sep=""))
lf_krig<-list.files(pattern=paste("*",date_selected,"_07312012_365d_Kriging_autokrig2.rst$",sep=""))
lf_gam<-list.files(pattern=paste("^GAM.*",date_selected,"_07242012_365d_GAM_fusion5.rst$",sep=""))
lf_fus<-list.files(pattern=paste("^fusion_tmax.*",date_selected,"_07242012_365d_GAM_fusion5.rst$",sep="")) #Search for files in relation to fusion
lf_cai<-list.files(pattern=paste("*CAI_tmax_pred.*",date_selected,"*.08072012_365d_GAM_CAI2.rst$",sep="")) #Search for files in relation to fusion
lf_gam2<-list.files(pattern=paste("^GAM.*",date_selected,"_08122012_365d_GAM_fusion6.rst$",sep=""))

d_gwr_rast<-stack(lf_gwr)
d_krig_rast<-stack(lf_krig)
d_gam_rast<-stack(lf_gam)
d_fus_rast<-stack(lf_fus)
d_cai_rast<-stack(lf_cai)
d_gam2_rast<-stack(lf_gam2)

list_day_method<-list(d_gwr_rast,d_krig_rast,d_gam_rast,d_fus_rast,d_cai_rast,d_gam2_rast)
names(list_day_method)<-paste(c("gwr_","krig_","gam1_","fus_","cai_","gam2_"),date_selected,sep="")
out_prefix2<-"_10042012"

for (k in 1:length(list_day_method)){
  
  predictions<-list_day_method[[k]]
  projection(predictions)<-proj_str
  predictions<-mask(predictions,mask_land_NA)
  #layerNames(predictions)<-c(paste('fusion',date_selected,sep=" "),paste('CAI',date_list2[[k]],sep=" "))
  # use overall min and max values to generate an nice, consistent set
  # of breaks for both colors (50 values) and legend labels (5 values)
  s.range <- c(min(minValue(predictions)), max(maxValue(predictions)))
  s.range<-c(-12,18)
  col.breaks <- pretty(s.range, n=60)
  lab.breaks <- pretty(s.range, n=6)
  temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
  
  # plot using these (common) breaks; note use of _reverse_ heat.colors,
  # making it so that larger numbers are redder
  X11(6,12)
  plot(predictions, breaks=col.breaks, col=temp.colors(length(col.breaks)-1),
       axis=list(at=lab.breaks, labels=lab.breaks))
  
  savePlot(paste(names(list_day_method)[[k]],"_method_prediction_",out_prefix2,".png", sep=""), type="png")
  dev.off()  
}

#### END OF THE SCRIPT
