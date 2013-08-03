####################################  INTERPOLATION OF TEMPERATURES  #######################################
############################  Script for manuscript analyses,tables and figures #######################################
#This script reads information concerning the Oregon case study to adapt data for the revised 
# interpolation code.
#Figures and data for the contribution of covariate paper are also produced.
#AUTHOR: Benoit Parmentier                                                                      #
#DATE: 08/02/2013            
#Version: 1
#PROJECT: Environmental Layers project                                       #
#################################################################################################

###Loading R library and packages                                                      
library(gtools)                              # loading some useful tools 
library(mgcv)                                # GAM package by Simon Wood
library(sp)                                  # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(gstat)                               # Kriging and co-kriging by Pebesma et al.
library(fields)                              # NCAR Spatial Interpolation methods such as kriging, splines
library(raster)                              # Hijmans et al. package for raster processing
library(gdata)                               # various tools with xls reading
library(rasterVis)
library(parallel)
library(maptools)
library(maps)
library(reshape)
library(plotrix)

#### FUNCTION USED IN SCRIPT

load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

extract_list_from_list_obj<-function(obj_list,list_name){
  #Create a list of an object from a given list of object using a name prodived as input
  
  list_tmp<-vector("list",length(obj_list))
  for (i in 1:length(obj_list)){
    tmp<-obj_list[[i]][[list_name]] #double bracket to return data.frame
    list_tmp[[i]]<-tmp
  }
  return(list_tmp) #this is  a data.frame
}

calc_stat_from_raster_prediction_obj <-function(raster_prediction_obj,stat){
  tb <-raster_prediction_obj$tb_diagnostic_v  #Kriging methods
  
  t<-melt(tb,
          measure=c("mae","rmse","r","m50"), 
          id=c("pred_mod"),
          na.rm=T)
  
  stat_tb<-cast(t,pred_mod~variable,stat)
  return(stat_tb)
}

#### Parameters and constants  


script_path<-"/home/parmentier/Data/IPLANT_project/env_layers_scripts/"
#source(file.path(script_path,"interpolation_method_day_function_multisampling_06082013.R")) #Include GAM_day

#in_dir<- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_GAM_fus_all_lst_05312013"
in_dir1 <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_lst_comb3_07092013/"
in_dir2 <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_lst_comb4_07152013/"

#kriging results:
in_dir3 <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_kriging_day_lst_comb3_07112013"
#gwr results:
in_dir4 <-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gwr_day_lst_comb3_part1_07122013"
#multisampling results (gam)
in_dir5<- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_mults15_lst_comb3_07232013"
#in_dir<-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_mult_lst_comb3_07202013"

in_dir <- in_dir1
out_dir<-"/home/parmentier/Data/IPLANT_project/paper_analyses_tables_fig_08032013"
setwd(out_dir)

y_var_name <- "dailyTmax"

out_prefix<-"analyses_08032013"

method_interpolation <- "gam_daily"
covar_obj_file1 <- "covar_obj__365d_gam_day_lst_comb3_07092013.RData"

#raster_prediciton object for baseline 1 () s(lat,lon) + s(elev)) and baseline 2 (slat,lon))
raster_obj_file_1 <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_day_lst_comb3_07092013.RData" 
raster_obj_file_2 <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_day_lst_comb4_07152013.RData"

raster_obj_file_3 <- "raster_prediction_obj_kriging_daily_dailyTmax_365d_kriging_day_lst_comb3_07112013.RData"
raster_obj_file_4 <- "raster_prediction_obj_gwr_daily_dailyTmax_365d_gwr_day_lst_comb3_part1_07122013.RData"
raster_obj_file_5 <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_day_mult_lst_comb3_07202013.RData"

#Load objects containing training, testing, models objects 

covar_obj <-load_obj(file.path(in_dir1,covar_obj_file1))
infile_covariates <- covar_obj$infile_covariates
infile_reg_outline <- covar_obj$infile_reg_outline
covar_names<- covar_obj$covar_names
#####
s_raster <- brick(file.path(in_dir1,infile_covariates))
names(s_raster)<-covar_names

raster_prediction_obj_1 <-load_obj(file.path(in_dir1,raster_obj_file_1))
raster_prediction_obj_2 <-load_obj(file.path(in_dir2,raster_obj_file_2))
raster_prediction_obj_3 <-load_obj(file.path(in_dir3,raster_obj_file_3))
raster_prediction_obj_4 <-load_obj(file.path(in_dir4,raster_obj_file_4))

names(raster_prediction_obj_1) #list of two objects

### ACCURACY TABLE WITH BASELINES

#Check input covariates and model formula:
raster_prediction_obj_1$method_mod_obj[[1]]$formulas
raster_prediction_obj_2$method_mod_obj[[1]]$formulas

#baseline 1:

summary_metrics_v1<-raster_prediction_obj_1$summary_metrics_v
summary_metrics_v2<-raster_prediction_obj_2$summary_metrics_v

table_data1 <-summary_metrics_v1$avg[,c("mae","rmse","me","r")]
table_data2 <-summary_metrics_v2$avg[,c("mae","rmse","me","r")]

model_col<-c("Baseline","Northing","Easting","LST","DISTOC","Forest","CANHEIGHT","LST*Forest") # removed ,"LST*CANHEIGHT")
names_table_col<-c("DiffMAE","DiffRMSE","DiffME","Diffr","Model")

df1<- as.data.frame(sapply(table_data1,FUN=function(x) x-x[1]))
df1<- round(df1,digit=3) #roundto three digits teh differences
df1$Model <-model_col
names(df1)<- names_table_col
df1

model_col<-c("Baseline","Elevation","Northing","Easting","LST","DISTOC","Forest","CANHEIGHT") #,"LST*Forest") # removed ,"LST*CANHEIGHT")
df2<- as.data.frame(sapply(table_data2,FUN=function(x) x-x[1]))
df2<- round(df2,digit=3) #roundto three digits teh differences
df2$Model <-model_col
names(df2)<- names_table_col
df2

file_name<-paste("table3a_paper","_",out_prefix,".txt",sep="")
write.table(df1,file=file_name,sep=",")

file_name<-paste("table3b_paper","_",out_prefix,".txt",sep="")
write.table(df2,file=file_name,sep=",")

##Table 4: Interpolation methods comparison

#get sd for kriging, gam and gwr
tb1 <-raster_prediction_obj_1$tb_diagnostic_v  #Kriging methods
tb2 <-raster_prediction_obj_2$tb_diagnostic_v  #Kriging methods
tb3 <-raster_prediction_obj_3$tb_diagnostic_v  #Kriging methods
tb4 <-raster_prediction_obj_4$tb_diagnostic_v  #Kriging methods

names_mod<-c("mod1","mod2","mod3","mod4","mod5","mod6","mod7","mod8","mod9")


sd1 <- calc_stat_from_raster_prediction_obj(raster_prediction_obj_1,"sd")
sd2 <- calc_stat_from_raster_prediction_obj(raster_prediction_obj_2,"sd")
sd3 <- calc_stat_from_raster_prediction_obj(raster_prediction_obj_3,"sd")
sd4 <- calc_stat_from_raster_prediction_obj(raster_prediction_obj_4,"sd")

table_sd<-rbind(sd1[1,],sd3[1,])
table_sd<-rbind(table_sd,sd4[1,])

summary_metrics_v3<-raster_prediction_obj_3$summary_metrics_v  #Kriging methods
summary_metrics_v4<-raster_prediction_obj_4$summary_metrics_v  # GWR method

table_data3 <-summary_metrics_v3$avg[1,c("mae","rmse","me","r")] #first line mod1 (baseline)
table_data4 <-summary_metrics_v4$avg[1,c("mae","rmse","me","r")] #first line mod1 (baseline)
table_data1 <- table_data1[1,]

table<-rbind(table_data1,table_data3)
table<-rbind(table,table_data4)
table<- round(table,digit=3) #roundto three digits teh differences

model_col<-c("GAM","Kriging","GWR")
names_table_col<-c("MAE","RMSE","ME","R","Model")

table$Model <-model_col
names(table)<- names_table_col
table

file_name<-paste("table4_avg_paper","_",out_prefix,".txt",sep="")
write.table(table,file=file_name,sep=",")

file_name<-paste("table34_sd_paper","_",out_prefix,".txt",sep="")
write.table(table_sd,file=file_name,sep=",")

#for(i in nrow(table))
#mean_val<-table[i,j]
#sd_val<-table_sd[i,j]
#element<-paste(mean_val,"+-",sd_val,sep="")
#table__paper[i,j]<-element

#########################
####### Now create figures ###

#figure 1: study area
#figure 2: methodological worklfow
#figure 3:Figure 3. MAE/RMSE and distance to closest fitting station.
#Figure 4. RMSE and MAE, mulitisampling and hold out for FSS and GAM.
#Figure 5. Overtraining tendency

### Figure 1


### Figure 2

# ANALYSES 1: ACCURACY IN TERMS OF DISTANCE TO CLOSEST STATIONS...
#?? for all models gam or only interpolation methods??

tb1<- raster_prediction_obj_1$tb_diagnostic_v

names(raster_prediction_obj$validation_mod_obj[[1]])

list_data_s <- extract_list_from_list_obj(raster_prediction_obj$validation_mod_obj,"data_s")
list_data_v <- extract_list_from_list_obj(raster_prediction_obj$validation_mod_obj,"data_v")

names_mod <- paste("res_mod",1:9,sep="")
