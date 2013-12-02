####################################  INTERPOLATION OF TEMPERATURES  #######################################
############################  Script for manuscript analyses,tables and figures: MULTITIME SCALE  ##############################
#This script uses the worklfow code applied to the Oregon case study. Multitime scale methods (GAM,GWR, Kriging) are tested with
#different covariates using FUSION and CAI. Accuracy methods are added in the the function script to evaluate results.
#Figures, tables and data for the  paper are also produced in the script.
#AUTHOR: Benoit Parmentier 
#CREATED ON: 10/31/2013  
#MODIFIED ON: 12/02/2013            
#Version: 1
#PROJECT: Environmental Layers project                                     
#################################################################################################

### Loading R library and packages        
#library used in the workflow production:
library(gtools)                              # loading some useful tools 
library(mgcv)                                # GAM package by Simon Wood
library(sp)                                  # Spatial pacakge with class definition by Bivand et al.
library(spdep)                               # Spatial pacakge with methods and spatial stat. by Bivand et al.
library(rgdal)                               # GDAL wrapper for R, spatial utilities
library(gstat)                               # Kriging and co-kriging by Pebesma et al.
library(fields)                              # NCAR Spatial Interpolation methods such as kriging, splines
library(raster)                              # Hijmans et al. package for raster processing
library(gdata)                               # various tools with xls reading, cbindX
library(rasterVis)                           # Raster plotting functions
library(parallel)                            # Parallelization of processes with multiple cores
library(maptools)                            # Tools and functions for sp and other spatial objects e.g. spCbind
library(maps)                                # Tools and data for spatial/geographic objects
library(reshape)                             # Change shape of object, summarize results 
library(plotrix)                             # Additional plotting functions
library(plyr)                                # Various tools including rbind.fill
library(spgwr)                               # GWR method
library(automap)                             # Kriging automatic fitting of variogram using gstat
library(rgeos)                               # Geometric, topologic library of functions
#RPostgreSQL                                 # Interface R and Postgres, not used in this script
library(gridExtra)
#Additional libraries not used in workflow
library(pgirmess)                            # Krusall Wallis test with mulitple options, Kruskalmc {pgirmess}  

#### FUNCTION USED IN SCRIPT

function_analyses_paper1 <-"contribution_of_covariates_paper_interpolation_functions_10222013.R"
function_analyses_paper2 <-"multi_timescales_paper_interpolation_functions_12022013.R"

##############################
#### Parameters and constants  

script_path<-"/home/parmentier/Data/IPLANT_project/env_layers_scripts/" #path to script
source(file.path(script_path,function_analyses_paper1)) #source all functions used in this script.
source(file.path(script_path,function_analyses_paper2)) #source all functions used in this script.

#direct methods: gam, kriging, gwr
in_dir1 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_daily_lst_comb5_11012013"
in_dir2 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_daily_lst_comb5_11022013"
in_dir3a <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_daily_lst_comb5p1_3_11062013"
in_dir3b <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_daily_lst_comb5p4_7_11292013"

#CAI: gam, kriging, gwr
in_dir4 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_cai_lst_comb5_11032013"
in_dir5 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_cai_lst_comb5_11032013"
in_dir6 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_cai_lst_comb5_11042013"
#FSS: gam, kriging, gwr
in_dir7 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_fss_lst_comb5_11062013"
in_dir8 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_fss_lst_comb5_11052013"
in_dir9 <- "/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_fss_lst_comb5_11052013"
###

### hold out 0-70

in_dir10 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_fss_lst_mults_0_70_comb5_11082013"
in_dir11 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_fss_lst_mults_0_70_comb5_11132013"
in_dir12 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_fss_lst_mults_0_70_comb5_11162013"
in_dir13 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_cai_lst_mults_0_70_comb5_11192013"
in_dir14 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_cai_lst_mults_0_70_comb5_11272013"
in_dir15 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_cai_lst_mults_0_70_comb5_11222013"

##raster_prediction object for comb5
#direct methods
raster_obj_file_1 <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_daily_lst_comb5_11012013.RData" 
raster_obj_file_2 <- "raster_prediction_obj_kriging_daily_dailyTmax_365d_kriging_daily_lst_comb5_11022013.RData"
raster_obj_file_3a <- "raster_prediction_obj_gwr_daily_dailyTmax_365d_gwr_daily_lst_comb5p1_3_11062013.RData"
raster_obj_file_3b <- "raster_prediction_obj_gwr_daily_dailyTmax_365d_gwr_daily_lst_comb5p4_7_11292013.RData"
#CAI
raster_obj_file_4 <- "raster_prediction_obj_gam_CAI_dailyTmax_365d_gam_cai_lst_comb5_11032013.RData"
raster_obj_file_5 <- "raster_prediction_obj_kriging_CAI_dailyTmax_365d_kriging_cai_lst_comb5_11032013.RData"
raster_obj_file_6 <- "raster_prediction_obj_gwr_CAI_dailyTmax_365d_gwr_cai_lst_comb5_11042013.RData"
#FSS
raster_obj_file_7 <- "raster_prediction_obj_gam_fusion_dailyTmax_365d_gam_fss_lst_comb5_11062013.RData"
raster_obj_file_8 <- "raster_prediction_obj_kriging_fusion_dailyTmax_365d_kriging_fss_lst_comb5_11052013.RData"
raster_obj_file_9 <- "raster_prediction_obj_gwr_fusion_dailyTmax_365d_gwr_fss_lst_comb5_11052013.RData"

## holdout

raster_obj_file_10 <- "raster_prediction_obj_gam_fusion_dailyTmax_365d_gam_fss_lst_mults_0_70_comb5_11082013.RData"
raster_obj_file_11 <- "raster_prediction_obj_kriging_fusion_dailyTmax_365d_kriging_fss_lst_mults_0_70_comb5_11132013.RData"
raster_obj_file_12 <- "raster_prediction_obj_gwr_fusion_dailyTmax_365d_gwr_fss_lst_mults_0_70_comb5_11162013.RData"
raster_obj_file_13 <- "raster_prediction_obj_gam_CAI_dailyTmax_365d_gam_cai_lst_mults_0_70_comb5_11192013.RData"
raster_obj_file_14 <- "raster_prediction_obj_kriging_CAI_dailyTmax_365d_kriging_cai_lst_mults_0_70_comb5_11272013.RData"
raster_obj_file_15 <- "raster_prediction_obj_gwr_CAI_dailyTmax_365d_gwr_cai_lst_mults_0_70_comb5_11222013.RData"

out_dir<-"/home/parmentier/Data/IPLANT_project/paper_multitime_scale__analyses_tables_fig_09032013"
setwd(out_dir)

infile_reg_outline <- "/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/OR83M_state_outline.shp"  #input region outline defined by polygon: Oregon
met_stations_outfiles_obj_file<-"/data/project/layers/commons/data_workflow/output_data_365d_gam_fus_lst_test_run_07172013/met_stations_outfiles_obj_gam_fusion__365d_gam_fus_lst_test_run_07172013.RData"
CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84
y_var_name <- "dailyTmax"
out_prefix<-"analyses_12022013"
ref_rast_name<- "/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/mean_day244_rescaled.rst"                     #This is the shape file of outline of the study area. #local raster name defining resolution, exent, local projection--. set on the fly??
infile_reg_outline <- "/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/OR83M_state_outline.shp"  #input region outline defined by polygon: Oregon
ref_rast_name <-"/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/mean_day244_rescaled.rst"  #local raster name defining resolution, exent: oregon
ref_rast_d001 <-"/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/mean_day001_rescaled.rst"
transect_list <-c("/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/transect_OR_1.shp",
                  "/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/transect_OR_2.shp",
                  "/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/transect_OR_3.shp")

#method_interpolation <- "gam_daily"
#covar_obj_file_1<- list.files(path=in_dir1,pattern="covar_obj.*.RData")
covar_obj_file_1 <- "covar_obj__365d_gam_daily_lst_comb5_11012013.RData"
met_obj_file_1 <- "met_stations_outfiles_obj_gam_daily__365d_gam_daily_lst_comb5_11012013.RData"

#Load objects containing training, testing, models objects 

met_stations_obj <- load_obj(file.path(in_dir1,met_obj_file_1))
covar_obj <-load_obj(file.path(in_dir1,covar_obj_file_1)) #Reading covariates object for GAM daily method
infile_covariates <- covar_obj$infile_covariates
infile_reg_outline <- covar_obj$infile_reg_outline
covar_names<- covar_obj$covar_names
#####
s_raster <- brick(infile_covariates)
names(s_raster)<-covar_names

#raster_prediction_obj_1 <-load_obj(file.path(in_dir1,raster_obj_file_1)) #comb5 gam_daily

############### BEGIN SCRIPT #################

############
##### 1) Generate: Table 4. Contribution of covariates using validation accuracy metrics
## This is a table of accuracy  

list_raster_obj_files  <- list(file.path(in_dir1,raster_obj_file_1),file.path(in_dir2,raster_obj_file_2),
                               file.path(in_dir3a,raster_obj_file_3a),file.path(in_dir3b,raster_obj_file_3b),
                               file.path(in_dir4,raster_obj_file_4),file.path(in_dir5,raster_obj_file_5),
                               file.path(in_dir6,raster_obj_file_6),file.path(in_dir7,raster_obj_file_7),
                               file.path(in_dir8,raster_obj_file_8),file.path(in_dir9,raster_obj_file_9))
 
names(list_raster_obj_files)<- c("gam_daily","kriging_daily","gwr_daily","gwr_daily",
                                 "gam_CAI","kriging_CAI","gwr_CAI",
                                 "gam_fss","kriging_fss","gwr_fss")
summary_metrics_v_list<-lapply(list_raster_obj_files,FUN=function(x){x<-load_obj(x);x[["summary_metrics_v"]]$avg$rmse})                           
names(summary_metrics_v_list)

#for gam models
table_gam<- summary_metrics_v_list[grep("gam",names(summary_metrics_v_list))]
table_gam <-do.call(cbind,table_gam)
table_gam<-table_gam[1:7,]                           
#for kriging models
table_kriging <- summary_metrics_v_list[grep("kriging",names(summary_metrics_v_list))]
table_kriging <- do.call(cbind,table_kriging)
table_kriging <- table_kriging[1:7,]                         

#for gwr models
table_gwr <- summary_metrics_v_list[grep("gwr",names(summary_metrics_v_list))]
table_gwr <- do.call(cbind,table_gwr)
table_gwr <- table_gwr[1:7,]                         

table4_paper <- as.data.frame(do.call(rbind,list(table_gam,table_kriging,table_gwr)))    
table4_paper <- round(table4_paper,digit=3) #roundto three digits teh differences
table4_paper$Methods <- c(rep("gam",7),
                          rep("kriging",7),
                          rep("gwr",7))    
                             
#Check input covariates and model formula:
#list_formulas <-raster_prediction_obj_2$method_mod_obj[[1]]$formulas #formulas for models run comb5
list_formulas <-unlist(lapply(list_raster_obj_files[[1]],FUN=function(x){x<-load_obj(x);x$method_mod_obj[[1]]$formulas}))
#strsplit(list_formulas,"~")
                             
table4_paper$Formulas<-rep(list_formulas,3)                             
table4_paper<-table4_paper[(c(5,4,1,2,3))]                             

#Testing siginificance between models
                             
#mod_compk1 <-kruskal.test(tb1$rmse ~ as.factor(tb1$pred_mod)) #Kruskal Wallis test
#mod_compk2 <-kruskal.test(tb2$rmse ~ as.factor(tb2$pred_mod))
#print(mod_compk1) #not significant

###Table 4, writeout
names_table_col <-c("Models","Method","Single time scale","Multiple time scale CAI","Multiple time scale FSS") # 

names(table4_paper)<- names_table_col
print(table4_paper) #show resulting table

#Now write out table 4

file_name<-paste("table4_multi_timescale_paper","_",out_prefix,".txt",sep="")
write.table(table4_paper,file=file_name,sep=",")

####################################
####### Now create figures #############

#figure 1: study area
#figure 2: methodological worklfow
#figure 3: daily mean compared to monthly mean
#Figure 4. RMSE and MAE, mulitisampling and hold out for FSS and GAM.
#Figure 5. Overtraining tendency
#Figure 6: Spatial pattern of prediction for one day (maps)
#Figure 7: Spatial transects for one day (maps)
#Figure 8: Spatial lag profiles and stations data  
#Figure 9: Image differencing and land cover                               

################################################
######### Figure 1: Oregon study area, add labeling names to  Willamette Valley an Mountain Ranges?
#3 parameters from output
#infile_monthly<-list_outfiles$monthly_covar_ghcn_data #outile4 from database_covar script
#infile_daily<-list_outfiles$daily_covar_ghcn_data  #outfile3 from database_covar script
#infile_locs<- list_outfiles$loc_stations_ghcn #outfile2? from database covar script

ghcn_dat <- readOGR(dsn=dirname(met_stations_obj$monthly_covar_ghcn_data),
        sub(".shp","",basename(met_stations_obj$monthly_covar_ghcn_data)))
ghcn_dat_WGS84 <-spTransform(ghcn_dat,CRS_locs_WGS84)         # Project from WGS84 to new coord. system

interp_area <- readOGR(dsn=dirname(infile_reg_outline),sub(".shp","",basename(infile_reg_outline)))
interp_area_WGS84 <-spTransform(interp_area,CRS_locs_WGS84)         # Project from WGS84 to new coord. system

usa_map <- getData('GADM', country='USA', level=1) #Get US map
usa_map_2 <- usa_map[usa_map$NAME_1!="Alaska",] #remove Alaska
usa_map_2 <- usa_map_2[usa_map_2$NAME_1!="Hawaii",] #remove Hawai 
usa_map_OR <- usa_map_2[usa_map_2$NAME_1=="Oregon",] #get OR

elev <- subset(s_raster,"elev_s")
elev_WGS84 <- projectRaster(from=elev,crs=CRS_locs_WGS84,method="ngb")

#set up the output file to plot
res_pix<-960
col_mfrow<-1
row_mfrow<-1
png(filename=paste("Figure1_paper_study_area_",out_prefix,".png",sep=""),
    width=col_mfrow*res_pix,height=row_mfrow*res_pix)
par(mfrow=c(1,1))

#plot(elev_WGS84,cex.axis=2.1,cex.z=2.1)
plot(elev_WGS84,cex.axis=1.4,axis.args=list(at=seq(0,3500,500),
               labels=seq(0, 3500, 500), 
               cex.axis=1.4))
plot(interp_area_WGS84,add=T)
plot(ghcn_dat_WGS84,add=T)
title_text <-c("Elevation (m) and meteorological"," stations in Oregon")
legend("topleft",legend=title_text,cex=2.1,bty="n")
#Add region label
par(mar = c(0,0,0,0)) # remove margin
#opar <- par(fig=c(0.9,0.95,0.8, 0.85), new=TRUE)
#opar <- par(fig=c(0.85,0.95,0.8, 0.9), new=TRUE)
#opar <- par(fig=c(0.8,0.95,0.75, 0.9), new=TRUE)
#opar <- par(fig=c(0.75,0.95,0.7, 0.9), new=TRUE)
opar <- par(fig=c(0.65,0.9,0.8, 0.92), new=TRUE)

plot(usa_map_2,border="black") #border and lwd are options of graphics package polygon object
plot(usa_map_OR,col="dark grey",add=T)
box()
dev.off()

################################################
######### Figure 2:  Method comparison workflow 

# Workflow figure is not generated in R

################################################
######### Figure 3: LST averaging: daily mean compared to monthly mean

lst_md<-raster(ref_rast_name)
projection(lst_md)<-projection(s_raster)
lst_mm_09<-subset(s_raster,"mm_09")

lst_md<-raster(ref_rast_d001)
lst_md<- lst_md - 273.16
lst_mm_01<-subset(s_raster,"mm_01")

png(filename=paste("Comparison_daily_monthly_mean_lst",out_prefix,".png",sep=""),width=960,height=480)
par(mfrow=c(1,2))
plot(lst_md)
plot(interp_area,add=TRUE)
title("Mean January 1")
plot(lst_mm_01)
plot(interp_area,add=TRUE)
title("Mean for month of January")
dev.off()

################################################
######### Figure 4. RMSE multi-timescale mulitple hold out for FSS and CAI

raster_prediction_obj_9 <-load_obj(file.path(in_dir9,raster_obj_file_9)) #comb5 gwr_fss

list_raster_obj_files_holdout  <- list(file.path(in_dir10,raster_obj_file_10),file.path(in_dir11,raster_obj_file_11),
                                  file.path(in_dir12,raster_obj_file_12),file.path(in_dir13,raster_obj_file_13),
                                  file.path(in_dir14,raster_obj_file_14),file.path(in_dir15,raster_obj_file_15))
 
names(list_raster_obj_files_holdout)<- c("gam_fss","kriging_fss","gwr_fss",
                                         "gam_CAI","kriging_CAI","gwr_CAI")
tb_v_list<-lapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_diagnostic_v"]]})                           
tb_s_list<-lapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_diagnostic_s"]]})                           
#tb_s_list<-mclapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_diagnostic_s"]]},mc.preschedule=FALSE,mc.cores = 6)                           
#tb_v_list<-mclapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_diagnostic_v"]]},mc.preschedule=FALSE,mc.cores = 6)                           
names(tb_s_list) <- paste("tb_s_",names(tb_s_list),sep="")
names(tb_v_list) <- paste("tb_v_",names(tb_v_list),sep="")

tb_mv_list<-lapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_month_diagnostic_mv"]]})                           
tb_ms_list<-lapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_month_diagnostic_ms"]]})                           
#tb_mv_list<-mclapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_month_diagnostic_v"]]},mc.preschedule=FALSE,mc.cores = 6)                           
#tb_ms_list<-mclapply(list_raster_obj_files_holdout,FUN=function(x){x<-load_obj(x);x[["tb_month_diagnostic_s"]]},mc.preschedule=FALSE,mc.cores = 6)                           
names(tb_ms_list) <- paste("tb_ms_",names(tb_ms_list),sep="") #monthly training accuracy
names(tb_mv_list) <- paste("tb_mv_",names(tb_mv_list),sep="") #monthly testing accuracy

list_tb <- c(tb_s_list,tb_v_list,tb_ms_list,tb_mv_list) #combined in one list
ac_metric <- "rmse"
#Quick function to explore accuracy make this a function to create solo figure...and run only a subset...
plot_accuracy_by_holdout_fun <-function(list_tb,ac_metric){
  #
  for(i in 1:length(list_tb)){
    #i <- i+1
    tb <-list_tb[[i]]
    plot_name <- names(list_tb)[i]
    pat_str <- "tb_m"
    if(substr(plot_name,start=1,stop=4)== pat_str){
      names_id <- c("pred_mod","prop")
      plot_formula <- paste(ac_metric,"~prop",sep="",collapse="") 
    }else{
      names_id <- c("pred_mod","prop_month")
      plot_formula <- paste(ac_metric,"~prop_month",collapse="")
    }
    names_mod <-unique(tb$pred_mod)
    prop_obj <- calc_stat_prop_tb_diagnostic(names_mod,names_id,tb)
    avg_tb <- prop_obj$avg_tb
  
    layout_m<-c(1,1) #one row two columns
    par(mfrow=layout_m)
    
    png(paste("Figure__accuracy_",ac_metric,"_prop_month_",plot_name,"_",out_prefix,".png", sep=""),
      height=480*layout_m[1],width=480*layout_m[2])
    
    p<- xyplot(as.formula(plot_formula),group=pred_mod,type="b",
          data=avg_tb,
          main=paste(ac_metric,plot_name,sep=" "),
          pch=1:length(avg_tb$pred_mod),
          par.settings=list(superpose.symbol = list(
          pch=1:length(avg_tb$pred_mod))),
          auto.key=list(columns=5))
    print(p)
  
    dev.off()
  }
  #end of function
}

#For paper...
#Combine figures... tb_v for GWR, Kriging and GAM for both FSS and CAI

################################################
######### Figure 5. RMSE multi-timescale mulitple hold out Overtraining tendency

#For paper...
#Combine figures... tb_v for GWR, Kriging and GAM for both FSS and CAI

##### Calculate differences

metric_names <- c("mae","rmse","me","r")
diff_kriging_CAI <- diff_df(list_tb[["tb_s_kriging_CAI"]],list_tb[["tb_v_kriging_CAI"]],metric_names)
diff_gam_CAI <- diff_df(list_tb[["tb_s_gam_CAI"]][tb_s_gam_CAI$pred_mod!="mod_kr"],list_tb[["tb_v_gam_CAI"]],metric_names)
diff_gwr_CAI <- diff_df(tb_s_gwr_CAI,tb_v_gwr_CAI,metric_names)

layout_m<-c(1,1) #one row two columns
par(mfrow=layout_m)

png(paste("Figure__accuracy_rmse_prop_month_",plot_name,out_suffix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
boxplot(diff_kriging_CAI$rmse,diff_gam_CAI$rmse,diff_gwr_CAI$rmse,names=c("kriging_CAI","gam_CAI","gwr_CAI"),
        main="Difference between training and testing daily rmse")
dev.off()

#remove prop 0,
diff_kriging_CAI <- diff_df(tb_s_kriging_CAI[tb_s_kriging_CAI$prop_month!=0,],tb_v_kriging_CAI[tb_v_kriging_CAI$prop_month!=0,],metric_names)
diff_gam_CAI <- diff_df(tb_s_gam_CAI[tb_s_gam_CAI$prop_month!=0,],tb_v_gam_CAI[tb_v_gam_CAI$prop_month!=0,],metric_names)
diff_gwr_CAI <- diff_df(tb_s_gwr_CAI[tb_s_gwr_CAI$prop_month!=0,],tb_v_gwr_CAI[tb_v_gwr_CAI$prop_month!=0,],metric_names)
boxplot(diff_kriging_CAI$rmse,diff_gam_CAI$rmse,diff_gwr_CAI$rmse,names=c("kriging_CAI","gam_CAI","gwr_CAI"),
        main="Difference between training and testing daily rmse")

#now monthly accuracy: use mapply and provide  a list of of inputs...
metric_names <- c("mae","rmse","me","r")
diff_kriging_m_CAI <- diff_df(tb_ms_kriging_CAI[tb_ms_kriging_CAI$prop!=0,],tb_mv_kriging_CAI,metric_names)
diff_gam_m_CAI <- diff_df(tb_ms_gam_CAI[tb_ms_gam_CAI$prop!=0,],tb_mv_gam_CAI,metric_names)
diff_gwr_m_CAI <- diff_df(tb_ms_gwr_CAI[tb_ms_gwr_CAI$prop!=0,],tb_mv_gwr_CAI,metric_names)

layout_m<-c(1,1) #one row two columns
par(mfrow=layout_m)

png(paste("Figure__accuracy_rmse_prop_month_",plot_name,out_suffix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
boxplot(diff_kriging_m_CAI$rmse,diff_gam_m_CAI$rmse,diff_gwr_m_CAI$rmse,names=c("kriging_CAI","gam_CAI","gwr_CAI"),
        main="Difference between training and monhtly testing rmse")
dev.off()

#boxplot(diff_kriging_m_CAI$rmse,diff_gam_m_CAI$rmse,diff_gwr_CAI,names=c("kriging_CAI","gam_CAI","gwr_CAI"),
#        main="Difference between training and monhtly testing rmse")

### For fusion

metric_names <- c("mae","rmse","me","r")
diff_kriging_fus <- diff_df(tb_s_kriging_fus,tb_v_kriging_fus,metric_names)
diff_gam_fus <- diff_df(tb_s_gam_fus,tb_v_gam_fus,metric_names)
diff_gwr_fus <- diff_df(tb_s_gwr_fus,tb_v_gwr_fus,metric_names)

layout_m<-c(1,1) #one row two columns
par(mfrow=layout_m)

png(paste("Figure__accuracy_rmse_prop_month_",plot_name,out_suffix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
boxplot(diff_kriging_fus$rmse,diff_gam_fus$rmse,diff_gwr_fus$rmse,names=c("kriging_fus","gam_fus","gwr_fus"),
        main="Difference between training and testing daily rmse")
dev.off()

metric_names <- c("mae","rmse","me","r")
diff_kriging_m_fus <- diff_df(tb_ms_kriging_fus[tb_ms_kriging_fus$prop!=0,],tb_mv_kriging_fus[tb_mv_kriging_fus$prop!=0,],metric_names)
diff_gam_m_fus <- diff_df(tb_ms_gam_fus[tb_ms_gam_fus$prop!=0,],tb_mv_gam_fus[tb_mv_gam_fus$prop!=0,],metric_names)
diff_gwr_m_fus <- diff_df(tb_ms_gwr_fus[tb_ms_gwr_fus$prop!=0,],tb_mv_gwr_fus[tb_mv_gwr_fus$prop!=0,],metric_names)

layout_m<-c(1,1) #one row two columns
par(mfrow=layout_m)

png(paste("Figure__accuracy_rmse_prop_month_",plot_name,out_suffix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
boxplot(diff_kriging_m_fus$rmse,diff_gam_m_fus$rmse,diff_gwr_m_fus$rmse, names=c("kriging_fus","gam_fus","gwr_fus"),
        main="Difference between training and testing FUS rmse")
dev.off()

################################################
######### Figure 6: Spatial pattern of prediction for one day (maps)

y_var_name <-"dailyTmax"
index<-244 #index corresponding to Sept 1

lf_list<-lapply(list_raster_obj_files[c("gam_daily","gam_CAI","gam_fss")],
                               FUN=function(x){x<-load_obj(x);x$method_mod_obj[[index]][[y_var_name]]})                           

#lf1 <- raster_prediction_obj_1$method_mod_obj[[index]][[y_var_name]] #select relevant raster images for the given dates
#lf4 <- raster_prediction_obj_4$method_mod_obj[[index]][[y_var_name]]
#lf7 <- raster_prediction_obj_7$method_mod_obj[[index]][[y_var_name]]

date_selected <- "20109101"
#methods_names <-c("gam","kriging","gwr")
methods_names <-c("gam_daily","gam_CAI","gam_FSS")

names_layers<-methods_names
#lf <- (list(lf1,lf4[1:7],lf7[1:7]))
lf<-list(lf_list[[1]],lf_list[[2]][1:7],lf_list[[3]][1:7])

names_layers <-c("mod1 = lat*long","mod2 = lat*long + LST","mod3 = lat*long + elev","mod4 = lat*long + N_w*E_w",
                 "mod5 = lat*long + elev + DISTOC","mod6 = lat*long + elev + LST","mod7 = lat*long + elev + LST*FOREST")
nb_fig<- c("7a","7b","7c")
for (i in 1:length(lf)){
  pred_temp_s <-stack(lf[[i]])

  #lf2 <- raster_prediction_obj_2$method_mod_obj[[index]][[y_var_name]]
  #lf2 #contains the models for gam

  #s.range <- c(min(minValue(pred_temp_s)), max(maxValue(pred_temp_s)))
  #s.range <- s.range+c(5,-5)
  #col.breaks <- pretty(s.range, n=200)
  #lab.breaks <- pretty(s.range, n=100)
  temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
  #max_val<-s.range[2]
  #min_val <-s.range[1]
  max_val <- 40
  min_val <- -10
  layout_m<-c(2,4) #one row two columns

  png(paste("Figure_",nb_fig[i],"_spatial_pattern_tmax_prediction_models_gam_levelplot_",date_selected,out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])

  p <- levelplot(pred_temp_s,main=methods_names[i], ylab=NULL,xlab=NULL,
          par.settings = list(axis.text = list(font = 2, cex = 1.3),layout=layout_m,
                              par.main.text=list(font=2,cex=2),strip.background=list(col="white")),par.strip.text=list(font=2,cex=1.5),
          names.attr=names_layers,col.regions=temp.colors,at=seq(min_val,max_val,by=0.25))
  #col.regions=temp.colors(25))
  print(p)
  dev.off()
}

################################################
#Figure 7: Spatial transects for one day (maps)

#######Figure 7a: Map of transects

## Transects image location in OR             
png(paste("Fig7_elevation_transect_paths_",date_selected,out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])

plot(elev)
for(i in 1:length(transect_list)){
  filename<-sub(".shp","",transect_list[i])             #Removing the extension from file.
  transect<-readOGR(dirname(filename), basename(filename))                 #reading shapefile 
  plot(transect_list[i],add=TRUE)
}
title("Transect Oregon")
dev.off()

#### TRANSECT PROFILES
nb_transect <- 3
list_transect2<-vector("list",nb_transect)
list_transect3<-vector("list",nb_transect)
list_transect4<-vector("list",nb_transect)

rast_pred<-stack(lf[[2]]) #GAM_CAI
rast_pred_selected2<-subset(rast_pred,c(1,6)) #3 is referring to FSS, plot it first because it has the
rast_pred_selected3<-subset(rast_pred,c(1,2)) #3 is referring to FSS, plot it first because it has the
rast_pred3<-stack(lf[[2]]) #GAM_CAI
                                          # the largest range.
rast_pred2 <- stack(rast_pred_selected2,subset(s_raster,"elev_s"))
rast_pred3 <- stack(rast_pred_selected3,subset(s_raster,"elev_s"))

#layers_names<-layerNames(rast_pred2)<-c("lat*lon","lat*lon + elev + LST","elev")
layers_names<- names(rast_pred2)<-c("mod1","mod6","elev")
layers_names<- names(rast_pred3)<-c("mod1","mod2","elev")
pos<-c(1,2) # postions in the layer prection
transect_list
list_transect2[[1]]<-c(transect_list[1],paste("figure_3_tmax_elevation_transect1_OR_",date_selected,
                                           paste("mod1_mod6",collapse="_"),out_prefix,sep="_"))
list_transect2[[2]]<-c(transect_list[2],paste("figure_3_tmax_elevation_transect2_OR_",date_selected,
                                           paste("mod1_mod6",collapse="_"),out_prefix,sep="_"))
list_transect2[[3]]<-c(transect_list[3],paste("figure_3_tmax_elevation_transect3_OR_",date_selected,
                                           paste("mod1_mod6",collapse="_"),out_prefix,sep="_"))

list_transect3[[1]]<-c(transect_list[1],paste("figure_3_tmax_elevation_transect1_OR_",date_selected,
                                           paste("mod1_mod2",collapse="_"),out_prefix,sep="_"))
list_transect3[[2]]<-c(transect_list[2],paste("figure_3_tmax_elevation_transect2_OR_",date_selected,
                                           paste("mod1_mod2",collapse="_"),out_prefix,sep="_"))
list_transect3[[3]]<-c(transect_list[3],paste("figure_3_tmax_elevation_transect3_OR_",date_selected,
                                           paste("mod1_mod2",collapse="_"),out_prefix,sep="_"))

names(list_transect2)<-c("transect_OR1","transect_OR2","transect_OR3")
names(list_transect3)<-c("transect_OR1","transect_OR2","transect_OR3")

names(rast_pred2)<-layers_names
names(rast_pred3)<-layers_names

title_plot2<-paste(names(list_transect2),date_selected,sep=" ")
title_plot2<-paste(rep("Oregon transect on ",3), date_selected,sep="")
title_plot3<-paste(names(list_transect3),date_selected,sep=" ")
title_plot3<-paste(rep("Oregon transect on ",3), date_selected,sep="")

#r_stack<-rast_pred
m_layers_sc<-c(3) #elevation in the third layer
#m_layers_sc<-c(4) #elevation in the third layer

#title_plot2
#rast_pred2
#debug(plot_transect_m2)
trans_data2 <-plot_transect_m2(list_transect2,rast_pred2,title_plot2,disp=FALSE,m_layers_sc)
trans_data3 <-plot_transect_m2(list_transect3,rast_pred3,title_plot3,disp=FALSE,m_layers_sc)

################################################

#Figure 9: Image differencing and land cover  
#Do for january and September...
png(paste("Fig9_image_difference_",date_selected,out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])

  pred_temp <-subset(rast_pred,c(1,2,6)) #3 

  p <- levelplot(pred_temp_s,main=methods_names[i], ylab=NULL,xlab=NULL,
          par.settings = list(axis.text = list(font = 2, cex = 1.3),layout=layout_m,
                              par.main.text=list(font=2,cex=2),strip.background=list(col="white")),par.strip.text=list(font=2,cex=1.5),
          names.attr=names_layers,col.regions=temp.colors,at=seq(min_val,max_val,by=0.25))
  #col.regions=temp.colors(25))
  print(p)
dev.off()

###################### END OF SCRIPT #######################


