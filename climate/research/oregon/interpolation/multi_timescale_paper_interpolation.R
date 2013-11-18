####################################  INTERPOLATION OF TEMPERATURES  #######################################
############################  Script for manuscript analyses,tables and figures: MULTITIME SCALE  ##############################
#This script uses the worklfow code applied to the Oregon case study. Multitime scale methods (GAM,GWR, Kriging) are tested with
#different covariates using FUSION and CAI. Accuracy methods are added in the the function script to evaluate results.
#Figures, tables and data for the  paper are also produced in the script.
#AUTHOR: Benoit Parmentier 
#CREATED ON: 10/31/2013  
#MODIFIED ON: 11/15/2013            
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

#Additional libraries not used in workflow
library(pgirmess)                            # Krusall Wallis test with mulitple options, Kruskalmc {pgirmess}  

#### FUNCTION USED IN SCRIPT

function_analyses_paper <-"contribution_of_covariates_paper_interpolation_functions_10152013.R"

plot_transect_m2<-function(list_trans,r_stack,title_plot,disp=FALSE,m_layers){
  #This function creates plot of transects for stack of raster images.
  #Arguments:
  #list_trans: list of files containing the transects lines in shapefile format
  #r_stack: raster stack containing the information to extect
  #title_plot: plot title
  #disp: display and save from X11 if TRUE or plot to png file if FALSE
  #m_layers: index for layerers containing alternate units to be drawned on a differnt scale
  #RETURN:
  #list containing transect information
  
  nb<-length(list_trans)
  t_col<-rainbow(nb)
  t_col<-c("red","green","black")
  lty_list<-c("dashed","solid","dotted")
  list_trans_data<-vector("list",nb)
  
  #For scale 1
  for (i in 1:nb){
    trans_file<-list_trans[[i]][1]
    filename<-sub(".shp","",trans_file)             #Removing the extension from file.
    transect<-readOGR(dirname(filename), basename(filename))                 #reading shapefile 
    trans_data<-extract(r_stack, transect)
    if (disp==FALSE){
      png(file=paste(list_trans[[i]]),".png",sep="")
    }
    #Plot layer values for specific transect
    for (k in 1:ncol(trans_data[[1]])){
      y<-trans_data[[1]][,k]
      x<-1:length(y)
      m<-match(k,m_layers)
      
      if (k==1 & is.na(m)){
        plot(x,y,type="l",xlab="transect distance from coastal origin (km)", ylab=" maximum temperature (degree C)",
             ,cex=1.2,col=t_col[k])
        #axis(2)
      }
      if (k==1 & !is.na(m)){
        plot(x,y,type="l",col=t_col[k],lty="dotted",axes=F) #plotting fusion profile
        #axis(4,xlab="",ylab="elevation(m)")  
        axis(4,cex=1.2)
      }
      if (k!=1 & is.na(m)){
        #par(new=TRUE)              # new plot without erasing old
        lines(x,y,type="l",xlab="",ylab="",col=t_col[k],axes=F) #plotting fusion profile
        #axis(2,xlab="",ylab="tmax (in degree C)")
      }
      if (k!=1 & !is.na(m)){
        par(new=TRUE)              # key: ask for new plot without erasing old
        plot(x,y,type="l",col=t_col[k],xlab="",ylab="",lty="dotted",axes=F) #plotting fusion profile
        #axis(4,xlab="",ylab="elevation(m)")  
        axis(4,cex=1.2)
      } 
    }
    title(title_plot[i])
    legend("topleft",legend=layerNames(r_stack)[1:2], 
           cex=1.2, col=t_col,lty=1,bty="n")
    legend("topright",legend=layerNames(r_stack)[3], 
           cex=1.2, col=t_col[3],lty="dotted",bty="n")
    if (disp==TRUE){
      savePlot(file=paste(list_trans[[i]][2],".png",sep=""),type="png")
    }
    if (disp==FALSE){
      dev.off()
    }
    list_trans_data[[i]]<-trans_data
  }
  names(list_trans_data)<-names(list_trans)
  return(list_trans_data)
}

##############################
#### Parameters and constants  

script_path<-"/home/parmentier/Data/IPLANT_project/env_layers_scripts/" #path to script
source(file.path(script_path,function_analyses_paper)) #source all functions used in this script.

#direct methods: gam, kriging, gwr
in_dir1 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_daily_lst_comb5_11012013"
in_dir2 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_daily_lst_comb5_11022013"
in_dir3 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_daily_lst_comb5p1_3_11062013"
#CAI: gam, kriging, gwr
in_dir4 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_cai_lst_comb5_11032013"
in_dir5 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_cai_lst_comb5_11032013"
in_dir6 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_cai_lst_comb5_11042013"
#FSS: gam, kriging, gwr
in_dir7 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_gam_fss_lst_comb5_11062013"
in_dir8 <-"/data/project/layers/commons/Oregon_interpolation/output_data_365d_kriging_fss_lst_comb5_11052013"
in_dir9 <- "/data/project/layers/commons/Oregon_interpolation/output_data_365d_gwr_fss_lst_comb5_11052013"
#

##raster_prediction object for comb5
#direct methods
raster_obj_file_1 <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_daily_lst_comb5_11012013.RData" 
raster_obj_file_2 <- "raster_prediction_obj_kriging_daily_dailyTmax_365d_kriging_daily_lst_comb5_11022013.RData"
raster_obj_file_3 <- "raster_prediction_obj_gwr_daily_dailyTmax_365d_gwr_daily_lst_comb5p1_3_11062013.RData"
raster_obj_file_3b <- "raster_prediction_obj_gwr_daily_dailyTmax_365d_gwr_daily_lst_comb5p1_3_11062013.RData"
#CAI
raster_obj_file_4 <- "raster_prediction_obj_gam_CAI_dailyTmax_365d_gam_cai_lst_comb5_11032013.RData"
raster_obj_file_5 <- "raster_prediction_obj_kriging_CAI_dailyTmax_365d_kriging_cai_lst_comb5_11032013.RData"
raster_obj_file_6 <- "raster_prediction_obj_gwr_CAI_dailyTmax_365d_gwr_cai_lst_comb5_11042013.RData"
#FSS
raster_obj_file_7 <- "raster_prediction_obj_gam_fusion_dailyTmax_365d_gam_fss_lst_comb5_11062013.RData"
raster_obj_file_8 <- "raster_prediction_obj_kriging_fusion_dailyTmax_365d_kriging_fss_lst_comb5_11052013.RData"
raster_obj_file_9 <- "raster_prediction_obj_gwr_fusion_dailyTmax_365d_gwr_fss_lst_comb5_11052013.RData"

out_dir<-"/home/parmentier/Data/IPLANT_project/paper_multitime_scale__analyses_tables_fig_09032013"
setwd(out_dir)

infile_reg_outline <- "/data/project/layers/commons/data_workflow/inputs/region_outlines_ref_files/OR83M_state_outline.shp"  #input region outline defined by polygon: Oregon
met_stations_outfiles_obj_file<-"/data/project/layers/commons/data_workflow/output_data_365d_gam_fus_lst_test_run_07172013/met_stations_outfiles_obj_gam_fusion__365d_gam_fus_lst_test_run_07172013.RData"
CRS_locs_WGS84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0") #Station coords WGS84
y_var_name <- "dailyTmax"
out_prefix<-"analyses_11082013"
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

raster_prediction_obj_1 <-load_obj(file.path(in_dir1,raster_obj_file_1)) #comb5 gam_daily
raster_prediction_obj_2 <-load_obj(file.path(in_dir2,raster_obj_file_2)) #comb5 kriging_daily
raster_prediction_obj_3 <-load_obj(file.path(in_dir3,raster_obj_file_3)) #comb5 gwr_daily mod1 to mod3
raster_prediction_obj_3b <-load_obj(file.path(in_dir3b,raster_obj_file_3b)) #comb5 gwr_daily mod4 to mod7
                             
raster_prediction_obj_4 <-load_obj(file.path(in_dir4,raster_obj_file_4)) #comb5 gam_CAI
raster_prediction_obj_5 <-load_obj(file.path(in_dir5,raster_obj_file_5)) #comb5 kriging_CAI
raster_prediction_obj_6 <-load_obj(file.path(in_dir6,raster_obj_file_6)) #comb5 gwr_CAI 
raster_prediction_obj_7 <-load_obj(file.path(in_dir7,raster_obj_file_7)) #comb5 gam_fss
raster_prediction_obj_8 <-load_obj(file.path(in_dir8,raster_obj_file_8)) #comb5 kriging_fss 
raster_prediction_obj_9 <-load_obj(file.path(in_dir9,raster_obj_file_9)) #comb5 gwr_fss

############### BEGIN SCRIPT #################

############
##### 1) Generate: Table 4. Contribution of covariates using validation accuracy metrics
## This is a table of accuracy  

list_raster_obj_files  <- list(file.path(in_dir1,raster_obj_file_1),file.path(in_dir2,raster_obj_file_2),
                               file.path(in_dir3,raster_obj_file_3),file.path(in_dir4,raster_obj_file_4),
                               file.path(in_dir5,raster_obj_file_5),file.path(in_dir6,raster_obj_file_6),
                               file.path(in_dir7,raster_obj_file_7),file.path(in_dir8,raster_obj_file_8),
                               file.path(in_dir9,raster_obj_file_9))
 
names(list_raster_obj_files)<- c("gam_daily","kriging_daily","gwr_daily",
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

#for kriging models
table_gwr <- summary_metrics_v_list[grep("gwr",names(summary_metrics_v_list))]
table_gwr <- do.call(cbind,table_gwr)
table_gwr <- table_gwr[1:7,]                         

table4_paper <- as.data.frame(do.call(rbind,list(table_gam,table_kriging,table_gwr)))    
table4_paper <- round(table4_paper,digit=3) #roundto three digits teh differences
table4_paper$Methods <- c(rep("gam",7),
                          rep("kriging",7),
                          rep("gwr",7))    
                             
#Check input covariates and model formula:
list_formulas <-raster_prediction_obj_2$method_mod_obj[[1]]$formulas #formulas for models run comb5
#strsplit(list_formulas,"~")
                             
table4_paper$Forumulas<-rep(list_formulas,3)                             
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
png(filename=paste("Figure1_contribution_covariates_study_area_",out_prefix,".png",sep=""),
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

### CREATE FIGURE MEAN DAILY AND MEAN MONTHLY: AAG 2013  ####

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

################################################
######### Figure 5. RMSE multi-timescale mulitple hold out Overtraining tendency
                             
################################################
######### Figure 6: Spatial pattern of prediction for one day (maps)

y_var_name <-"dailyTmax"
index<-244 #index corresponding to Sept 1

lf1 <- raster_prediction_obj_1$method_mod_obj[[index]][[y_var_name]] #select relevant raster images for the given dates
lf4 <- raster_prediction_obj_4$method_mod_obj[[index]][[y_var_name]]
lf7 <- raster_prediction_obj_7$method_mod_obj[[index]][[y_var_name]]

date_selected <- "20109101"
#methods_names <-c("gam","kriging","gwr")
methods_names <-c("gam_daily","gam_CAI","gam_FSS")

names_layers<-methods_names
lf <- (list(lf1,lf4[1:7],lf7[1:7]))

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

  p <- levelplot(pred_temp_s,main="Interpolated Surfaces Model Comparison baseline 1", ylab=NULL,xlab=NULL,
          par.settings = list(axis.text = list(font = 2, cex = 1.3),layout=layout_m,
                              par.main.text=list(font=2,cex=2),strip.background=list(col="white")),par.strip.text=list(font=2,cex=1.5),
          names.attr=names_layers,col.regions=temp.colors,at=seq(min_val,max_val,by=0.2))
  #col.regions=temp.colors(25))
  print(p)
  dev.off()
}

################################################
#Figure 7: Spatial transects for one day (maps)

#######Figure 7a: Map of transects

nb_transect<-3
list_transect2<-vector("list",nb_transect)
rast_pred<-stack(lf[[2]]) #GAM_CAI
rast_pred_selected<-subset(rast_pred,c(1,6)) #3 is referring to FSS, plot it first because it has the
                                             # the largest range.
rast_pred2<-stack(rast_pred_selected,subset(s_raster,"elev_s"))

#layers_names<-layerNames(rast_pred2)<-c("lat*lon","lat*lon + elev + LST","elev")
layers_names<-layerNames(rast_pred2)<-c("mod1","mod6","elev")
pos<-c(1,2) # postions in the layer prection
transect_list
list_transect2[[1]]<-c(transect_list[1],paste("figure_3_tmax_elevation_transect1_OR_",date_selected,
                                           paste("mod1_mod6",collapse="_"),out_prefix,sep="_"))
list_transect2[[2]]<-c(transect_list[2],paste("figure_3_tmax_elevation_transect2_OR_",date_selected,
                                           paste("mod1_mod6",collapse="_"),out_prefix,sep="_"))
list_transect2[[3]]<-c(transect_list[3],paste("figure_3_tmax_elevation_transect3_OR_",date_selected,
                                           paste("mod1_mod6",collapse="_"),out_prefix,sep="_"))

names(list_transect2)<-c("transect_OR1","transect_OR2","transect_OR3")

#X11(width=9,height=9)
#png(paste("fig3_elevation_transect1_path_CAI_fusion_",date_selected,out_prefix,".png", sep=""))
#plot(elev)
#k<-1  #transect to plot
#trans_file<-list_transect2[[k]][[1]]
#filename<-sub(".shp","",trans_file)             #Removing the extension from file.
#transect<-readOGR(".", filename)                 #reading shapefile 
#plot(transect,add=TRUE)
#title("Transect Oregon")
#dev.off()

layerNames(rast_pred2)<-layers_names
title_plot2<-paste(names(list_transect2),date_selected,sep=" ")
title_plot2<-paste(rep("Oregon transect on ",3), date_selected,sep="")
#r_stack<-rast_pred
m_layers_sc<-c(3)
#title_plot2
#rast_pred2
debug(plot_transect_m2)
trans_data2<-plot_transect_m2(list_transect2,rast_pred2,title_plot2,disp=FALSE,m_layers_sc)

png(filename=paste("Comparison_daily_monthly_mean_lst",out_prefix,".png",sep=""),width=960,height=480)
par(mfrow=c(1,2))

dev.off()

             
###################### END OF SCRIPT #######################


