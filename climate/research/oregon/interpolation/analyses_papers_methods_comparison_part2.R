######################################## Paper Methods_comparison #######################################
############################ Scripts for figures and analyses for the the IBS poster #####################################
#This script performs analyses and create figures for the FSS paper.
#It uses inputs from interpolation objects created at earlier stages...                          #
#AUTHOR: Benoit Parmentier                                                                       #
#DATE: 07/20/2013                                                                                #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#491--                                  #
###################################################################################################

###Loading R library and packages                                                      
#library(gtools)                                        # loading some useful tools 
library(mgcv)                   # GAM package by Wood 2006 (version 2012)
library(sp)                     # Spatial pacakge with class definition by Bivand et al. 2008
library(spdep)                  # Spatial package with methods and spatial stat. by Bivand et al. 2012
library(rgdal)                  # GDAL wrapper for R, spatial utilities (Keitt et al. 2012)
library(gstat)                  # Kriging and co-kriging by Pebesma et al. 2004
library(automap)                # Automated Kriging based on gstat module by Hiemstra et al. 2008
library(spgwr)
library(maptools)
library(graphics)
library(parallel)               # Urbanek S. and Ripley B., package for multi cores & parralel processing
library(raster)
library(rasterVis)
library(plotrix)                # Draw circle on graph and additional plotting options
library(reshape)                # Data format and type transformation

## Functions
#loading R objects that might have similar names
load_obj <- function(f)
{
  env <- new.env()
  nm <- load(f, env)[1]
  env[[nm]]
}

script_path<-"/home/parmentier/Data/IPLANT_project/env_layers_scripts/"
#source(file.path(script_path,"interpolation_method_day_function_multisampling_06082013.R")) #Include GAM_day

## Parmeters  

#in_dir<- "/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_GAM_fus_all_lst_05312013"
#in_dir<-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_lst_comb3_07092013/"
in_dir<-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_lst_comb4_07152013/"
#in_dir<-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_gam_day_mult_lst_comb3_07202013"

out_dir<-""
setwd(in_dir)
y_var_name <- "dailyTmax"
y_var_month <- "TMax"
#y_var_month <- "LSTD_bias"

out_prefix<-"/home/parmentier/Data/IPLANT_project/Oregon_interpolation/Oregon_03142013/output_data_365d_GAM_fus_all_lst_05312013"
method_interpolation <- "gam_daily"
covar_obj_file <- "covar_obj__365d_gam_day_lst_comb3_07092013.RData"
raster_obj_file <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_day_lst_comb3_07092013.RData"
raster_obj_file <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_day_lst_comb4_07152013.RData"
#raster_obj_file <- "raster_prediction_obj_gam_daily_dailyTmax_365d_gam_day_mult_lst_comb3_07202013.RData"


#Load objects containing training, testing, models objects 
covar_obj <-load_obj(covar_obj_file)
infile_covariates <- covar_obj$infile_covariates
infile_reg_outline <- covar_obj$infile_reg_outline
covar_names<- covar_obj$covar_names

raster_prediction_obj <-load_obj(raster_obj_file)

names(raster_prediction_obj) #list of two objects

raster_prediction_obj$summary_metrics_v

j<-1 #selected month for climatology 
i<-1
data_s <- raster_prediction_obj$method_mod_obj[[i]]$data_s #training data
data_v <- raster_prediction_obj$method_mod_obj[[i]]$data_v #testing data

#data_month1 <- clim_method_mod_obj[[1]]$data_month #monthly data
#data_month <- clim_method_mod_obj[[j]]$data_month #monthly data
#clim_mod_obj_month <- clim_method_mod_obj[[j]]
#names(clim_mod_obj_month)

#####
s_raster <- brick(infile_covariates)
names(s_raster)<-covar_names


### COMPARING MEANS AND DISTRIBUTIONS

tb<- raster_prediction_obj$tb_diagnostic_v

mod_comp<-lm(tb$rmse ~ tb$pred_mod)
summary(mod_comp)

m<-aov(tb$rmse ~tb$pred_mod)
TukeyHSD(m)
plot(TukeyHSD(m))
#Testing for normality

histogram(~tb$rmse| tb$pred_mod)
x1<-subset(tb,tb$pred_mod=="mod1")
shapiro.test(x1$rmse)

x3<-subset(tb,tb$pred_mod=="mod4")
shapiro.test(x3$rmse)

x5<-subset(tb,tb$pred_mod=="mod5")
shapiro.test(x1$rmse)

xyplot(tb$rmse~tb$mae | tb$pred_mod) #scatterplot by categories...
xyplot(tb$rmse~tb$mae, groups=tb$pred_mod) #scatterplot by categories...


xyplot(tb$rmse~1:365 | tb$pred_mod,type="l")
xyplot(rmse~1:365, groups=pred_mod,type="l",data=tb)
tb1<-subset(tb,tb$pred_mod%in%c("mod1","mod2","mod3","mod4","mod5","mod6"))
xyplot(rmse~1:365, groups=pred_mod,type="l",data=tb1)

wilcox.test(x1$rmse,x5$rmse)

lapply(tb_list,FUN=wilcox.test,x1$rmse)

## ACCOUNT FOR NON NORMALITY:

#mod_compk<-kruskal.test(tbp$rmse ~ as.factor(tbp$pred_mod))

mod_compk<-kruskal.test(tb1$rmse ~ as.factor(tb1$pred_mod))
mod_compk

#kruskalmc {pgirmess}  
library(pgirmess)
mod_compk<-kruskalmc(tb1$rmse ~ as.factor(tb1$pred_mod))
mod_compk<-kruskalmc(tbp$rmse ~ as.factor(tbp$pred_mod))

mod_compk

m<-aov(tb$rmse ~tb$pred_mod)
TukeyHSD(m)
plot(TukeyHSD(m))

t<-melt(tb,
        measure=c("mae","rmse","r","m50"), 
        id=c("pred_mod","prop"),
        na.rm=T)

avg_tb<-cast(t,pred_mod+prop~variable,mean)
sd_tb<-cast(t,pred_mod+prop~variable,sd)

n_tb<-cast(t,pred_mod+prop~variable,length)

n=n_tb[["rmse"]]
y <-avg_tb[["rmse"]]
x<- 1:length(avg_tb[["pred_mod"]])
y_sd <- sd_tb[["rmse"]]

ciw <-y_sd
#ciw2   <- qt(0.975, n) * y_sd2 / sqrt(n)

plotCI(y=y, x=x, uiw=ciw, col="red", main=" Mean and Std_dev RMSE per model", barcol="blue", lwd=1,
       ylab="RMSE (C)", xlab="Models")

ciw   <- qt(0.975, n) * y_sd / sqrt(n)

plotCI(y=y, x=x, uiw=ciw, col="red", main=" Mean and CI RMSE per model", barcol="blue", lwd=1,
       ylab="RMSE (C)", xlab="Models")

#lines(x,y,col="red")
#legend("bottomright",legend=c("fus"), cex=1.2, col=c("red"),
#       lty=1, title="RMSE")

### BY proportion use xy box plot?








