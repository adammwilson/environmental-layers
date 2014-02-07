#for(i in 1:length(dates)){
accuracy_comp_CAI_fus_function <- function(i){
  date_selected<-dates[i]
  
  ## Get the relevant raster layers with prediction for fusion and CAI
  oldpath<-getwd()
  setwd(path_data_cai)
  file_pat<-glob2rx(paste("*tmax_predicted*",date_selected,"*_365d_GAM_CAI2_const_all_10312012.rst",sep="")) #Search for files in relation to fusion                  
  lf_cai2c<-list.files(pattern=file_pat) #Search for files in relation to fusion                  
  rast_cai2c<-stack(lf_cai2c)                   #lf_cai2c CAI results with constant sampling over 365 dates
  rast_cai2c<-mask(rast_cai2c,mask_ELEV_SRTM)
  
  oldpath<-getwd()
  setwd(path_data_fus)
  file_pat<-glob2rx(paste("*tmax_predicted*",date_selected,"*_365d_GAM_fusion_const_all_lstd_11022012.rst",sep="")) #Search for files in relation to fusion                  
  lf_fus1c<-list.files(pattern=file_pat) #Search for files in relation to fusion                        
  rast_fus1c<-stack(lf_fus1c)
  rast_fus1c<-mask(rast_fus1c,mask_ELEV_SRTM)
  
  #PLOT ALL MODELS
  #Prepare for plotting
  
  setwd(path) #set path to the output path
  
  rast_fus_pred<-raster(rast_fus1c,1)  # Select the first model from the stack i.e fusion with kriging for both steps
  rast_cai_pred<-raster(rast_cai2c,1)  
  layerNames(rast_cai_pred)<-paste("cai",date_selected,sep="_")
  layerNames(rast_fus_pred)<-paste("fus",date_selected,sep="_")
  rast_pred2<-stack(rast_fus_pred,rast_cai_pred)
  #function to extract training and test from object from object models created earlier during interpolation...
  
  #load training and testing date for the specified date for fusion and CAI
  data_vf<-station_data_interp(date_selected,file.path(path_data_fus,obj_mod_fus_name),training=FALSE,testing=TRUE)
  #data_sf<-station_data_interp(date_selected,file.path(path_data_fus,obj_mod_fus_name),training=TRUE,testing=FALSE)
  data_vc<-station_data_interp(date_selected,file.path(path_data_cai,obj_mod_cai_name),training=FALSE,testing=TRUE)
  #data_sc<-station_data_interp(date_selected,file.path(path_data_cai,obj_mod_cai_name),training=TRUE,testing=FALSE)
  
  date_selected_snot<-strptime(date_selected,"%Y%m%d")
  snot_selected <-snot_OR_2010_sp[snot_OR_2010_sp$date_formatted==date_selected_snot,]
  #snot_selected<-na.omit(as.data.frame(snot_OR_2010_sp[snot_OR_2010_sp$date==90110,]))
  rast_diff_fc<-rast_fus_pred-rast_cai_pred
  LC_stack<-stack(LC1,LC2,LC3,LC4,LC6,LC7)
  rast_pred3<-stack(rast_diff_fc,rast_pred2,ELEV_SRTM,LC_stack)
  layerNames(rast_pred3)<-c("diff_fc","fus","CAI","ELEV_SRTM","LC1","LC2","LC3","LC4","LC6","LC7")   #extract amount of veg...
  
  #extract predicted tmax corresponding to 
  extract_snot<-extract(rast_pred3,snot_selected)  #return value from extract is a matrix (with input SPDF)
  snot_data_selected<-cbind(as.data.frame(snot_selected),extract_snot)  #bind data together
  snot_data_selected$res_f<-snot_data_selected$fus-snot_data_selected$tmax #calculate the residuals for Fusion
  snot_data_selected$res_c<-snot_data_selected$CAI-snot_data_selected$tmax #calculate the residuals for CAI
  #snot_data_selected<-(na.omit(as.data.frame(snot_data_selected))) #remove rows containing NA, this may need to be modified later.
  
  ###fig3: Plot predicted vs observed tmax
  #fig3a: FUS
  png(paste("fig3_testing_scatterplot_pred_fus_CAI_observed_SNOT_GHCN_",date_selected,out_prefix,".png", sep=""))
  par(mfrow=c(1,2))
  x_range<-range(c(data_vf$pred_mod7,snot_data_selected$fus,data_vc$pred_mod9,snot_data_selected$CAI),na.rm=T)
  y_range<-range(c(data_vf$dailyTmax,snot_data_selected$tmax,data_vc$dailyTmax,snot_data_selected$tmax),na.rm=T)
  plot(data_vf$pred_mod7,data_vf$dailyTmax, ylab="Observed daily tmax (C)", xlab="Fusion predicted daily tmax (C)", 
       ylim=y_range,xlim=x_range)
  #text(data_vf$pred_mod7,data_vf$dailyTmax,labels=data_vf$idx,pos=3)
  abline(0,1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  grid(lwd=0.5,col="black")
  points(snot_data_selected$fus,snot_data_selected$tmax,pch=2,co="red")
  title(paste("Testing stations tmax fusion vs daily tmax",date_selected,sep=" "))
  legend("topleft",legend=c("GHCN", "SNOT"), 
         cex=1.2, col=c("black","red"),
         pch=c(1,2))  
  #fig 3b: CAI
  #x_range<-range(c(data_vc$pred_mod9,snot_data_selected$CAI))
  #y_range<-range(c(data_vc$dailyTmax,snot_data_selected$tmax))
  plot(data_vc$pred_mod9,data_vc$dailyTmax, ylab="Observed daily tmax (C)", xlab="CAI predicted daily tmax (C)", 
       ylim=y_range,xlim=x_range)
  #text(data_vc$pred_mod9,data_vc$dailyTmax,labels=data_vf$idx,pos=3)
  abline(0,1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  grid(lwd=0.5,col="black")
  points(snot_data_selected$CAI,snot_data_selected$tmax,pch=2,co="red") 
  #text(snot_data_selected$CAI,snot_data_selected$tmax,labels=1:nrow(snot_data_selected),pos=3)
  #title(paste("Testing stations tmax CAI vs daily tmax",date_selected,sep=" "))
  legend("topleft",legend=c("GHCN", "SNOT"), 
         cex=1.2, col=c("black","red"),
         pch=c(1,2))
  #savePlot(paste("fig3_testing_scatterplot_pred_fus_CAI_observed_SNOT_GHCN_",date_selected,out_prefix,".png", sep=""), type="png")
  dev.off()
  
  ##### Fig4a: ELEV-CAI
  png(paste("fig4_testing_scatterplot_pred_fus_CIA_elev_SNOT_GHCN_",date_selected,out_prefix,".png", sep="")) 
  par(mfrow=c(1,2)) 
  y_range<-range(c(data_vc$pred_mod9,snot_data_selected$CAI),na.rm=T)
  #y_range<-range(c(data_vc$pred_mod9,snot_data_selected$CAI),na.rm=T)
  x_range<-range(c(data_vc$ELEV_SRTM,snot_data_selected$ELEV_SRTM),na.rm=T)
  lm_mod1<-lm(data_vc$pred_mod9~data_vc$ELEV_SRTM)
  lm_mod2<-lm(snot_data_selected$CAI~snot_data_selected$ELEV_SRTM)
  plot(data_vc$ELEV_SRTM,data_vc$pred_mod9,ylab="Observed daily tmax (C)", xlab="Elevation (m)", 
       ylim=y_range,xlim=x_range)
  #text(data_vc$ELEV_SRTM,data_vc$pred_mod9,labels=data_vc$idx,pos=3)
  abline(lm_mod1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  abline(lm_mod2,col="red") #takes intercept at 0 and slope as 1 so display 1:1 ine
  grid(lwd=0.5, col="black")
  points(snot_data_selected$ELEV_SRTM,snot_data_selected$CAI,pch=2,co="red")
  title(paste("Testing stations tmax CAI vs elevation",date_selected,sep=" "))
  legend("topleft",legend=c("GHCN", "SNOT"), 
         cex=1.2, col=c("black","red"),
         pch=c(1,2))
  
  #Fig4bELEV-FUS
  y_range<-range(c(data_vf$pred_mod7,snot_data_selected$fus),na.rm=T)
  x_range<-range(c(data_vf$ELEV_SRTM,snot_data_selected$ELEV_SRTM),na.rm=T)
  lm_mod1<-lm(data_vf$pred_mod7~data_vf$ELEV_SRTM)
  lm_mod2<-lm(snot_data_selected$fus~snot_data_selected$ELEV_SRTM)
  plot(data_vf$ELEV_SRTM,data_vf$pred_mod7,ylab="Observed daily tmax (C)", xlab="Elevation (m)", 
       ylim=y_range,xlim=x_range)
  #text(data_vc$ELEV_SRTM,data_vc$pred_mod9,labels=data_vc$idx,pos=3)
  abline(lm_mod1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  abline(lm_mod2,col="red") #takes intercept at 0 and slope as 1 so display 1:1 ine
  grid(lwd=0.5, col="black")
  points(snot_data_selected$ELEV_SRTM,snot_data_selected$fus,pch=2,co="red")
  title(paste("Testing stations tmax  vs elevation",date_selected,sep=" "))
  legend("topleft",legend=c("GHCN", "SNOT"), 
         cex=1.2, col=c("black","red"),
         pch=c(1,2))
  #savePlot(paste("fig4_testing_scatterplot_pred_fus_CIA_elev_SNOT_GHCN_",date_selected,out_prefix,".png", sep=""), type="png") 
  dev.off()
  ############ ACCURACY METRICS AND RESIDUALS #############
  
  #START FIG 5
  #####Fig5a: CAI vs FUSION: difference by plotting on in terms of the other
  png(paste("fig5_testing_scatterplot_pred_fus_CAI_observed_SNOT_GHCN_",date_selected,out_prefix,".png", sep=""))
  par(mfrow=c(1,2)) 
  lm_mod<-lm(snot_data_selected$CAI~snot_data_selected$fus)
  y_range<-range(c(data_vc$pred_mod9,snot_data_selected$CAI),na.rm=T)
  x_range<-range(c(data_vf$pred_mod7,snot_data_selected$fus),na.rm=T)
  
  plot(data_vf$pred_mod7,data_vc$pred_mod9,ylab="Predicted CAI daily tmax (C)", xlab="Predicted fusion daily tmax (C)", 
       ylim=y_range,xlim=x_range)
  #text(data_vc$ELEV_SRTM,data_vc$dailyTmax,labels=data_vc$idx,pos=3)
  abline(0,1) #takes intercept at 0 and slope as 1 so display 1:1 ine
  abline(lm_mod,col="red")
  grid(lwd=0.5, col="black")
  points(snot_data_selected$fus,snot_data_selected$CAI,pch=2,co="red")
  title(paste("Testing stations predicted tmax fusion vs CAI tmax",date_selected,sep=" "))
  legend("topleft",legend=c("GHCN", "SNOT"), 
         cex=1.2, col=c("black","red"),
         pch=c(1,2))
  ####Fig5b: diff vs elev: difference by plotting on in terms of elev
  diff_fc<-data_vf$pred_mod7-data_vc$pred_mod9
  plot(snot_data_selected$ELEV_SRTM,snot_data_selected$diff_fc,pch=2,col="red")
  lm_mod<-lm(snot_data_selected$diff_fc~snot_data_selected$ELEV_SRTM)
  abline(lm_mod,col="red")
  points(data_vf$ELEV_SRTM,diff_fc)
  lm_mod<-lm(diff_fc~data_vf$ELEV_SRTM)
  abline(lm_mod)
  legend("topleft",legend=c("GHCN", "SNOT"), 
         cex=1.2, col=c("black","red"),
         pch=c(1,2))
  title(paste("Prediction tmax difference and elevation ",sep=""))
  dev.off()
  #savePlot(paste("fig5_testing_scatterplot_pred_fus_CAI_observed_SNOT_GHCN_",date_selected,out_prefix,".png", sep=""), type="png")
  
  #DO diff IN TERM OF ELEVATION CLASSES as well as diff..
  
  #### START FIG 6: difference fc vs elev
  #fig6a
  png(paste("fig6_elevation_classes_diff_SNOT_GHCN_network",date_selected,out_prefix,".png", sep=""))
  par(mfrow=c(1,2)) 
  brks<-c(0,500,1000,1500,2000,2500,4000)
  lab_brks<-1:6
  elev_rcstat<-cut(snot_data_selected$ELEV_SRTM,breaks=brks,labels=lab_brks,right=F)
  snot_data_selected$elev_rec<-elev_rcstat
  y_range<-range(c(snot_data_selected$diff_fc),na.rm=T)
  x_range<-range(c(elev_rcstat),na.rm=T)
  plot(elev_rcstat,snot_data_selected$diff_fc, ylab="diff_fc", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  #text(elev_rcstat,diff_cf,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("SNOT stations diff f vs Elevation",date_selected,sep=" "))
  
  ###With fewer classes...fig6b
  brks<-c(0,1000,2000,3000,4000)
  lab_brks<-1:4
  elev_rcstat<-cut(snot_data_selected$ELEV_SRTM,breaks=brks,labels=lab_brks,right=F)
  snot_data_selected$elev_rec<-elev_rcstat
  y_range<-range(c(snot_data_selected$diff_fc),na.rm=T)
  x_range<-range(c(elev_rcstat),na.rm=T)
  plot(elev_rcstat,snot_data_selected$diff_fc, ylab="diff_fc", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  #text(elev_rcstat,diff_cf,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("SNOT stations diff f vs Elevation",date_selected,sep=" "))
  #savePlot(paste("fig6_elevation_classes_diff_SNOT_GHCN_network",date_selected,out_prefix,".png", sep=""), type="png")
  dev.off()
  #START FIG 7 with residuals
  #fig 7a
  png(paste("fig7_elevation_classes_residuals_SNOT_GHCN_network",date_selected,out_prefix,".png", sep=""))
  par(mfrow=c(1,2)) 
  brks<-c(0,1000,2000,3000,4000)
  lab_brks<-1:4
  elev_rcstat<-cut(snot_data_selected$ELEV_SRTM,breaks=brks,labels=lab_brks,right=F)
  snot_data_selected$elev_rec<-elev_rcstat
  y_range<-range(c(snot_data_selected$res_f,snot_data_selected$res_c),na.rm=T)
  x_range<-range(c(elev_rcstat),na.rm=T)
  plot(elev_rcstat,snot_data_selected$res_f, ylab="res_f", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  #text(elev_rcstat,diff_cf,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("SNOT stations residuals fusion vs Elevation",date_selected,sep=" "))
  #fig 7b
  elev_rcstat<-cut(snot_data_selected$ELEV_SRTM,breaks=brks,labels=lab_brks,right=F)
  y_range<-range(c(snot_data_selected$res_c,snot_data_selected$res_f),na.rm=T)
  x_range<-range(c(elev_rcstat))
  plot(elev_rcstat,snot_data_selected$res_c, ylab="res_c", xlab="ELEV_SRTM (m) ", 
       ylim=y_range, xlim=x_range)
  #text(elev_rcstat,diff_cf,labels=data_vf$idx,pos=3)
  grid(lwd=0.5,col="black")
  title(paste("SNOT stations residuals CAI vs Elevation",date_selected,sep=" "))
  #savePlot(paste("fig7_elevation_classes_residuals_SNOT_GHCN_network",date_selected,out_prefix,".png", sep=""), type="png")
  dev.off()
  ####### COMPARE CAI FUSION USING SNOTEL DATA WITH ACCURACY METRICS###############
  ################ RESIDUALS and MAE etc.  #####################
  browser()
  ### Run for full list of date? --365
  ac_tab_snot_fus<-calc_accuracy_metrics(snot_data_selected$tmax,snot_data_selected$fus) 
  ac_tab_snot_cai<-calc_accuracy_metrics(snot_data_selected$tmax,snot_data_selected$CAI) 
  ac_tab_ghcn_fus<-calc_accuracy_metrics(data_vf$dailyTmax,data_vf$pred_mod7) 
  ac_tab_ghcn_cai<-calc_accuracy_metrics(data_vc$dailyTmax,data_vc$pred_mod9)
  
  ac_tab<-do.call(rbind,list(ac_tab_snot_fus,ac_tab_snot_cai,ac_tab_ghcn_fus,ac_tab_ghcn_cai))
  ac_tab$mod_id<-c("snot_fus","snot_cai","ghcn_fus","ghcn_cai")
  ac_tab$date<-date_selected
   
  list_ac_tab[[i]]<-ac_tab  #storing the accuracy metric data.frame in a list...
  #save(list_ac_tab,)
  save(list_ac_tab,file= paste("list_ac_tab_", date_selected,out_prefix,".RData",sep=""))
  
  #FIG8: boxplot of residuals for methods (fus, cai) using SNOT and GHCN data
  #fig8a
  png(paste("fig8_residuals_boxplot_SNOT_GHCN_network",date_selected,out_prefix,".png", sep=""))
  par(mfrow=c(1,2)) 
  y_range<-range(c(snot_data_selected$res_f,snot_data_selected$res_c,data_vf$res_mod7,data_vc$res_mod9),na.rm=T)
  boxplot(snot_data_selected$res_f,snot_data_selected$res_c,names=c("FUS","CAI"),ylim=y_range,ylab="Residuals tmax degree C")
  title(paste("Residuals for fusion and CAI methods for SNOT data ",date_selected,sep=" "))
  #fig8b
  boxplot(data_vf$res_mod7,data_vc$res_mod9,names=c("FUS","CAI"),ylim=y_range,ylab="Residuals tmax degree C")
  title(paste("Residuals for fusion and CAI methods for GHCN data ",date_selected,sep=" "))
  #savePlot(paste("fig8_residuals_boxplot_SNOT_GHCN_network",date_selected,out_prefix,".png", sep=""), type="png")
  dev.off()
  mae_fun<-function(residuals){
    mean(abs(residuals),na.rm=T)
  }
  
  mean_diff_fc<-aggregate(diff_fc~elev_rec,data=snot_data_selected,mean)
  mean_mae_c<-aggregate(res_c~elev_rec,data=snot_data_selected,mae_fun)
  mean_mae_f<-aggregate(res_f~elev_rec,data=snot_data_selected,mae_fun)
  
  ####FIG 9: plot MAE for fusion and CAI as well as boxplots of both thechnique
  #fig 9a: boxplot of residuals for MAE and CAI
  png(paste("fig9_residuals_boxplot_MAE_SNOT_GHCN_network",date_selected,out_prefix,".png", sep=""))
  par(mfrow=c(1,2)) 
  height<-cbind(snot_data_selected$res_f,snot_data_selected$res_c)
  boxplot(height,names=c("FUS","CAI"),ylab="Residuals tmax degree C")
  title(paste("Residuals for fusion and CAI methods for SNOT data ",date_selected,sep=" "))
  #par(new=TRUE)
  #abline(h=ac_tab[1,1],col="red")
  points(1,ac_tab[1,1],pch=5,col="red")
  points(2,ac_tab[2,1],pch=5,col="black")
  legend("bottom",legend=c("FUS_MAE", "CAI_MAE"), 
         cex=0.8, col=c("red","black"),
         pch=c(2,1))
  #fig 9b: MAE per 3 elevation classes:0-1000,1000-2000,2000-3000,3000-4000
  y_range<-c(0,max(c(mean_mae_c[,2],mean_mae_f[,2]),na.rm=T))
  plot(1:3,mean_mae_c[,2],ylim=y_range,type="n",ylab="MAE in degree C",xlab="elevation classes")
  points(mean_mae_c,ylim=y_range)
  lines(1:3,mean_mae_c[,2],col="black")
  par(new=TRUE)              # key: ask for new plot without erasing old
  points(mean_mae_f,ylim=y_range)
  lines(1:3,mean_mae_f[,2],col="red")
  legend("bottom",legend=c("FUS_MAE", "CAI_MAE"), 
         cex=0.8, col=c("red","black"),
         pch=c(2,1))
  title(paste("MAE per elevation classes for SNOT data ",date_selected,sep=" "))
  #savePlot(paste("fig9_residuals_boxplot_MAE_SNOT_GHCN_network",date_selected,out_prefix,".png", sep=""), type="png")
  dev.off()
  ### LM MODELS for difference and elevation categories
  ## Are the differences plotted on fig 9 significant??
  diffelev_mod<-lm(diff_fc~elev_rec,data=snot_data_selected)
  summary(diffelev_mod)
  ##LM MODEL MAE PER ELEVATION CLASS: residuals for CAI
  diffelev_mod<-lm(res_c~elev_rec,data=snot_data_selected)
  summary(diffelev_mod)
  ##LM MODEL MAE PER ELEVATION CLASS: residuals for Fusions
  diffelev_mod<-lm(res_f~elev_rec,data=snot_data_selected)
  summary(diffelev_mod)
  
  ### LM MODELS for RESIDUALS BETWEEN CAI AND FUSION
  ## Are the differences plotted on fig 9 significant??
  ## STORE THE p values...?? overall and per cat?
  
  #diffelev_mod<-lm(res_f~elev_rec,data=snot_data_selected)
  #table(snot_data_selected$elev_rec) #Number of observation per class
  #max(snot_data_selected$E_STRM)
  
  #res
  
  #############################################
  #USING BOTH validation and training
  #This part is exploratory....
  ################## EXAMINING RESIDUALS AND DIFFERENCES IN LAND COVER......############
  ######
  
  #LC_names<-c("LC1_rec","LC2_rec","LC3_rec","LC4_rec","LC6_rec")
  suf_name<-c("rec1")
  sum_var<-c("diff_fc")
  LC_names<-c("LC1","LC2","LC3","LC4","LC6")
  brks<-c(-1,20,40,60,80,101)
  lab_brks<-seq(1,5,1)
  #var_name<-LC_names; suffix<-"rec1"; s_function<-"mean";df<-snot_data_selected;summary_var<-"diff_fc"
  #reclassify_df(snot_data_selected,LC_names,var_name,brks,lab_brks,suffix,summary_var)
  
  #Calculate mean per land cover percentage
  data_agg<-reclassify_df(snot_data_selected,LC_names,brks,lab_brks,suf_name,sum_var)
  data_lc<-data_agg[[1]]
  snot_data_selected<-data_agg[[2]]
  
  by_name<-"rec1"
  df_lc_diff_fc<-merge_multiple_df(data_lc,by_name)
  
  ###### FIG10: PLOT LAND COVER
  png(paste("fig10_diff_prediction_tmax_diff_res_f_land cover",date_selected,out_prefix,".png", sep=""))
  par(mfrow=c(1,2))
  zones_stat<-df_lc_diff_fc #first land cover
  #names(zones_stat)<-c("lab_brks","LC")
  y_range<-range(as.vector(t(zones_stat[,-1])),na.rm=T)
  lab_brks_mid<-c(10,30,50,70,90)
  plot(lab_brks_mid,zones_stat[,2],type="b",ylim=y_range,col="black", lwd=2,
       ylab="difference between fusion and CAI",xlab="land cover percent classes")
  lines(lab_brks_mid,zones_stat[,3],col="red",type="b")
  lines(lab_brks_mid,zones_stat[,4],col="blue",type="b")
  lines(lab_brks_mid,zones_stat[,5],col="darkgreen",type="b")
  lines(lab_brks_mid,zones_stat[,6],col="purple",type="b")
  legend("topleft",legend=c("LC1_forest", "LC2_shrub", "LC3_grass", "LC4_crop", "LC6_urban"), 
         cex=1.2, col=c("black","red","blue","darkgreen","purple"),
         lty=1,lwd=1.8)
  title(paste("Prediction tmax difference and land cover ",date_selected,sep=""))
  
  ###NOW USE RESIDUALS FOR FUSION
  sum_var<-"res_f"
  suf_name<-"rec2"
  data_agg2<-reclassify_df(snot_data_selected,LC_names,brks,lab_brks,suf_name,sum_var)
  data_resf_lc<-data_agg2[[1]]
  #snot_data_selected<-data_agg[[2]]
  
  by_name<-"rec2"
  df_lc_resf<-merge_multiple_df(data_resf_lc,by_name)
  
  zones_stat<-df_lc_resf #first land cover
  #names(zones_stat)<-c("lab_brks","LC")
  lab_brks_mid<-c(10,30,50,70,90)
  plot(lab_brks_mid,zones_stat[,2],type="b",ylim=y_range,col="black",lwd=2,
       ylab="tmax residuals fusion ",xlab="land cover percent classes")
  lines(lab_brks_mid,zones_stat[,3],col="red",type="b")
  lines(lab_brks_mid,zones_stat[,4],col="blue",type="b")
  lines(lab_brks_mid,zones_stat[,5],col="darkgreen",type="b")
  lines(lab_brks_mid,zones_stat[,6],col="purple",type="b")
  legend("topleft",legend=c("LC1_forest", "LC2_shrub", "LC3_grass", "LC4_crop", "LC6_urban"), 
         cex=1.2, col=c("black","red","blue","darkgreen","purple"),
         lty=1,lwd=1.2)
  title(paste("Prediction tmax residuals and land cover ",date_selected,sep=""))
  #savePlot(paste("fig10_diff_prediction_tmax_diff_res_f_land cover",date_selected,out_prefix,".png", sep=""), type="png")
  dev.off()
  #### FIGURE11: res_f and res_c per land cover
  
  sum_var<-"res_c"
  suf_name<-"rec3"
  data_agg3<-reclassify_df(snot_data_selected,LC_names,brks,lab_brks,suf_name,sum_var)
  data_resc_lc<-data_agg3[[1]]
  snot_data_selected<-data_agg3[[2]]
  
  by_name<-"rec3"
  df_lc_resc<-merge_multiple_df(data_resc_lc,by_name)
  
  zones_stat<-df_lc_resc #first land cover
  #names(zones_stat)<-c("lab_brks","LC")
  png(paste("fig11_prediction_tmax_res_f_res_c_land cover",date_selected,out_prefix,".png", sep=""))
  par(mfrow=c(1,2))
  y_range<-range(as.vector(t(zones_stat[,-1])),na.rm=T)
  lab_brks_mid<-c(10,30,50,70,90)
  plot(lab_brks_mid,zones_stat[,2],type="b",ylim=y_range,col="black",lwd=2,
       ylab="tmax residuals CAI",xlab="land cover percent classes")
  lines(lab_brks_mid,zones_stat[,3],col="red",type="b")
  lines(lab_brks_mid,zones_stat[,4],col="blue",type="b")
  lines(lab_brks_mid,zones_stat[,5],col="darkgreen",type="b")
  lines(lab_brks_mid,zones_stat[,6],col="purple",type="b")
  legend("topleft",legend=c("LC1_forest", "LC2_shrub", "LC3_grass", "LC4_crop", "LC6_urban"), 
         cex=1.2, col=c("black","red","blue","darkgreen","purple"),
         lty=1,lwd=1.2)
  title(paste("Prediction tmax residuals CAI and land cover ",date_selected,sep=""))
  
  #fig11b
  zones_stat<-df_lc_resf #first land cover
  #names(zones_stat)<-c("lab_brks","LC")
  y_range<-range(as.vector(t(zones_stat[,-1])),na.rm=T)
  lab_brks_mid<-c(10,30,50,70,90)
  plot(lab_brks_mid,zones_stat[,2],type="b",ylim=y_range,col="black",lwd=2,
       ylab="tmax residuals fusion ",xlab="land cover percent classes")
  lines(lab_brks_mid,zones_stat[,3],col="red",type="b")
  lines(lab_brks_mid,zones_stat[,4],col="blue",type="b")
  lines(lab_brks_mid,zones_stat[,5],col="darkgreen",type="b")
  lines(lab_brks_mid,zones_stat[,6],col="purple",type="b")
  legend("topleft",legend=c("LC1_forest", "LC2_shrub", "LC3_grass", "LC4_crop", "LC6_urban"), 
         cex=1.2, col=c("black","red","blue","darkgreen","purple"),
         lty=1,lwd=1.2)
  title(paste("Prediction tmax residuals and land cover ",date_selected,sep=""))
  #savePlot(paste("fig10_diff_prediction_tmax_diff_res_f_land cover",date_selected,out_prefix,".png", sep=""), type="png")
  #savePlot(paste("fig11_prediction_tmax_res_f_res_c_land cover",date_selected,out_prefix,".png", sep=""), type="png")
  dev.off()
  ac_data_obj<-list(snot_data_selected,data_vf,data_vc,ac_tab)
  names(ac_data_obj)<-c("snot_data_selected","data_vf", "data_vc","ac_tab")
  return(ac_data_obj)
} 
