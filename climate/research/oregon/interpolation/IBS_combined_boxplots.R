### PLOTTING ALL MODELS IN ONE PLOT

names(tmp44)
tb_gam<-subset(tmp44$gam1)
tb_gwr
tb_gam<-tmp44
tb_cai4
tb_fus5
tab_list<-tmp44

names(tb_gam)
tb_gam<-tb_gam[,-11]
names_methods<-c("kr","gwr","gam","cai","fss")
tb_list[[1]]<-tb_gam
metric_selected<-"RMSE"
tb_methods_combined<-function(tab_list,metric_selected,names_methods){
  tab_metric<-tab_list
  for (k in 1:length(tab_list)){
    #tab_list[[k]]$methods<-rep(names_methods[k],nrow(tab_list[[k]]))
    tab_metric[[k]]<-subset(tab_list[[k]],metric==metric_selected)
    new_names<-paste(names_methods[k],names(tab_metric[[k]])[4:ncol(tab_metric[[k]])],sep="_")
    names(tab_metric[[k]])[4:ncol(tab_metric[[k]])]<-new_names
    tab_metric[[k]]<-tab_metric[[k]][4:ncol(tab_metric[[k]])]
  }
  tab_sub<-tab_list[[k]][,1:3]
  tab_all<-do.call(cbind,tab_metric)
  tab_all<-cbind(tab_sub,tab_all)
  return(tab_all)
}

tab_combined<-tb_methods_combined(tab_list,metric_selected,names_methods)
head(tab_combined)
dim(tab_combined)
tmp_names<-names(tab_combined)
tmp_names2<-strsplit(tmp_names[4:length(tmp_names)],"\\.")
for (k in 1:length(tmp_names2)){
  tmp_names2[[k]]<-tmp_names2[[k]][[2]]
}
tab_combined2<-tab_combined
names(tab_combined2[4:length(tab_combined)])<-as.character(tmp_names2)
##DROP SOME MODELS
drop_mod_names<-c("kr_mod8","kr_mod9","gam_mod8","gam_mod9","cai_mod9","fss_mod9")
pos<-match(drop_mod_names,tmp_names2)
tab_combined2<-tab_combined2[4:length(tab_combined2)]
tab_combined2<-tab_combined2[,-pos]
tmp_names2<-as.character(tmp_names2[-pos])
pos<-match(c("cai_mod10","fss_mod10"),tmp_names2)
tmp_names2[pos]<-c("cai_mod9","fss_mod9")
X11(width=18,height=6)
boxplot(tab_combined2,las=2,names=tmp_names2,
        ylim=c(1,5),ylab="RMSE", outline=FALSE)
title(paste("Root Mean Square Errors for daily maximum temperature models",sep=""))
grid()
savePlot(paste("fig2_boxplot_all_models_combined_",out_prefix,".png", sep=""), type="png")
dev.off()

mean(tab_combined[4:49])

inlistobj2<-inlistobj[3:5]
namesobj2<-names_obj[3:5]
namesobj2<-c("gam","cai","fss")

tab_formulas_fun<-function(list_obj2,path_data,names_obj2){
  inlistobj2<-list_obj2
  nel<-length(inlistobj2)
  #method_mod <-vector("list",nel) #list of one row data.frame
  method_tb <-vector("list",nel) #list of one row data.frame
  list_formulas_mod<-vector("list",length(inlistobj2))
  for (k in 1:length(inlistobj2)){
    mod_tmp<-load_obj(as.character(inlistobj2[k]))
    tmp_formulas<-vector("list",length(mod_tmp[[1]]$mod_obj))
    for (j in 1:length(mod_tmp[[1]]$mod_obj)){
      mod<-mod_tmp[[1]]$mod_obj[[j]]
      tmp_formulas[[j]]<-formula(mod)
      #tb<-mod_tmp[[1]][[3]][0,] #copy of the data.frame structure that holds the acuary metrics
    }
    #j=1
    list_formulas_mod[[k]]<-as.character(tmp_formulas)
  }
  names(tmp_formulas)<-names_obj2
  return(tmp_formulas)
}

#write.table(as.character(list_formulas_mod[[k]]),file="formulas_CAI4_01052013.txt",sep=",")
formulas_all<-list_formulas_mod[[1]]
formulas_all<-tab_formulas_fun(inlistobj2,path_data,namesobj2)
write.table(as.character(formulas_all),file="formulas_GAM1_01052013.txt",sep=",")

##EXPLORATION CODE

lf_raster_fus<-"_365d_GAM_fus5_all_lstd_12302012.rst"
#lf_raster_fus<-"_365d_GAM_fusion_all_lstd_12272012.rst"

lf_raster_cai<-"_365d_GAM_CAI4_all_12272012.rst"
date_selected<-"20100901"
#titles<-list(cai=c("cai_kr","cai_mod5","cai mod8"),
#             fusion=c("fusion_kr"," fusion_mod5"," fusion_mod8"))

file_pat1<-glob2rx(paste("*",date_selected,"*",lf_raster_cai,sep="")) #Search for files in relation to fusion                                   
file_pat2<-glob2rx(paste("*",date_selected,"*",lf_raster_fus,sep="")) #Search for files in relation to fusion
lf_1<-list.files(pattern=file_pat1) #Search for files in relation to fusion                  
lf_2<-list.files(pattern=file_pat2) #Search for files in relation to fusion

#titles<-list(c("cai_kr","cai_mod1","cai_mod2","cai_mod3","cai_mod4","cai_mod5","cai_mod6","cai_mod7","cai_mod8","cai_mod9","fss_kr"))
titles<-list(c("fss_kr","fss_mod1","fss_mod2","fss_mod3","fss_mod4","fss_mod5","fss_mod6","fss_mod7","fss_mod8","fss_mod9","cai_kr"))

r1<-stack(lf_cai) #CAI
r2<-stack(lf_fus)#FUS

predictions<-stack(r2,subset(r1,1))
predictions<-mask(predictions,mask_rast)
layerNames(predictions)<-unlist(titles)
plot_name_pan<-unlist(titles)
#plot_name_pan<-c('cai_kr (RMSE=2.29)','cai_mod7 (RMSE=2.38)','fss_kr (RMSE=2.29')
s.range <- c(min(minValue(predictions)), max(maxValue(predictions)))
#s.range <- s.range+c(5,-5)
col.breaks <- pretty(s.range, n=50)
lab.breaks <- pretty(s.range, n=5)
temp.colors <- colorRampPalette(c('blue', 'white', 'red'))
X11(height=6,width=36)
X11(height=6,width=18)
s.range<-c(-5,35)
#plot(predictions, breaks=col.breaks, col=rev(heat.colors(length(col.breaks)-1)),
#   axis=list(at=lab.breaks, labels=lab.breaks))
plot(predictions, breaks=col.breaks, col=temp.colors(length(col.breaks)-1),
     axis=list(at=lab.breaks, labels=lab.breaks))
#plot(reg_outline, add=TRUE)
savePlot(paste("comparison_one_date_CAI_fusion_tmax_prediction_levelplot",date_selected,out_prefix,".png", sep=""),type="png")
X11(height=12,width=24)

png(paste("comparison_one_date_CAI_fusion_tmax_prediction_levelplot_",date_selected,out_prefix,".png", sep=""),
    height=480*layout_m[1],width=480*layout_m[2])
levelplot(predictions,main="Interpolated Surfaces Comparison", ylab=NULL,xlab=NULL,par.settings = list(axis.text = list(font = 2, cex = 1.3),
          par.main.text=list(font=2,cex=2),strip.background=list(col="white")),par.strip.text=list(font=2,cex=1.5),
          names.attr=plot_name_pan,col.regions=temp.colors,at=seq(s.range[1],s.range[2],by=0.25))
#col.regions=temp.colors(25))
saveplot(paste("comparison_one_date_CAI_fusion_tmax_prediction_levelplot_all",date_selected,out_prefix,".png", sep=""),type="png")
dev.off()


#daily_delta<-raster(lf_2[c(2)])
#bias_mod6<-raster(lf_2[c(9)])
#fus_mod6<-raster(lf_2[c(18)])

#clim_mod6<-raster(lf_1[c(9)])
#cai_mod6<-raster(lf_1[c(18)])
#tmp_cai<-daily_delta+clim_mod6
#clim2_mod6<-themolst-bias_mod6-273.16
#tmp_fus<-daily_delta+themolst-bias_mod6-273.16
#test_fus<-fus_mod6-tmp_fus
#test_cai<-cai_mod6-tmp_cai
#s_pred<-stack(test_fus,test_cai)
#plot(s_pred)
#s_clim<-stack(clim2_mod6,clim_mod6)
#diff<-test_fus-test_cai
#plot(diff)
#diff<-clim2_mod6-clim2_mod6
#plot(diff)
