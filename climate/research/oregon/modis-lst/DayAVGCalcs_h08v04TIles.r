library (raster)

path<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/ByDate/BadPixelsCorrected_Tiles"

#Find daily means for h08v04 tiles:
h08v04_infiles <- list.files(path=path, pattern="*.h08v04.005.ClearDay_corrected.img$")
for (i in 1: length(h08v04_infiles)){
  h08v04_infiles[i]<- paste (path,"/",h08v04_infiles[i],sep="")
}
h08v04_infiles

#Creating empty vectors in the form of day_1- day_366
N<- c(1:366)*NA
val<- c(1:length(h08v04_infiles))*NA

for (i in 1:366){
   N[i]<- paste("day_",i,"_h08v04",sep="")
   assign (N[i],val)
}

#Avg. calcs days 1-59:

for (i in 1:length(h08v04_infiles)){
    if(length(grep("\\.1.1.h",h08v04_infiles[i])!=0)){
        day_1_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.1.2.h",h08v04_infiles[i])!=0)){
        day_2_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.3.h",h08v04_infiles[i])!=0)){
        day_3_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.4.h",h08v04_infiles[i])!=0)){
        day_4_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.5.h",h08v04_infiles[i])!=0)){
        day_5_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.6.h",h08v04_infiles[i])!=0)){
        day_6_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.7.h",h08v04_infiles[i])!=0)){
        day_7_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.8.h",h08v04_infiles[i])!=0)){
        day_8_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.9.h",h08v04_infiles[i])!=0)){
        day_9_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.10.h",h08v04_infiles[i])!=0)){
        day_10_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.1.11.h",h08v04_infiles[i])!=0)){
        day_11_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.1.12.h",h08v04_infiles[i])!=0)){
        day_12_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.13.h",h08v04_infiles[i])!=0)){
        day_13_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.14.h",h08v04_infiles[i])!=0)){
        day_14_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.15.h",h08v04_infiles[i])!=0)){
        day_15_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.16.h",h08v04_infiles[i])!=0)){
        day_16_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.17.h",h08v04_infiles[i])!=0)){
        day_17_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.18.h",h08v04_infiles[i])!=0)){
        day_18_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.19.h",h08v04_infiles[i])!=0)){
        day_19_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.20.h",h08v04_infiles[i])!=0)){
        day_20_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.1.21.h",h08v04_infiles[i])!=0)){
        day_21_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.1.22.h",h08v04_infiles[i])!=0)){
        day_22_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.23.h",h08v04_infiles[i])!=0)){
        day_23_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.24.h",h08v04_infiles[i])!=0)){
        day_24_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.25.h",h08v04_infiles[i])!=0)){
        day_25_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.26.h",h08v04_infiles[i])!=0)){
        day_26_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.27.h",h08v04_infiles[i])!=0)){
        day_27_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.28.h",h08v04_infiles[i])!=0)){
        day_28_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.29.h",h08v04_infiles[i])!=0)){
        day_29_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.30.h",h08v04_infiles[i])!=0)){
        day_30_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.1.31.h",h08v04_infiles[i])!=0)){
        day_31_h08v04[i]<- h08v04_infiles[i]
    }  else if(length(grep("\\.2.1.h",h08v04_infiles[i])!=0)){
        day_32_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.2.2.h",h08v04_infiles[i])!=0)){
        day_33_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.3.h",h08v04_infiles[i])!=0)){
        day_34_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.4.h",h08v04_infiles[i])!=0)){
        day_35_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.5.h",h08v04_infiles[i])!=0)){
        day_36_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.6.h",h08v04_infiles[i])!=0)){
        day_37_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.7.h",h08v04_infiles[i])!=0)){
        day_38_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.8.h",h08v04_infiles[i])!=0)){
        day_39_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.9.h",h08v04_infiles[i])!=0)){
        day_40_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.10.h",h08v04_infiles[i])!=0)){
        day_41_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.2.11.h",h08v04_infiles[i])!=0)){
        day_42_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.2.12.h",h08v04_infiles[i])!=0)){
        day_43_h08v04[i]<- h08v04_infiles[i]  
    } else if (length(grep("\\.2.13.h",h08v04_infiles[i])!=0)){
        day_44_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.14.h",h08v04_infiles[i])!=0)){
        day_45_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.15.h",h08v04_infiles[i])!=0)){
        day_46_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.16.h",h08v04_infiles[i])!=0)){
        day_47_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.17.h",h08v04_infiles[i])!=0)){
        day_48_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.18.h",h08v04_infiles[i])!=0)){
        day_49_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.19.h",h08v04_infiles[i])!=0)){
        day_50_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.20.h",h08v04_infiles[i])!=0)){
        day_51_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.2.21.h",h08v04_infiles[i])!=0)){
        day_52_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.2.22.h",h08v04_infiles[i])!=0)){
        day_53_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.23.h",h08v04_infiles[i])!=0)){
        day_54_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.24.h",h08v04_infiles[i])!=0)){
        day_55_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.25.h",h08v04_infiles[i])!=0)){
        day_56_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.26.h",h08v04_infiles[i])!=0)){
        day_57_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.27.h",h08v04_infiles[i])!=0)){
        day_58_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.2.28.h",h08v04_infiles[i])!=0)){
        day_59_h08v04[i]<- h08v04_infiles[i]
    }
}

day_1_h08v04<- day_1_h08v04[!is.na(day_1_h08v04)]
day_2_h08v04<- day_2_h08v04[!is.na(day_2_h08v04)]
day_3_h08v04<- day_3_h08v04[!is.na(day_3_h08v04)]
day_4_h08v04<- day_4_h08v04[!is.na(day_4_h08v04)]
day_5_h08v04<- day_5_h08v04[!is.na(day_5_h08v04)]
day_6_h08v04<- day_6_h08v04[!is.na(day_6_h08v04)]
day_7_h08v04<- day_7_h08v04[!is.na(day_7_h08v04)]
day_8_h08v04<- day_8_h08v04[!is.na(day_8_h08v04)]
day_9_h08v04<- day_9_h08v04[!is.na(day_9_h08v04)]
day_10_h08v04<- day_10_h08v04[!is.na(day_10_h08v04)]
day_11_h08v04<- day_11_h08v04[!is.na(day_11_h08v04)]
day_12_h08v04<- day_12_h08v04[!is.na(day_12_h08v04)]
day_13_h08v04<- day_13_h08v04[!is.na(day_13_h08v04)]
day_14_h08v04<- day_14_h08v04[!is.na(day_14_h08v04)]
day_15_h08v04<- day_15_h08v04[!is.na(day_15_h08v04)]
day_16_h08v04<- day_16_h08v04[!is.na(day_16_h08v04)]
day_17_h08v04<- day_17_h08v04[!is.na(day_17_h08v04)]
day_18_h08v04<- day_18_h08v04[!is.na(day_18_h08v04)]
day_19_h08v04<- day_19_h08v04[!is.na(day_19_h08v04)]
day_20_h08v04<- day_20_h08v04[!is.na(day_20_h08v04)]
day_21_h08v04<- day_21_h08v04[!is.na(day_21_h08v04)]
day_22_h08v04<- day_22_h08v04[!is.na(day_22_h08v04)]
day_23_h08v04<- day_23_h08v04[!is.na(day_23_h08v04)]
day_24_h08v04<- day_24_h08v04[!is.na(day_24_h08v04)]
day_25_h08v04<- day_25_h08v04[!is.na(day_25_h08v04)]
day_26_h08v04<- day_26_h08v04[!is.na(day_26_h08v04)]
day_27_h08v04<- day_27_h08v04[!is.na(day_27_h08v04)]
day_28_h08v04<- day_28_h08v04[!is.na(day_28_h08v04)]
day_29_h08v04<- day_29_h08v04[!is.na(day_29_h08v04)]
day_30_h08v04<- day_30_h08v04[!is.na(day_30_h08v04)]
day_31_h08v04<- day_31_h08v04[!is.na(day_31_h08v04)]
day_32_h08v04<- day_32_h08v04[!is.na(day_32_h08v04)]
day_33_h08v04<- day_33_h08v04[!is.na(day_33_h08v04)]
day_34_h08v04<- day_34_h08v04[!is.na(day_34_h08v04)]
day_35_h08v04<- day_35_h08v04[!is.na(day_35_h08v04)]
day_36_h08v04<- day_36_h08v04[!is.na(day_36_h08v04)]
day_37_h08v04<- day_37_h08v04[!is.na(day_37_h08v04)]
day_38_h08v04<- day_38_h08v04[!is.na(day_38_h08v04)]
day_39_h08v04<- day_39_h08v04[!is.na(day_39_h08v04)]
day_40_h08v04<- day_40_h08v04[!is.na(day_40_h08v04)]
day_41_h08v04<- day_41_h08v04[!is.na(day_41_h08v04)]
day_42_h08v04<- day_42_h08v04[!is.na(day_42_h08v04)]
day_43_h08v04<- day_43_h08v04[!is.na(day_43_h08v04)]
day_44_h08v04<- day_44_h08v04[!is.na(day_44_h08v04)]
day_45_h08v04<- day_45_h08v04[!is.na(day_45_h08v04)]
day_46_h08v04<- day_46_h08v04[!is.na(day_46_h08v04)]
day_47_h08v04<- day_47_h08v04[!is.na(day_47_h08v04)]
day_48_h08v04<- day_48_h08v04[!is.na(day_48_h08v04)]
day_49_h08v04<- day_49_h08v04[!is.na(day_49_h08v04)]
day_50_h08v04<- day_50_h08v04[!is.na(day_50_h08v04)]
day_51_h08v04<- day_51_h08v04[!is.na(day_51_h08v04)]
day_52_h08v04<- day_52_h08v04[!is.na(day_52_h08v04)]
day_53_h08v04<- day_53_h08v04[!is.na(day_53_h08v04)]
day_54_h08v04<- day_54_h08v04[!is.na(day_54_h08v04)]
day_55_h08v04<- day_55_h08v04[!is.na(day_55_h08v04)]
day_56_h08v04<- day_56_h08v04[!is.na(day_56_h08v04)]
day_57_h08v04<- day_57_h08v04[!is.na(day_57_h08v04)]
day_58_h08v04<- day_58_h08v04[!is.na(day_58_h08v04)]
day_59_h08v04<- day_59_h08v04[!is.na(day_59_h08v04)]

day_1_h08v04_stack<- stack(day_1_h08v04)
nlayers(day_1_h08v04_stack) 
day_1_h08v04_mn<- mean(day_1_h08v04_stack, na.rm=TRUE)
day_1_h08v04_mnScaled<- day_1_h08v04_mn*0.0005
Out_day_1<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_1_h08v04_AVG_Scaled.img"
writeRaster (day_1_h08v04_mnScaled, filename=Out_day_1,format="HFA",overwrite=TRUE)
day_2_h08v04_stack<- stack(day_2_h08v04)
nlayers(day_2_h08v04_stack) 
day_2_h08v04_mn<- mean(day_2_h08v04_stack, na.rm=TRUE)
day_2_h08v04_mnScaled<- day_2_h08v04_mn*0.0005
Out_day_2<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_2_h08v04_AVG_Scaled.img"
writeRaster (day_2_h08v04_mnScaled, filename=Out_day_2,format="HFA",overwrite=TRUE)
day_3_h08v04_stack<- stack(day_3_h08v04)
nlayers(day_3_h08v04_stack) 
day_3_h08v04_mn<- mean(day_3_h08v04_stack, na.rm=TRUE)
day_3_h08v04_mnScaled<- day_3_h08v04_mn*0.0005
Out_day_3<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_3_h08v04_AVG_Scaled.img"
writeRaster (day_3_h08v04_mnScaled, filename=Out_day_3,format="HFA",overwrite=TRUE)
day_4_h08v04_stack<- stack(day_4_h08v04)
nlayers(day_4_h08v04_stack) 
day_4_h08v04_mn<- mean(day_4_h08v04_stack, na.rm=TRUE)
day_4_h08v04_mnScaled<- day_4_h08v04_mn*0.0005
Out_day_4<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_4_h08v04_AVG_Scaled.img"
writeRaster (day_4_h08v04_mnScaled, filename=Out_day_4,format="HFA",overwrite=TRUE)
day_5_h08v04_stack<- stack(day_5_h08v04)
nlayers(day_5_h08v04_stack) 
day_5_h08v04_mn<- mean(day_5_h08v04_stack, na.rm=TRUE)
day_5_h08v04_mnScaled<- day_5_h08v04_mn*0.0005
Out_day_5<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_5_h08v04_AVG_Scaled.img"
writeRaster (day_5_h08v04_mnScaled, filename=Out_day_5,format="HFA",overwrite=TRUE)
day_6_h08v04_stack<- stack(day_6_h08v04)
nlayers(day_6_h08v04_stack) 
day_6_h08v04_mn<- mean(day_6_h08v04_stack, na.rm=TRUE)
day_6_h08v04_mnScaled<- day_6_h08v04_mn*0.0005
Out_day_6<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_6_h08v04_AVG_Scaled.img"
writeRaster (day_6_h08v04_mnScaled, filename=Out_day_6,format="HFA",overwrite=TRUE)
day_7_h08v04_stack<- stack(day_7_h08v04)
nlayers(day_7_h08v04_stack) 
day_7_h08v04_mn<- mean(day_7_h08v04_stack, na.rm=TRUE)
day_7_h08v04_mnScaled<- day_7_h08v04_mn*0.0005
Out_day_7<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_7_h08v04_AVG_Scaled.img"
writeRaster (day_7_h08v04_mnScaled, filename=Out_day_7,format="HFA",overwrite=TRUE)
day_8_h08v04_stack<- stack(day_8_h08v04)
nlayers(day_8_h08v04_stack) 
day_8_h08v04_mn<- mean(day_8_h08v04_stack, na.rm=TRUE)
day_8_h08v04_mnScaled<- day_8_h08v04_mn*0.0005
Out_day_8<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_8_h08v04_AVG_Scaled.img"
writeRaster (day_8_h08v04_mnScaled, filename=Out_day_8,format="HFA",overwrite=TRUE)
day_9_h08v04_stack<- stack(day_9_h08v04)
nlayers(day_9_h08v04_stack) 
day_9_h08v04_mn<- mean(day_9_h08v04_stack, na.rm=TRUE)
day_9_h08v04_mnScaled<- day_9_h08v04_mn*0.0005
Out_day_9<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_9_h08v04_AVG_Scaled.img"
writeRaster (day_9_h08v04_mnScaled, filename=Out_day_9,format="HFA",overwrite=TRUE)
day_10_h08v04_stack<- stack(day_10_h08v04)
nlayers(day_10_h08v04_stack) 
day_10_h08v04_mn<- mean(day_10_h08v04_stack, na.rm=TRUE)
day_10_h08v04_mnScaled<- day_10_h08v04_mn*0.0005
Out_day_10<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_10_h08v04_AVG_Scaled.img"
writeRaster (day_10_h08v04_mnScaled, filename=Out_day_10,format="HFA",overwrite=TRUE)
day_11_h08v04_stack<- stack(day_11_h08v04)
nlayers(day_11_h08v04_stack) 
day_11_h08v04_mn<- mean(day_11_h08v04_stack, na.rm=TRUE)
day_11_h08v04_mnScaled<- day_11_h08v04_mn*0.0005
Out_day_11<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_11_h08v04_AVG_Scaled.img"
writeRaster (day_11_h08v04_mnScaled, filename=Out_day_11,format="HFA",overwrite=TRUE)
day_12_h08v04_stack<- stack(day_12_h08v04)
nlayers(day_12_h08v04_stack) 
day_12_h08v04_mn<- mean(day_12_h08v04_stack, na.rm=TRUE)
day_12_h08v04_mnScaled<- day_12_h08v04_mn*0.0005
Out_day_12<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_12_h08v04_AVG_Scaled.img"
writeRaster (day_12_h08v04_mnScaled, filename=Out_day_12,format="HFA",overwrite=TRUE)
day_13_h08v04_stack<- stack(day_13_h08v04)
nlayers(day_13_h08v04_stack) 
day_13_h08v04_mn<- mean(day_13_h08v04_stack, na.rm=TRUE)
day_13_h08v04_mnScaled<- day_13_h08v04_mn*0.0005
Out_day_13<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_13_h08v04_AVG_Scaled.img"
writeRaster (day_13_h08v04_mnScaled, filename=Out_day_13,format="HFA",overwrite=TRUE)
day_14_h08v04_stack<- stack(day_14_h08v04)
nlayers(day_14_h08v04_stack) 
day_14_h08v04_mn<- mean(day_14_h08v04_stack, na.rm=TRUE)
day_14_h08v04_mnScaled<- day_14_h08v04_mn*0.0005
Out_day_14<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_14_h08v04_AVG_Scaled.img"
writeRaster (day_14_h08v04_mnScaled, filename=Out_day_14,format="HFA",overwrite=TRUE)
day_15_h08v04_stack<- stack(day_15_h08v04)
nlayers(day_15_h08v04_stack) 
day_15_h08v04_mn<- mean(day_15_h08v04_stack, na.rm=TRUE)
day_15_h08v04_mnScaled<- day_15_h08v04_mn*0.0005
Out_day_15<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_15_h08v04_AVG_Scaled.img"
writeRaster (day_15_h08v04_mnScaled, filename=Out_day_15,format="HFA",overwrite=TRUE)
day_16_h08v04_stack<- stack(day_16_h08v04)
nlayers(day_16_h08v04_stack) 
day_16_h08v04_mn<- mean(day_16_h08v04_stack, na.rm=TRUE)
day_16_h08v04_mnScaled<- day_16_h08v04_mn*0.0005
Out_day_16<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_16_h08v04_AVG_Scaled.img"
writeRaster (day_16_h08v04_mnScaled, filename=Out_day_16,format="HFA",overwrite=TRUE)
day_17_h08v04_stack<- stack(day_17_h08v04)
nlayers(day_17_h08v04_stack) 
day_17_h08v04_mn<- mean(day_17_h08v04_stack, na.rm=TRUE)
day_17_h08v04_mnScaled<- day_17_h08v04_mn*0.0005
Out_day_17<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_17_h08v04_AVG_Scaled.img"
writeRaster (day_17_h08v04_mnScaled, filename=Out_day_17,format="HFA",overwrite=TRUE)
day_18_h08v04_stack<- stack(day_18_h08v04)
nlayers(day_18_h08v04_stack) 
day_18_h08v04_mn<- mean(day_18_h08v04_stack, na.rm=TRUE)
day_18_h08v04_mnScaled<- day_18_h08v04_mn*0.0005
Out_day_18<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_18_h08v04_AVG_Scaled.img"
writeRaster (day_18_h08v04_mnScaled, filename=Out_day_18,format="HFA",overwrite=TRUE)
day_19_h08v04_stack<- stack(day_19_h08v04)
nlayers(day_19_h08v04_stack) 
day_19_h08v04_mn<- mean(day_19_h08v04_stack, na.rm=TRUE)
day_19_h08v04_mnScaled<- day_19_h08v04_mn*0.0005
Out_day_19<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_19_h08v04_AVG_Scaled.img"
writeRaster (day_19_h08v04_mnScaled, filename=Out_day_19,format="HFA",overwrite=TRUE)
day_20_h08v04_stack<- stack(day_20_h08v04)
nlayers(day_20_h08v04_stack) 
day_20_h08v04_mn<- mean(day_20_h08v04_stack, na.rm=TRUE)
day_20_h08v04_mnScaled<- day_20_h08v04_mn*0.0005
Out_day_20<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_20_h08v04_AVG_Scaled.img"
writeRaster (day_20_h08v04_mnScaled, filename=Out_day_20,format="HFA",overwrite=TRUE)
day_21_h08v04_stack<- stack(day_21_h08v04)
nlayers(day_21_h08v04_stack) 
day_21_h08v04_mn<- mean(day_21_h08v04_stack, na.rm=TRUE)
day_21_h08v04_mnScaled<- day_21_h08v04_mn*0.0005
Out_day_21<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_21_h08v04_AVG_Scaled.img"
writeRaster (day_21_h08v04_mnScaled, filename=Out_day_21,format="HFA",overwrite=TRUE)
day_22_h08v04_stack<- stack(day_22_h08v04)
nlayers(day_22_h08v04_stack) 
day_22_h08v04_mn<- mean(day_22_h08v04_stack, na.rm=TRUE)
day_22_h08v04_mnScaled<- day_22_h08v04_mn*0.0005
Out_day_22<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_22_h08v04_AVG_Scaled.img"
writeRaster (day_22_h08v04_mnScaled, filename=Out_day_22,format="HFA",overwrite=TRUE)
day_23_h08v04_stack<- stack(day_23_h08v04)
nlayers(day_23_h08v04_stack) 
day_23_h08v04_mn<- mean(day_23_h08v04_stack, na.rm=TRUE)
day_23_h08v04_mnScaled<- day_23_h08v04_mn*0.0005
Out_day_23<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_23_h08v04_AVG_Scaled.img"
writeRaster (day_23_h08v04_mnScaled, filename=Out_day_23,format="HFA",overwrite=TRUE)
day_24_h08v04_stack<- stack(day_24_h08v04)
nlayers(day_24_h08v04_stack) 
day_24_h08v04_mn<- mean(day_24_h08v04_stack, na.rm=TRUE)
day_24_h08v04_mnScaled<- day_24_h08v04_mn*0.0005
Out_day_24<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_24_h08v04_AVG_Scaled.img"
writeRaster (day_24_h08v04_mnScaled, filename=Out_day_24,format="HFA",overwrite=TRUE)
day_25_h08v04_stack<- stack(day_25_h08v04)
nlayers(day_25_h08v04_stack) 
day_25_h08v04_mn<- mean(day_25_h08v04_stack, na.rm=TRUE)
day_25_h08v04_mnScaled<- day_25_h08v04_mn*0.0005
Out_day_25<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_25_h08v04_AVG_Scaled.img"
writeRaster (day_25_h08v04_mnScaled, filename=Out_day_25,format="HFA",overwrite=TRUE)
day_26_h08v04_stack<- stack(day_26_h08v04)
nlayers(day_26_h08v04_stack) 
day_26_h08v04_mn<- mean(day_26_h08v04_stack, na.rm=TRUE)
day_26_h08v04_mnScaled<- day_26_h08v04_mn*0.0005
Out_day_26<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_26_h08v04_AVG_Scaled.img"
writeRaster (day_26_h08v04_mnScaled, filename=Out_day_26,format="HFA",overwrite=TRUE)
day_27_h08v04_stack<- stack(day_27_h08v04)
nlayers(day_27_h08v04_stack) 
day_27_h08v04_mn<- mean(day_27_h08v04_stack, na.rm=TRUE)
day_27_h08v04_mnScaled<- day_27_h08v04_mn*0.0005
Out_day_27<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_27_h08v04_AVG_Scaled.img"
writeRaster (day_27_h08v04_mnScaled, filename=Out_day_27,format="HFA",overwrite=TRUE)
day_28_h08v04_stack<- stack(day_28_h08v04)
nlayers(day_28_h08v04_stack) 
day_28_h08v04_mn<- mean(day_28_h08v04_stack, na.rm=TRUE)
day_28_h08v04_mnScaled<- day_28_h08v04_mn*0.0005
Out_day_28<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_28_h08v04_AVG_Scaled.img"
writeRaster (day_28_h08v04_mnScaled, filename=Out_day_28,format="HFA",overwrite=TRUE)
day_29_h08v04_stack<- stack(day_29_h08v04)
nlayers(day_29_h08v04_stack) 
day_29_h08v04_mn<- mean(day_29_h08v04_stack, na.rm=TRUE)
day_29_h08v04_mnScaled<- day_29_h08v04_mn*0.0005
Out_day_29<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_29_h08v04_AVG_Scaled.img"
writeRaster (day_29_h08v04_mnScaled, filename=Out_day_29,format="HFA",overwrite=TRUE)
day_30_h08v04_stack<- stack(day_30_h08v04)
nlayers(day_30_h08v04_stack) 
day_30_h08v04_mn<- mean(day_30_h08v04_stack, na.rm=TRUE)
day_30_h08v04_mnScaled<- day_30_h08v04_mn*0.0005
Out_day_30<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_30_h08v04_AVG_Scaled.img"
writeRaster (day_30_h08v04_mnScaled, filename=Out_day_30,format="HFA",overwrite=TRUE)
day_31_h08v04_stack<- stack(day_31_h08v04)
nlayers(day_31_h08v04_stack) 
day_31_h08v04_mn<- mean(day_31_h08v04_stack, na.rm=TRUE)
day_31_h08v04_mnScaled<- day_31_h08v04_mn*0.0005
Out_day_31<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_31_h08v04_AVG_Scaled.img"
writeRaster (day_31_h08v04_mnScaled, filename=Out_day_31,format="HFA",overwrite=TRUE)
day_32_h08v04_stack<- stack(day_32_h08v04)
nlayers(day_32_h08v04_stack) 
day_32_h08v04_mn<- mean(day_32_h08v04_stack, na.rm=TRUE)
day_32_h08v04_mnScaled<- day_32_h08v04_mn*0.0005
Out_day_32<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_32_h08v04_AVG_Scaled.img"
writeRaster (day_32_h08v04_mnScaled, filename=Out_day_32,format="HFA",overwrite=TRUE)
day_33_h08v04_stack<- stack(day_33_h08v04)
nlayers(day_33_h08v04_stack) 
day_33_h08v04_mn<- mean(day_33_h08v04_stack, na.rm=TRUE)
day_33_h08v04_mnScaled<- day_33_h08v04_mn*0.0005
Out_day_33<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_33_h08v04_AVG_Scaled.img"
writeRaster (day_33_h08v04_mnScaled, filename=Out_day_33,format="HFA",overwrite=TRUE)
day_34_h08v04_stack<- stack(day_34_h08v04)
nlayers(day_34_h08v04_stack) 
day_34_h08v04_mn<- mean(day_34_h08v04_stack, na.rm=TRUE)
day_34_h08v04_mnScaled<- day_34_h08v04_mn*0.0005
Out_day_34<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_34_h08v04_AVG_Scaled.img"
writeRaster (day_34_h08v04_mnScaled, filename=Out_day_34,format="HFA",overwrite=TRUE)
day_35_h08v04_stack<- stack(day_35_h08v04)
nlayers(day_35_h08v04_stack) 
day_35_h08v04_mn<- mean(day_35_h08v04_stack, na.rm=TRUE)
day_35_h08v04_mnScaled<- day_35_h08v04_mn*0.0005
Out_day_35<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_35_h08v04_AVG_Scaled.img"
writeRaster (day_35_h08v04_mnScaled, filename=Out_day_35,format="HFA",overwrite=TRUE)
day_36_h08v04_stack<- stack(day_36_h08v04)
nlayers(day_36_h08v04_stack) 
day_36_h08v04_mn<- mean(day_36_h08v04_stack, na.rm=TRUE)
day_36_h08v04_mnScaled<- day_36_h08v04_mn*0.0005
Out_day_36<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_36_h08v04_AVG_Scaled.img"
writeRaster (day_36_h08v04_mnScaled, filename=Out_day_36,format="HFA",overwrite=TRUE)
day_37_h08v04_stack<- stack(day_37_h08v04)
nlayers(day_37_h08v04_stack) 
day_37_h08v04_mn<- mean(day_37_h08v04_stack, na.rm=TRUE)
day_37_h08v04_mnScaled<- day_37_h08v04_mn*0.0005
Out_day_37<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_37_h08v04_AVG_Scaled.img"
writeRaster (day_37_h08v04_mnScaled, filename=Out_day_37,format="HFA",overwrite=TRUE)
day_38_h08v04_stack<- stack(day_38_h08v04)
nlayers(day_38_h08v04_stack) 
day_38_h08v04_mn<- mean(day_38_h08v04_stack, na.rm=TRUE)
day_38_h08v04_mnScaled<- day_38_h08v04_mn*0.0005
Out_day_38<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_38_h08v04_AVG_Scaled.img"
writeRaster (day_38_h08v04_mnScaled, filename=Out_day_38,format="HFA",overwrite=TRUE)
day_39_h08v04_stack<- stack(day_39_h08v04)
nlayers(day_39_h08v04_stack) 
day_39_h08v04_mn<- mean(day_39_h08v04_stack, na.rm=TRUE)
day_39_h08v04_mnScaled<- day_39_h08v04_mn*0.0005
Out_day_39<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_39_h08v04_AVG_Scaled.img"
writeRaster (day_39_h08v04_mnScaled, filename=Out_day_39,format="HFA",overwrite=TRUE)
day_40_h08v04_stack<- stack(day_40_h08v04)
nlayers(day_40_h08v04_stack) 
day_40_h08v04_mn<- mean(day_40_h08v04_stack, na.rm=TRUE)
day_40_h08v04_mnScaled<- day_40_h08v04_mn*0.0005
Out_day_40<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_40_h08v04_AVG_Scaled.img"
writeRaster (day_40_h08v04_mnScaled, filename=Out_day_40,format="HFA",overwrite=TRUE)
day_41_h08v04_stack<- stack(day_41_h08v04)
nlayers(day_41_h08v04_stack) 
day_41_h08v04_mn<- mean(day_41_h08v04_stack, na.rm=TRUE)
day_41_h08v04_mnScaled<- day_41_h08v04_mn*0.0005
Out_day_41<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_41_h08v04_AVG_Scaled.img"
writeRaster (day_41_h08v04_mnScaled, filename=Out_day_41,format="HFA",overwrite=TRUE)
day_42_h08v04_stack<- stack(day_42_h08v04)
nlayers(day_42_h08v04_stack) 
day_42_h08v04_mn<- mean(day_42_h08v04_stack, na.rm=TRUE)
day_42_h08v04_mnScaled<- day_42_h08v04_mn*0.0005
Out_day_42<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_42_h08v04_AVG_Scaled.img"
writeRaster (day_42_h08v04_mnScaled, filename=Out_day_42,format="HFA",overwrite=TRUE)
day_43_h08v04_stack<- stack(day_43_h08v04)
nlayers(day_43_h08v04_stack) 
day_43_h08v04_mn<- mean(day_43_h08v04_stack, na.rm=TRUE)
day_43_h08v04_mnScaled<- day_43_h08v04_mn*0.0005
Out_day_43<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_43_h08v04_AVG_Scaled.img"
writeRaster (day_43_h08v04_mnScaled, filename=Out_day_43,format="HFA",overwrite=TRUE)
day_44_h08v04_stack<- stack(day_44_h08v04)
nlayers(day_44_h08v04_stack) 
day_44_h08v04_mn<- mean(day_44_h08v04_stack, na.rm=TRUE)
day_44_h08v04_mnScaled<- day_44_h08v04_mn*0.0005
Out_day_44<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_44_h08v04_AVG_Scaled.img"
writeRaster (day_44_h08v04_mnScaled, filename=Out_day_44,format="HFA",overwrite=TRUE)
day_45_h08v04_stack<- stack(day_45_h08v04)
nlayers(day_45_h08v04_stack) 
day_45_h08v04_mn<- mean(day_45_h08v04_stack, na.rm=TRUE)
day_45_h08v04_mnScaled<- day_45_h08v04_mn*0.0005
Out_day_45<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_45_h08v04_AVG_Scaled.img"
writeRaster (day_45_h08v04_mnScaled, filename=Out_day_45,format="HFA",overwrite=TRUE)
day_46_h08v04_stack<- stack(day_46_h08v04)
nlayers(day_46_h08v04_stack) 
day_46_h08v04_mn<- mean(day_46_h08v04_stack, na.rm=TRUE)
day_46_h08v04_mnScaled<- day_46_h08v04_mn*0.0005
Out_day_46<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_46_h08v04_AVG_Scaled.img"
writeRaster (day_46_h08v04_mnScaled, filename=Out_day_46,format="HFA",overwrite=TRUE)
day_47_h08v04_stack<- stack(day_47_h08v04)
nlayers(day_47_h08v04_stack) 
day_47_h08v04_mn<- mean(day_47_h08v04_stack, na.rm=TRUE)
day_47_h08v04_mnScaled<- day_47_h08v04_mn*0.0005
Out_day_47<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_47_h08v04_AVG_Scaled.img"
writeRaster (day_47_h08v04_mnScaled, filename=Out_day_47,format="HFA",overwrite=TRUE)
day_48_h08v04_stack<- stack(day_48_h08v04)
nlayers(day_48_h08v04_stack) 
day_48_h08v04_mn<- mean(day_48_h08v04_stack, na.rm=TRUE)
day_48_h08v04_mnScaled<- day_48_h08v04_mn*0.0005
Out_day_48<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_48_h08v04_AVG_Scaled.img"
writeRaster (day_48_h08v04_mnScaled, filename=Out_day_48,format="HFA",overwrite=TRUE)
day_49_h08v04_stack<- stack(day_49_h08v04)
nlayers(day_49_h08v04_stack) 
day_49_h08v04_mn<- mean(day_49_h08v04_stack, na.rm=TRUE)
day_49_h08v04_mnScaled<- day_49_h08v04_mn*0.0005
Out_day_49<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_49_h08v04_AVG_Scaled.img"
writeRaster (day_49_h08v04_mnScaled, filename=Out_day_49,format="HFA",overwrite=TRUE)
day_50_h08v04_stack<- stack(day_50_h08v04)
nlayers(day_50_h08v04_stack) 
day_50_h08v04_mn<- mean(day_50_h08v04_stack, na.rm=TRUE)
day_50_h08v04_mnScaled<- day_50_h08v04_mn*0.0005
Out_day_50<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_50_h08v04_AVG_Scaled.img"
writeRaster (day_50_h08v04_mnScaled, filename=Out_day_50,format="HFA",overwrite=TRUE)
day_51_h08v04_stack<- stack(day_51_h08v04)
nlayers(day_51_h08v04_stack) 
day_51_h08v04_mn<- mean(day_51_h08v04_stack, na.rm=TRUE)
day_51_h08v04_mnScaled<- day_51_h08v04_mn*0.0005
Out_day_51<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_51_h08v04_AVG_Scaled.img"
writeRaster (day_51_h08v04_mnScaled, filename=Out_day_51,format="HFA",overwrite=TRUE)
day_52_h08v04_stack<- stack(day_52_h08v04)
nlayers(day_52_h08v04_stack) 
day_52_h08v04_mn<- mean(day_52_h08v04_stack, na.rm=TRUE)
day_52_h08v04_mnScaled<- day_52_h08v04_mn*0.0005
Out_day_52<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_52_h08v04_AVG_Scaled.img"
writeRaster (day_52_h08v04_mnScaled, filename=Out_day_52,format="HFA",overwrite=TRUE)
day_53_h08v04_stack<- stack(day_53_h08v04)
nlayers(day_53_h08v04_stack) 
day_53_h08v04_mn<- mean(day_53_h08v04_stack, na.rm=TRUE)
day_53_h08v04_mnScaled<- day_53_h08v04_mn*0.0005
Out_day_53<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_53_h08v04_AVG_Scaled.img"
writeRaster (day_53_h08v04_mnScaled, filename=Out_day_53,format="HFA",overwrite=TRUE)
day_54_h08v04_stack<- stack(day_54_h08v04)
nlayers(day_54_h08v04_stack) 
day_54_h08v04_mn<- mean(day_54_h08v04_stack, na.rm=TRUE)
day_54_h08v04_mnScaled<- day_54_h08v04_mn*0.0005
Out_day_54<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_54_h08v04_AVG_Scaled.img"
writeRaster (day_54_h08v04_mnScaled, filename=Out_day_54,format="HFA",overwrite=TRUE)
day_55_h08v04_stack<- stack(day_55_h08v04)
nlayers(day_55_h08v04_stack) 
day_55_h08v04_mn<- mean(day_55_h08v04_stack, na.rm=TRUE)
day_55_h08v04_mnScaled<- day_55_h08v04_mn*0.0005
Out_day_55<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_55_h08v04_AVG_Scaled.img"
writeRaster (day_55_h08v04_mnScaled, filename=Out_day_55,format="HFA",overwrite=TRUE)
day_56_h08v04_stack<- stack(day_56_h08v04)
nlayers(day_56_h08v04_stack) 
day_56_h08v04_mn<- mean(day_56_h08v04_stack, na.rm=TRUE)
day_56_h08v04_mnScaled<- day_56_h08v04_mn*0.0005
Out_day_56<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_56_h08v04_AVG_Scaled.img"
writeRaster (day_56_h08v04_mnScaled, filename=Out_day_56,format="HFA",overwrite=TRUE)
day_57_h08v04_stack<- stack(day_57_h08v04)
nlayers(day_57_h08v04_stack) 
day_57_h08v04_mn<- mean(day_57_h08v04_stack, na.rm=TRUE)
day_57_h08v04_mnScaled<- day_57_h08v04_mn*0.0005
Out_day_57<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_57_h08v04_AVG_Scaled.img"
writeRaster (day_57_h08v04_mnScaled, filename=Out_day_57,format="HFA",overwrite=TRUE)
day_58_h08v04_stack<- stack(day_58_h08v04)
nlayers(day_58_h08v04_stack) 
day_58_h08v04_mn<- mean(day_58_h08v04_stack, na.rm=TRUE)
day_58_h08v04_mnScaled<- day_58_h08v04_mn*0.0005
Out_day_58<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_58_h08v04_AVG_Scaled.img"
writeRaster (day_58_h08v04_mnScaled, filename=Out_day_58,format="HFA",overwrite=TRUE)
day_59_h08v04_stack<- stack(day_59_h08v04)
nlayers(day_59_h08v04_stack) 
day_59_h08v04_mn<- mean(day_59_h08v04_stack, na.rm=TRUE)
day_59_h08v04_mnScaled<- day_59_h08v04_mn*0.0005
Out_day_59<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_59_h08v04_AVG_Scaled.img"
writeRaster (day_59_h08v04_mnScaled, filename=Out_day_59,format="HFA",overwrite=TRUE)

# Days 60-365 or 366-----------------------------------------------------------------------------
for (i in 1:length(h08v04_infiles)){
  if (length(grep("\\.2004\\.",h08v04_infiles[i])!=0)|| length(grep("\\.2008\\.",h08v04_infiles[i])!=0)){
    if(length(grep("\\.2.29.h",h08v04_infiles[i])!=0)){
        day_60_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.3.1.h",h08v04_infiles[i])!=0)){
        day_61_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.3.2.h",h08v04_infiles[i])!=0)){
        day_62_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.3.h",h08v04_infiles[i])!=0)){
        day_63_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.4.h",h08v04_infiles[i])!=0)){
        day_64_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.5.h",h08v04_infiles[i])!=0)){
        day_65_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.6.h",h08v04_infiles[i])!=0)){
        day_66_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.7.h",h08v04_infiles[i])!=0)){
        day_67_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.8.h",h08v04_infiles[i])!=0)){
        day_68_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.9.h",h08v04_infiles[i])!=0)){
        day_69_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.10.h",h08v04_infiles[i])!=0)){
        day_70_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.3.11.h",h08v04_infiles[i])!=0)){
        day_71_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.3.12.h",h08v04_infiles[i])!=0)){
        day_72_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.13.h",h08v04_infiles[i])!=0)){
        day_73_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.14.h",h08v04_infiles[i])!=0)){
        day_74_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.15.h",h08v04_infiles[i])!=0)){
        day_75_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.16.h",h08v04_infiles[i])!=0)){
        day_76_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.17.h",h08v04_infiles[i])!=0)){
        day_77_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.18.h",h08v04_infiles[i])!=0)){
        day_78_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.19.h",h08v04_infiles[i])!=0)){
        day_79_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.20.h",h08v04_infiles[i])!=0)){
        day_80_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.3.21.h",h08v04_infiles[i])!=0)){
        day_81_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.3.22.h",h08v04_infiles[i])!=0)){
        day_82_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.23.h",h08v04_infiles[i])!=0)){
        day_83_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.24.h",h08v04_infiles[i])!=0)){
        day_84_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.25.h",h08v04_infiles[i])!=0)){
        day_85_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.26.h",h08v04_infiles[i])!=0)){
        day_86_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.27.h",h08v04_infiles[i])!=0)){
        day_87_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.28.h",h08v04_infiles[i])!=0)){
        day_88_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.29.h",h08v04_infiles[i])!=0)){
        day_89_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.30.h",h08v04_infiles[i])!=0)){
        day_90_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.31.h",h08v04_infiles[i])!=0)){
        day_91_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.4.1.h",h08v04_infiles[i])!=0)){
        day_92_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.4.2.h",h08v04_infiles[i])!=0)){
        day_93_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.3.h",h08v04_infiles[i])!=0)){
        day_94_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.4.h",h08v04_infiles[i])!=0)){
        day_95_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.5.h",h08v04_infiles[i])!=0)){
        day_96_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.6.h",h08v04_infiles[i])!=0)){
        day_97_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.7.h",h08v04_infiles[i])!=0)){
        day_98_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.8.h",h08v04_infiles[i])!=0)){
        day_99_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.9.h",h08v04_infiles[i])!=0)){
        day_100_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.10.h",h08v04_infiles[i])!=0)){
        day_101_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.4.11.h",h08v04_infiles[i])!=0)){
        day_102_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.4.12.h",h08v04_infiles[i])!=0)){
        day_103_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.13.h",h08v04_infiles[i])!=0)){
        day_104_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.14.h",h08v04_infiles[i])!=0)){
        day_105_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.15.h",h08v04_infiles[i])!=0)){
        day_106_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.16.h",h08v04_infiles[i])!=0)){
        day_107_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.17.h",h08v04_infiles[i])!=0)){
        day_108_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.18.h",h08v04_infiles[i])!=0)){
        day_109_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.19.h",h08v04_infiles[i])!=0)){
        day_110_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.20.h",h08v04_infiles[i])!=0)){
        day_111_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.4.21.h",h08v04_infiles[i])!=0)){
        day_112_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.4.22.h",h08v04_infiles[i])!=0)){
        day_113_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.23.h",h08v04_infiles[i])!=0)){
        day_114_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.24.h",h08v04_infiles[i])!=0)){
        day_115_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.25.h",h08v04_infiles[i])!=0)){
        day_116_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.26.h",h08v04_infiles[i])!=0)){
        day_117_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.27.h",h08v04_infiles[i])!=0)){
        day_118_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.28.h",h08v04_infiles[i])!=0)){
        day_119_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.29.h",h08v04_infiles[i])!=0)){
        day_120_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.30.h",h08v04_infiles[i])!=0)){
        day_121_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.5.1.h",h08v04_infiles[i])!=0)){
        day_122_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.5.2.h",h08v04_infiles[i])!=0)){
        day_123_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.3.h",h08v04_infiles[i])!=0)){
        day_124_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.4.h",h08v04_infiles[i])!=0)){
        day_125_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.5.h",h08v04_infiles[i])!=0)){
        day_126_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.6.h",h08v04_infiles[i])!=0)){
        day_127_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.7.h",h08v04_infiles[i])!=0)){
        day_128_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.8.h",h08v04_infiles[i])!=0)){
        day_129_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.9.h",h08v04_infiles[i])!=0)){
        day_130_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.10.h",h08v04_infiles[i])!=0)){
        day_131_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.5.11.h",h08v04_infiles[i])!=0)){
        day_132_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.5.12.h",h08v04_infiles[i])!=0)){
        day_133_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.13.h",h08v04_infiles[i])!=0)){
        day_134_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.14.h",h08v04_infiles[i])!=0)){
        day_135_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.15.h",h08v04_infiles[i])!=0)){
        day_136_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.16.h",h08v04_infiles[i])!=0)){
        day_137_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.17.h",h08v04_infiles[i])!=0)){
        day_138_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.18.h",h08v04_infiles[i])!=0)){
        day_139_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.19.h",h08v04_infiles[i])!=0)){
        day_140_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.20.h",h08v04_infiles[i])!=0)){
        day_141_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.5.21.h",h08v04_infiles[i])!=0)){
        day_142_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.5.22.h",h08v04_infiles[i])!=0)){
        day_143_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.23.h",h08v04_infiles[i])!=0)){
        day_144_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.24.h",h08v04_infiles[i])!=0)){
        day_145_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.25.h",h08v04_infiles[i])!=0)){
        day_146_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.26.h",h08v04_infiles[i])!=0)){
        day_147_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.27.h",h08v04_infiles[i])!=0)){
        day_148_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.28.h",h08v04_infiles[i])!=0)){
        day_149_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.29.h",h08v04_infiles[i])!=0)){
        day_150_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.30.h",h08v04_infiles[i])!=0)){
        day_151_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.31.h",h08v04_infiles[i])!=0)){
        day_152_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.6.1.h",h08v04_infiles[i])!=0)){
        day_153_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.6.2.h",h08v04_infiles[i])!=0)){
        day_154_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.3.h",h08v04_infiles[i])!=0)){
        day_155_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.4.h",h08v04_infiles[i])!=0)){
        day_156_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.5.h",h08v04_infiles[i])!=0)){
        day_157_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.6.h",h08v04_infiles[i])!=0)){
        day_158_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.7.h",h08v04_infiles[i])!=0)){
        day_159_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.8.h",h08v04_infiles[i])!=0)){
        day_160_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.9.h",h08v04_infiles[i])!=0)){
        day_161_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.10.h",h08v04_infiles[i])!=0)){
        day_162_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.6.11.h",h08v04_infiles[i])!=0)){
        day_163_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.6.12.h",h08v04_infiles[i])!=0)){
        day_164_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.13.h",h08v04_infiles[i])!=0)){
        day_165_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.14.h",h08v04_infiles[i])!=0)){
        day_166_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.15.h",h08v04_infiles[i])!=0)){
        day_167_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.16.h",h08v04_infiles[i])!=0)){
        day_168_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.17.h",h08v04_infiles[i])!=0)){
        day_169_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.18.h",h08v04_infiles[i])!=0)){
        day_170_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.19.h",h08v04_infiles[i])!=0)){
        day_171_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.20.h",h08v04_infiles[i])!=0)){
        day_172_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.6.21.h",h08v04_infiles[i])!=0)){
        day_173_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.6.22.h",h08v04_infiles[i])!=0)){
        day_174_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.23.h",h08v04_infiles[i])!=0)){
        day_175_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.24.h",h08v04_infiles[i])!=0)){
        day_176_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.25.h",h08v04_infiles[i])!=0)){
        day_177_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.26.h",h08v04_infiles[i])!=0)){
        day_178_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.27.h",h08v04_infiles[i])!=0)){
        day_179_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.28.h",h08v04_infiles[i])!=0)){
        day_180_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.29.h",h08v04_infiles[i])!=0)){
        day_181_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.30.h",h08v04_infiles[i])!=0)){
        day_182_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.7.1.h",h08v04_infiles[i])!=0)){
        day_183_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.7.2.h",h08v04_infiles[i])!=0)){
        day_184_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.3.h",h08v04_infiles[i])!=0)){
        day_185_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.4.h",h08v04_infiles[i])!=0)){
        day_186_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.5.h",h08v04_infiles[i])!=0)){
        day_187_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.6.h",h08v04_infiles[i])!=0)){
        day_188_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.7.h",h08v04_infiles[i])!=0)){
        day_189_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.8.h",h08v04_infiles[i])!=0)){
        day_190_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.9.h",h08v04_infiles[i])!=0)){
        day_191_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.10.h",h08v04_infiles[i])!=0)){
        day_192_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.7.11.h",h08v04_infiles[i])!=0)){
        day_193_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.7.12.h",h08v04_infiles[i])!=0)){
        day_194_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.13.h",h08v04_infiles[i])!=0)){
        day_195_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.14.h",h08v04_infiles[i])!=0)){
        day_196_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.15.h",h08v04_infiles[i])!=0)){
        day_197_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.16.h",h08v04_infiles[i])!=0)){
        day_198_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.17.h",h08v04_infiles[i])!=0)){
        day_199_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.18.h",h08v04_infiles[i])!=0)){
        day_200_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.19.h",h08v04_infiles[i])!=0)){
        day_201_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.20.h",h08v04_infiles[i])!=0)){
        day_202_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.7.21.h",h08v04_infiles[i])!=0)){
        day_203_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.7.22.h",h08v04_infiles[i])!=0)){
        day_204_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.23.h",h08v04_infiles[i])!=0)){
        day_205_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.24.h",h08v04_infiles[i])!=0)){
        day_206_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.25.h",h08v04_infiles[i])!=0)){
        day_207_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.26.h",h08v04_infiles[i])!=0)){
        day_208_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.27.h",h08v04_infiles[i])!=0)){
        day_209_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.28.h",h08v04_infiles[i])!=0)){
        day_210_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.29.h",h08v04_infiles[i])!=0)){
        day_211_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.30.h",h08v04_infiles[i])!=0)){
        day_212_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.31.h",h08v04_infiles[i])!=0)){
        day_213_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.8.1.h",h08v04_infiles[i])!=0)){
        day_214_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.8.2.h",h08v04_infiles[i])!=0)){
        day_215_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.3.h",h08v04_infiles[i])!=0)){
        day_216_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.4.h",h08v04_infiles[i])!=0)){
        day_217_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.5.h",h08v04_infiles[i])!=0)){
        day_218_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.6.h",h08v04_infiles[i])!=0)){
        day_219_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.7.h",h08v04_infiles[i])!=0)){
        day_220_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.8.h",h08v04_infiles[i])!=0)){
        day_221_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.9.h",h08v04_infiles[i])!=0)){
        day_222_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.10.h",h08v04_infiles[i])!=0)){
        day_223_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.8.11.h",h08v04_infiles[i])!=0)){
        day_224_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.8.12.h",h08v04_infiles[i])!=0)){
        day_225_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.13.h",h08v04_infiles[i])!=0)){
        day_226_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.14.h",h08v04_infiles[i])!=0)){
        day_227_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.15.h",h08v04_infiles[i])!=0)){
        day_228_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.16.h",h08v04_infiles[i])!=0)){
        day_229_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.17.h",h08v04_infiles[i])!=0)){
        day_230_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.18.h",h08v04_infiles[i])!=0)){
        day_231_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.19.h",h08v04_infiles[i])!=0)){
        day_232_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.20.h",h08v04_infiles[i])!=0)){
        day_233_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.8.21.h",h08v04_infiles[i])!=0)){
        day_234_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.8.22.h",h08v04_infiles[i])!=0)){
        day_235_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.23.h",h08v04_infiles[i])!=0)){
        day_236_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.24.h",h08v04_infiles[i])!=0)){
        day_237_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.25.h",h08v04_infiles[i])!=0)){
        day_238_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.26.h",h08v04_infiles[i])!=0)){
        day_239_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.27.h",h08v04_infiles[i])!=0)){
        day_240_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.28.h",h08v04_infiles[i])!=0)){
        day_241_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.29.h",h08v04_infiles[i])!=0)){
        day_242_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.30.h",h08v04_infiles[i])!=0)){
        day_243_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.31.h",h08v04_infiles[i])!=0)){
        day_244_h08v04[i]<- h08v04_infiles[i]
    }else if(length(grep("\\.9.1.h",h08v04_infiles[i])!=0)){
        day_245_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.9.2.h",h08v04_infiles[i])!=0)){
        day_246_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.3.h",h08v04_infiles[i])!=0)){
        day_247_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.4.h",h08v04_infiles[i])!=0)){
        day_248_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.5.h",h08v04_infiles[i])!=0)){
        day_249_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.6.h",h08v04_infiles[i])!=0)){
        day_250_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.7.h",h08v04_infiles[i])!=0)){
        day_251_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.8.h",h08v04_infiles[i])!=0)){
        day_252_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.9.h",h08v04_infiles[i])!=0)){
        day_253_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.10.h",h08v04_infiles[i])!=0)){
        day_254_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.9.11.h",h08v04_infiles[i])!=0)){
        day_255_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.9.12.h",h08v04_infiles[i])!=0)){
        day_256_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.13.h",h08v04_infiles[i])!=0)){
        day_257_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.14.h",h08v04_infiles[i])!=0)){
        day_258_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.15.h",h08v04_infiles[i])!=0)){
        day_259_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.16.h",h08v04_infiles[i])!=0)){
        day_260_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.17.h",h08v04_infiles[i])!=0)){
        day_261_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.18.h",h08v04_infiles[i])!=0)){
        day_262_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.19.h",h08v04_infiles[i])!=0)){
        day_263_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.20.h",h08v04_infiles[i])!=0)){
        day_264_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.9.21.h",h08v04_infiles[i])!=0)){
        day_265_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.9.22.h",h08v04_infiles[i])!=0)){
        day_266_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.23.h",h08v04_infiles[i])!=0)){
        day_267_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.24.h",h08v04_infiles[i])!=0)){
        day_268_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.25.h",h08v04_infiles[i])!=0)){
        day_269_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.26.h",h08v04_infiles[i])!=0)){
        day_270_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.27.h",h08v04_infiles[i])!=0)){
        day_271_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.28.h",h08v04_infiles[i])!=0)){
        day_272_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.29.h",h08v04_infiles[i])!=0)){
        day_273_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.30.h",h08v04_infiles[i])!=0)){
        day_274_h08v04[i]<- h08v04_infiles[i]
    }else if(length(grep("\\.10.1.h",h08v04_infiles[i])!=0)){
        day_275_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.10.2.h",h08v04_infiles[i])!=0)){
        day_276_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.3.h",h08v04_infiles[i])!=0)){
        day_277_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.4.h",h08v04_infiles[i])!=0)){
        day_278_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.5.h",h08v04_infiles[i])!=0)){
        day_279_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.6.h",h08v04_infiles[i])!=0)){
        day_280_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.7.h",h08v04_infiles[i])!=0)){
        day_281_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.8.h",h08v04_infiles[i])!=0)){
        day_282_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.9.h",h08v04_infiles[i])!=0)){
        day_283_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.10.h",h08v04_infiles[i])!=0)){
        day_284_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.10.11.h",h08v04_infiles[i])!=0)){
        day_285_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.10.12.h",h08v04_infiles[i])!=0)){
        day_286_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.13.h",h08v04_infiles[i])!=0)){
        day_287_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.14.h",h08v04_infiles[i])!=0)){
        day_288_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.15.h",h08v04_infiles[i])!=0)){
        day_289_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.16.h",h08v04_infiles[i])!=0)){
        day_290_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.17.h",h08v04_infiles[i])!=0)){
        day_291_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.18.h",h08v04_infiles[i])!=0)){
        day_292_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.19.h",h08v04_infiles[i])!=0)){
        day_293_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.20.h",h08v04_infiles[i])!=0)){
        day_294_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.10.21.h",h08v04_infiles[i])!=0)){
        day_295_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.10.22.h",h08v04_infiles[i])!=0)){
        day_296_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.23.h",h08v04_infiles[i])!=0)){
        day_297_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.24.h",h08v04_infiles[i])!=0)){
        day_298_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.25.h",h08v04_infiles[i])!=0)){
        day_299_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.26.h",h08v04_infiles[i])!=0)){
        day_300_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.27.h",h08v04_infiles[i])!=0)){
        day_301_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.28.h",h08v04_infiles[i])!=0)){
        day_302_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.29.h",h08v04_infiles[i])!=0)){
        day_303_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.30.h",h08v04_infiles[i])!=0)){
        day_304_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.31.h",h08v04_infiles[i])!=0)){
        day_305_h08v04[i]<- h08v04_infiles[i]
    }else if(length(grep("\\.11.1.h",h08v04_infiles[i])!=0)){
        day_306_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.11.2.h",h08v04_infiles[i])!=0)){
        day_307_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.3.h",h08v04_infiles[i])!=0)){
        day_308_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.4.h",h08v04_infiles[i])!=0)){
        day_309_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.5.h",h08v04_infiles[i])!=0)){
        day_310_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.6.h",h08v04_infiles[i])!=0)){
        day_311_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.7.h",h08v04_infiles[i])!=0)){
        day_312_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.8.h",h08v04_infiles[i])!=0)){
        day_313_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.9.h",h08v04_infiles[i])!=0)){
        day_314_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.10.h",h08v04_infiles[i])!=0)){
        day_315_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.11.11.h",h08v04_infiles[i])!=0)){
        day_316_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.11.12.h",h08v04_infiles[i])!=0)){
        day_317_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.13.h",h08v04_infiles[i])!=0)){
        day_318_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.14.h",h08v04_infiles[i])!=0)){
        day_319_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.15.h",h08v04_infiles[i])!=0)){
        day_320_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.16.h",h08v04_infiles[i])!=0)){
        day_321_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.17.h",h08v04_infiles[i])!=0)){
        day_322_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.18.h",h08v04_infiles[i])!=0)){
        day_323_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.19.h",h08v04_infiles[i])!=0)){
        day_324_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.20.h",h08v04_infiles[i])!=0)){
        day_325_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.11.21.h",h08v04_infiles[i])!=0)){
        day_326_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.11.22.h",h08v04_infiles[i])!=0)){
        day_327_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.23.h",h08v04_infiles[i])!=0)){
        day_328_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.24.h",h08v04_infiles[i])!=0)){
        day_329_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.25.h",h08v04_infiles[i])!=0)){
        day_330_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.26.h",h08v04_infiles[i])!=0)){
        day_331_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.27.h",h08v04_infiles[i])!=0)){
        day_332_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.28.h",h08v04_infiles[i])!=0)){
        day_333_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.29.h",h08v04_infiles[i])!=0)){
        day_334_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.30.h",h08v04_infiles[i])!=0)){
        day_335_h08v04[i]<- h08v04_infiles[i]
    }else if(length(grep("\\.12.1.h",h08v04_infiles[i])!=0)){
        day_336_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.12.2.h",h08v04_infiles[i])!=0)){
        day_337_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.3.h",h08v04_infiles[i])!=0)){
        day_338_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.4.h",h08v04_infiles[i])!=0)){
        day_339_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.5.h",h08v04_infiles[i])!=0)){
        day_340_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.6.h",h08v04_infiles[i])!=0)){
        day_341_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.7.h",h08v04_infiles[i])!=0)){
        day_342_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.8.h",h08v04_infiles[i])!=0)){
        day_343_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.9.h",h08v04_infiles[i])!=0)){
        day_344_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.10.h",h08v04_infiles[i])!=0)){
        day_345_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.12.11.h",h08v04_infiles[i])!=0)){
        day_346_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.12.12.h",h08v04_infiles[i])!=0)){
        day_347_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.13.h",h08v04_infiles[i])!=0)){
        day_348_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.14.h",h08v04_infiles[i])!=0)){
        day_349_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.15.h",h08v04_infiles[i])!=0)){
        day_350_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.16.h",h08v04_infiles[i])!=0)){
        day_351_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.17.h",h08v04_infiles[i])!=0)){
        day_352_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.18.h",h08v04_infiles[i])!=0)){
        day_353_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.19.h",h08v04_infiles[i])!=0)){
        day_354_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.20.h",h08v04_infiles[i])!=0)){
        day_355_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.12.21.h",h08v04_infiles[i])!=0)){
        day_356_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.12.22.h",h08v04_infiles[i])!=0)){
        day_357_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.23.h",h08v04_infiles[i])!=0)){
        day_358_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.24.h",h08v04_infiles[i])!=0)){
        day_359_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.25.h",h08v04_infiles[i])!=0)){
        day_360_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.26.h",h08v04_infiles[i])!=0)){
        day_361_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.27.h",h08v04_infiles[i])!=0)){
        day_362_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.28.h",h08v04_infiles[i])!=0)){
        day_363_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.29.h",h08v04_infiles[i])!=0)){
        day_364_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.30.h",h08v04_infiles[i])!=0)){
        day_365_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.31.h",h08v04_infiles[i])!=0)){
        day_366_h08v04[i]<- h08v04_infiles[i]  
    }
  }else #if (length(grep("\\.2004\\.",h08v04_infiles[i])==0) || length(grep("\\.2008\\.",h08v04_infiles[i])==0)){
    if(length(grep("\\.3.1.h",h08v04_infiles[i])!=0)){
        day_60_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.3.2.h",h08v04_infiles[i])!=0)){
        day_61_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.3.h",h08v04_infiles[i])!=0)){
        day_62_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.4.h",h08v04_infiles[i])!=0)){
        day_63_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.5.h",h08v04_infiles[i])!=0)){
        day_64_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.6.h",h08v04_infiles[i])!=0)){
        day_65_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.7.h",h08v04_infiles[i])!=0)){
        day_66_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.8.h",h08v04_infiles[i])!=0)){
        day_67_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.9.h",h08v04_infiles[i])!=0)){
        day_68_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.10.h",h08v04_infiles[i])!=0)){
        day_69_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.3.11.h",h08v04_infiles[i])!=0)){
        day_70_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.3.12.h",h08v04_infiles[i])!=0)){
        day_71_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.13.h",h08v04_infiles[i])!=0)){
        day_72_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.14.h",h08v04_infiles[i])!=0)){
        day_73_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.15.h",h08v04_infiles[i])!=0)){
        day_74_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.16.h",h08v04_infiles[i])!=0)){
        day_75_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.17.h",h08v04_infiles[i])!=0)){
        day_76_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.18.h",h08v04_infiles[i])!=0)){
        day_77_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.19.h",h08v04_infiles[i])!=0)){
        day_78_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.20.h",h08v04_infiles[i])!=0)){
        day_79_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.3.21.h",h08v04_infiles[i])!=0)){
        day_80_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.3.22.h",h08v04_infiles[i])!=0)){
        day_81_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.23.h",h08v04_infiles[i])!=0)){
        day_82_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.24.h",h08v04_infiles[i])!=0)){
        day_83_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.25.h",h08v04_infiles[i])!=0)){
        day_84_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.26.h",h08v04_infiles[i])!=0)){
        day_85_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.27.h",h08v04_infiles[i])!=0)){
        day_86_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.28.h",h08v04_infiles[i])!=0)){
        day_87_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.29.h",h08v04_infiles[i])!=0)){
        day_88_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.30.h",h08v04_infiles[i])!=0)){
        day_89_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.3.31.h",h08v04_infiles[i])!=0)){
        day_90_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.4.1.h",h08v04_infiles[i])!=0)){
        day_91_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.4.2.h",h08v04_infiles[i])!=0)){
        day_92_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.3.h",h08v04_infiles[i])!=0)){
        day_93_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.4.h",h08v04_infiles[i])!=0)){
        day_94_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.5.h",h08v04_infiles[i])!=0)){
        day_95_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.6.h",h08v04_infiles[i])!=0)){
        day_96_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.7.h",h08v04_infiles[i])!=0)){
        day_97_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.8.h",h08v04_infiles[i])!=0)){
        day_98_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.9.h",h08v04_infiles[i])!=0)){
        day_99_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.10.h",h08v04_infiles[i])!=0)){
        day_100_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.4.11.h",h08v04_infiles[i])!=0)){
        day_101_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.4.12.h",h08v04_infiles[i])!=0)){
        day_102_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.13.h",h08v04_infiles[i])!=0)){
        day_103_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.14.h",h08v04_infiles[i])!=0)){
        day_104_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.15.h",h08v04_infiles[i])!=0)){
        day_105_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.16.h",h08v04_infiles[i])!=0)){
        day_106_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.17.h",h08v04_infiles[i])!=0)){
        day_107_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.18.h",h08v04_infiles[i])!=0)){
        day_108_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.19.h",h08v04_infiles[i])!=0)){
        day_109_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.20.h",h08v04_infiles[i])!=0)){
        day_110_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.4.21.h",h08v04_infiles[i])!=0)){
        day_111_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.4.22.h",h08v04_infiles[i])!=0)){
        day_112_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.23.h",h08v04_infiles[i])!=0)){
        day_113_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.24.h",h08v04_infiles[i])!=0)){
        day_114_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.25.h",h08v04_infiles[i])!=0)){
        day_115_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.26.h",h08v04_infiles[i])!=0)){
        day_116_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.27.h",h08v04_infiles[i])!=0)){
        day_117_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.28.h",h08v04_infiles[i])!=0)){
        day_118_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.29.h",h08v04_infiles[i])!=0)){
        day_119_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.4.30.h",h08v04_infiles[i])!=0)){
        day_120_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.5.1.h",h08v04_infiles[i])!=0)){
        day_121_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.5.2.h",h08v04_infiles[i])!=0)){
        day_122_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.3.h",h08v04_infiles[i])!=0)){
        day_123_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.4.h",h08v04_infiles[i])!=0)){
        day_124_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.5.h",h08v04_infiles[i])!=0)){
        day_125_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.6.h",h08v04_infiles[i])!=0)){
        day_126_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.7.h",h08v04_infiles[i])!=0)){
        day_127_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.8.h",h08v04_infiles[i])!=0)){
        day_128_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.9.h",h08v04_infiles[i])!=0)){
        day_129_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.10.h",h08v04_infiles[i])!=0)){
        day_130_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.5.11.h",h08v04_infiles[i])!=0)){
        day_131_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.5.12.h",h08v04_infiles[i])!=0)){
        day_132_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.13.h",h08v04_infiles[i])!=0)){
        day_133_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.14.h",h08v04_infiles[i])!=0)){
        day_134_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.15.h",h08v04_infiles[i])!=0)){
        day_135_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.16.h",h08v04_infiles[i])!=0)){
        day_136_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.17.h",h08v04_infiles[i])!=0)){
        day_137_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.18.h",h08v04_infiles[i])!=0)){
        day_138_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.19.h",h08v04_infiles[i])!=0)){
        day_139_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.20.h",h08v04_infiles[i])!=0)){
        day_140_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.5.21.h",h08v04_infiles[i])!=0)){
        day_141_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.5.22.h",h08v04_infiles[i])!=0)){
        day_142_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.23.h",h08v04_infiles[i])!=0)){
        day_143_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.24.h",h08v04_infiles[i])!=0)){
        day_144_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.25.h",h08v04_infiles[i])!=0)){
        day_145_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.26.h",h08v04_infiles[i])!=0)){
        day_146_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.27.h",h08v04_infiles[i])!=0)){
        day_147_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.28.h",h08v04_infiles[i])!=0)){
        day_148_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.29.h",h08v04_infiles[i])!=0)){
        day_149_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.30.h",h08v04_infiles[i])!=0)){
        day_150_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.5.31.h",h08v04_infiles[i])!=0)){
        day_151_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.6.1.h",h08v04_infiles[i])!=0)){
        day_152_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.6.2.h",h08v04_infiles[i])!=0)){
        day_153_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.3.h",h08v04_infiles[i])!=0)){
        day_154_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.4.h",h08v04_infiles[i])!=0)){
        day_155_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.5.h",h08v04_infiles[i])!=0)){
        day_156_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.6.h",h08v04_infiles[i])!=0)){
        day_157_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.7.h",h08v04_infiles[i])!=0)){
        day_158_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.8.h",h08v04_infiles[i])!=0)){
        day_159_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.9.h",h08v04_infiles[i])!=0)){
        day_160_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.10.h",h08v04_infiles[i])!=0)){
        day_161_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.6.11.h",h08v04_infiles[i])!=0)){
        day_162_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.6.12.h",h08v04_infiles[i])!=0)){
        day_163_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.13.h",h08v04_infiles[i])!=0)){
        day_164_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.14.h",h08v04_infiles[i])!=0)){
        day_165_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.15.h",h08v04_infiles[i])!=0)){
        day_166_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.16.h",h08v04_infiles[i])!=0)){
        day_167_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.17.h",h08v04_infiles[i])!=0)){
        day_168_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.18.h",h08v04_infiles[i])!=0)){
        day_169_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.19.h",h08v04_infiles[i])!=0)){
        day_170_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.20.h",h08v04_infiles[i])!=0)){
        day_171_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.6.21.h",h08v04_infiles[i])!=0)){
        day_172_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.6.22.h",h08v04_infiles[i])!=0)){
        day_173_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.23.h",h08v04_infiles[i])!=0)){
        day_174_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.24.h",h08v04_infiles[i])!=0)){
        day_175_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.25.h",h08v04_infiles[i])!=0)){
        day_176_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.26.h",h08v04_infiles[i])!=0)){
        day_177_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.27.h",h08v04_infiles[i])!=0)){
        day_178_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.28.h",h08v04_infiles[i])!=0)){
        day_179_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.29.h",h08v04_infiles[i])!=0)){
        day_180_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.6.30.h",h08v04_infiles[i])!=0)){
        day_181_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.7.1.h",h08v04_infiles[i])!=0)){
        day_182_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.7.2.h",h08v04_infiles[i])!=0)){
        day_183_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.3.h",h08v04_infiles[i])!=0)){
        day_184_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.4.h",h08v04_infiles[i])!=0)){
        day_185_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.5.h",h08v04_infiles[i])!=0)){
        day_186_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.6.h",h08v04_infiles[i])!=0)){
        day_187_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.7.h",h08v04_infiles[i])!=0)){
        day_188_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.8.h",h08v04_infiles[i])!=0)){
        day_189_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.9.h",h08v04_infiles[i])!=0)){
        day_190_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.10.h",h08v04_infiles[i])!=0)){
        day_191_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.7.11.h",h08v04_infiles[i])!=0)){
        day_192_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.7.12.h",h08v04_infiles[i])!=0)){
        day_193_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.13.h",h08v04_infiles[i])!=0)){
        day_194_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.14.h",h08v04_infiles[i])!=0)){
        day_195_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.15.h",h08v04_infiles[i])!=0)){
        day_196_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.16.h",h08v04_infiles[i])!=0)){
        day_197_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.17.h",h08v04_infiles[i])!=0)){
        day_198_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.18.h",h08v04_infiles[i])!=0)){
        day_199_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.19.h",h08v04_infiles[i])!=0)){
        day_200_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.20.h",h08v04_infiles[i])!=0)){
        day_201_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.7.21.h",h08v04_infiles[i])!=0)){
        day_202_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.7.22.h",h08v04_infiles[i])!=0)){
        day_203_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.23.h",h08v04_infiles[i])!=0)){
        day_204_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.24.h",h08v04_infiles[i])!=0)){
        day_205_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.25.h",h08v04_infiles[i])!=0)){
        day_206_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.26.h",h08v04_infiles[i])!=0)){
        day_207_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.27.h",h08v04_infiles[i])!=0)){
        day_208_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.28.h",h08v04_infiles[i])!=0)){
        day_209_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.29.h",h08v04_infiles[i])!=0)){
        day_210_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.30.h",h08v04_infiles[i])!=0)){
        day_211_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.7.31.h",h08v04_infiles[i])!=0)){
        day_212_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.8.1.h",h08v04_infiles[i])!=0)){
        day_213_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.8.2.h",h08v04_infiles[i])!=0)){
        day_214_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.3.h",h08v04_infiles[i])!=0)){
        day_215_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.4.h",h08v04_infiles[i])!=0)){
        day_216_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.5.h",h08v04_infiles[i])!=0)){
        day_217_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.6.h",h08v04_infiles[i])!=0)){
        day_218_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.7.h",h08v04_infiles[i])!=0)){
        day_219_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.8.h",h08v04_infiles[i])!=0)){
        day_220_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.9.h",h08v04_infiles[i])!=0)){
        day_221_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.10.h",h08v04_infiles[i])!=0)){
        day_222_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.8.11.h",h08v04_infiles[i])!=0)){
        day_223_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.8.12.h",h08v04_infiles[i])!=0)){
        day_224_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.13.h",h08v04_infiles[i])!=0)){
        day_225_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.14.h",h08v04_infiles[i])!=0)){
        day_226_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.15.h",h08v04_infiles[i])!=0)){
        day_227_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.16.h",h08v04_infiles[i])!=0)){
        day_228_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.17.h",h08v04_infiles[i])!=0)){
        day_229_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.18.h",h08v04_infiles[i])!=0)){
        day_230_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.19.h",h08v04_infiles[i])!=0)){
        day_231_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.20.h",h08v04_infiles[i])!=0)){
        day_232_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.8.21.h",h08v04_infiles[i])!=0)){
        day_233_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.8.22.h",h08v04_infiles[i])!=0)){
        day_234_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.23.h",h08v04_infiles[i])!=0)){
        day_235_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.24.h",h08v04_infiles[i])!=0)){
        day_236_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.25.h",h08v04_infiles[i])!=0)){
        day_237_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.26.h",h08v04_infiles[i])!=0)){
        day_238_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.27.h",h08v04_infiles[i])!=0)){
        day_239_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.28.h",h08v04_infiles[i])!=0)){
        day_240_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.29.h",h08v04_infiles[i])!=0)){
        day_241_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.30.h",h08v04_infiles[i])!=0)){
        day_242_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.8.31.h",h08v04_infiles[i])!=0)){
        day_243_h08v04[i]<- h08v04_infiles[i]
    }else if(length(grep("\\.9.1.h",h08v04_infiles[i])!=0)){
        day_244_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.9.2.h",h08v04_infiles[i])!=0)){
        day_245_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.3.h",h08v04_infiles[i])!=0)){
        day_246_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.4.h",h08v04_infiles[i])!=0)){
        day_247_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.5.h",h08v04_infiles[i])!=0)){
        day_248_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.6.h",h08v04_infiles[i])!=0)){
        day_249_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.7.h",h08v04_infiles[i])!=0)){
        day_250_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.8.h",h08v04_infiles[i])!=0)){
        day_251_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.9.h",h08v04_infiles[i])!=0)){
        day_252_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.10.h",h08v04_infiles[i])!=0)){
        day_253_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.9.11.h",h08v04_infiles[i])!=0)){
        day_254_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.9.12.h",h08v04_infiles[i])!=0)){
        day_255_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.13.h",h08v04_infiles[i])!=0)){
        day_256_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.14.h",h08v04_infiles[i])!=0)){
        day_257_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.15.h",h08v04_infiles[i])!=0)){
        day_258_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.16.h",h08v04_infiles[i])!=0)){
        day_259_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.17.h",h08v04_infiles[i])!=0)){
        day_260_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.18.h",h08v04_infiles[i])!=0)){
        day_261_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.19.h",h08v04_infiles[i])!=0)){
        day_262_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.20.h",h08v04_infiles[i])!=0)){
        day_263_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.9.21.h",h08v04_infiles[i])!=0)){
        day_264_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.9.22.h",h08v04_infiles[i])!=0)){
        day_265_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.23.h",h08v04_infiles[i])!=0)){
        day_266_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.24.h",h08v04_infiles[i])!=0)){
        day_267_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.25.h",h08v04_infiles[i])!=0)){
        day_268_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.26.h",h08v04_infiles[i])!=0)){
        day_269_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.27.h",h08v04_infiles[i])!=0)){
        day_270_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.28.h",h08v04_infiles[i])!=0)){
        day_271_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.29.h",h08v04_infiles[i])!=0)){
        day_272_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.9.30.h",h08v04_infiles[i])!=0)){
        day_273_h08v04[i]<- h08v04_infiles[i]
    }else if(length(grep("\\.10.1.h",h08v04_infiles[i])!=0)){
        day_274_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.10.2.h",h08v04_infiles[i])!=0)){
        day_275_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.3.h",h08v04_infiles[i])!=0)){
        day_276_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.4.h",h08v04_infiles[i])!=0)){
        day_277_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.5.h",h08v04_infiles[i])!=0)){
        day_278_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.6.h",h08v04_infiles[i])!=0)){
        day_279_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.7.h",h08v04_infiles[i])!=0)){
        day_280_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.8.h",h08v04_infiles[i])!=0)){
        day_281_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.9.h",h08v04_infiles[i])!=0)){
        day_282_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.10.h",h08v04_infiles[i])!=0)){
        day_283_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.10.11.h",h08v04_infiles[i])!=0)){
        day_284_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.10.12.h",h08v04_infiles[i])!=0)){
        day_285_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.13.h",h08v04_infiles[i])!=0)){
        day_286_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.14.h",h08v04_infiles[i])!=0)){
        day_287_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.15.h",h08v04_infiles[i])!=0)){
        day_288_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.16.h",h08v04_infiles[i])!=0)){
        day_289_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.17.h",h08v04_infiles[i])!=0)){
        day_290_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.18.h",h08v04_infiles[i])!=0)){
        day_291_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.19.h",h08v04_infiles[i])!=0)){
        day_292_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.20.h",h08v04_infiles[i])!=0)){
        day_293_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.10.21.h",h08v04_infiles[i])!=0)){
        day_294_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.10.22.h",h08v04_infiles[i])!=0)){
        day_295_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.23.h",h08v04_infiles[i])!=0)){
        day_296_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.24.h",h08v04_infiles[i])!=0)){
        day_297_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.25.h",h08v04_infiles[i])!=0)){
        day_298_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.26.h",h08v04_infiles[i])!=0)){
        day_299_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.27.h",h08v04_infiles[i])!=0)){
        day_300_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.28.h",h08v04_infiles[i])!=0)){
        day_301_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.29.h",h08v04_infiles[i])!=0)){
        day_302_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.30.h",h08v04_infiles[i])!=0)){
        day_303_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.10.31.h",h08v04_infiles[i])!=0)){
        day_304_h08v04[i]<- h08v04_infiles[i]
    }else if(length(grep("\\.11.1.h",h08v04_infiles[i])!=0)){
        day_305_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.11.2.h",h08v04_infiles[i])!=0)){
        day_306_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.3.h",h08v04_infiles[i])!=0)){
        day_307_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.4.h",h08v04_infiles[i])!=0)){
        day_308_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.5.h",h08v04_infiles[i])!=0)){
        day_309_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.6.h",h08v04_infiles[i])!=0)){
        day_310_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.7.h",h08v04_infiles[i])!=0)){
        day_311_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.8.h",h08v04_infiles[i])!=0)){
        day_312_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.9.h",h08v04_infiles[i])!=0)){
        day_313_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.10.h",h08v04_infiles[i])!=0)){
        day_314_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.11.11.h",h08v04_infiles[i])!=0)){
        day_315_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.11.12.h",h08v04_infiles[i])!=0)){
        day_316_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.13.h",h08v04_infiles[i])!=0)){
        day_317_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.14.h",h08v04_infiles[i])!=0)){
        day_318_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.15.h",h08v04_infiles[i])!=0)){
        day_319_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.16.h",h08v04_infiles[i])!=0)){
        day_320_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.17.h",h08v04_infiles[i])!=0)){
        day_321_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.18.h",h08v04_infiles[i])!=0)){
        day_322_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.19.h",h08v04_infiles[i])!=0)){
        day_323_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.20.h",h08v04_infiles[i])!=0)){
        day_324_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.11.21.h",h08v04_infiles[i])!=0)){
        day_325_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.11.22.h",h08v04_infiles[i])!=0)){
        day_326_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.23.h",h08v04_infiles[i])!=0)){
        day_327_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.24.h",h08v04_infiles[i])!=0)){
        day_328_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.25.h",h08v04_infiles[i])!=0)){
        day_329_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.26.h",h08v04_infiles[i])!=0)){
        day_330_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.27.h",h08v04_infiles[i])!=0)){
        day_331_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.28.h",h08v04_infiles[i])!=0)){
        day_332_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.29.h",h08v04_infiles[i])!=0)){
        day_333_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.11.30.h",h08v04_infiles[i])!=0)){
        day_334_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.12.1.h",h08v04_infiles[i])!=0)){
        day_335_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.12.2.h",h08v04_infiles[i])!=0)){
        day_336_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.3.h",h08v04_infiles[i])!=0)){
        day_337_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.4.h",h08v04_infiles[i])!=0)){
        day_338_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.5.h",h08v04_infiles[i])!=0)){
        day_339_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.6.h",h08v04_infiles[i])!=0)){
        day_340_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.7.h",h08v04_infiles[i])!=0)){
        day_341_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.8.h",h08v04_infiles[i])!=0)){
        day_342_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.9.h",h08v04_infiles[i])!=0)){
        day_343_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.10.h",h08v04_infiles[i])!=0)){
        day_344_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.12.11.h",h08v04_infiles[i])!=0)){
        day_345_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.12.12.h",h08v04_infiles[i])!=0)){
        day_346_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.13.h",h08v04_infiles[i])!=0)){
        day_347_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.14.h",h08v04_infiles[i])!=0)){
        day_348_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.15.h",h08v04_infiles[i])!=0)){
        day_349_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.16.h",h08v04_infiles[i])!=0)){
        day_350_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.17.h",h08v04_infiles[i])!=0)){
        day_351_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.18.h",h08v04_infiles[i])!=0)){
        day_352_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.19.h",h08v04_infiles[i])!=0)){
        day_353_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.20.h",h08v04_infiles[i])!=0)){
        day_354_h08v04[i]<- h08v04_infiles[i]
    } else if(length(grep("\\.12.21.h",h08v04_infiles[i])!=0)){
        day_355_h08v04[i]<- h08v04_infiles[i]         
    } else if (length(grep("\\.12.22.h",h08v04_infiles[i])!=0)){
        day_356_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.23.h",h08v04_infiles[i])!=0)){
        day_357_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.24.h",h08v04_infiles[i])!=0)){
        day_358_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.25.h",h08v04_infiles[i])!=0)){
        day_359_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.26.h",h08v04_infiles[i])!=0)){
        day_360_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.27.h",h08v04_infiles[i])!=0)){
        day_361_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.28.h",h08v04_infiles[i])!=0)){
        day_362_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.29.h",h08v04_infiles[i])!=0)){
        day_363_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.30.h",h08v04_infiles[i])!=0)){
        day_364_h08v04[i]<- h08v04_infiles[i]
    } else if (length(grep("\\.12.31.h",h08v04_infiles[i])!=0)){
        day_365_h08v04[i]<- h08v04_infiles[i]
    }
}
  

day_60_h08v04<- day_60_h08v04[!is.na(day_60_h08v04)]
day_61_h08v04<- day_61_h08v04[!is.na(day_61_h08v04)]
day_62_h08v04<- day_62_h08v04[!is.na(day_62_h08v04)]
day_63_h08v04<- day_63_h08v04[!is.na(day_63_h08v04)]
day_64_h08v04<- day_64_h08v04[!is.na(day_64_h08v04)]
day_65_h08v04<- day_65_h08v04[!is.na(day_65_h08v04)]
day_66_h08v04<- day_66_h08v04[!is.na(day_66_h08v04)]
day_67_h08v04<- day_67_h08v04[!is.na(day_67_h08v04)]
day_68_h08v04<- day_68_h08v04[!is.na(day_68_h08v04)]
day_69_h08v04<- day_69_h08v04[!is.na(day_69_h08v04)]
day_70_h08v04<- day_70_h08v04[!is.na(day_70_h08v04)]
day_71_h08v04<- day_71_h08v04[!is.na(day_71_h08v04)]
day_72_h08v04<- day_72_h08v04[!is.na(day_72_h08v04)]
day_73_h08v04<- day_73_h08v04[!is.na(day_73_h08v04)]
day_74_h08v04<- day_74_h08v04[!is.na(day_74_h08v04)]
day_75_h08v04<- day_75_h08v04[!is.na(day_75_h08v04)]
day_76_h08v04<- day_76_h08v04[!is.na(day_76_h08v04)]
day_77_h08v04<- day_77_h08v04[!is.na(day_77_h08v04)]
day_78_h08v04<- day_78_h08v04[!is.na(day_78_h08v04)]
day_79_h08v04<- day_79_h08v04[!is.na(day_79_h08v04)]
day_80_h08v04<- day_80_h08v04[!is.na(day_80_h08v04)]
day_81_h08v04<- day_81_h08v04[!is.na(day_81_h08v04)]
day_82_h08v04<- day_82_h08v04[!is.na(day_82_h08v04)]
day_83_h08v04<- day_83_h08v04[!is.na(day_83_h08v04)]
day_84_h08v04<- day_84_h08v04[!is.na(day_84_h08v04)]
day_85_h08v04<- day_85_h08v04[!is.na(day_85_h08v04)]
day_86_h08v04<- day_86_h08v04[!is.na(day_86_h08v04)]
day_87_h08v04<- day_87_h08v04[!is.na(day_87_h08v04)]
day_88_h08v04<- day_88_h08v04[!is.na(day_88_h08v04)]
day_89_h08v04<- day_89_h08v04[!is.na(day_89_h08v04)]
day_90_h08v04<- day_90_h08v04[!is.na(day_90_h08v04)]
day_91_h08v04<- day_91_h08v04[!is.na(day_91_h08v04)]
day_92_h08v04<- day_92_h08v04[!is.na(day_92_h08v04)]
day_93_h08v04<- day_93_h08v04[!is.na(day_93_h08v04)]
day_94_h08v04<- day_94_h08v04[!is.na(day_94_h08v04)]
day_95_h08v04<- day_95_h08v04[!is.na(day_95_h08v04)]
day_96_h08v04<- day_96_h08v04[!is.na(day_96_h08v04)]
day_97_h08v04<- day_97_h08v04[!is.na(day_97_h08v04)]
day_98_h08v04<- day_98_h08v04[!is.na(day_98_h08v04)]
day_99_h08v04<- day_99_h08v04[!is.na(day_99_h08v04)]
day_100_h08v04<- day_100_h08v04[!is.na(day_100_h08v04)]
day_101_h08v04<- day_101_h08v04[!is.na(day_101_h08v04)]
day_102_h08v04<- day_102_h08v04[!is.na(day_102_h08v04)]
day_103_h08v04<- day_103_h08v04[!is.na(day_103_h08v04)]
day_104_h08v04<- day_104_h08v04[!is.na(day_104_h08v04)]
day_105_h08v04<- day_105_h08v04[!is.na(day_105_h08v04)]
day_106_h08v04<- day_106_h08v04[!is.na(day_106_h08v04)]
day_107_h08v04<- day_107_h08v04[!is.na(day_107_h08v04)]
day_108_h08v04<- day_108_h08v04[!is.na(day_108_h08v04)]
day_109_h08v04<- day_109_h08v04[!is.na(day_109_h08v04)]
day_110_h08v04<- day_110_h08v04[!is.na(day_110_h08v04)]
day_111_h08v04<- day_111_h08v04[!is.na(day_111_h08v04)]
day_112_h08v04<- day_112_h08v04[!is.na(day_112_h08v04)]
day_113_h08v04<- day_113_h08v04[!is.na(day_113_h08v04)]
day_114_h08v04<- day_114_h08v04[!is.na(day_114_h08v04)]
day_115_h08v04<- day_115_h08v04[!is.na(day_115_h08v04)]
day_116_h08v04<- day_116_h08v04[!is.na(day_116_h08v04)]
day_117_h08v04<- day_117_h08v04[!is.na(day_117_h08v04)]
day_118_h08v04<- day_118_h08v04[!is.na(day_118_h08v04)]
day_119_h08v04<- day_119_h08v04[!is.na(day_119_h08v04)]
day_120_h08v04<- day_120_h08v04[!is.na(day_120_h08v04)]
day_121_h08v04<- day_121_h08v04[!is.na(day_121_h08v04)]
day_122_h08v04<- day_122_h08v04[!is.na(day_122_h08v04)]
day_123_h08v04<- day_123_h08v04[!is.na(day_123_h08v04)]
day_124_h08v04<- day_124_h08v04[!is.na(day_124_h08v04)]
day_125_h08v04<- day_125_h08v04[!is.na(day_125_h08v04)]
day_126_h08v04<- day_126_h08v04[!is.na(day_126_h08v04)]
day_127_h08v04<- day_127_h08v04[!is.na(day_127_h08v04)]
day_128_h08v04<- day_128_h08v04[!is.na(day_128_h08v04)]
day_129_h08v04<- day_129_h08v04[!is.na(day_129_h08v04)]
day_130_h08v04<- day_130_h08v04[!is.na(day_130_h08v04)]
day_131_h08v04<- day_131_h08v04[!is.na(day_131_h08v04)]
day_132_h08v04<- day_132_h08v04[!is.na(day_132_h08v04)]
day_133_h08v04<- day_133_h08v04[!is.na(day_133_h08v04)]
day_134_h08v04<- day_134_h08v04[!is.na(day_134_h08v04)]
day_135_h08v04<- day_135_h08v04[!is.na(day_135_h08v04)]
day_136_h08v04<- day_136_h08v04[!is.na(day_136_h08v04)]
day_137_h08v04<- day_137_h08v04[!is.na(day_137_h08v04)]
day_138_h08v04<- day_138_h08v04[!is.na(day_138_h08v04)]
day_139_h08v04<- day_139_h08v04[!is.na(day_139_h08v04)]
day_140_h08v04<- day_140_h08v04[!is.na(day_140_h08v04)]
day_141_h08v04<- day_141_h08v04[!is.na(day_141_h08v04)]
day_142_h08v04<- day_142_h08v04[!is.na(day_142_h08v04)]
day_143_h08v04<- day_143_h08v04[!is.na(day_143_h08v04)]
day_144_h08v04<- day_144_h08v04[!is.na(day_144_h08v04)]
day_145_h08v04<- day_145_h08v04[!is.na(day_145_h08v04)]
day_146_h08v04<- day_146_h08v04[!is.na(day_146_h08v04)]
day_147_h08v04<- day_147_h08v04[!is.na(day_147_h08v04)]
day_148_h08v04<- day_148_h08v04[!is.na(day_148_h08v04)]
day_149_h08v04<- day_149_h08v04[!is.na(day_149_h08v04)]
day_150_h08v04<- day_150_h08v04[!is.na(day_150_h08v04)]
day_151_h08v04<- day_151_h08v04[!is.na(day_151_h08v04)]
day_152_h08v04<- day_152_h08v04[!is.na(day_152_h08v04)]
day_153_h08v04<- day_153_h08v04[!is.na(day_153_h08v04)]
day_154_h08v04<- day_154_h08v04[!is.na(day_154_h08v04)]
day_155_h08v04<- day_155_h08v04[!is.na(day_155_h08v04)]
day_156_h08v04<- day_156_h08v04[!is.na(day_156_h08v04)]
day_157_h08v04<- day_157_h08v04[!is.na(day_157_h08v04)]
day_158_h08v04<- day_158_h08v04[!is.na(day_158_h08v04)]
day_159_h08v04<- day_159_h08v04[!is.na(day_159_h08v04)]
day_160_h08v04<- day_160_h08v04[!is.na(day_160_h08v04)]
day_161_h08v04<- day_161_h08v04[!is.na(day_161_h08v04)]
day_162_h08v04<- day_162_h08v04[!is.na(day_162_h08v04)]
day_163_h08v04<- day_163_h08v04[!is.na(day_163_h08v04)]
day_164_h08v04<- day_164_h08v04[!is.na(day_164_h08v04)]
day_165_h08v04<- day_165_h08v04[!is.na(day_165_h08v04)]
day_166_h08v04<- day_166_h08v04[!is.na(day_166_h08v04)]
day_167_h08v04<- day_167_h08v04[!is.na(day_167_h08v04)]
day_168_h08v04<- day_168_h08v04[!is.na(day_168_h08v04)]
day_169_h08v04<- day_169_h08v04[!is.na(day_169_h08v04)]
day_170_h08v04<- day_170_h08v04[!is.na(day_170_h08v04)]
day_171_h08v04<- day_171_h08v04[!is.na(day_171_h08v04)]
day_172_h08v04<- day_172_h08v04[!is.na(day_172_h08v04)]
day_173_h08v04<- day_173_h08v04[!is.na(day_173_h08v04)]
day_174_h08v04<- day_174_h08v04[!is.na(day_174_h08v04)]
day_175_h08v04<- day_175_h08v04[!is.na(day_175_h08v04)]
day_176_h08v04<- day_176_h08v04[!is.na(day_176_h08v04)]
day_177_h08v04<- day_177_h08v04[!is.na(day_177_h08v04)]
day_178_h08v04<- day_178_h08v04[!is.na(day_178_h08v04)]
day_179_h08v04<- day_179_h08v04[!is.na(day_179_h08v04)]
day_180_h08v04<- day_180_h08v04[!is.na(day_180_h08v04)]
day_181_h08v04<- day_181_h08v04[!is.na(day_181_h08v04)]
day_182_h08v04<- day_182_h08v04[!is.na(day_182_h08v04)]
day_183_h08v04<- day_183_h08v04[!is.na(day_183_h08v04)]
day_184_h08v04<- day_184_h08v04[!is.na(day_184_h08v04)]
day_185_h08v04<- day_185_h08v04[!is.na(day_185_h08v04)]
day_186_h08v04<- day_186_h08v04[!is.na(day_186_h08v04)]
day_187_h08v04<- day_187_h08v04[!is.na(day_187_h08v04)]
day_188_h08v04<- day_188_h08v04[!is.na(day_188_h08v04)]
day_189_h08v04<- day_189_h08v04[!is.na(day_189_h08v04)]
day_190_h08v04<- day_190_h08v04[!is.na(day_190_h08v04)]
day_191_h08v04<- day_191_h08v04[!is.na(day_191_h08v04)]
day_192_h08v04<- day_192_h08v04[!is.na(day_192_h08v04)]
day_193_h08v04<- day_193_h08v04[!is.na(day_193_h08v04)]
day_194_h08v04<- day_194_h08v04[!is.na(day_194_h08v04)]
day_195_h08v04<- day_195_h08v04[!is.na(day_195_h08v04)]
day_196_h08v04<- day_196_h08v04[!is.na(day_196_h08v04)]
day_197_h08v04<- day_197_h08v04[!is.na(day_197_h08v04)]
day_198_h08v04<- day_198_h08v04[!is.na(day_198_h08v04)]
day_199_h08v04<- day_199_h08v04[!is.na(day_199_h08v04)]
day_200_h08v04<- day_200_h08v04[!is.na(day_200_h08v04)]
day_201_h08v04<- day_201_h08v04[!is.na(day_201_h08v04)]
day_202_h08v04<- day_202_h08v04[!is.na(day_202_h08v04)]
day_203_h08v04<- day_203_h08v04[!is.na(day_203_h08v04)]
day_204_h08v04<- day_204_h08v04[!is.na(day_204_h08v04)]
day_205_h08v04<- day_205_h08v04[!is.na(day_205_h08v04)]
day_206_h08v04<- day_206_h08v04[!is.na(day_206_h08v04)]
day_207_h08v04<- day_207_h08v04[!is.na(day_207_h08v04)]
day_208_h08v04<- day_208_h08v04[!is.na(day_208_h08v04)]
day_209_h08v04<- day_209_h08v04[!is.na(day_209_h08v04)]
day_210_h08v04<- day_210_h08v04[!is.na(day_210_h08v04)]
day_211_h08v04<- day_211_h08v04[!is.na(day_211_h08v04)]
day_212_h08v04<- day_212_h08v04[!is.na(day_212_h08v04)]
day_213_h08v04<- day_213_h08v04[!is.na(day_213_h08v04)]
day_214_h08v04<- day_214_h08v04[!is.na(day_214_h08v04)]
day_215_h08v04<- day_215_h08v04[!is.na(day_215_h08v04)]
day_216_h08v04<- day_216_h08v04[!is.na(day_216_h08v04)]
day_217_h08v04<- day_217_h08v04[!is.na(day_217_h08v04)]
day_218_h08v04<- day_218_h08v04[!is.na(day_218_h08v04)]
day_219_h08v04<- day_219_h08v04[!is.na(day_219_h08v04)]
day_220_h08v04<- day_220_h08v04[!is.na(day_220_h08v04)]
day_221_h08v04<- day_221_h08v04[!is.na(day_221_h08v04)]
day_222_h08v04<- day_222_h08v04[!is.na(day_222_h08v04)]
day_223_h08v04<- day_223_h08v04[!is.na(day_223_h08v04)]
day_224_h08v04<- day_224_h08v04[!is.na(day_224_h08v04)]
day_225_h08v04<- day_225_h08v04[!is.na(day_225_h08v04)]
day_226_h08v04<- day_226_h08v04[!is.na(day_226_h08v04)]
day_227_h08v04<- day_227_h08v04[!is.na(day_227_h08v04)]
day_228_h08v04<- day_228_h08v04[!is.na(day_228_h08v04)]
day_229_h08v04<- day_229_h08v04[!is.na(day_229_h08v04)]
day_230_h08v04<- day_230_h08v04[!is.na(day_230_h08v04)]
day_231_h08v04<- day_231_h08v04[!is.na(day_231_h08v04)]
day_232_h08v04<- day_232_h08v04[!is.na(day_232_h08v04)]
day_233_h08v04<- day_233_h08v04[!is.na(day_233_h08v04)]
day_234_h08v04<- day_234_h08v04[!is.na(day_234_h08v04)]
day_235_h08v04<- day_235_h08v04[!is.na(day_235_h08v04)]
day_236_h08v04<- day_236_h08v04[!is.na(day_236_h08v04)]
day_237_h08v04<- day_237_h08v04[!is.na(day_237_h08v04)]
day_238_h08v04<- day_238_h08v04[!is.na(day_238_h08v04)]
day_239_h08v04<- day_239_h08v04[!is.na(day_239_h08v04)]
day_240_h08v04<- day_240_h08v04[!is.na(day_240_h08v04)]
day_241_h08v04<- day_241_h08v04[!is.na(day_241_h08v04)]
day_242_h08v04<- day_242_h08v04[!is.na(day_242_h08v04)]
day_243_h08v04<- day_243_h08v04[!is.na(day_243_h08v04)]
day_244_h08v04<- day_244_h08v04[!is.na(day_244_h08v04)]
day_245_h08v04<- day_245_h08v04[!is.na(day_245_h08v04)]
day_246_h08v04<- day_246_h08v04[!is.na(day_246_h08v04)]
day_247_h08v04<- day_247_h08v04[!is.na(day_247_h08v04)]
day_248_h08v04<- day_248_h08v04[!is.na(day_248_h08v04)]
day_249_h08v04<- day_249_h08v04[!is.na(day_249_h08v04)]
day_250_h08v04<- day_250_h08v04[!is.na(day_250_h08v04)]
day_251_h08v04<- day_251_h08v04[!is.na(day_251_h08v04)]
day_252_h08v04<- day_252_h08v04[!is.na(day_252_h08v04)]
day_253_h08v04<- day_253_h08v04[!is.na(day_253_h08v04)]
day_254_h08v04<- day_254_h08v04[!is.na(day_254_h08v04)]
day_255_h08v04<- day_255_h08v04[!is.na(day_255_h08v04)]
day_256_h08v04<- day_256_h08v04[!is.na(day_256_h08v04)]
day_257_h08v04<- day_257_h08v04[!is.na(day_257_h08v04)]
day_258_h08v04<- day_258_h08v04[!is.na(day_258_h08v04)]
day_259_h08v04<- day_259_h08v04[!is.na(day_259_h08v04)]
day_260_h08v04<- day_260_h08v04[!is.na(day_260_h08v04)]
day_261_h08v04<- day_261_h08v04[!is.na(day_261_h08v04)]
day_262_h08v04<- day_262_h08v04[!is.na(day_262_h08v04)]
day_263_h08v04<- day_263_h08v04[!is.na(day_263_h08v04)]
day_264_h08v04<- day_264_h08v04[!is.na(day_264_h08v04)]
day_265_h08v04<- day_265_h08v04[!is.na(day_265_h08v04)]
day_266_h08v04<- day_266_h08v04[!is.na(day_266_h08v04)]
day_267_h08v04<- day_267_h08v04[!is.na(day_267_h08v04)]
day_268_h08v04<- day_268_h08v04[!is.na(day_268_h08v04)]
day_269_h08v04<- day_269_h08v04[!is.na(day_269_h08v04)]
day_270_h08v04<- day_270_h08v04[!is.na(day_270_h08v04)]
day_271_h08v04<- day_271_h08v04[!is.na(day_271_h08v04)]
day_272_h08v04<- day_272_h08v04[!is.na(day_272_h08v04)]
day_273_h08v04<- day_273_h08v04[!is.na(day_273_h08v04)]
day_274_h08v04<- day_274_h08v04[!is.na(day_274_h08v04)]
day_275_h08v04<- day_275_h08v04[!is.na(day_275_h08v04)]
day_276_h08v04<- day_276_h08v04[!is.na(day_276_h08v04)]
day_277_h08v04<- day_277_h08v04[!is.na(day_277_h08v04)]
day_278_h08v04<- day_278_h08v04[!is.na(day_278_h08v04)]
day_279_h08v04<- day_279_h08v04[!is.na(day_279_h08v04)]
day_280_h08v04<- day_280_h08v04[!is.na(day_280_h08v04)]
day_281_h08v04<- day_281_h08v04[!is.na(day_281_h08v04)]
day_282_h08v04<- day_282_h08v04[!is.na(day_282_h08v04)]
day_283_h08v04<- day_283_h08v04[!is.na(day_283_h08v04)]
day_284_h08v04<- day_284_h08v04[!is.na(day_284_h08v04)]
day_285_h08v04<- day_285_h08v04[!is.na(day_285_h08v04)]
day_286_h08v04<- day_286_h08v04[!is.na(day_286_h08v04)]
day_287_h08v04<- day_287_h08v04[!is.na(day_287_h08v04)]
day_288_h08v04<- day_288_h08v04[!is.na(day_288_h08v04)]
day_289_h08v04<- day_289_h08v04[!is.na(day_289_h08v04)]
day_290_h08v04<- day_290_h08v04[!is.na(day_290_h08v04)]
day_291_h08v04<- day_291_h08v04[!is.na(day_291_h08v04)]
day_292_h08v04<- day_292_h08v04[!is.na(day_292_h08v04)]
day_293_h08v04<- day_293_h08v04[!is.na(day_293_h08v04)]
day_294_h08v04<- day_294_h08v04[!is.na(day_294_h08v04)]
day_295_h08v04<- day_295_h08v04[!is.na(day_295_h08v04)]
day_296_h08v04<- day_296_h08v04[!is.na(day_296_h08v04)]
day_297_h08v04<- day_297_h08v04[!is.na(day_297_h08v04)]
day_298_h08v04<- day_298_h08v04[!is.na(day_298_h08v04)]
day_299_h08v04<- day_299_h08v04[!is.na(day_299_h08v04)]
day_300_h08v04<- day_300_h08v04[!is.na(day_300_h08v04)]
day_301_h08v04<- day_301_h08v04[!is.na(day_301_h08v04)]
day_302_h08v04<- day_302_h08v04[!is.na(day_302_h08v04)]
day_303_h08v04<- day_303_h08v04[!is.na(day_303_h08v04)]
day_304_h08v04<- day_304_h08v04[!is.na(day_304_h08v04)]
day_305_h08v04<- day_305_h08v04[!is.na(day_305_h08v04)]
day_306_h08v04<- day_306_h08v04[!is.na(day_306_h08v04)]
day_307_h08v04<- day_307_h08v04[!is.na(day_307_h08v04)]
day_308_h08v04<- day_308_h08v04[!is.na(day_308_h08v04)]
day_309_h08v04<- day_309_h08v04[!is.na(day_309_h08v04)]
day_310_h08v04<- day_310_h08v04[!is.na(day_310_h08v04)]
day_311_h08v04<- day_311_h08v04[!is.na(day_311_h08v04)]
day_312_h08v04<- day_312_h08v04[!is.na(day_312_h08v04)]
day_313_h08v04<- day_313_h08v04[!is.na(day_313_h08v04)]
day_314_h08v04<- day_314_h08v04[!is.na(day_314_h08v04)]
day_315_h08v04<- day_315_h08v04[!is.na(day_315_h08v04)]
day_316_h08v04<- day_316_h08v04[!is.na(day_316_h08v04)]
day_317_h08v04<- day_317_h08v04[!is.na(day_317_h08v04)]
day_318_h08v04<- day_318_h08v04[!is.na(day_318_h08v04)]
day_319_h08v04<- day_319_h08v04[!is.na(day_319_h08v04)]
day_320_h08v04<- day_320_h08v04[!is.na(day_320_h08v04)]
day_321_h08v04<- day_321_h08v04[!is.na(day_321_h08v04)]
day_322_h08v04<- day_322_h08v04[!is.na(day_322_h08v04)]
day_323_h08v04<- day_323_h08v04[!is.na(day_323_h08v04)]
day_324_h08v04<- day_324_h08v04[!is.na(day_324_h08v04)]
day_325_h08v04<- day_325_h08v04[!is.na(day_325_h08v04)]
day_326_h08v04<- day_326_h08v04[!is.na(day_326_h08v04)]
day_327_h08v04<- day_327_h08v04[!is.na(day_327_h08v04)]
day_328_h08v04<- day_328_h08v04[!is.na(day_328_h08v04)]
day_329_h08v04<- day_329_h08v04[!is.na(day_329_h08v04)]
day_330_h08v04<- day_330_h08v04[!is.na(day_330_h08v04)]
day_331_h08v04<- day_331_h08v04[!is.na(day_331_h08v04)]
day_332_h08v04<- day_332_h08v04[!is.na(day_332_h08v04)]
day_333_h08v04<- day_333_h08v04[!is.na(day_333_h08v04)]
day_334_h08v04<- day_334_h08v04[!is.na(day_334_h08v04)]
day_335_h08v04<- day_335_h08v04[!is.na(day_335_h08v04)]
day_336_h08v04<- day_336_h08v04[!is.na(day_336_h08v04)]
day_337_h08v04<- day_337_h08v04[!is.na(day_337_h08v04)]
day_338_h08v04<- day_338_h08v04[!is.na(day_338_h08v04)]
day_339_h08v04<- day_339_h08v04[!is.na(day_339_h08v04)]
day_340_h08v04<- day_340_h08v04[!is.na(day_340_h08v04)]
day_341_h08v04<- day_341_h08v04[!is.na(day_341_h08v04)]
day_342_h08v04<- day_342_h08v04[!is.na(day_342_h08v04)]
day_343_h08v04<- day_343_h08v04[!is.na(day_343_h08v04)]
day_344_h08v04<- day_344_h08v04[!is.na(day_344_h08v04)]
day_345_h08v04<- day_345_h08v04[!is.na(day_345_h08v04)]
day_346_h08v04<- day_346_h08v04[!is.na(day_346_h08v04)]
day_347_h08v04<- day_347_h08v04[!is.na(day_347_h08v04)]
day_348_h08v04<- day_348_h08v04[!is.na(day_348_h08v04)]
day_349_h08v04<- day_349_h08v04[!is.na(day_349_h08v04)]
day_350_h08v04<- day_350_h08v04[!is.na(day_350_h08v04)]
day_351_h08v04<- day_351_h08v04[!is.na(day_351_h08v04)]
day_352_h08v04<- day_352_h08v04[!is.na(day_352_h08v04)]
day_353_h08v04<- day_353_h08v04[!is.na(day_353_h08v04)]
day_354_h08v04<- day_354_h08v04[!is.na(day_354_h08v04)]
day_355_h08v04<- day_355_h08v04[!is.na(day_355_h08v04)]
day_356_h08v04<- day_356_h08v04[!is.na(day_356_h08v04)]
day_357_h08v04<- day_357_h08v04[!is.na(day_357_h08v04)]
day_358_h08v04<- day_358_h08v04[!is.na(day_358_h08v04)]
day_359_h08v04<- day_359_h08v04[!is.na(day_359_h08v04)]
day_360_h08v04<- day_360_h08v04[!is.na(day_360_h08v04)]
day_361_h08v04<- day_361_h08v04[!is.na(day_361_h08v04)]
day_362_h08v04<- day_362_h08v04[!is.na(day_362_h08v04)]
day_363_h08v04<- day_363_h08v04[!is.na(day_363_h08v04)]
day_364_h08v04<- day_364_h08v04[!is.na(day_364_h08v04)]
day_365_h08v04<- day_365_h08v04[!is.na(day_365_h08v04)]
day_366_h08v04<- day_366_h08v04[!is.na(day_366_h08v04)]

day_60_h08v04_stack<- stack(day_60_h08v04)
nlayers(day_60_h08v04_stack) 
day_60_h08v04_mn<- mean(day_60_h08v04_stack, na.rm=TRUE)
day_60_h08v04_mnScaled<- day_60_h08v04_mn*0.0005
Out_day_60<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_60_h08v04_AVG_Scaled.img"
writeRaster (day_60_h08v04_mnScaled, filename=Out_day_60,format="HFA",overwrite=TRUE)
day_61_h08v04_stack<- stack(day_61_h08v04)
nlayers(day_61_h08v04_stack) 
day_61_h08v04_mn<- mean(day_61_h08v04_stack, na.rm=TRUE)
day_61_h08v04_mnScaled<- day_61_h08v04_mn*0.0005
Out_day_61<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_61_h08v04_AVG_Scaled.img"
writeRaster (day_61_h08v04_mnScaled, filename=Out_day_61,format="HFA",overwrite=TRUE)
day_62_h08v04_stack<- stack(day_62_h08v04)
nlayers(day_62_h08v04_stack) 
day_62_h08v04_mn<- mean(day_62_h08v04_stack, na.rm=TRUE)
day_62_h08v04_mnScaled<- day_62_h08v04_mn*0.0005
Out_day_62<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_62_h08v04_AVG_Scaled.img"
writeRaster (day_62_h08v04_mnScaled, filename=Out_day_62,format="HFA",overwrite=TRUE)
day_63_h08v04_stack<- stack(day_63_h08v04)
nlayers(day_63_h08v04_stack) 
day_63_h08v04_mn<- mean(day_63_h08v04_stack, na.rm=TRUE)
day_63_h08v04_mnScaled<- day_63_h08v04_mn*0.0005
Out_day_63<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_63_h08v04_AVG_Scaled.img"
writeRaster (day_63_h08v04_mnScaled, filename=Out_day_63,format="HFA",overwrite=TRUE)
day_64_h08v04_stack<- stack(day_64_h08v04)
nlayers(day_64_h08v04_stack) 
day_64_h08v04_mn<- mean(day_64_h08v04_stack, na.rm=TRUE)
day_64_h08v04_mnScaled<- day_64_h08v04_mn*0.0005
Out_day_64<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_64_h08v04_AVG_Scaled.img"
writeRaster (day_64_h08v04_mnScaled, filename=Out_day_64,format="HFA",overwrite=TRUE)
day_65_h08v04_stack<- stack(day_65_h08v04)
nlayers(day_65_h08v04_stack) 
day_65_h08v04_mn<- mean(day_65_h08v04_stack, na.rm=TRUE)
day_65_h08v04_mnScaled<- day_65_h08v04_mn*0.0005
Out_day_65<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_65_h08v04_AVG_Scaled.img"
writeRaster (day_65_h08v04_mnScaled, filename=Out_day_65,format="HFA",overwrite=TRUE)
day_66_h08v04_stack<- stack(day_66_h08v04)
nlayers(day_66_h08v04_stack) 
day_66_h08v04_mn<- mean(day_66_h08v04_stack, na.rm=TRUE)
day_66_h08v04_mnScaled<- day_66_h08v04_mn*0.0005
Out_day_66<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_66_h08v04_AVG_Scaled.img"
writeRaster (day_66_h08v04_mnScaled, filename=Out_day_66,format="HFA",overwrite=TRUE)
day_67_h08v04_stack<- stack(day_67_h08v04)
nlayers(day_67_h08v04_stack) 
day_67_h08v04_mn<- mean(day_67_h08v04_stack, na.rm=TRUE)
day_67_h08v04_mnScaled<- day_67_h08v04_mn*0.0005
Out_day_67<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_67_h08v04_AVG_Scaled.img"
writeRaster (day_67_h08v04_mnScaled, filename=Out_day_67,format="HFA",overwrite=TRUE)
day_68_h08v04_stack<- stack(day_68_h08v04)
nlayers(day_68_h08v04_stack) 
day_68_h08v04_mn<- mean(day_68_h08v04_stack, na.rm=TRUE)
day_68_h08v04_mnScaled<- day_68_h08v04_mn*0.0005
Out_day_68<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_68_h08v04_AVG_Scaled.img"
writeRaster (day_68_h08v04_mnScaled, filename=Out_day_68,format="HFA",overwrite=TRUE)
day_69_h08v04_stack<- stack(day_69_h08v04)
nlayers(day_69_h08v04_stack) 
day_69_h08v04_mn<- mean(day_69_h08v04_stack, na.rm=TRUE)
day_69_h08v04_mnScaled<- day_69_h08v04_mn*0.0005
Out_day_69<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_69_h08v04_AVG_Scaled.img"
writeRaster (day_69_h08v04_mnScaled, filename=Out_day_69,format="HFA",overwrite=TRUE)
day_70_h08v04_stack<- stack(day_70_h08v04)
nlayers(day_70_h08v04_stack) 
day_70_h08v04_mn<- mean(day_70_h08v04_stack, na.rm=TRUE)
day_70_h08v04_mnScaled<- day_70_h08v04_mn*0.0005
Out_day_70<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_70_h08v04_AVG_Scaled.img"
writeRaster (day_70_h08v04_mnScaled, filename=Out_day_70,format="HFA",overwrite=TRUE)
day_71_h08v04_stack<- stack(day_71_h08v04)
nlayers(day_71_h08v04_stack) 
day_71_h08v04_mn<- mean(day_71_h08v04_stack, na.rm=TRUE)
day_71_h08v04_mnScaled<- day_71_h08v04_mn*0.0005
Out_day_71<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_71_h08v04_AVG_Scaled.img"
writeRaster (day_71_h08v04_mnScaled, filename=Out_day_71,format="HFA",overwrite=TRUE)
day_72_h08v04_stack<- stack(day_72_h08v04)
nlayers(day_72_h08v04_stack) 
day_72_h08v04_mn<- mean(day_72_h08v04_stack, na.rm=TRUE)
day_72_h08v04_mnScaled<- day_72_h08v04_mn*0.0005
Out_day_72<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_72_h08v04_AVG_Scaled.img"
writeRaster (day_72_h08v04_mnScaled, filename=Out_day_72,format="HFA",overwrite=TRUE)
day_73_h08v04_stack<- stack(day_73_h08v04)
nlayers(day_73_h08v04_stack) 
day_73_h08v04_mn<- mean(day_73_h08v04_stack, na.rm=TRUE)
day_73_h08v04_mnScaled<- day_73_h08v04_mn*0.0005
Out_day_73<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_73_h08v04_AVG_Scaled.img"
writeRaster (day_73_h08v04_mnScaled, filename=Out_day_73,format="HFA",overwrite=TRUE)
day_74_h08v04_stack<- stack(day_74_h08v04)
nlayers(day_74_h08v04_stack) 
day_74_h08v04_mn<- mean(day_74_h08v04_stack, na.rm=TRUE)
day_74_h08v04_mnScaled<- day_74_h08v04_mn*0.0005
Out_day_74<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_74_h08v04_AVG_Scaled.img"
writeRaster (day_74_h08v04_mnScaled, filename=Out_day_74,format="HFA",overwrite=TRUE)
day_75_h08v04_stack<- stack(day_75_h08v04)
nlayers(day_75_h08v04_stack) 
day_75_h08v04_mn<- mean(day_75_h08v04_stack, na.rm=TRUE)
day_75_h08v04_mnScaled<- day_75_h08v04_mn*0.0005
Out_day_75<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_75_h08v04_AVG_Scaled.img"
writeRaster (day_75_h08v04_mnScaled, filename=Out_day_75,format="HFA",overwrite=TRUE)
day_76_h08v04_stack<- stack(day_76_h08v04)
nlayers(day_76_h08v04_stack) 
day_76_h08v04_mn<- mean(day_76_h08v04_stack, na.rm=TRUE)
day_76_h08v04_mnScaled<- day_76_h08v04_mn*0.0005
Out_day_76<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_76_h08v04_AVG_Scaled.img"
writeRaster (day_76_h08v04_mnScaled, filename=Out_day_76,format="HFA",overwrite=TRUE)
day_77_h08v04_stack<- stack(day_77_h08v04)
nlayers(day_77_h08v04_stack) 
day_77_h08v04_mn<- mean(day_77_h08v04_stack, na.rm=TRUE)
day_77_h08v04_mnScaled<- day_77_h08v04_mn*0.0005
Out_day_77<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_77_h08v04_AVG_Scaled.img"
writeRaster (day_77_h08v04_mnScaled, filename=Out_day_77,format="HFA",overwrite=TRUE)
day_78_h08v04_stack<- stack(day_78_h08v04)
nlayers(day_78_h08v04_stack) 
day_78_h08v04_mn<- mean(day_78_h08v04_stack, na.rm=TRUE)
day_78_h08v04_mnScaled<- day_78_h08v04_mn*0.0005
Out_day_78<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_78_h08v04_AVG_Scaled.img"
writeRaster (day_78_h08v04_mnScaled, filename=Out_day_78,format="HFA",overwrite=TRUE)
day_79_h08v04_stack<- stack(day_79_h08v04)
nlayers(day_79_h08v04_stack) 
day_79_h08v04_mn<- mean(day_79_h08v04_stack, na.rm=TRUE)
day_79_h08v04_mnScaled<- day_79_h08v04_mn*0.0005
Out_day_79<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_79_h08v04_AVG_Scaled.img"
writeRaster (day_79_h08v04_mnScaled, filename=Out_day_79,format="HFA",overwrite=TRUE)
day_80_h08v04_stack<- stack(day_80_h08v04)
nlayers(day_80_h08v04_stack) 
day_80_h08v04_mn<- mean(day_80_h08v04_stack, na.rm=TRUE)
day_80_h08v04_mnScaled<- day_80_h08v04_mn*0.0005
Out_day_80<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_80_h08v04_AVG_Scaled.img"
writeRaster (day_80_h08v04_mnScaled, filename=Out_day_80,format="HFA",overwrite=TRUE)
day_81_h08v04_stack<- stack(day_81_h08v04)
nlayers(day_81_h08v04_stack) 
day_81_h08v04_mn<- mean(day_81_h08v04_stack, na.rm=TRUE)
day_81_h08v04_mnScaled<- day_81_h08v04_mn*0.0005
Out_day_81<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_81_h08v04_AVG_Scaled.img"
writeRaster (day_81_h08v04_mnScaled, filename=Out_day_81,format="HFA",overwrite=TRUE)
day_82_h08v04_stack<- stack(day_82_h08v04)
nlayers(day_82_h08v04_stack) 
day_82_h08v04_mn<- mean(day_82_h08v04_stack, na.rm=TRUE)
day_82_h08v04_mnScaled<- day_82_h08v04_mn*0.0005
Out_day_82<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_82_h08v04_AVG_Scaled.img"
writeRaster (day_82_h08v04_mnScaled, filename=Out_day_82,format="HFA",overwrite=TRUE)
day_83_h08v04_stack<- stack(day_83_h08v04)
nlayers(day_83_h08v04_stack) 
day_83_h08v04_mn<- mean(day_83_h08v04_stack, na.rm=TRUE)
day_83_h08v04_mnScaled<- day_83_h08v04_mn*0.0005
Out_day_83<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_83_h08v04_AVG_Scaled.img"
writeRaster (day_83_h08v04_mnScaled, filename=Out_day_83,format="HFA",overwrite=TRUE)
day_84_h08v04_stack<- stack(day_84_h08v04)
nlayers(day_84_h08v04_stack) 
day_84_h08v04_mn<- mean(day_84_h08v04_stack, na.rm=TRUE)
day_84_h08v04_mnScaled<- day_84_h08v04_mn*0.0005
Out_day_84<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_84_h08v04_AVG_Scaled.img"
writeRaster (day_84_h08v04_mnScaled, filename=Out_day_84,format="HFA",overwrite=TRUE)
day_85_h08v04_stack<- stack(day_85_h08v04)
nlayers(day_85_h08v04_stack) 
day_85_h08v04_mn<- mean(day_85_h08v04_stack, na.rm=TRUE)
day_85_h08v04_mnScaled<- day_85_h08v04_mn*0.0005
Out_day_85<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_85_h08v04_AVG_Scaled.img"
writeRaster (day_85_h08v04_mnScaled, filename=Out_day_85,format="HFA",overwrite=TRUE)
day_86_h08v04_stack<- stack(day_86_h08v04)
nlayers(day_86_h08v04_stack) 
day_86_h08v04_mn<- mean(day_86_h08v04_stack, na.rm=TRUE)
day_86_h08v04_mnScaled<- day_86_h08v04_mn*0.0005
Out_day_86<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_86_h08v04_AVG_Scaled.img"
writeRaster (day_86_h08v04_mnScaled, filename=Out_day_86,format="HFA",overwrite=TRUE)
day_87_h08v04_stack<- stack(day_87_h08v04)
nlayers(day_87_h08v04_stack) 
day_87_h08v04_mn<- mean(day_87_h08v04_stack, na.rm=TRUE)
day_87_h08v04_mnScaled<- day_87_h08v04_mn*0.0005
Out_day_87<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_87_h08v04_AVG_Scaled.img"
writeRaster (day_87_h08v04_mnScaled, filename=Out_day_87,format="HFA",overwrite=TRUE)
day_88_h08v04_stack<- stack(day_88_h08v04)
nlayers(day_88_h08v04_stack) 
day_88_h08v04_mn<- mean(day_88_h08v04_stack, na.rm=TRUE)
day_88_h08v04_mnScaled<- day_88_h08v04_mn*0.0005
Out_day_88<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_88_h08v04_AVG_Scaled.img"
writeRaster (day_88_h08v04_mnScaled, filename=Out_day_88,format="HFA",overwrite=TRUE)
day_89_h08v04_stack<- stack(day_89_h08v04)
nlayers(day_89_h08v04_stack) 
day_89_h08v04_mn<- mean(day_89_h08v04_stack, na.rm=TRUE)
day_89_h08v04_mnScaled<- day_89_h08v04_mn*0.0005
Out_day_89<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_89_h08v04_AVG_Scaled.img"
writeRaster (day_89_h08v04_mnScaled, filename=Out_day_89,format="HFA",overwrite=TRUE)
day_90_h08v04_stack<- stack(day_90_h08v04)
nlayers(day_90_h08v04_stack) 
day_90_h08v04_mn<- mean(day_90_h08v04_stack, na.rm=TRUE)
day_90_h08v04_mnScaled<- day_90_h08v04_mn*0.0005
Out_day_90<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_90_h08v04_AVG_Scaled.img"
writeRaster (day_90_h08v04_mnScaled, filename=Out_day_90,format="HFA",overwrite=TRUE)
day_91_h08v04_stack<- stack(day_91_h08v04)
nlayers(day_91_h08v04_stack) 
day_91_h08v04_mn<- mean(day_91_h08v04_stack, na.rm=TRUE)
day_91_h08v04_mnScaled<- day_91_h08v04_mn*0.0005
Out_day_91<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_91_h08v04_AVG_Scaled.img"
writeRaster (day_91_h08v04_mnScaled, filename=Out_day_91,format="HFA",overwrite=TRUE)
day_92_h08v04_stack<- stack(day_92_h08v04)
nlayers(day_92_h08v04_stack) 
day_92_h08v04_mn<- mean(day_92_h08v04_stack, na.rm=TRUE)
day_92_h08v04_mnScaled<- day_92_h08v04_mn*0.0005
Out_day_92<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_92_h08v04_AVG_Scaled.img"
writeRaster (day_92_h08v04_mnScaled, filename=Out_day_92,format="HFA",overwrite=TRUE)
day_93_h08v04_stack<- stack(day_93_h08v04)
nlayers(day_93_h08v04_stack) 
day_93_h08v04_mn<- mean(day_93_h08v04_stack, na.rm=TRUE)
day_93_h08v04_mnScaled<- day_93_h08v04_mn*0.0005
Out_day_93<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_93_h08v04_AVG_Scaled.img"
writeRaster (day_93_h08v04_mnScaled, filename=Out_day_93,format="HFA",overwrite=TRUE)
day_94_h08v04_stack<- stack(day_94_h08v04)
nlayers(day_94_h08v04_stack) 
day_94_h08v04_mn<- mean(day_94_h08v04_stack, na.rm=TRUE)
day_94_h08v04_mnScaled<- day_94_h08v04_mn*0.0005
Out_day_94<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_94_h08v04_AVG_Scaled.img"
writeRaster (day_94_h08v04_mnScaled, filename=Out_day_94,format="HFA",overwrite=TRUE)
day_95_h08v04_stack<- stack(day_95_h08v04)
nlayers(day_95_h08v04_stack) 
day_95_h08v04_mn<- mean(day_95_h08v04_stack, na.rm=TRUE)
day_95_h08v04_mnScaled<- day_95_h08v04_mn*0.0005
Out_day_95<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_95_h08v04_AVG_Scaled.img"
writeRaster (day_95_h08v04_mnScaled, filename=Out_day_95,format="HFA",overwrite=TRUE)
day_96_h08v04_stack<- stack(day_96_h08v04)
nlayers(day_96_h08v04_stack) 
day_96_h08v04_mn<- mean(day_96_h08v04_stack, na.rm=TRUE)
day_96_h08v04_mnScaled<- day_96_h08v04_mn*0.0005
Out_day_96<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_96_h08v04_AVG_Scaled.img"
writeRaster (day_96_h08v04_mnScaled, filename=Out_day_96,format="HFA",overwrite=TRUE)
day_97_h08v04_stack<- stack(day_97_h08v04)
nlayers(day_97_h08v04_stack) 
day_97_h08v04_mn<- mean(day_97_h08v04_stack, na.rm=TRUE)
day_97_h08v04_mnScaled<- day_97_h08v04_mn*0.0005
Out_day_97<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_97_h08v04_AVG_Scaled.img"
writeRaster (day_97_h08v04_mnScaled, filename=Out_day_97,format="HFA",overwrite=TRUE)
day_98_h08v04_stack<- stack(day_98_h08v04)
nlayers(day_98_h08v04_stack) 
day_98_h08v04_mn<- mean(day_98_h08v04_stack, na.rm=TRUE)
day_98_h08v04_mnScaled<- day_98_h08v04_mn*0.0005
Out_day_98<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_98_h08v04_AVG_Scaled.img"
writeRaster (day_98_h08v04_mnScaled, filename=Out_day_98,format="HFA",overwrite=TRUE)
day_99_h08v04_stack<- stack(day_99_h08v04)
nlayers(day_99_h08v04_stack) 
day_99_h08v04_mn<- mean(day_99_h08v04_stack, na.rm=TRUE)
day_99_h08v04_mnScaled<- day_99_h08v04_mn*0.0005
Out_day_99<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_99_h08v04_AVG_Scaled.img"
writeRaster (day_99_h08v04_mnScaled, filename=Out_day_99,format="HFA",overwrite=TRUE)
day_100_h08v04_stack<- stack(day_100_h08v04)
nlayers(day_100_h08v04_stack) 
day_100_h08v04_mn<- mean(day_100_h08v04_stack, na.rm=TRUE)
day_100_h08v04_mnScaled<- day_100_h08v04_mn*0.0005
Out_day_100<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_100_h08v04_AVG_Scaled.img"
writeRaster (day_100_h08v04_mnScaled, filename=Out_day_100,format="HFA",overwrite=TRUE)
day_101_h08v04_stack<- stack(day_101_h08v04)
nlayers(day_101_h08v04_stack) 
day_101_h08v04_mn<- mean(day_101_h08v04_stack, na.rm=TRUE)
day_101_h08v04_mnScaled<- day_101_h08v04_mn*0.0005
Out_day_101<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_101_h08v04_AVG_Scaled.img"
writeRaster (day_101_h08v04_mnScaled, filename=Out_day_101,format="HFA",overwrite=TRUE)
day_102_h08v04_stack<- stack(day_102_h08v04)
nlayers(day_102_h08v04_stack) 
day_102_h08v04_mn<- mean(day_102_h08v04_stack, na.rm=TRUE)
day_102_h08v04_mnScaled<- day_102_h08v04_mn*0.0005
Out_day_102<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_102_h08v04_AVG_Scaled.img"
writeRaster (day_102_h08v04_mnScaled, filename=Out_day_102,format="HFA",overwrite=TRUE)
day_103_h08v04_stack<- stack(day_103_h08v04)
nlayers(day_103_h08v04_stack) 
day_103_h08v04_mn<- mean(day_103_h08v04_stack, na.rm=TRUE)
day_103_h08v04_mnScaled<- day_103_h08v04_mn*0.0005
Out_day_103<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_103_h08v04_AVG_Scaled.img"
writeRaster (day_103_h08v04_mnScaled, filename=Out_day_103,format="HFA",overwrite=TRUE)
day_104_h08v04_stack<- stack(day_104_h08v04)
nlayers(day_104_h08v04_stack) 
day_104_h08v04_mn<- mean(day_104_h08v04_stack, na.rm=TRUE)
day_104_h08v04_mnScaled<- day_104_h08v04_mn*0.0005
Out_day_104<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_104_h08v04_AVG_Scaled.img"
writeRaster (day_104_h08v04_mnScaled, filename=Out_day_104,format="HFA",overwrite=TRUE)
day_105_h08v04_stack<- stack(day_105_h08v04)
nlayers(day_105_h08v04_stack) 
day_105_h08v04_mn<- mean(day_105_h08v04_stack, na.rm=TRUE)
day_105_h08v04_mnScaled<- day_105_h08v04_mn*0.0005
Out_day_105<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_105_h08v04_AVG_Scaled.img"
writeRaster (day_105_h08v04_mnScaled, filename=Out_day_105,format="HFA",overwrite=TRUE)
day_106_h08v04_stack<- stack(day_106_h08v04)
nlayers(day_106_h08v04_stack) 
day_106_h08v04_mn<- mean(day_106_h08v04_stack, na.rm=TRUE)
day_106_h08v04_mnScaled<- day_106_h08v04_mn*0.0005
Out_day_106<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_106_h08v04_AVG_Scaled.img"
writeRaster (day_106_h08v04_mnScaled, filename=Out_day_106,format="HFA",overwrite=TRUE)
day_107_h08v04_stack<- stack(day_107_h08v04)
nlayers(day_107_h08v04_stack) 
day_107_h08v04_mn<- mean(day_107_h08v04_stack, na.rm=TRUE)
day_107_h08v04_mnScaled<- day_107_h08v04_mn*0.0005
Out_day_107<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_107_h08v04_AVG_Scaled.img"
writeRaster (day_107_h08v04_mnScaled, filename=Out_day_107,format="HFA",overwrite=TRUE)
day_108_h08v04_stack<- stack(day_108_h08v04)
nlayers(day_108_h08v04_stack) 
day_108_h08v04_mn<- mean(day_108_h08v04_stack, na.rm=TRUE)
day_108_h08v04_mnScaled<- day_108_h08v04_mn*0.0005
Out_day_108<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_108_h08v04_AVG_Scaled.img"
writeRaster (day_108_h08v04_mnScaled, filename=Out_day_108,format="HFA",overwrite=TRUE)
day_109_h08v04_stack<- stack(day_109_h08v04)
nlayers(day_109_h08v04_stack) 
day_109_h08v04_mn<- mean(day_109_h08v04_stack, na.rm=TRUE)
day_109_h08v04_mnScaled<- day_109_h08v04_mn*0.0005
Out_day_109<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_109_h08v04_AVG_Scaled.img"
writeRaster (day_109_h08v04_mnScaled, filename=Out_day_109,format="HFA",overwrite=TRUE)
day_110_h08v04_stack<- stack(day_110_h08v04)
nlayers(day_110_h08v04_stack) 
day_110_h08v04_mn<- mean(day_110_h08v04_stack, na.rm=TRUE)
day_110_h08v04_mnScaled<- day_110_h08v04_mn*0.0005
Out_day_110<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_110_h08v04_AVG_Scaled.img"
writeRaster (day_110_h08v04_mnScaled, filename=Out_day_110,format="HFA",overwrite=TRUE)
day_111_h08v04_stack<- stack(day_111_h08v04)
nlayers(day_111_h08v04_stack) 
day_111_h08v04_mn<- mean(day_111_h08v04_stack, na.rm=TRUE)
day_111_h08v04_mnScaled<- day_111_h08v04_mn*0.0005
Out_day_111<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_111_h08v04_AVG_Scaled.img"
writeRaster (day_111_h08v04_mnScaled, filename=Out_day_111,format="HFA",overwrite=TRUE)
day_112_h08v04_stack<- stack(day_112_h08v04)
nlayers(day_112_h08v04_stack) 
day_112_h08v04_mn<- mean(day_112_h08v04_stack, na.rm=TRUE)
day_112_h08v04_mnScaled<- day_112_h08v04_mn*0.0005
Out_day_112<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_112_h08v04_AVG_Scaled.img"
writeRaster (day_112_h08v04_mnScaled, filename=Out_day_112,format="HFA",overwrite=TRUE)
day_113_h08v04_stack<- stack(day_113_h08v04)
nlayers(day_113_h08v04_stack) 
day_113_h08v04_mn<- mean(day_113_h08v04_stack, na.rm=TRUE)
day_113_h08v04_mnScaled<- day_113_h08v04_mn*0.0005
Out_day_113<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_113_h08v04_AVG_Scaled.img"
writeRaster (day_113_h08v04_mnScaled, filename=Out_day_113,format="HFA",overwrite=TRUE)
day_114_h08v04_stack<- stack(day_114_h08v04)
nlayers(day_114_h08v04_stack) 
day_114_h08v04_mn<- mean(day_114_h08v04_stack, na.rm=TRUE)
day_114_h08v04_mnScaled<- day_114_h08v04_mn*0.0005
Out_day_114<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_114_h08v04_AVG_Scaled.img"
writeRaster (day_114_h08v04_mnScaled, filename=Out_day_114,format="HFA",overwrite=TRUE)
day_115_h08v04_stack<- stack(day_115_h08v04)
nlayers(day_115_h08v04_stack) 
day_115_h08v04_mn<- mean(day_115_h08v04_stack, na.rm=TRUE)
day_115_h08v04_mnScaled<- day_115_h08v04_mn*0.0005
Out_day_115<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_115_h08v04_AVG_Scaled.img"
writeRaster (day_115_h08v04_mnScaled, filename=Out_day_115,format="HFA",overwrite=TRUE)
day_116_h08v04_stack<- stack(day_116_h08v04)
nlayers(day_116_h08v04_stack) 
day_116_h08v04_mn<- mean(day_116_h08v04_stack, na.rm=TRUE)
day_116_h08v04_mnScaled<- day_116_h08v04_mn*0.0005
Out_day_116<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_116_h08v04_AVG_Scaled.img"
writeRaster (day_116_h08v04_mnScaled, filename=Out_day_116,format="HFA",overwrite=TRUE)
day_117_h08v04_stack<- stack(day_117_h08v04)
nlayers(day_117_h08v04_stack) 
day_117_h08v04_mn<- mean(day_117_h08v04_stack, na.rm=TRUE)
day_117_h08v04_mnScaled<- day_117_h08v04_mn*0.0005
Out_day_117<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_117_h08v04_AVG_Scaled.img"
writeRaster (day_117_h08v04_mnScaled, filename=Out_day_117,format="HFA",overwrite=TRUE)
day_118_h08v04_stack<- stack(day_118_h08v04)
nlayers(day_118_h08v04_stack) 
day_118_h08v04_mn<- mean(day_118_h08v04_stack, na.rm=TRUE)
day_118_h08v04_mnScaled<- day_118_h08v04_mn*0.0005
Out_day_118<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_118_h08v04_AVG_Scaled.img"
writeRaster (day_118_h08v04_mnScaled, filename=Out_day_118,format="HFA",overwrite=TRUE)
day_119_h08v04_stack<- stack(day_119_h08v04)
nlayers(day_119_h08v04_stack) 
day_119_h08v04_mn<- mean(day_119_h08v04_stack, na.rm=TRUE)
day_119_h08v04_mnScaled<- day_119_h08v04_mn*0.0005
Out_day_119<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_119_h08v04_AVG_Scaled.img"
writeRaster (day_119_h08v04_mnScaled, filename=Out_day_119,format="HFA",overwrite=TRUE)
day_120_h08v04_stack<- stack(day_120_h08v04)
nlayers(day_120_h08v04_stack) 
day_120_h08v04_mn<- mean(day_120_h08v04_stack, na.rm=TRUE)
day_120_h08v04_mnScaled<- day_120_h08v04_mn*0.0005
Out_day_120<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_120_h08v04_AVG_Scaled.img"
writeRaster (day_120_h08v04_mnScaled, filename=Out_day_120,format="HFA",overwrite=TRUE)
day_121_h08v04_stack<- stack(day_121_h08v04)
nlayers(day_121_h08v04_stack) 
day_121_h08v04_mn<- mean(day_121_h08v04_stack, na.rm=TRUE)
day_121_h08v04_mnScaled<- day_121_h08v04_mn*0.0005
Out_day_121<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_121_h08v04_AVG_Scaled.img"
writeRaster (day_121_h08v04_mnScaled, filename=Out_day_121,format="HFA",overwrite=TRUE)
day_122_h08v04_stack<- stack(day_122_h08v04)
nlayers(day_122_h08v04_stack) 
day_122_h08v04_mn<- mean(day_122_h08v04_stack, na.rm=TRUE)
day_122_h08v04_mnScaled<- day_122_h08v04_mn*0.0005
Out_day_122<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_122_h08v04_AVG_Scaled.img"
writeRaster (day_122_h08v04_mnScaled, filename=Out_day_122,format="HFA",overwrite=TRUE)
day_123_h08v04_stack<- stack(day_123_h08v04)
nlayers(day_123_h08v04_stack) 
day_123_h08v04_mn<- mean(day_123_h08v04_stack, na.rm=TRUE)
day_123_h08v04_mnScaled<- day_123_h08v04_mn*0.0005
Out_day_123<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_123_h08v04_AVG_Scaled.img"
writeRaster (day_123_h08v04_mnScaled, filename=Out_day_123,format="HFA",overwrite=TRUE)
day_124_h08v04_stack<- stack(day_124_h08v04)
nlayers(day_124_h08v04_stack) 
day_124_h08v04_mn<- mean(day_124_h08v04_stack, na.rm=TRUE)
day_124_h08v04_mnScaled<- day_124_h08v04_mn*0.0005
Out_day_124<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_124_h08v04_AVG_Scaled.img"
writeRaster (day_124_h08v04_mnScaled, filename=Out_day_124,format="HFA",overwrite=TRUE)
day_125_h08v04_stack<- stack(day_125_h08v04)
nlayers(day_125_h08v04_stack) 
day_125_h08v04_mn<- mean(day_125_h08v04_stack, na.rm=TRUE)
day_125_h08v04_mnScaled<- day_125_h08v04_mn*0.0005
Out_day_125<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_125_h08v04_AVG_Scaled.img"
writeRaster (day_125_h08v04_mnScaled, filename=Out_day_125,format="HFA",overwrite=TRUE)
day_126_h08v04_stack<- stack(day_126_h08v04)
nlayers(day_126_h08v04_stack) 
day_126_h08v04_mn<- mean(day_126_h08v04_stack, na.rm=TRUE)
day_126_h08v04_mnScaled<- day_126_h08v04_mn*0.0005
Out_day_126<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_126_h08v04_AVG_Scaled.img"
writeRaster (day_126_h08v04_mnScaled, filename=Out_day_126,format="HFA",overwrite=TRUE)
day_127_h08v04_stack<- stack(day_127_h08v04)
nlayers(day_127_h08v04_stack) 
day_127_h08v04_mn<- mean(day_127_h08v04_stack, na.rm=TRUE)
day_127_h08v04_mnScaled<- day_127_h08v04_mn*0.0005
Out_day_127<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_127_h08v04_AVG_Scaled.img"
writeRaster (day_127_h08v04_mnScaled, filename=Out_day_127,format="HFA",overwrite=TRUE)
day_128_h08v04_stack<- stack(day_128_h08v04)
nlayers(day_128_h08v04_stack) 
day_128_h08v04_mn<- mean(day_128_h08v04_stack, na.rm=TRUE)
day_128_h08v04_mnScaled<- day_128_h08v04_mn*0.0005
Out_day_128<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_128_h08v04_AVG_Scaled.img"
writeRaster (day_128_h08v04_mnScaled, filename=Out_day_128,format="HFA",overwrite=TRUE)
day_129_h08v04_stack<- stack(day_129_h08v04)
nlayers(day_129_h08v04_stack) 
day_129_h08v04_mn<- mean(day_129_h08v04_stack, na.rm=TRUE)
day_129_h08v04_mnScaled<- day_129_h08v04_mn*0.0005
Out_day_129<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_129_h08v04_AVG_Scaled.img"
writeRaster (day_129_h08v04_mnScaled, filename=Out_day_129,format="HFA",overwrite=TRUE)
day_130_h08v04_stack<- stack(day_130_h08v04)
nlayers(day_130_h08v04_stack) 
day_130_h08v04_mn<- mean(day_130_h08v04_stack, na.rm=TRUE)
day_130_h08v04_mnScaled<- day_130_h08v04_mn*0.0005
Out_day_130<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_130_h08v04_AVG_Scaled.img"
writeRaster (day_130_h08v04_mnScaled, filename=Out_day_130,format="HFA",overwrite=TRUE)
day_131_h08v04_stack<- stack(day_131_h08v04)
nlayers(day_131_h08v04_stack) 
day_131_h08v04_mn<- mean(day_131_h08v04_stack, na.rm=TRUE)
day_131_h08v04_mnScaled<- day_131_h08v04_mn*0.0005
Out_day_131<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_131_h08v04_AVG_Scaled.img"
writeRaster (day_131_h08v04_mnScaled, filename=Out_day_131,format="HFA",overwrite=TRUE)
day_132_h08v04_stack<- stack(day_132_h08v04)
nlayers(day_132_h08v04_stack) 
day_132_h08v04_mn<- mean(day_132_h08v04_stack, na.rm=TRUE)
day_132_h08v04_mnScaled<- day_132_h08v04_mn*0.0005
Out_day_132<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_132_h08v04_AVG_Scaled.img"
writeRaster (day_132_h08v04_mnScaled, filename=Out_day_132,format="HFA",overwrite=TRUE)
day_133_h08v04_stack<- stack(day_133_h08v04)
nlayers(day_133_h08v04_stack) 
day_133_h08v04_mn<- mean(day_133_h08v04_stack, na.rm=TRUE)
day_133_h08v04_mnScaled<- day_133_h08v04_mn*0.0005
Out_day_133<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_133_h08v04_AVG_Scaled.img"
writeRaster (day_133_h08v04_mnScaled, filename=Out_day_133,format="HFA",overwrite=TRUE)
day_134_h08v04_stack<- stack(day_134_h08v04)
nlayers(day_134_h08v04_stack) 
day_134_h08v04_mn<- mean(day_134_h08v04_stack, na.rm=TRUE)
day_134_h08v04_mnScaled<- day_134_h08v04_mn*0.0005
Out_day_134<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_134_h08v04_AVG_Scaled.img"
writeRaster (day_134_h08v04_mnScaled, filename=Out_day_134,format="HFA",overwrite=TRUE)
day_135_h08v04_stack<- stack(day_135_h08v04)
nlayers(day_135_h08v04_stack) 
day_135_h08v04_mn<- mean(day_135_h08v04_stack, na.rm=TRUE)
day_135_h08v04_mnScaled<- day_135_h08v04_mn*0.0005
Out_day_135<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_135_h08v04_AVG_Scaled.img"
writeRaster (day_135_h08v04_mnScaled, filename=Out_day_135,format="HFA",overwrite=TRUE)
day_136_h08v04_stack<- stack(day_136_h08v04)
nlayers(day_136_h08v04_stack) 
day_136_h08v04_mn<- mean(day_136_h08v04_stack, na.rm=TRUE)
day_136_h08v04_mnScaled<- day_136_h08v04_mn*0.0005
Out_day_136<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_136_h08v04_AVG_Scaled.img"
writeRaster (day_136_h08v04_mnScaled, filename=Out_day_136,format="HFA",overwrite=TRUE)
day_137_h08v04_stack<- stack(day_137_h08v04)
nlayers(day_137_h08v04_stack) 
day_137_h08v04_mn<- mean(day_137_h08v04_stack, na.rm=TRUE)
day_137_h08v04_mnScaled<- day_137_h08v04_mn*0.0005
Out_day_137<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_137_h08v04_AVG_Scaled.img"
writeRaster (day_137_h08v04_mnScaled, filename=Out_day_137,format="HFA",overwrite=TRUE)
day_138_h08v04_stack<- stack(day_138_h08v04)
nlayers(day_138_h08v04_stack) 
day_138_h08v04_mn<- mean(day_138_h08v04_stack, na.rm=TRUE)
day_138_h08v04_mnScaled<- day_138_h08v04_mn*0.0005
Out_day_138<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_138_h08v04_AVG_Scaled.img"
writeRaster (day_138_h08v04_mnScaled, filename=Out_day_138,format="HFA",overwrite=TRUE)
day_139_h08v04_stack<- stack(day_139_h08v04)
nlayers(day_139_h08v04_stack) 
day_139_h08v04_mn<- mean(day_139_h08v04_stack, na.rm=TRUE)
day_139_h08v04_mnScaled<- day_139_h08v04_mn*0.0005
Out_day_139<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_139_h08v04_AVG_Scaled.img"
writeRaster (day_139_h08v04_mnScaled, filename=Out_day_139,format="HFA",overwrite=TRUE)
day_140_h08v04_stack<- stack(day_140_h08v04)
nlayers(day_140_h08v04_stack) 
day_140_h08v04_mn<- mean(day_140_h08v04_stack, na.rm=TRUE)
day_140_h08v04_mnScaled<- day_140_h08v04_mn*0.0005
Out_day_140<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_140_h08v04_AVG_Scaled.img"
writeRaster (day_140_h08v04_mnScaled, filename=Out_day_140,format="HFA",overwrite=TRUE)
day_141_h08v04_stack<- stack(day_141_h08v04)
nlayers(day_141_h08v04_stack) 
day_141_h08v04_mn<- mean(day_141_h08v04_stack, na.rm=TRUE)
day_141_h08v04_mnScaled<- day_141_h08v04_mn*0.0005
Out_day_141<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_141_h08v04_AVG_Scaled.img"
writeRaster (day_141_h08v04_mnScaled, filename=Out_day_141,format="HFA",overwrite=TRUE)
day_142_h08v04_stack<- stack(day_142_h08v04)
nlayers(day_142_h08v04_stack) 
day_142_h08v04_mn<- mean(day_142_h08v04_stack, na.rm=TRUE)
day_142_h08v04_mnScaled<- day_142_h08v04_mn*0.0005
Out_day_142<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_142_h08v04_AVG_Scaled.img"
writeRaster (day_142_h08v04_mnScaled, filename=Out_day_142,format="HFA",overwrite=TRUE)
day_143_h08v04_stack<- stack(day_143_h08v04)
nlayers(day_143_h08v04_stack) 
day_143_h08v04_mn<- mean(day_143_h08v04_stack, na.rm=TRUE)
day_143_h08v04_mnScaled<- day_143_h08v04_mn*0.0005
Out_day_143<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_143_h08v04_AVG_Scaled.img"
writeRaster (day_143_h08v04_mnScaled, filename=Out_day_143,format="HFA",overwrite=TRUE)
day_144_h08v04_stack<- stack(day_144_h08v04)
nlayers(day_144_h08v04_stack) 
day_144_h08v04_mn<- mean(day_144_h08v04_stack, na.rm=TRUE)
day_144_h08v04_mnScaled<- day_144_h08v04_mn*0.0005
Out_day_144<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_144_h08v04_AVG_Scaled.img"
writeRaster (day_144_h08v04_mnScaled, filename=Out_day_144,format="HFA",overwrite=TRUE)
day_145_h08v04_stack<- stack(day_145_h08v04)
nlayers(day_145_h08v04_stack) 
day_145_h08v04_mn<- mean(day_145_h08v04_stack, na.rm=TRUE)
day_145_h08v04_mnScaled<- day_145_h08v04_mn*0.0005
Out_day_145<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_145_h08v04_AVG_Scaled.img"
writeRaster (day_145_h08v04_mnScaled, filename=Out_day_145,format="HFA",overwrite=TRUE)
day_146_h08v04_stack<- stack(day_146_h08v04)
nlayers(day_146_h08v04_stack) 
day_146_h08v04_mn<- mean(day_146_h08v04_stack, na.rm=TRUE)
day_146_h08v04_mnScaled<- day_146_h08v04_mn*0.0005
Out_day_146<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_146_h08v04_AVG_Scaled.img"
writeRaster (day_146_h08v04_mnScaled, filename=Out_day_146,format="HFA",overwrite=TRUE)
day_147_h08v04_stack<- stack(day_147_h08v04)
nlayers(day_147_h08v04_stack) 
day_147_h08v04_mn<- mean(day_147_h08v04_stack, na.rm=TRUE)
day_147_h08v04_mnScaled<- day_147_h08v04_mn*0.0005
Out_day_147<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_147_h08v04_AVG_Scaled.img"
writeRaster (day_147_h08v04_mnScaled, filename=Out_day_147,format="HFA",overwrite=TRUE)
day_148_h08v04_stack<- stack(day_148_h08v04)
nlayers(day_148_h08v04_stack) 
day_148_h08v04_mn<- mean(day_148_h08v04_stack, na.rm=TRUE)
day_148_h08v04_mnScaled<- day_148_h08v04_mn*0.0005
Out_day_148<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_148_h08v04_AVG_Scaled.img"
writeRaster (day_148_h08v04_mnScaled, filename=Out_day_148,format="HFA",overwrite=TRUE)
day_149_h08v04_stack<- stack(day_149_h08v04)
nlayers(day_149_h08v04_stack) 
day_149_h08v04_mn<- mean(day_149_h08v04_stack, na.rm=TRUE)
day_149_h08v04_mnScaled<- day_149_h08v04_mn*0.0005
Out_day_149<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_149_h08v04_AVG_Scaled.img"
writeRaster (day_149_h08v04_mnScaled, filename=Out_day_149,format="HFA",overwrite=TRUE)
day_150_h08v04_stack<- stack(day_150_h08v04)
nlayers(day_150_h08v04_stack) 
day_150_h08v04_mn<- mean(day_150_h08v04_stack, na.rm=TRUE)
day_150_h08v04_mnScaled<- day_150_h08v04_mn*0.0005
Out_day_150<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_150_h08v04_AVG_Scaled.img"
writeRaster (day_150_h08v04_mnScaled, filename=Out_day_150,format="HFA",overwrite=TRUE)
day_151_h08v04_stack<- stack(day_151_h08v04)
nlayers(day_151_h08v04_stack) 
day_151_h08v04_mn<- mean(day_151_h08v04_stack, na.rm=TRUE)
day_151_h08v04_mnScaled<- day_151_h08v04_mn*0.0005
Out_day_151<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_151_h08v04_AVG_Scaled.img"
writeRaster (day_151_h08v04_mnScaled, filename=Out_day_151,format="HFA",overwrite=TRUE)
day_152_h08v04_stack<- stack(day_152_h08v04)
nlayers(day_152_h08v04_stack) 
day_152_h08v04_mn<- mean(day_152_h08v04_stack, na.rm=TRUE)
day_152_h08v04_mnScaled<- day_152_h08v04_mn*0.0005
Out_day_152<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_152_h08v04_AVG_Scaled.img"
writeRaster (day_152_h08v04_mnScaled, filename=Out_day_152,format="HFA",overwrite=TRUE)
day_153_h08v04_stack<- stack(day_153_h08v04)
nlayers(day_153_h08v04_stack) 
day_153_h08v04_mn<- mean(day_153_h08v04_stack, na.rm=TRUE)
day_153_h08v04_mnScaled<- day_153_h08v04_mn*0.0005
Out_day_153<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_153_h08v04_AVG_Scaled.img"
writeRaster (day_153_h08v04_mnScaled, filename=Out_day_153,format="HFA",overwrite=TRUE)
day_154_h08v04_stack<- stack(day_154_h08v04)
nlayers(day_154_h08v04_stack) 
day_154_h08v04_mn<- mean(day_154_h08v04_stack, na.rm=TRUE)
day_154_h08v04_mnScaled<- day_154_h08v04_mn*0.0005
Out_day_154<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_154_h08v04_AVG_Scaled.img"
writeRaster (day_154_h08v04_mnScaled, filename=Out_day_154,format="HFA",overwrite=TRUE)
day_155_h08v04_stack<- stack(day_155_h08v04)
nlayers(day_155_h08v04_stack) 
day_155_h08v04_mn<- mean(day_155_h08v04_stack, na.rm=TRUE)
day_155_h08v04_mnScaled<- day_155_h08v04_mn*0.0005
Out_day_155<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_155_h08v04_AVG_Scaled.img"
writeRaster (day_155_h08v04_mnScaled, filename=Out_day_155,format="HFA",overwrite=TRUE)
day_156_h08v04_stack<- stack(day_156_h08v04)
nlayers(day_156_h08v04_stack) 
day_156_h08v04_mn<- mean(day_156_h08v04_stack, na.rm=TRUE)
day_156_h08v04_mnScaled<- day_156_h08v04_mn*0.0005
Out_day_156<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_156_h08v04_AVG_Scaled.img"
writeRaster (day_156_h08v04_mnScaled, filename=Out_day_156,format="HFA",overwrite=TRUE)
day_157_h08v04_stack<- stack(day_157_h08v04)
nlayers(day_157_h08v04_stack) 
day_157_h08v04_mn<- mean(day_157_h08v04_stack, na.rm=TRUE)
day_157_h08v04_mnScaled<- day_157_h08v04_mn*0.0005
Out_day_157<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_157_h08v04_AVG_Scaled.img"
writeRaster (day_157_h08v04_mnScaled, filename=Out_day_157,format="HFA",overwrite=TRUE)
day_158_h08v04_stack<- stack(day_158_h08v04)
nlayers(day_158_h08v04_stack) 
day_158_h08v04_mn<- mean(day_158_h08v04_stack, na.rm=TRUE)
day_158_h08v04_mnScaled<- day_158_h08v04_mn*0.0005
Out_day_158<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_158_h08v04_AVG_Scaled.img"
writeRaster (day_158_h08v04_mnScaled, filename=Out_day_158,format="HFA",overwrite=TRUE)
day_159_h08v04_stack<- stack(day_159_h08v04)
nlayers(day_159_h08v04_stack) 
day_159_h08v04_mn<- mean(day_159_h08v04_stack, na.rm=TRUE)
day_159_h08v04_mnScaled<- day_159_h08v04_mn*0.0005
Out_day_159<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_159_h08v04_AVG_Scaled.img"
writeRaster (day_159_h08v04_mnScaled, filename=Out_day_159,format="HFA",overwrite=TRUE)
day_160_h08v04_stack<- stack(day_160_h08v04)
nlayers(day_160_h08v04_stack) 
day_160_h08v04_mn<- mean(day_160_h08v04_stack, na.rm=TRUE)
day_160_h08v04_mnScaled<- day_160_h08v04_mn*0.0005
Out_day_160<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_160_h08v04_AVG_Scaled.img"
writeRaster (day_160_h08v04_mnScaled, filename=Out_day_160,format="HFA",overwrite=TRUE)
day_161_h08v04_stack<- stack(day_161_h08v04)
nlayers(day_161_h08v04_stack) 
day_161_h08v04_mn<- mean(day_161_h08v04_stack, na.rm=TRUE)
day_161_h08v04_mnScaled<- day_161_h08v04_mn*0.0005
Out_day_161<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_161_h08v04_AVG_Scaled.img"
writeRaster (day_161_h08v04_mnScaled, filename=Out_day_161,format="HFA",overwrite=TRUE)
day_162_h08v04_stack<- stack(day_162_h08v04)
nlayers(day_162_h08v04_stack) 
day_162_h08v04_mn<- mean(day_162_h08v04_stack, na.rm=TRUE)
day_162_h08v04_mnScaled<- day_162_h08v04_mn*0.0005
Out_day_162<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_162_h08v04_AVG_Scaled.img"
writeRaster (day_162_h08v04_mnScaled, filename=Out_day_162,format="HFA",overwrite=TRUE)
day_163_h08v04_stack<- stack(day_163_h08v04)
nlayers(day_163_h08v04_stack) 
day_163_h08v04_mn<- mean(day_163_h08v04_stack, na.rm=TRUE)
day_163_h08v04_mnScaled<- day_163_h08v04_mn*0.0005
Out_day_163<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_163_h08v04_AVG_Scaled.img"
writeRaster (day_163_h08v04_mnScaled, filename=Out_day_163,format="HFA",overwrite=TRUE)
day_164_h08v04_stack<- stack(day_164_h08v04)
nlayers(day_164_h08v04_stack) 
day_164_h08v04_mn<- mean(day_164_h08v04_stack, na.rm=TRUE)
day_164_h08v04_mnScaled<- day_164_h08v04_mn*0.0005
Out_day_164<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_164_h08v04_AVG_Scaled.img"
writeRaster (day_164_h08v04_mnScaled, filename=Out_day_164,format="HFA",overwrite=TRUE)
day_165_h08v04_stack<- stack(day_165_h08v04)
nlayers(day_165_h08v04_stack) 
day_165_h08v04_mn<- mean(day_165_h08v04_stack, na.rm=TRUE)
day_165_h08v04_mnScaled<- day_165_h08v04_mn*0.0005
Out_day_165<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_165_h08v04_AVG_Scaled.img"
writeRaster (day_165_h08v04_mnScaled, filename=Out_day_165,format="HFA",overwrite=TRUE)
day_166_h08v04_stack<- stack(day_166_h08v04)
nlayers(day_166_h08v04_stack) 
day_166_h08v04_mn<- mean(day_166_h08v04_stack, na.rm=TRUE)
day_166_h08v04_mnScaled<- day_166_h08v04_mn*0.0005
Out_day_166<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_166_h08v04_AVG_Scaled.img"
writeRaster (day_166_h08v04_mnScaled, filename=Out_day_166,format="HFA",overwrite=TRUE)
day_167_h08v04_stack<- stack(day_167_h08v04)
nlayers(day_167_h08v04_stack) 
day_167_h08v04_mn<- mean(day_167_h08v04_stack, na.rm=TRUE)
day_167_h08v04_mnScaled<- day_167_h08v04_mn*0.0005
Out_day_167<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_167_h08v04_AVG_Scaled.img"
writeRaster (day_167_h08v04_mnScaled, filename=Out_day_167,format="HFA",overwrite=TRUE)
day_168_h08v04_stack<- stack(day_168_h08v04)
nlayers(day_168_h08v04_stack) 
day_168_h08v04_mn<- mean(day_168_h08v04_stack, na.rm=TRUE)
day_168_h08v04_mnScaled<- day_168_h08v04_mn*0.0005
Out_day_168<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_168_h08v04_AVG_Scaled.img"
writeRaster (day_168_h08v04_mnScaled, filename=Out_day_168,format="HFA",overwrite=TRUE)
day_169_h08v04_stack<- stack(day_169_h08v04)
nlayers(day_169_h08v04_stack) 
day_169_h08v04_mn<- mean(day_169_h08v04_stack, na.rm=TRUE)
day_169_h08v04_mnScaled<- day_169_h08v04_mn*0.0005
Out_day_169<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_169_h08v04_AVG_Scaled.img"
writeRaster (day_169_h08v04_mnScaled, filename=Out_day_169,format="HFA",overwrite=TRUE)
day_170_h08v04_stack<- stack(day_170_h08v04)
nlayers(day_170_h08v04_stack) 
day_170_h08v04_mn<- mean(day_170_h08v04_stack, na.rm=TRUE)
day_170_h08v04_mnScaled<- day_170_h08v04_mn*0.0005
Out_day_170<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_170_h08v04_AVG_Scaled.img"
writeRaster (day_170_h08v04_mnScaled, filename=Out_day_170,format="HFA",overwrite=TRUE)
day_171_h08v04_stack<- stack(day_171_h08v04)
nlayers(day_171_h08v04_stack) 
day_171_h08v04_mn<- mean(day_171_h08v04_stack, na.rm=TRUE)
day_171_h08v04_mnScaled<- day_171_h08v04_mn*0.0005
Out_day_171<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_171_h08v04_AVG_Scaled.img"
writeRaster (day_171_h08v04_mnScaled, filename=Out_day_171,format="HFA",overwrite=TRUE)
day_172_h08v04_stack<- stack(day_172_h08v04)
nlayers(day_172_h08v04_stack) 
day_172_h08v04_mn<- mean(day_172_h08v04_stack, na.rm=TRUE)
day_172_h08v04_mnScaled<- day_172_h08v04_mn*0.0005
Out_day_172<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_172_h08v04_AVG_Scaled.img"
writeRaster (day_172_h08v04_mnScaled, filename=Out_day_172,format="HFA",overwrite=TRUE)
day_173_h08v04_stack<- stack(day_173_h08v04)
nlayers(day_173_h08v04_stack) 
day_173_h08v04_mn<- mean(day_173_h08v04_stack, na.rm=TRUE)
day_173_h08v04_mnScaled<- day_173_h08v04_mn*0.0005
Out_day_173<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_173_h08v04_AVG_Scaled.img"
writeRaster (day_173_h08v04_mnScaled, filename=Out_day_173,format="HFA",overwrite=TRUE)
day_174_h08v04_stack<- stack(day_174_h08v04)
nlayers(day_174_h08v04_stack) 
day_174_h08v04_mn<- mean(day_174_h08v04_stack, na.rm=TRUE)
day_174_h08v04_mnScaled<- day_174_h08v04_mn*0.0005
Out_day_174<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_174_h08v04_AVG_Scaled.img"
writeRaster (day_174_h08v04_mnScaled, filename=Out_day_174,format="HFA",overwrite=TRUE)
day_175_h08v04_stack<- stack(day_175_h08v04)
nlayers(day_175_h08v04_stack) 
day_175_h08v04_mn<- mean(day_175_h08v04_stack, na.rm=TRUE)
day_175_h08v04_mnScaled<- day_175_h08v04_mn*0.0005
Out_day_175<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_175_h08v04_AVG_Scaled.img"
writeRaster (day_175_h08v04_mnScaled, filename=Out_day_175,format="HFA",overwrite=TRUE)
day_176_h08v04_stack<- stack(day_176_h08v04)
nlayers(day_176_h08v04_stack) 
day_176_h08v04_mn<- mean(day_176_h08v04_stack, na.rm=TRUE)
day_176_h08v04_mnScaled<- day_176_h08v04_mn*0.0005
Out_day_176<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_176_h08v04_AVG_Scaled.img"
writeRaster (day_176_h08v04_mnScaled, filename=Out_day_176,format="HFA",overwrite=TRUE)
day_177_h08v04_stack<- stack(day_177_h08v04)
nlayers(day_177_h08v04_stack) 
day_177_h08v04_mn<- mean(day_177_h08v04_stack, na.rm=TRUE)
day_177_h08v04_mnScaled<- day_177_h08v04_mn*0.0005
Out_day_177<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_177_h08v04_AVG_Scaled.img"
writeRaster (day_177_h08v04_mnScaled, filename=Out_day_177,format="HFA",overwrite=TRUE)
day_178_h08v04_stack<- stack(day_178_h08v04)
nlayers(day_178_h08v04_stack) 
day_178_h08v04_mn<- mean(day_178_h08v04_stack, na.rm=TRUE)
day_178_h08v04_mnScaled<- day_178_h08v04_mn*0.0005
Out_day_178<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_178_h08v04_AVG_Scaled.img"
writeRaster (day_178_h08v04_mnScaled, filename=Out_day_178,format="HFA",overwrite=TRUE)
day_179_h08v04_stack<- stack(day_179_h08v04)
nlayers(day_179_h08v04_stack) 
day_179_h08v04_mn<- mean(day_179_h08v04_stack, na.rm=TRUE)
day_179_h08v04_mnScaled<- day_179_h08v04_mn*0.0005
Out_day_179<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_179_h08v04_AVG_Scaled.img"
writeRaster (day_179_h08v04_mnScaled, filename=Out_day_179,format="HFA",overwrite=TRUE)
day_180_h08v04_stack<- stack(day_180_h08v04)
nlayers(day_180_h08v04_stack) 
day_180_h08v04_mn<- mean(day_180_h08v04_stack, na.rm=TRUE)
day_180_h08v04_mnScaled<- day_180_h08v04_mn*0.0005
Out_day_180<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_180_h08v04_AVG_Scaled.img"
writeRaster (day_180_h08v04_mnScaled, filename=Out_day_180,format="HFA",overwrite=TRUE)
day_181_h08v04_stack<- stack(day_181_h08v04)
nlayers(day_181_h08v04_stack) 
day_181_h08v04_mn<- mean(day_181_h08v04_stack, na.rm=TRUE)
day_181_h08v04_mnScaled<- day_181_h08v04_mn*0.0005
Out_day_181<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_181_h08v04_AVG_Scaled.img"
writeRaster (day_181_h08v04_mnScaled, filename=Out_day_181,format="HFA",overwrite=TRUE)
day_182_h08v04_stack<- stack(day_182_h08v04)
nlayers(day_182_h08v04_stack) 
day_182_h08v04_mn<- mean(day_182_h08v04_stack, na.rm=TRUE)
day_182_h08v04_mnScaled<- day_182_h08v04_mn*0.0005
Out_day_182<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_182_h08v04_AVG_Scaled.img"
writeRaster (day_182_h08v04_mnScaled, filename=Out_day_182,format="HFA",overwrite=TRUE)
day_183_h08v04_stack<- stack(day_183_h08v04)
nlayers(day_183_h08v04_stack) 
day_183_h08v04_mn<- mean(day_183_h08v04_stack, na.rm=TRUE)
day_183_h08v04_mnScaled<- day_183_h08v04_mn*0.0005
Out_day_183<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_183_h08v04_AVG_Scaled.img"
writeRaster (day_183_h08v04_mnScaled, filename=Out_day_183,format="HFA",overwrite=TRUE)
day_184_h08v04_stack<- stack(day_184_h08v04)
nlayers(day_184_h08v04_stack) 
day_184_h08v04_mn<- mean(day_184_h08v04_stack, na.rm=TRUE)
day_184_h08v04_mnScaled<- day_184_h08v04_mn*0.0005
Out_day_184<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_184_h08v04_AVG_Scaled.img"
writeRaster (day_184_h08v04_mnScaled, filename=Out_day_184,format="HFA",overwrite=TRUE)
day_185_h08v04_stack<- stack(day_185_h08v04)
nlayers(day_185_h08v04_stack) 
day_185_h08v04_mn<- mean(day_185_h08v04_stack, na.rm=TRUE)
day_185_h08v04_mnScaled<- day_185_h08v04_mn*0.0005
Out_day_185<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_185_h08v04_AVG_Scaled.img"
writeRaster (day_185_h08v04_mnScaled, filename=Out_day_185,format="HFA",overwrite=TRUE)
day_186_h08v04_stack<- stack(day_186_h08v04)
nlayers(day_186_h08v04_stack) 
day_186_h08v04_mn<- mean(day_186_h08v04_stack, na.rm=TRUE)
day_186_h08v04_mnScaled<- day_186_h08v04_mn*0.0005
Out_day_186<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_186_h08v04_AVG_Scaled.img"
writeRaster (day_186_h08v04_mnScaled, filename=Out_day_186,format="HFA",overwrite=TRUE)
day_187_h08v04_stack<- stack(day_187_h08v04)
nlayers(day_187_h08v04_stack) 
day_187_h08v04_mn<- mean(day_187_h08v04_stack, na.rm=TRUE)
day_187_h08v04_mnScaled<- day_187_h08v04_mn*0.0005
Out_day_187<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_187_h08v04_AVG_Scaled.img"
writeRaster (day_187_h08v04_mnScaled, filename=Out_day_187,format="HFA",overwrite=TRUE)
day_188_h08v04_stack<- stack(day_188_h08v04)
nlayers(day_188_h08v04_stack) 
day_188_h08v04_mn<- mean(day_188_h08v04_stack, na.rm=TRUE)
day_188_h08v04_mnScaled<- day_188_h08v04_mn*0.0005
Out_day_188<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_188_h08v04_AVG_Scaled.img"
writeRaster (day_188_h08v04_mnScaled, filename=Out_day_188,format="HFA",overwrite=TRUE)
day_189_h08v04_stack<- stack(day_189_h08v04)
nlayers(day_189_h08v04_stack) 
day_189_h08v04_mn<- mean(day_189_h08v04_stack, na.rm=TRUE)
day_189_h08v04_mnScaled<- day_189_h08v04_mn*0.0005
Out_day_189<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_189_h08v04_AVG_Scaled.img"
writeRaster (day_189_h08v04_mnScaled, filename=Out_day_189,format="HFA",overwrite=TRUE)
day_190_h08v04_stack<- stack(day_190_h08v04)
nlayers(day_190_h08v04_stack) 
day_190_h08v04_mn<- mean(day_190_h08v04_stack, na.rm=TRUE)
day_190_h08v04_mnScaled<- day_190_h08v04_mn*0.0005
Out_day_190<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_190_h08v04_AVG_Scaled.img"
writeRaster (day_190_h08v04_mnScaled, filename=Out_day_190,format="HFA",overwrite=TRUE)
day_191_h08v04_stack<- stack(day_191_h08v04)
nlayers(day_191_h08v04_stack) 
day_191_h08v04_mn<- mean(day_191_h08v04_stack, na.rm=TRUE)
day_191_h08v04_mnScaled<- day_191_h08v04_mn*0.0005
Out_day_191<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_191_h08v04_AVG_Scaled.img"
writeRaster (day_191_h08v04_mnScaled, filename=Out_day_191,format="HFA",overwrite=TRUE)
day_192_h08v04_stack<- stack(day_192_h08v04)
nlayers(day_192_h08v04_stack) 
day_192_h08v04_mn<- mean(day_192_h08v04_stack, na.rm=TRUE)
day_192_h08v04_mnScaled<- day_192_h08v04_mn*0.0005
Out_day_192<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_192_h08v04_AVG_Scaled.img"
writeRaster (day_192_h08v04_mnScaled, filename=Out_day_192,format="HFA",overwrite=TRUE)
day_193_h08v04_stack<- stack(day_193_h08v04)
nlayers(day_193_h08v04_stack) 
day_193_h08v04_mn<- mean(day_193_h08v04_stack, na.rm=TRUE)
day_193_h08v04_mnScaled<- day_193_h08v04_mn*0.0005
Out_day_193<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_193_h08v04_AVG_Scaled.img"
writeRaster (day_193_h08v04_mnScaled, filename=Out_day_193,format="HFA",overwrite=TRUE)
day_194_h08v04_stack<- stack(day_194_h08v04)
nlayers(day_194_h08v04_stack) 
day_194_h08v04_mn<- mean(day_194_h08v04_stack, na.rm=TRUE)
day_194_h08v04_mnScaled<- day_194_h08v04_mn*0.0005
Out_day_194<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_194_h08v04_AVG_Scaled.img"
writeRaster (day_194_h08v04_mnScaled, filename=Out_day_194,format="HFA",overwrite=TRUE)
day_195_h08v04_stack<- stack(day_195_h08v04)
nlayers(day_195_h08v04_stack) 
day_195_h08v04_mn<- mean(day_195_h08v04_stack, na.rm=TRUE)
day_195_h08v04_mnScaled<- day_195_h08v04_mn*0.0005
Out_day_195<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_195_h08v04_AVG_Scaled.img"
writeRaster (day_195_h08v04_mnScaled, filename=Out_day_195,format="HFA",overwrite=TRUE)
day_196_h08v04_stack<- stack(day_196_h08v04)
nlayers(day_196_h08v04_stack) 
day_196_h08v04_mn<- mean(day_196_h08v04_stack, na.rm=TRUE)
day_196_h08v04_mnScaled<- day_196_h08v04_mn*0.0005
Out_day_196<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_196_h08v04_AVG_Scaled.img"
writeRaster (day_196_h08v04_mnScaled, filename=Out_day_196,format="HFA",overwrite=TRUE)
day_197_h08v04_stack<- stack(day_197_h08v04)
nlayers(day_197_h08v04_stack) 
day_197_h08v04_mn<- mean(day_197_h08v04_stack, na.rm=TRUE)
day_197_h08v04_mnScaled<- day_197_h08v04_mn*0.0005
Out_day_197<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_197_h08v04_AVG_Scaled.img"
writeRaster (day_197_h08v04_mnScaled, filename=Out_day_197,format="HFA",overwrite=TRUE)
day_198_h08v04_stack<- stack(day_198_h08v04)
nlayers(day_198_h08v04_stack) 
day_198_h08v04_mn<- mean(day_198_h08v04_stack, na.rm=TRUE)
day_198_h08v04_mnScaled<- day_198_h08v04_mn*0.0005
Out_day_198<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_198_h08v04_AVG_Scaled.img"
writeRaster (day_198_h08v04_mnScaled, filename=Out_day_198,format="HFA",overwrite=TRUE)
day_199_h08v04_stack<- stack(day_199_h08v04)
nlayers(day_199_h08v04_stack) 
day_199_h08v04_mn<- mean(day_199_h08v04_stack, na.rm=TRUE)
day_199_h08v04_mnScaled<- day_199_h08v04_mn*0.0005
Out_day_199<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_199_h08v04_AVG_Scaled.img"
writeRaster (day_199_h08v04_mnScaled, filename=Out_day_199,format="HFA",overwrite=TRUE)
day_200_h08v04_stack<- stack(day_200_h08v04)
nlayers(day_200_h08v04_stack) 
day_200_h08v04_mn<- mean(day_200_h08v04_stack, na.rm=TRUE)
day_200_h08v04_mnScaled<- day_200_h08v04_mn*0.0005
Out_day_200<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_200_h08v04_AVG_Scaled.img"
writeRaster (day_200_h08v04_mnScaled, filename=Out_day_200,format="HFA",overwrite=TRUE)
day_201_h08v04_stack<- stack(day_201_h08v04)
nlayers(day_201_h08v04_stack) 
day_201_h08v04_mn<- mean(day_201_h08v04_stack, na.rm=TRUE)
day_201_h08v04_mnScaled<- day_201_h08v04_mn*0.0005
Out_day_201<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_201_h08v04_AVG_Scaled.img"
writeRaster (day_201_h08v04_mnScaled, filename=Out_day_201,format="HFA",overwrite=TRUE)
day_202_h08v04_stack<- stack(day_202_h08v04)
nlayers(day_202_h08v04_stack) 
day_202_h08v04_mn<- mean(day_202_h08v04_stack, na.rm=TRUE)
day_202_h08v04_mnScaled<- day_202_h08v04_mn*0.0005
Out_day_202<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_202_h08v04_AVG_Scaled.img"
writeRaster (day_202_h08v04_mnScaled, filename=Out_day_202,format="HFA",overwrite=TRUE)
day_203_h08v04_stack<- stack(day_203_h08v04)
nlayers(day_203_h08v04_stack) 
day_203_h08v04_mn<- mean(day_203_h08v04_stack, na.rm=TRUE)
day_203_h08v04_mnScaled<- day_203_h08v04_mn*0.0005
Out_day_203<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_203_h08v04_AVG_Scaled.img"
writeRaster (day_203_h08v04_mnScaled, filename=Out_day_203,format="HFA",overwrite=TRUE)
day_204_h08v04_stack<- stack(day_204_h08v04)
nlayers(day_204_h08v04_stack) 
day_204_h08v04_mn<- mean(day_204_h08v04_stack, na.rm=TRUE)
day_204_h08v04_mnScaled<- day_204_h08v04_mn*0.0005
Out_day_204<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_204_h08v04_AVG_Scaled.img"
writeRaster (day_204_h08v04_mnScaled, filename=Out_day_204,format="HFA",overwrite=TRUE)
day_205_h08v04_stack<- stack(day_205_h08v04)
nlayers(day_205_h08v04_stack) 
day_205_h08v04_mn<- mean(day_205_h08v04_stack, na.rm=TRUE)
day_205_h08v04_mnScaled<- day_205_h08v04_mn*0.0005
Out_day_205<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_205_h08v04_AVG_Scaled.img"
writeRaster (day_205_h08v04_mnScaled, filename=Out_day_205,format="HFA",overwrite=TRUE)
day_206_h08v04_stack<- stack(day_206_h08v04)
nlayers(day_206_h08v04_stack) 
day_206_h08v04_mn<- mean(day_206_h08v04_stack, na.rm=TRUE)
day_206_h08v04_mnScaled<- day_206_h08v04_mn*0.0005
Out_day_206<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_206_h08v04_AVG_Scaled.img"
writeRaster (day_206_h08v04_mnScaled, filename=Out_day_206,format="HFA",overwrite=TRUE)
day_207_h08v04_stack<- stack(day_207_h08v04)
nlayers(day_207_h08v04_stack) 
day_207_h08v04_mn<- mean(day_207_h08v04_stack, na.rm=TRUE)
day_207_h08v04_mnScaled<- day_207_h08v04_mn*0.0005
Out_day_207<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_207_h08v04_AVG_Scaled.img"
writeRaster (day_207_h08v04_mnScaled, filename=Out_day_207,format="HFA",overwrite=TRUE)
day_208_h08v04_stack<- stack(day_208_h08v04)
nlayers(day_208_h08v04_stack) 
day_208_h08v04_mn<- mean(day_208_h08v04_stack, na.rm=TRUE)
day_208_h08v04_mnScaled<- day_208_h08v04_mn*0.0005
Out_day_208<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_208_h08v04_AVG_Scaled.img"
writeRaster (day_208_h08v04_mnScaled, filename=Out_day_208,format="HFA",overwrite=TRUE)
day_209_h08v04_stack<- stack(day_209_h08v04)
nlayers(day_209_h08v04_stack) 
day_209_h08v04_mn<- mean(day_209_h08v04_stack, na.rm=TRUE)
day_209_h08v04_mnScaled<- day_209_h08v04_mn*0.0005
Out_day_209<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_209_h08v04_AVG_Scaled.img"
writeRaster (day_209_h08v04_mnScaled, filename=Out_day_209,format="HFA",overwrite=TRUE)
day_210_h08v04_stack<- stack(day_210_h08v04)
nlayers(day_210_h08v04_stack) 
day_210_h08v04_mn<- mean(day_210_h08v04_stack, na.rm=TRUE)
day_210_h08v04_mnScaled<- day_210_h08v04_mn*0.0005
Out_day_210<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_210_h08v04_AVG_Scaled.img"
writeRaster (day_210_h08v04_mnScaled, filename=Out_day_210,format="HFA",overwrite=TRUE)
day_211_h08v04_stack<- stack(day_211_h08v04)
nlayers(day_211_h08v04_stack) 
day_211_h08v04_mn<- mean(day_211_h08v04_stack, na.rm=TRUE)
day_211_h08v04_mnScaled<- day_211_h08v04_mn*0.0005
Out_day_211<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_211_h08v04_AVG_Scaled.img"
writeRaster (day_211_h08v04_mnScaled, filename=Out_day_211,format="HFA",overwrite=TRUE)
day_212_h08v04_stack<- stack(day_212_h08v04)
nlayers(day_212_h08v04_stack) 
day_212_h08v04_mn<- mean(day_212_h08v04_stack, na.rm=TRUE)
day_212_h08v04_mnScaled<- day_212_h08v04_mn*0.0005
Out_day_212<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_212_h08v04_AVG_Scaled.img"
writeRaster (day_212_h08v04_mnScaled, filename=Out_day_212,format="HFA",overwrite=TRUE)
day_213_h08v04_stack<- stack(day_213_h08v04)
nlayers(day_213_h08v04_stack) 
day_213_h08v04_mn<- mean(day_213_h08v04_stack, na.rm=TRUE)
day_213_h08v04_mnScaled<- day_213_h08v04_mn*0.0005
Out_day_213<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_213_h08v04_AVG_Scaled.img"
writeRaster (day_213_h08v04_mnScaled, filename=Out_day_213,format="HFA",overwrite=TRUE)
day_214_h08v04_stack<- stack(day_214_h08v04)
nlayers(day_214_h08v04_stack) 
day_214_h08v04_mn<- mean(day_214_h08v04_stack, na.rm=TRUE)
day_214_h08v04_mnScaled<- day_214_h08v04_mn*0.0005
Out_day_214<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_214_h08v04_AVG_Scaled.img"
writeRaster (day_214_h08v04_mnScaled, filename=Out_day_214,format="HFA",overwrite=TRUE)
day_215_h08v04_stack<- stack(day_215_h08v04)
nlayers(day_215_h08v04_stack) 
day_215_h08v04_mn<- mean(day_215_h08v04_stack, na.rm=TRUE)
day_215_h08v04_mnScaled<- day_215_h08v04_mn*0.0005
Out_day_215<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_215_h08v04_AVG_Scaled.img"
writeRaster (day_215_h08v04_mnScaled, filename=Out_day_215,format="HFA",overwrite=TRUE)
day_216_h08v04_stack<- stack(day_216_h08v04)
nlayers(day_216_h08v04_stack) 
day_216_h08v04_mn<- mean(day_216_h08v04_stack, na.rm=TRUE)
day_216_h08v04_mnScaled<- day_216_h08v04_mn*0.0005
Out_day_216<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_216_h08v04_AVG_Scaled.img"
writeRaster (day_216_h08v04_mnScaled, filename=Out_day_216,format="HFA",overwrite=TRUE)
day_217_h08v04_stack<- stack(day_217_h08v04)
nlayers(day_217_h08v04_stack) 
day_217_h08v04_mn<- mean(day_217_h08v04_stack, na.rm=TRUE)
day_217_h08v04_mnScaled<- day_217_h08v04_mn*0.0005
Out_day_217<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_217_h08v04_AVG_Scaled.img"
writeRaster (day_217_h08v04_mnScaled, filename=Out_day_217,format="HFA",overwrite=TRUE)
day_218_h08v04_stack<- stack(day_218_h08v04)
nlayers(day_218_h08v04_stack) 
day_218_h08v04_mn<- mean(day_218_h08v04_stack, na.rm=TRUE)
day_218_h08v04_mnScaled<- day_218_h08v04_mn*0.0005
Out_day_218<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_218_h08v04_AVG_Scaled.img"
writeRaster (day_218_h08v04_mnScaled, filename=Out_day_218,format="HFA",overwrite=TRUE)
day_219_h08v04_stack<- stack(day_219_h08v04)
nlayers(day_219_h08v04_stack) 
day_219_h08v04_mn<- mean(day_219_h08v04_stack, na.rm=TRUE)
day_219_h08v04_mnScaled<- day_219_h08v04_mn*0.0005
Out_day_219<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_219_h08v04_AVG_Scaled.img"
writeRaster (day_219_h08v04_mnScaled, filename=Out_day_219,format="HFA",overwrite=TRUE)
day_220_h08v04_stack<- stack(day_220_h08v04)
nlayers(day_220_h08v04_stack) 
day_220_h08v04_mn<- mean(day_220_h08v04_stack, na.rm=TRUE)
day_220_h08v04_mnScaled<- day_220_h08v04_mn*0.0005
Out_day_220<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_220_h08v04_AVG_Scaled.img"
writeRaster (day_220_h08v04_mnScaled, filename=Out_day_220,format="HFA",overwrite=TRUE)
day_221_h08v04_stack<- stack(day_221_h08v04)
nlayers(day_221_h08v04_stack) 
day_221_h08v04_mn<- mean(day_221_h08v04_stack, na.rm=TRUE)
day_221_h08v04_mnScaled<- day_221_h08v04_mn*0.0005
Out_day_221<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_221_h08v04_AVG_Scaled.img"
writeRaster (day_221_h08v04_mnScaled, filename=Out_day_221,format="HFA",overwrite=TRUE)
day_222_h08v04_stack<- stack(day_222_h08v04)
nlayers(day_222_h08v04_stack) 
day_222_h08v04_mn<- mean(day_222_h08v04_stack, na.rm=TRUE)
day_222_h08v04_mnScaled<- day_222_h08v04_mn*0.0005
Out_day_222<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_222_h08v04_AVG_Scaled.img"
writeRaster (day_222_h08v04_mnScaled, filename=Out_day_222,format="HFA",overwrite=TRUE)
day_223_h08v04_stack<- stack(day_223_h08v04)
nlayers(day_223_h08v04_stack) 
day_223_h08v04_mn<- mean(day_223_h08v04_stack, na.rm=TRUE)
day_223_h08v04_mnScaled<- day_223_h08v04_mn*0.0005
Out_day_223<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_223_h08v04_AVG_Scaled.img"
writeRaster (day_223_h08v04_mnScaled, filename=Out_day_223,format="HFA",overwrite=TRUE)
day_224_h08v04_stack<- stack(day_224_h08v04)
nlayers(day_224_h08v04_stack) 
day_224_h08v04_mn<- mean(day_224_h08v04_stack, na.rm=TRUE)
day_224_h08v04_mnScaled<- day_224_h08v04_mn*0.0005
Out_day_224<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_224_h08v04_AVG_Scaled.img"
writeRaster (day_224_h08v04_mnScaled, filename=Out_day_224,format="HFA",overwrite=TRUE)
day_225_h08v04_stack<- stack(day_225_h08v04)
nlayers(day_225_h08v04_stack) 
day_225_h08v04_mn<- mean(day_225_h08v04_stack, na.rm=TRUE)
day_225_h08v04_mnScaled<- day_225_h08v04_mn*0.0005
Out_day_225<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_225_h08v04_AVG_Scaled.img"
writeRaster (day_225_h08v04_mnScaled, filename=Out_day_225,format="HFA",overwrite=TRUE)
day_226_h08v04_stack<- stack(day_226_h08v04)
nlayers(day_226_h08v04_stack) 
day_226_h08v04_mn<- mean(day_226_h08v04_stack, na.rm=TRUE)
day_226_h08v04_mnScaled<- day_226_h08v04_mn*0.0005
Out_day_226<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_226_h08v04_AVG_Scaled.img"
writeRaster (day_226_h08v04_mnScaled, filename=Out_day_226,format="HFA",overwrite=TRUE)
day_227_h08v04_stack<- stack(day_227_h08v04)
nlayers(day_227_h08v04_stack) 
day_227_h08v04_mn<- mean(day_227_h08v04_stack, na.rm=TRUE)
day_227_h08v04_mnScaled<- day_227_h08v04_mn*0.0005
Out_day_227<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_227_h08v04_AVG_Scaled.img"
writeRaster (day_227_h08v04_mnScaled, filename=Out_day_227,format="HFA",overwrite=TRUE)
day_228_h08v04_stack<- stack(day_228_h08v04)
nlayers(day_228_h08v04_stack) 
day_228_h08v04_mn<- mean(day_228_h08v04_stack, na.rm=TRUE)
day_228_h08v04_mnScaled<- day_228_h08v04_mn*0.0005
Out_day_228<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_228_h08v04_AVG_Scaled.img"
writeRaster (day_228_h08v04_mnScaled, filename=Out_day_228,format="HFA",overwrite=TRUE)
day_229_h08v04_stack<- stack(day_229_h08v04)
nlayers(day_229_h08v04_stack) 
day_229_h08v04_mn<- mean(day_229_h08v04_stack, na.rm=TRUE)
day_229_h08v04_mnScaled<- day_229_h08v04_mn*0.0005
Out_day_229<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_229_h08v04_AVG_Scaled.img"
writeRaster (day_229_h08v04_mnScaled, filename=Out_day_229,format="HFA",overwrite=TRUE)
day_230_h08v04_stack<- stack(day_230_h08v04)
nlayers(day_230_h08v04_stack) 
day_230_h08v04_mn<- mean(day_230_h08v04_stack, na.rm=TRUE)
day_230_h08v04_mnScaled<- day_230_h08v04_mn*0.0005
Out_day_230<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_230_h08v04_AVG_Scaled.img"
writeRaster (day_230_h08v04_mnScaled, filename=Out_day_230,format="HFA",overwrite=TRUE)
day_231_h08v04_stack<- stack(day_231_h08v04)
nlayers(day_231_h08v04_stack) 
day_231_h08v04_mn<- mean(day_231_h08v04_stack, na.rm=TRUE)
day_231_h08v04_mnScaled<- day_231_h08v04_mn*0.0005
Out_day_231<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_231_h08v04_AVG_Scaled.img"
writeRaster (day_231_h08v04_mnScaled, filename=Out_day_231,format="HFA",overwrite=TRUE)
day_232_h08v04_stack<- stack(day_232_h08v04)
nlayers(day_232_h08v04_stack) 
day_232_h08v04_mn<- mean(day_232_h08v04_stack, na.rm=TRUE)
day_232_h08v04_mnScaled<- day_232_h08v04_mn*0.0005
Out_day_232<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_232_h08v04_AVG_Scaled.img"
writeRaster (day_232_h08v04_mnScaled, filename=Out_day_232,format="HFA",overwrite=TRUE)
day_233_h08v04_stack<- stack(day_233_h08v04)
nlayers(day_233_h08v04_stack) 
day_233_h08v04_mn<- mean(day_233_h08v04_stack, na.rm=TRUE)
day_233_h08v04_mnScaled<- day_233_h08v04_mn*0.0005
Out_day_233<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_233_h08v04_AVG_Scaled.img"
writeRaster (day_233_h08v04_mnScaled, filename=Out_day_233,format="HFA",overwrite=TRUE)
day_234_h08v04_stack<- stack(day_234_h08v04)
nlayers(day_234_h08v04_stack) 
day_234_h08v04_mn<- mean(day_234_h08v04_stack, na.rm=TRUE)
day_234_h08v04_mnScaled<- day_234_h08v04_mn*0.0005
Out_day_234<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_234_h08v04_AVG_Scaled.img"
writeRaster (day_234_h08v04_mnScaled, filename=Out_day_234,format="HFA",overwrite=TRUE)
day_235_h08v04_stack<- stack(day_235_h08v04)
nlayers(day_235_h08v04_stack) 
day_235_h08v04_mn<- mean(day_235_h08v04_stack, na.rm=TRUE)
day_235_h08v04_mnScaled<- day_235_h08v04_mn*0.0005
Out_day_235<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_235_h08v04_AVG_Scaled.img"
writeRaster (day_235_h08v04_mnScaled, filename=Out_day_235,format="HFA",overwrite=TRUE)
day_236_h08v04_stack<- stack(day_236_h08v04)
nlayers(day_236_h08v04_stack) 
day_236_h08v04_mn<- mean(day_236_h08v04_stack, na.rm=TRUE)
day_236_h08v04_mnScaled<- day_236_h08v04_mn*0.0005
Out_day_236<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_236_h08v04_AVG_Scaled.img"
writeRaster (day_236_h08v04_mnScaled, filename=Out_day_236,format="HFA",overwrite=TRUE)
day_237_h08v04_stack<- stack(day_237_h08v04)
nlayers(day_237_h08v04_stack) 
day_237_h08v04_mn<- mean(day_237_h08v04_stack, na.rm=TRUE)
day_237_h08v04_mnScaled<- day_237_h08v04_mn*0.0005
Out_day_237<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_237_h08v04_AVG_Scaled.img"
writeRaster (day_237_h08v04_mnScaled, filename=Out_day_237,format="HFA",overwrite=TRUE)
day_238_h08v04_stack<- stack(day_238_h08v04)
nlayers(day_238_h08v04_stack) 
day_238_h08v04_mn<- mean(day_238_h08v04_stack, na.rm=TRUE)
day_238_h08v04_mnScaled<- day_238_h08v04_mn*0.0005
Out_day_238<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_238_h08v04_AVG_Scaled.img"
writeRaster (day_238_h08v04_mnScaled, filename=Out_day_238,format="HFA",overwrite=TRUE)
day_239_h08v04_stack<- stack(day_239_h08v04)
nlayers(day_239_h08v04_stack) 
day_239_h08v04_mn<- mean(day_239_h08v04_stack, na.rm=TRUE)
day_239_h08v04_mnScaled<- day_239_h08v04_mn*0.0005
Out_day_239<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_239_h08v04_AVG_Scaled.img"
writeRaster (day_239_h08v04_mnScaled, filename=Out_day_239,format="HFA",overwrite=TRUE)
day_240_h08v04_stack<- stack(day_240_h08v04)
nlayers(day_240_h08v04_stack) 
day_240_h08v04_mn<- mean(day_240_h08v04_stack, na.rm=TRUE)
day_240_h08v04_mnScaled<- day_240_h08v04_mn*0.0005
Out_day_240<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_240_h08v04_AVG_Scaled.img"
writeRaster (day_240_h08v04_mnScaled, filename=Out_day_240,format="HFA",overwrite=TRUE)
day_241_h08v04_stack<- stack(day_241_h08v04)
nlayers(day_241_h08v04_stack) 
day_241_h08v04_mn<- mean(day_241_h08v04_stack, na.rm=TRUE)
day_241_h08v04_mnScaled<- day_241_h08v04_mn*0.0005
Out_day_241<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_241_h08v04_AVG_Scaled.img"
writeRaster (day_241_h08v04_mnScaled, filename=Out_day_241,format="HFA",overwrite=TRUE)
day_242_h08v04_stack<- stack(day_242_h08v04)
nlayers(day_242_h08v04_stack) 
day_242_h08v04_mn<- mean(day_242_h08v04_stack, na.rm=TRUE)
day_242_h08v04_mnScaled<- day_242_h08v04_mn*0.0005
Out_day_242<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_242_h08v04_AVG_Scaled.img"
writeRaster (day_242_h08v04_mnScaled, filename=Out_day_242,format="HFA",overwrite=TRUE)
day_243_h08v04_stack<- stack(day_243_h08v04)
nlayers(day_243_h08v04_stack) 
day_243_h08v04_mn<- mean(day_243_h08v04_stack, na.rm=TRUE)
day_243_h08v04_mnScaled<- day_243_h08v04_mn*0.0005
Out_day_243<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_243_h08v04_AVG_Scaled.img"
writeRaster (day_243_h08v04_mnScaled, filename=Out_day_243,format="HFA",overwrite=TRUE)
day_244_h08v04_stack<- stack(day_244_h08v04)
nlayers(day_244_h08v04_stack) 
day_244_h08v04_mn<- mean(day_244_h08v04_stack, na.rm=TRUE)
day_244_h08v04_mnScaled<- day_244_h08v04_mn*0.0005
Out_day_244<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_244_h08v04_AVG_Scaled.img"
writeRaster (day_244_h08v04_mnScaled, filename=Out_day_244,format="HFA",overwrite=TRUE)
day_245_h08v04_stack<- stack(day_245_h08v04)
nlayers(day_245_h08v04_stack) 
day_245_h08v04_mn<- mean(day_245_h08v04_stack, na.rm=TRUE)
day_245_h08v04_mnScaled<- day_245_h08v04_mn*0.0005
Out_day_245<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_245_h08v04_AVG_Scaled.img"
writeRaster (day_245_h08v04_mnScaled, filename=Out_day_245,format="HFA",overwrite=TRUE)
day_246_h08v04_stack<- stack(day_246_h08v04)
nlayers(day_246_h08v04_stack) 
day_246_h08v04_mn<- mean(day_246_h08v04_stack, na.rm=TRUE)
day_246_h08v04_mnScaled<- day_246_h08v04_mn*0.0005
Out_day_246<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_246_h08v04_AVG_Scaled.img"
writeRaster (day_246_h08v04_mnScaled, filename=Out_day_246,format="HFA",overwrite=TRUE)
day_247_h08v04_stack<- stack(day_247_h08v04)
nlayers(day_247_h08v04_stack) 
day_247_h08v04_mn<- mean(day_247_h08v04_stack, na.rm=TRUE)
day_247_h08v04_mnScaled<- day_247_h08v04_mn*0.0005
Out_day_247<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_247_h08v04_AVG_Scaled.img"
writeRaster (day_247_h08v04_mnScaled, filename=Out_day_247,format="HFA",overwrite=TRUE)
day_248_h08v04_stack<- stack(day_248_h08v04)
nlayers(day_248_h08v04_stack) 
day_248_h08v04_mn<- mean(day_248_h08v04_stack, na.rm=TRUE)
day_248_h08v04_mnScaled<- day_248_h08v04_mn*0.0005
Out_day_248<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_248_h08v04_AVG_Scaled.img"
writeRaster (day_248_h08v04_mnScaled, filename=Out_day_248,format="HFA",overwrite=TRUE)
day_249_h08v04_stack<- stack(day_249_h08v04)
nlayers(day_249_h08v04_stack) 
day_249_h08v04_mn<- mean(day_249_h08v04_stack, na.rm=TRUE)
day_249_h08v04_mnScaled<- day_249_h08v04_mn*0.0005
Out_day_249<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_249_h08v04_AVG_Scaled.img"
writeRaster (day_249_h08v04_mnScaled, filename=Out_day_249,format="HFA",overwrite=TRUE)
day_250_h08v04_stack<- stack(day_250_h08v04)
nlayers(day_250_h08v04_stack) 
day_250_h08v04_mn<- mean(day_250_h08v04_stack, na.rm=TRUE)
day_250_h08v04_mnScaled<- day_250_h08v04_mn*0.0005
Out_day_250<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_250_h08v04_AVG_Scaled.img"
writeRaster (day_250_h08v04_mnScaled, filename=Out_day_250,format="HFA",overwrite=TRUE)
day_251_h08v04_stack<- stack(day_251_h08v04)
nlayers(day_251_h08v04_stack) 
day_251_h08v04_mn<- mean(day_251_h08v04_stack, na.rm=TRUE)
day_251_h08v04_mnScaled<- day_251_h08v04_mn*0.0005
Out_day_251<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_251_h08v04_AVG_Scaled.img"
writeRaster (day_251_h08v04_mnScaled, filename=Out_day_251,format="HFA",overwrite=TRUE)
day_252_h08v04_stack<- stack(day_252_h08v04)
nlayers(day_252_h08v04_stack) 
day_252_h08v04_mn<- mean(day_252_h08v04_stack, na.rm=TRUE)
day_252_h08v04_mnScaled<- day_252_h08v04_mn*0.0005
Out_day_252<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_252_h08v04_AVG_Scaled.img"
writeRaster (day_252_h08v04_mnScaled, filename=Out_day_252,format="HFA",overwrite=TRUE)
day_253_h08v04_stack<- stack(day_253_h08v04)
nlayers(day_253_h08v04_stack) 
day_253_h08v04_mn<- mean(day_253_h08v04_stack, na.rm=TRUE)
day_253_h08v04_mnScaled<- day_253_h08v04_mn*0.0005
Out_day_253<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_253_h08v04_AVG_Scaled.img"
writeRaster (day_253_h08v04_mnScaled, filename=Out_day_253,format="HFA",overwrite=TRUE)
day_254_h08v04_stack<- stack(day_254_h08v04)
nlayers(day_254_h08v04_stack) 
day_254_h08v04_mn<- mean(day_254_h08v04_stack, na.rm=TRUE)
day_254_h08v04_mnScaled<- day_254_h08v04_mn*0.0005
Out_day_254<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_254_h08v04_AVG_Scaled.img"
writeRaster (day_254_h08v04_mnScaled, filename=Out_day_254,format="HFA",overwrite=TRUE)
day_255_h08v04_stack<- stack(day_255_h08v04)
nlayers(day_255_h08v04_stack) 
day_255_h08v04_mn<- mean(day_255_h08v04_stack, na.rm=TRUE)
day_255_h08v04_mnScaled<- day_255_h08v04_mn*0.0005
Out_day_255<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_255_h08v04_AVG_Scaled.img"
writeRaster (day_255_h08v04_mnScaled, filename=Out_day_255,format="HFA",overwrite=TRUE)
day_256_h08v04_stack<- stack(day_256_h08v04)
nlayers(day_256_h08v04_stack) 
day_256_h08v04_mn<- mean(day_256_h08v04_stack, na.rm=TRUE)
day_256_h08v04_mnScaled<- day_256_h08v04_mn*0.0005
Out_day_256<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_256_h08v04_AVG_Scaled.img"
writeRaster (day_256_h08v04_mnScaled, filename=Out_day_256,format="HFA",overwrite=TRUE)
day_257_h08v04_stack<- stack(day_257_h08v04)
nlayers(day_257_h08v04_stack) 
day_257_h08v04_mn<- mean(day_257_h08v04_stack, na.rm=TRUE)
day_257_h08v04_mnScaled<- day_257_h08v04_mn*0.0005
Out_day_257<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_257_h08v04_AVG_Scaled.img"
writeRaster (day_257_h08v04_mnScaled, filename=Out_day_257,format="HFA",overwrite=TRUE)
day_258_h08v04_stack<- stack(day_258_h08v04)
nlayers(day_258_h08v04_stack) 
day_258_h08v04_mn<- mean(day_258_h08v04_stack, na.rm=TRUE)
day_258_h08v04_mnScaled<- day_258_h08v04_mn*0.0005
Out_day_258<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_258_h08v04_AVG_Scaled.img"
writeRaster (day_258_h08v04_mnScaled, filename=Out_day_258,format="HFA",overwrite=TRUE)
day_259_h08v04_stack<- stack(day_259_h08v04)
nlayers(day_259_h08v04_stack) 
day_259_h08v04_mn<- mean(day_259_h08v04_stack, na.rm=TRUE)
day_259_h08v04_mnScaled<- day_259_h08v04_mn*0.0005
Out_day_259<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_259_h08v04_AVG_Scaled.img"
writeRaster (day_259_h08v04_mnScaled, filename=Out_day_259,format="HFA",overwrite=TRUE)
day_260_h08v04_stack<- stack(day_260_h08v04)
nlayers(day_260_h08v04_stack) 
day_260_h08v04_mn<- mean(day_260_h08v04_stack, na.rm=TRUE)
day_260_h08v04_mnScaled<- day_260_h08v04_mn*0.0005
Out_day_260<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_260_h08v04_AVG_Scaled.img"
writeRaster (day_260_h08v04_mnScaled, filename=Out_day_260,format="HFA",overwrite=TRUE)
day_261_h08v04_stack<- stack(day_261_h08v04)
nlayers(day_261_h08v04_stack) 
day_261_h08v04_mn<- mean(day_261_h08v04_stack, na.rm=TRUE)
day_261_h08v04_mnScaled<- day_261_h08v04_mn*0.0005
Out_day_261<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_261_h08v04_AVG_Scaled.img"
writeRaster (day_261_h08v04_mnScaled, filename=Out_day_261,format="HFA",overwrite=TRUE)
day_262_h08v04_stack<- stack(day_262_h08v04)
nlayers(day_262_h08v04_stack) 
day_262_h08v04_mn<- mean(day_262_h08v04_stack, na.rm=TRUE)
day_262_h08v04_mnScaled<- day_262_h08v04_mn*0.0005
Out_day_262<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_262_h08v04_AVG_Scaled.img"
writeRaster (day_262_h08v04_mnScaled, filename=Out_day_262,format="HFA",overwrite=TRUE)
day_263_h08v04_stack<- stack(day_263_h08v04)
nlayers(day_263_h08v04_stack) 
day_263_h08v04_mn<- mean(day_263_h08v04_stack, na.rm=TRUE)
day_263_h08v04_mnScaled<- day_263_h08v04_mn*0.0005
Out_day_263<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_263_h08v04_AVG_Scaled.img"
writeRaster (day_263_h08v04_mnScaled, filename=Out_day_263,format="HFA",overwrite=TRUE)
day_264_h08v04_stack<- stack(day_264_h08v04)
nlayers(day_264_h08v04_stack) 
day_264_h08v04_mn<- mean(day_264_h08v04_stack, na.rm=TRUE)
day_264_h08v04_mnScaled<- day_264_h08v04_mn*0.0005
Out_day_264<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_264_h08v04_AVG_Scaled.img"
writeRaster (day_264_h08v04_mnScaled, filename=Out_day_264,format="HFA",overwrite=TRUE)
day_265_h08v04_stack<- stack(day_265_h08v04)
nlayers(day_265_h08v04_stack) 
day_265_h08v04_mn<- mean(day_265_h08v04_stack, na.rm=TRUE)
day_265_h08v04_mnScaled<- day_265_h08v04_mn*0.0005
Out_day_265<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_265_h08v04_AVG_Scaled.img"
writeRaster (day_265_h08v04_mnScaled, filename=Out_day_265,format="HFA",overwrite=TRUE)
day_266_h08v04_stack<- stack(day_266_h08v04)
nlayers(day_266_h08v04_stack) 
day_266_h08v04_mn<- mean(day_266_h08v04_stack, na.rm=TRUE)
day_266_h08v04_mnScaled<- day_266_h08v04_mn*0.0005
Out_day_266<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_266_h08v04_AVG_Scaled.img"
writeRaster (day_266_h08v04_mnScaled, filename=Out_day_266,format="HFA",overwrite=TRUE)
day_267_h08v04_stack<- stack(day_267_h08v04)
nlayers(day_267_h08v04_stack) 
day_267_h08v04_mn<- mean(day_267_h08v04_stack, na.rm=TRUE)
day_267_h08v04_mnScaled<- day_267_h08v04_mn*0.0005
Out_day_267<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_267_h08v04_AVG_Scaled.img"
writeRaster (day_267_h08v04_mnScaled, filename=Out_day_267,format="HFA",overwrite=TRUE)
day_268_h08v04_stack<- stack(day_268_h08v04)
nlayers(day_268_h08v04_stack) 
day_268_h08v04_mn<- mean(day_268_h08v04_stack, na.rm=TRUE)
day_268_h08v04_mnScaled<- day_268_h08v04_mn*0.0005
Out_day_268<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_268_h08v04_AVG_Scaled.img"
writeRaster (day_268_h08v04_mnScaled, filename=Out_day_268,format="HFA",overwrite=TRUE)
day_269_h08v04_stack<- stack(day_269_h08v04)
nlayers(day_269_h08v04_stack) 
day_269_h08v04_mn<- mean(day_269_h08v04_stack, na.rm=TRUE)
day_269_h08v04_mnScaled<- day_269_h08v04_mn*0.0005
Out_day_269<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_269_h08v04_AVG_Scaled.img"
writeRaster (day_269_h08v04_mnScaled, filename=Out_day_269,format="HFA",overwrite=TRUE)
day_270_h08v04_stack<- stack(day_270_h08v04)
nlayers(day_270_h08v04_stack) 
day_270_h08v04_mn<- mean(day_270_h08v04_stack, na.rm=TRUE)
day_270_h08v04_mnScaled<- day_270_h08v04_mn*0.0005
Out_day_270<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_270_h08v04_AVG_Scaled.img"
writeRaster (day_270_h08v04_mnScaled, filename=Out_day_270,format="HFA",overwrite=TRUE)
day_271_h08v04_stack<- stack(day_271_h08v04)
nlayers(day_271_h08v04_stack) 
day_271_h08v04_mn<- mean(day_271_h08v04_stack, na.rm=TRUE)
day_271_h08v04_mnScaled<- day_271_h08v04_mn*0.0005
Out_day_271<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_271_h08v04_AVG_Scaled.img"
writeRaster (day_271_h08v04_mnScaled, filename=Out_day_271,format="HFA",overwrite=TRUE)
day_272_h08v04_stack<- stack(day_272_h08v04)
nlayers(day_272_h08v04_stack) 
day_272_h08v04_mn<- mean(day_272_h08v04_stack, na.rm=TRUE)
day_272_h08v04_mnScaled<- day_272_h08v04_mn*0.0005
Out_day_272<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_272_h08v04_AVG_Scaled.img"
writeRaster (day_272_h08v04_mnScaled, filename=Out_day_272,format="HFA",overwrite=TRUE)
day_273_h08v04_stack<- stack(day_273_h08v04)
nlayers(day_273_h08v04_stack) 
day_273_h08v04_mn<- mean(day_273_h08v04_stack, na.rm=TRUE)
day_273_h08v04_mnScaled<- day_273_h08v04_mn*0.0005
Out_day_273<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_273_h08v04_AVG_Scaled.img"
writeRaster (day_273_h08v04_mnScaled, filename=Out_day_273,format="HFA",overwrite=TRUE)
day_274_h08v04_stack<- stack(day_274_h08v04)
nlayers(day_274_h08v04_stack) 
day_274_h08v04_mn<- mean(day_274_h08v04_stack, na.rm=TRUE)
day_274_h08v04_mnScaled<- day_274_h08v04_mn*0.0005
Out_day_274<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_274_h08v04_AVG_Scaled.img"
writeRaster (day_274_h08v04_mnScaled, filename=Out_day_274,format="HFA",overwrite=TRUE)
day_275_h08v04_stack<- stack(day_275_h08v04)
nlayers(day_275_h08v04_stack) 
day_275_h08v04_mn<- mean(day_275_h08v04_stack, na.rm=TRUE)
day_275_h08v04_mnScaled<- day_275_h08v04_mn*0.0005
Out_day_275<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_275_h08v04_AVG_Scaled.img"
writeRaster (day_275_h08v04_mnScaled, filename=Out_day_275,format="HFA",overwrite=TRUE)
day_276_h08v04_stack<- stack(day_276_h08v04)
nlayers(day_276_h08v04_stack) 
day_276_h08v04_mn<- mean(day_276_h08v04_stack, na.rm=TRUE)
day_276_h08v04_mnScaled<- day_276_h08v04_mn*0.0005
Out_day_276<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_276_h08v04_AVG_Scaled.img"
writeRaster (day_276_h08v04_mnScaled, filename=Out_day_276,format="HFA",overwrite=TRUE)
day_277_h08v04_stack<- stack(day_277_h08v04)
nlayers(day_277_h08v04_stack) 
day_277_h08v04_mn<- mean(day_277_h08v04_stack, na.rm=TRUE)
day_277_h08v04_mnScaled<- day_277_h08v04_mn*0.0005
Out_day_277<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_277_h08v04_AVG_Scaled.img"
writeRaster (day_277_h08v04_mnScaled, filename=Out_day_277,format="HFA",overwrite=TRUE)
day_278_h08v04_stack<- stack(day_278_h08v04)
nlayers(day_278_h08v04_stack) 
day_278_h08v04_mn<- mean(day_278_h08v04_stack, na.rm=TRUE)
day_278_h08v04_mnScaled<- day_278_h08v04_mn*0.0005
Out_day_278<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_278_h08v04_AVG_Scaled.img"
writeRaster (day_278_h08v04_mnScaled, filename=Out_day_278,format="HFA",overwrite=TRUE)
day_279_h08v04_stack<- stack(day_279_h08v04)
nlayers(day_279_h08v04_stack) 
day_279_h08v04_mn<- mean(day_279_h08v04_stack, na.rm=TRUE)
day_279_h08v04_mnScaled<- day_279_h08v04_mn*0.0005
Out_day_279<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_279_h08v04_AVG_Scaled.img"
writeRaster (day_279_h08v04_mnScaled, filename=Out_day_279,format="HFA",overwrite=TRUE)
day_280_h08v04_stack<- stack(day_280_h08v04)
nlayers(day_280_h08v04_stack) 
day_280_h08v04_mn<- mean(day_280_h08v04_stack, na.rm=TRUE)
day_280_h08v04_mnScaled<- day_280_h08v04_mn*0.0005
Out_day_280<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_280_h08v04_AVG_Scaled.img"
writeRaster (day_280_h08v04_mnScaled, filename=Out_day_280,format="HFA",overwrite=TRUE)
day_281_h08v04_stack<- stack(day_281_h08v04)
nlayers(day_281_h08v04_stack) 
day_281_h08v04_mn<- mean(day_281_h08v04_stack, na.rm=TRUE)
day_281_h08v04_mnScaled<- day_281_h08v04_mn*0.0005
Out_day_281<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_281_h08v04_AVG_Scaled.img"
writeRaster (day_281_h08v04_mnScaled, filename=Out_day_281,format="HFA",overwrite=TRUE)
day_282_h08v04_stack<- stack(day_282_h08v04)
nlayers(day_282_h08v04_stack) 
day_282_h08v04_mn<- mean(day_282_h08v04_stack, na.rm=TRUE)
day_282_h08v04_mnScaled<- day_282_h08v04_mn*0.0005
Out_day_282<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_282_h08v04_AVG_Scaled.img"
writeRaster (day_282_h08v04_mnScaled, filename=Out_day_282,format="HFA",overwrite=TRUE)
day_283_h08v04_stack<- stack(day_283_h08v04)
nlayers(day_283_h08v04_stack) 
day_283_h08v04_mn<- mean(day_283_h08v04_stack, na.rm=TRUE)
day_283_h08v04_mnScaled<- day_283_h08v04_mn*0.0005
Out_day_283<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_283_h08v04_AVG_Scaled.img"
writeRaster (day_283_h08v04_mnScaled, filename=Out_day_283,format="HFA",overwrite=TRUE)
day_284_h08v04_stack<- stack(day_284_h08v04)
nlayers(day_284_h08v04_stack) 
day_284_h08v04_mn<- mean(day_284_h08v04_stack, na.rm=TRUE)
day_284_h08v04_mnScaled<- day_284_h08v04_mn*0.0005
Out_day_284<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_284_h08v04_AVG_Scaled.img"
writeRaster (day_284_h08v04_mnScaled, filename=Out_day_284,format="HFA",overwrite=TRUE)
day_285_h08v04_stack<- stack(day_285_h08v04)
nlayers(day_285_h08v04_stack) 
day_285_h08v04_mn<- mean(day_285_h08v04_stack, na.rm=TRUE)
day_285_h08v04_mnScaled<- day_285_h08v04_mn*0.0005
Out_day_285<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_285_h08v04_AVG_Scaled.img"
writeRaster (day_285_h08v04_mnScaled, filename=Out_day_285,format="HFA",overwrite=TRUE)
day_286_h08v04_stack<- stack(day_286_h08v04)
nlayers(day_286_h08v04_stack) 
day_286_h08v04_mn<- mean(day_286_h08v04_stack, na.rm=TRUE)
day_286_h08v04_mnScaled<- day_286_h08v04_mn*0.0005
Out_day_286<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_286_h08v04_AVG_Scaled.img"
writeRaster (day_286_h08v04_mnScaled, filename=Out_day_286,format="HFA",overwrite=TRUE)
day_287_h08v04_stack<- stack(day_287_h08v04)
nlayers(day_287_h08v04_stack) 
day_287_h08v04_mn<- mean(day_287_h08v04_stack, na.rm=TRUE)
day_287_h08v04_mnScaled<- day_287_h08v04_mn*0.0005
Out_day_287<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_287_h08v04_AVG_Scaled.img"
writeRaster (day_287_h08v04_mnScaled, filename=Out_day_287,format="HFA",overwrite=TRUE)
day_288_h08v04_stack<- stack(day_288_h08v04)
nlayers(day_288_h08v04_stack) 
day_288_h08v04_mn<- mean(day_288_h08v04_stack, na.rm=TRUE)
day_288_h08v04_mnScaled<- day_288_h08v04_mn*0.0005
Out_day_288<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_288_h08v04_AVG_Scaled.img"
writeRaster (day_288_h08v04_mnScaled, filename=Out_day_288,format="HFA",overwrite=TRUE)
day_289_h08v04_stack<- stack(day_289_h08v04)
nlayers(day_289_h08v04_stack) 
day_289_h08v04_mn<- mean(day_289_h08v04_stack, na.rm=TRUE)
day_289_h08v04_mnScaled<- day_289_h08v04_mn*0.0005
Out_day_289<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_289_h08v04_AVG_Scaled.img"
writeRaster (day_289_h08v04_mnScaled, filename=Out_day_289,format="HFA",overwrite=TRUE)
day_290_h08v04_stack<- stack(day_290_h08v04)
nlayers(day_290_h08v04_stack) 
day_290_h08v04_mn<- mean(day_290_h08v04_stack, na.rm=TRUE)
day_290_h08v04_mnScaled<- day_290_h08v04_mn*0.0005
Out_day_290<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_290_h08v04_AVG_Scaled.img"
writeRaster (day_290_h08v04_mnScaled, filename=Out_day_290,format="HFA",overwrite=TRUE)
day_291_h08v04_stack<- stack(day_291_h08v04)
nlayers(day_291_h08v04_stack) 
day_291_h08v04_mn<- mean(day_291_h08v04_stack, na.rm=TRUE)
day_291_h08v04_mnScaled<- day_291_h08v04_mn*0.0005
Out_day_291<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_291_h08v04_AVG_Scaled.img"
writeRaster (day_291_h08v04_mnScaled, filename=Out_day_291,format="HFA",overwrite=TRUE)
day_292_h08v04_stack<- stack(day_292_h08v04)
nlayers(day_292_h08v04_stack) 
day_292_h08v04_mn<- mean(day_292_h08v04_stack, na.rm=TRUE)
day_292_h08v04_mnScaled<- day_292_h08v04_mn*0.0005
Out_day_292<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_292_h08v04_AVG_Scaled.img"
writeRaster (day_292_h08v04_mnScaled, filename=Out_day_292,format="HFA",overwrite=TRUE)
day_293_h08v04_stack<- stack(day_293_h08v04)
nlayers(day_293_h08v04_stack) 
day_293_h08v04_mn<- mean(day_293_h08v04_stack, na.rm=TRUE)
day_293_h08v04_mnScaled<- day_293_h08v04_mn*0.0005
Out_day_293<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_293_h08v04_AVG_Scaled.img"
writeRaster (day_293_h08v04_mnScaled, filename=Out_day_293,format="HFA",overwrite=TRUE)
day_294_h08v04_stack<- stack(day_294_h08v04)
nlayers(day_294_h08v04_stack) 
day_294_h08v04_mn<- mean(day_294_h08v04_stack, na.rm=TRUE)
day_294_h08v04_mnScaled<- day_294_h08v04_mn*0.0005
Out_day_294<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_294_h08v04_AVG_Scaled.img"
writeRaster (day_294_h08v04_mnScaled, filename=Out_day_294,format="HFA",overwrite=TRUE)
day_295_h08v04_stack<- stack(day_295_h08v04)
nlayers(day_295_h08v04_stack) 
day_295_h08v04_mn<- mean(day_295_h08v04_stack, na.rm=TRUE)
day_295_h08v04_mnScaled<- day_295_h08v04_mn*0.0005
Out_day_295<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_295_h08v04_AVG_Scaled.img"
writeRaster (day_295_h08v04_mnScaled, filename=Out_day_295,format="HFA",overwrite=TRUE)
day_296_h08v04_stack<- stack(day_296_h08v04)
nlayers(day_296_h08v04_stack) 
day_296_h08v04_mn<- mean(day_296_h08v04_stack, na.rm=TRUE)
day_296_h08v04_mnScaled<- day_296_h08v04_mn*0.0005
Out_day_296<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_296_h08v04_AVG_Scaled.img"
writeRaster (day_296_h08v04_mnScaled, filename=Out_day_296,format="HFA",overwrite=TRUE)
day_297_h08v04_stack<- stack(day_297_h08v04)
nlayers(day_297_h08v04_stack) 
day_297_h08v04_mn<- mean(day_297_h08v04_stack, na.rm=TRUE)
day_297_h08v04_mnScaled<- day_297_h08v04_mn*0.0005
Out_day_297<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_297_h08v04_AVG_Scaled.img"
writeRaster (day_297_h08v04_mnScaled, filename=Out_day_297,format="HFA",overwrite=TRUE)
day_298_h08v04_stack<- stack(day_298_h08v04)
nlayers(day_298_h08v04_stack) 
day_298_h08v04_mn<- mean(day_298_h08v04_stack, na.rm=TRUE)
day_298_h08v04_mnScaled<- day_298_h08v04_mn*0.0005
Out_day_298<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_298_h08v04_AVG_Scaled.img"
writeRaster (day_298_h08v04_mnScaled, filename=Out_day_298,format="HFA",overwrite=TRUE)
day_299_h08v04_stack<- stack(day_299_h08v04)
nlayers(day_299_h08v04_stack) 
day_299_h08v04_mn<- mean(day_299_h08v04_stack, na.rm=TRUE)
day_299_h08v04_mnScaled<- day_299_h08v04_mn*0.0005
Out_day_299<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_299_h08v04_AVG_Scaled.img"
writeRaster (day_299_h08v04_mnScaled, filename=Out_day_299,format="HFA",overwrite=TRUE)
day_300_h08v04_stack<- stack(day_300_h08v04)
nlayers(day_300_h08v04_stack) 
day_300_h08v04_mn<- mean(day_300_h08v04_stack, na.rm=TRUE)
day_300_h08v04_mnScaled<- day_300_h08v04_mn*0.0005
Out_day_300<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_300_h08v04_AVG_Scaled.img"
writeRaster (day_300_h08v04_mnScaled, filename=Out_day_300,format="HFA",overwrite=TRUE)
day_301_h08v04_stack<- stack(day_301_h08v04)
nlayers(day_301_h08v04_stack) 
day_301_h08v04_mn<- mean(day_301_h08v04_stack, na.rm=TRUE)
day_301_h08v04_mnScaled<- day_301_h08v04_mn*0.0005
Out_day_301<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_301_h08v04_AVG_Scaled.img"
writeRaster (day_301_h08v04_mnScaled, filename=Out_day_301,format="HFA",overwrite=TRUE)
day_302_h08v04_stack<- stack(day_302_h08v04)
nlayers(day_302_h08v04_stack) 
day_302_h08v04_mn<- mean(day_302_h08v04_stack, na.rm=TRUE)
day_302_h08v04_mnScaled<- day_302_h08v04_mn*0.0005
Out_day_302<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_302_h08v04_AVG_Scaled.img"
writeRaster (day_302_h08v04_mnScaled, filename=Out_day_302,format="HFA",overwrite=TRUE)
day_303_h08v04_stack<- stack(day_303_h08v04)
nlayers(day_303_h08v04_stack) 
day_303_h08v04_mn<- mean(day_303_h08v04_stack, na.rm=TRUE)
day_303_h08v04_mnScaled<- day_303_h08v04_mn*0.0005
Out_day_303<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_303_h08v04_AVG_Scaled.img"
writeRaster (day_303_h08v04_mnScaled, filename=Out_day_303,format="HFA",overwrite=TRUE)
day_304_h08v04_stack<- stack(day_304_h08v04)
nlayers(day_304_h08v04_stack) 
day_304_h08v04_mn<- mean(day_304_h08v04_stack, na.rm=TRUE)
day_304_h08v04_mnScaled<- day_304_h08v04_mn*0.0005
Out_day_304<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_304_h08v04_AVG_Scaled.img"
writeRaster (day_304_h08v04_mnScaled, filename=Out_day_304,format="HFA",overwrite=TRUE)
day_305_h08v04_stack<- stack(day_305_h08v04)
nlayers(day_305_h08v04_stack) 
day_305_h08v04_mn<- mean(day_305_h08v04_stack, na.rm=TRUE)
day_305_h08v04_mnScaled<- day_305_h08v04_mn*0.0005
Out_day_305<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_305_h08v04_AVG_Scaled.img"
writeRaster (day_305_h08v04_mnScaled, filename=Out_day_305,format="HFA",overwrite=TRUE)
day_306_h08v04_stack<- stack(day_306_h08v04)
nlayers(day_306_h08v04_stack) 
day_306_h08v04_mn<- mean(day_306_h08v04_stack, na.rm=TRUE)
day_306_h08v04_mnScaled<- day_306_h08v04_mn*0.0005
Out_day_306<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_306_h08v04_AVG_Scaled.img"
writeRaster (day_306_h08v04_mnScaled, filename=Out_day_306,format="HFA",overwrite=TRUE)
day_307_h08v04_stack<- stack(day_307_h08v04)
nlayers(day_307_h08v04_stack) 
day_307_h08v04_mn<- mean(day_307_h08v04_stack, na.rm=TRUE)
day_307_h08v04_mnScaled<- day_307_h08v04_mn*0.0005
Out_day_307<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_307_h08v04_AVG_Scaled.img"
writeRaster (day_307_h08v04_mnScaled, filename=Out_day_307,format="HFA",overwrite=TRUE)
day_308_h08v04_stack<- stack(day_308_h08v04)
nlayers(day_308_h08v04_stack) 
day_308_h08v04_mn<- mean(day_308_h08v04_stack, na.rm=TRUE)
day_308_h08v04_mnScaled<- day_308_h08v04_mn*0.0005
Out_day_308<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_308_h08v04_AVG_Scaled.img"
writeRaster (day_308_h08v04_mnScaled, filename=Out_day_308,format="HFA",overwrite=TRUE)
day_309_h08v04_stack<- stack(day_309_h08v04)
nlayers(day_309_h08v04_stack) 
day_309_h08v04_mn<- mean(day_309_h08v04_stack, na.rm=TRUE)
day_309_h08v04_mnScaled<- day_309_h08v04_mn*0.0005
Out_day_309<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_309_h08v04_AVG_Scaled.img"
writeRaster (day_309_h08v04_mnScaled, filename=Out_day_309,format="HFA",overwrite=TRUE)
day_310_h08v04_stack<- stack(day_310_h08v04)
nlayers(day_310_h08v04_stack) 
day_310_h08v04_mn<- mean(day_310_h08v04_stack, na.rm=TRUE)
day_310_h08v04_mnScaled<- day_310_h08v04_mn*0.0005
Out_day_310<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_310_h08v04_AVG_Scaled.img"
writeRaster (day_310_h08v04_mnScaled, filename=Out_day_310,format="HFA",overwrite=TRUE)
day_311_h08v04_stack<- stack(day_311_h08v04)
nlayers(day_311_h08v04_stack) 
day_311_h08v04_mn<- mean(day_311_h08v04_stack, na.rm=TRUE)
day_311_h08v04_mnScaled<- day_311_h08v04_mn*0.0005
Out_day_311<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_311_h08v04_AVG_Scaled.img"
writeRaster (day_311_h08v04_mnScaled, filename=Out_day_311,format="HFA",overwrite=TRUE)
day_312_h08v04_stack<- stack(day_312_h08v04)
nlayers(day_312_h08v04_stack) 
day_312_h08v04_mn<- mean(day_312_h08v04_stack, na.rm=TRUE)
day_312_h08v04_mnScaled<- day_312_h08v04_mn*0.0005
Out_day_312<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_312_h08v04_AVG_Scaled.img"
writeRaster (day_312_h08v04_mnScaled, filename=Out_day_312,format="HFA",overwrite=TRUE)
day_313_h08v04_stack<- stack(day_313_h08v04)
nlayers(day_313_h08v04_stack) 
day_313_h08v04_mn<- mean(day_313_h08v04_stack, na.rm=TRUE)
day_313_h08v04_mnScaled<- day_313_h08v04_mn*0.0005
Out_day_313<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_313_h08v04_AVG_Scaled.img"
writeRaster (day_313_h08v04_mnScaled, filename=Out_day_313,format="HFA",overwrite=TRUE)
day_314_h08v04_stack<- stack(day_314_h08v04)
nlayers(day_314_h08v04_stack) 
day_314_h08v04_mn<- mean(day_314_h08v04_stack, na.rm=TRUE)
day_314_h08v04_mnScaled<- day_314_h08v04_mn*0.0005
Out_day_314<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_314_h08v04_AVG_Scaled.img"
writeRaster (day_314_h08v04_mnScaled, filename=Out_day_314,format="HFA",overwrite=TRUE)
day_315_h08v04_stack<- stack(day_315_h08v04)
nlayers(day_315_h08v04_stack) 
day_315_h08v04_mn<- mean(day_315_h08v04_stack, na.rm=TRUE)
day_315_h08v04_mnScaled<- day_315_h08v04_mn*0.0005
Out_day_315<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_315_h08v04_AVG_Scaled.img"
writeRaster (day_315_h08v04_mnScaled, filename=Out_day_315,format="HFA",overwrite=TRUE)
day_316_h08v04_stack<- stack(day_316_h08v04)
nlayers(day_316_h08v04_stack) 
day_316_h08v04_mn<- mean(day_316_h08v04_stack, na.rm=TRUE)
day_316_h08v04_mnScaled<- day_316_h08v04_mn*0.0005
Out_day_316<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_316_h08v04_AVG_Scaled.img"
writeRaster (day_316_h08v04_mnScaled, filename=Out_day_316,format="HFA",overwrite=TRUE)
day_317_h08v04_stack<- stack(day_317_h08v04)
nlayers(day_317_h08v04_stack) 
day_317_h08v04_mn<- mean(day_317_h08v04_stack, na.rm=TRUE)
day_317_h08v04_mnScaled<- day_317_h08v04_mn*0.0005
Out_day_317<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_317_h08v04_AVG_Scaled.img"
writeRaster (day_317_h08v04_mnScaled, filename=Out_day_317,format="HFA",overwrite=TRUE)
day_318_h08v04_stack<- stack(day_318_h08v04)
nlayers(day_318_h08v04_stack) 
day_318_h08v04_mn<- mean(day_318_h08v04_stack, na.rm=TRUE)
day_318_h08v04_mnScaled<- day_318_h08v04_mn*0.0005
Out_day_318<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_318_h08v04_AVG_Scaled.img"
writeRaster (day_318_h08v04_mnScaled, filename=Out_day_318,format="HFA",overwrite=TRUE)
day_319_h08v04_stack<- stack(day_319_h08v04)
nlayers(day_319_h08v04_stack) 
day_319_h08v04_mn<- mean(day_319_h08v04_stack, na.rm=TRUE)
day_319_h08v04_mnScaled<- day_319_h08v04_mn*0.0005
Out_day_319<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_319_h08v04_AVG_Scaled.img"
writeRaster (day_319_h08v04_mnScaled, filename=Out_day_319,format="HFA",overwrite=TRUE)
day_320_h08v04_stack<- stack(day_320_h08v04)
nlayers(day_320_h08v04_stack) 
day_320_h08v04_mn<- mean(day_320_h08v04_stack, na.rm=TRUE)
day_320_h08v04_mnScaled<- day_320_h08v04_mn*0.0005
Out_day_320<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_320_h08v04_AVG_Scaled.img"
writeRaster (day_320_h08v04_mnScaled, filename=Out_day_320,format="HFA",overwrite=TRUE)
day_321_h08v04_stack<- stack(day_321_h08v04)
nlayers(day_321_h08v04_stack) 
day_321_h08v04_mn<- mean(day_321_h08v04_stack, na.rm=TRUE)
day_321_h08v04_mnScaled<- day_321_h08v04_mn*0.0005
Out_day_321<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_321_h08v04_AVG_Scaled.img"
writeRaster (day_321_h08v04_mnScaled, filename=Out_day_321,format="HFA",overwrite=TRUE)
day_322_h08v04_stack<- stack(day_322_h08v04)
nlayers(day_322_h08v04_stack) 
day_322_h08v04_mn<- mean(day_322_h08v04_stack, na.rm=TRUE)
day_322_h08v04_mnScaled<- day_322_h08v04_mn*0.0005
Out_day_322<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_322_h08v04_AVG_Scaled.img"
writeRaster (day_322_h08v04_mnScaled, filename=Out_day_322,format="HFA",overwrite=TRUE)
day_323_h08v04_stack<- stack(day_323_h08v04)
nlayers(day_323_h08v04_stack) 
day_323_h08v04_mn<- mean(day_323_h08v04_stack, na.rm=TRUE)
day_323_h08v04_mnScaled<- day_323_h08v04_mn*0.0005
Out_day_323<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_323_h08v04_AVG_Scaled.img"
writeRaster (day_323_h08v04_mnScaled, filename=Out_day_323,format="HFA",overwrite=TRUE)
day_324_h08v04_stack<- stack(day_324_h08v04)
nlayers(day_324_h08v04_stack) 
day_324_h08v04_mn<- mean(day_324_h08v04_stack, na.rm=TRUE)
day_324_h08v04_mnScaled<- day_324_h08v04_mn*0.0005
Out_day_324<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_324_h08v04_AVG_Scaled.img"
writeRaster (day_324_h08v04_mnScaled, filename=Out_day_324,format="HFA",overwrite=TRUE)
day_325_h08v04_stack<- stack(day_325_h08v04)
nlayers(day_325_h08v04_stack) 
day_325_h08v04_mn<- mean(day_325_h08v04_stack, na.rm=TRUE)
day_325_h08v04_mnScaled<- day_325_h08v04_mn*0.0005
Out_day_325<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_325_h08v04_AVG_Scaled.img"
writeRaster (day_325_h08v04_mnScaled, filename=Out_day_325,format="HFA",overwrite=TRUE)
day_326_h08v04_stack<- stack(day_326_h08v04)
nlayers(day_326_h08v04_stack) 
day_326_h08v04_mn<- mean(day_326_h08v04_stack, na.rm=TRUE)
day_326_h08v04_mnScaled<- day_326_h08v04_mn*0.0005
Out_day_326<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_326_h08v04_AVG_Scaled.img"
writeRaster (day_326_h08v04_mnScaled, filename=Out_day_326,format="HFA",overwrite=TRUE)
day_327_h08v04_stack<- stack(day_327_h08v04)
nlayers(day_327_h08v04_stack) 
day_327_h08v04_mn<- mean(day_327_h08v04_stack, na.rm=TRUE)
day_327_h08v04_mnScaled<- day_327_h08v04_mn*0.0005
Out_day_327<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_327_h08v04_AVG_Scaled.img"
writeRaster (day_327_h08v04_mnScaled, filename=Out_day_327,format="HFA",overwrite=TRUE)
day_328_h08v04_stack<- stack(day_328_h08v04)
nlayers(day_328_h08v04_stack) 
day_328_h08v04_mn<- mean(day_328_h08v04_stack, na.rm=TRUE)
day_328_h08v04_mnScaled<- day_328_h08v04_mn*0.0005
Out_day_328<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_328_h08v04_AVG_Scaled.img"
writeRaster (day_328_h08v04_mnScaled, filename=Out_day_328,format="HFA",overwrite=TRUE)
day_329_h08v04_stack<- stack(day_329_h08v04)
nlayers(day_329_h08v04_stack) 
day_329_h08v04_mn<- mean(day_329_h08v04_stack, na.rm=TRUE)
day_329_h08v04_mnScaled<- day_329_h08v04_mn*0.0005
Out_day_329<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_329_h08v04_AVG_Scaled.img"
writeRaster (day_329_h08v04_mnScaled, filename=Out_day_329,format="HFA",overwrite=TRUE)
day_330_h08v04_stack<- stack(day_330_h08v04)
nlayers(day_330_h08v04_stack) 
day_330_h08v04_mn<- mean(day_330_h08v04_stack, na.rm=TRUE)
day_330_h08v04_mnScaled<- day_330_h08v04_mn*0.0005
Out_day_330<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_330_h08v04_AVG_Scaled.img"
writeRaster (day_330_h08v04_mnScaled, filename=Out_day_330,format="HFA",overwrite=TRUE)
day_331_h08v04_stack<- stack(day_331_h08v04)
nlayers(day_331_h08v04_stack) 
day_331_h08v04_mn<- mean(day_331_h08v04_stack, na.rm=TRUE)
day_331_h08v04_mnScaled<- day_331_h08v04_mn*0.0005
Out_day_331<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_331_h08v04_AVG_Scaled.img"
writeRaster (day_331_h08v04_mnScaled, filename=Out_day_331,format="HFA",overwrite=TRUE)
day_332_h08v04_stack<- stack(day_332_h08v04)
nlayers(day_332_h08v04_stack) 
day_332_h08v04_mn<- mean(day_332_h08v04_stack, na.rm=TRUE)
day_332_h08v04_mnScaled<- day_332_h08v04_mn*0.0005
Out_day_332<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_332_h08v04_AVG_Scaled.img"
writeRaster (day_332_h08v04_mnScaled, filename=Out_day_332,format="HFA",overwrite=TRUE)
day_333_h08v04_stack<- stack(day_333_h08v04)
nlayers(day_333_h08v04_stack) 
day_333_h08v04_mn<- mean(day_333_h08v04_stack, na.rm=TRUE)
day_333_h08v04_mnScaled<- day_333_h08v04_mn*0.0005
Out_day_333<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_333_h08v04_AVG_Scaled.img"
writeRaster (day_333_h08v04_mnScaled, filename=Out_day_333,format="HFA",overwrite=TRUE)
day_334_h08v04_stack<- stack(day_334_h08v04)
nlayers(day_334_h08v04_stack) 
day_334_h08v04_mn<- mean(day_334_h08v04_stack, na.rm=TRUE)
day_334_h08v04_mnScaled<- day_334_h08v04_mn*0.0005
Out_day_334<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_334_h08v04_AVG_Scaled.img"
writeRaster (day_334_h08v04_mnScaled, filename=Out_day_334,format="HFA",overwrite=TRUE)
day_335_h08v04_stack<- stack(day_335_h08v04)
nlayers(day_335_h08v04_stack) 
day_335_h08v04_mn<- mean(day_335_h08v04_stack, na.rm=TRUE)
day_335_h08v04_mnScaled<- day_335_h08v04_mn*0.0005
Out_day_335<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_335_h08v04_AVG_Scaled.img"
writeRaster (day_335_h08v04_mnScaled, filename=Out_day_335,format="HFA",overwrite=TRUE)
day_336_h08v04_stack<- stack(day_336_h08v04)
nlayers(day_336_h08v04_stack) 
day_336_h08v04_mn<- mean(day_336_h08v04_stack, na.rm=TRUE)
day_336_h08v04_mnScaled<- day_336_h08v04_mn*0.0005
Out_day_336<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_336_h08v04_AVG_Scaled.img"
writeRaster (day_336_h08v04_mnScaled, filename=Out_day_336,format="HFA",overwrite=TRUE)
day_337_h08v04_stack<- stack(day_337_h08v04)
nlayers(day_337_h08v04_stack) 
day_337_h08v04_mn<- mean(day_337_h08v04_stack, na.rm=TRUE)
day_337_h08v04_mnScaled<- day_337_h08v04_mn*0.0005
Out_day_337<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_337_h08v04_AVG_Scaled.img"
writeRaster (day_337_h08v04_mnScaled, filename=Out_day_337,format="HFA",overwrite=TRUE)
day_338_h08v04_stack<- stack(day_338_h08v04)
nlayers(day_338_h08v04_stack) 
day_338_h08v04_mn<- mean(day_338_h08v04_stack, na.rm=TRUE)
day_338_h08v04_mnScaled<- day_338_h08v04_mn*0.0005
Out_day_338<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_338_h08v04_AVG_Scaled.img"
writeRaster (day_338_h08v04_mnScaled, filename=Out_day_338,format="HFA",overwrite=TRUE)
day_339_h08v04_stack<- stack(day_339_h08v04)
nlayers(day_339_h08v04_stack) 
day_339_h08v04_mn<- mean(day_339_h08v04_stack, na.rm=TRUE)
day_339_h08v04_mnScaled<- day_339_h08v04_mn*0.0005
Out_day_339<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_339_h08v04_AVG_Scaled.img"
writeRaster (day_339_h08v04_mnScaled, filename=Out_day_339,format="HFA",overwrite=TRUE)
day_340_h08v04_stack<- stack(day_340_h08v04)
nlayers(day_340_h08v04_stack) 
day_340_h08v04_mn<- mean(day_340_h08v04_stack, na.rm=TRUE)
day_340_h08v04_mnScaled<- day_340_h08v04_mn*0.0005
Out_day_340<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_340_h08v04_AVG_Scaled.img"
writeRaster (day_340_h08v04_mnScaled, filename=Out_day_340,format="HFA",overwrite=TRUE)
day_341_h08v04_stack<- stack(day_341_h08v04)
nlayers(day_341_h08v04_stack) 
day_341_h08v04_mn<- mean(day_341_h08v04_stack, na.rm=TRUE)
day_341_h08v04_mnScaled<- day_341_h08v04_mn*0.0005
Out_day_341<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_341_h08v04_AVG_Scaled.img"
writeRaster (day_341_h08v04_mnScaled, filename=Out_day_341,format="HFA",overwrite=TRUE)
day_342_h08v04_stack<- stack(day_342_h08v04)
nlayers(day_342_h08v04_stack) 
day_342_h08v04_mn<- mean(day_342_h08v04_stack, na.rm=TRUE)
day_342_h08v04_mnScaled<- day_342_h08v04_mn*0.0005
Out_day_342<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_342_h08v04_AVG_Scaled.img"
writeRaster (day_342_h08v04_mnScaled, filename=Out_day_342,format="HFA",overwrite=TRUE)
day_343_h08v04_stack<- stack(day_343_h08v04)
nlayers(day_343_h08v04_stack) 
day_343_h08v04_mn<- mean(day_343_h08v04_stack, na.rm=TRUE)
day_343_h08v04_mnScaled<- day_343_h08v04_mn*0.0005
Out_day_343<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_343_h08v04_AVG_Scaled.img"
writeRaster (day_343_h08v04_mnScaled, filename=Out_day_343,format="HFA",overwrite=TRUE)
day_344_h08v04_stack<- stack(day_344_h08v04)
nlayers(day_344_h08v04_stack) 
day_344_h08v04_mn<- mean(day_344_h08v04_stack, na.rm=TRUE)
day_344_h08v04_mnScaled<- day_344_h08v04_mn*0.0005
Out_day_344<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_344_h08v04_AVG_Scaled.img"
writeRaster (day_344_h08v04_mnScaled, filename=Out_day_344,format="HFA",overwrite=TRUE)
day_345_h08v04_stack<- stack(day_345_h08v04)
nlayers(day_345_h08v04_stack) 
day_345_h08v04_mn<- mean(day_345_h08v04_stack, na.rm=TRUE)
day_345_h08v04_mnScaled<- day_345_h08v04_mn*0.0005
Out_day_345<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_345_h08v04_AVG_Scaled.img"
writeRaster (day_345_h08v04_mnScaled, filename=Out_day_345,format="HFA",overwrite=TRUE)
day_346_h08v04_stack<- stack(day_346_h08v04)
nlayers(day_346_h08v04_stack) 
day_346_h08v04_mn<- mean(day_346_h08v04_stack, na.rm=TRUE)
day_346_h08v04_mnScaled<- day_346_h08v04_mn*0.0005
Out_day_346<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_346_h08v04_AVG_Scaled.img"
writeRaster (day_346_h08v04_mnScaled, filename=Out_day_346,format="HFA",overwrite=TRUE)
day_347_h08v04_stack<- stack(day_347_h08v04)
nlayers(day_347_h08v04_stack) 
day_347_h08v04_mn<- mean(day_347_h08v04_stack, na.rm=TRUE)
day_347_h08v04_mnScaled<- day_347_h08v04_mn*0.0005
Out_day_347<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_347_h08v04_AVG_Scaled.img"
writeRaster (day_347_h08v04_mnScaled, filename=Out_day_347,format="HFA",overwrite=TRUE)
day_348_h08v04_stack<- stack(day_348_h08v04)
nlayers(day_348_h08v04_stack) 
day_348_h08v04_mn<- mean(day_348_h08v04_stack, na.rm=TRUE)
day_348_h08v04_mnScaled<- day_348_h08v04_mn*0.0005
Out_day_348<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_348_h08v04_AVG_Scaled.img"
writeRaster (day_348_h08v04_mnScaled, filename=Out_day_348,format="HFA",overwrite=TRUE)
day_349_h08v04_stack<- stack(day_349_h08v04)
nlayers(day_349_h08v04_stack) 
day_349_h08v04_mn<- mean(day_349_h08v04_stack, na.rm=TRUE)
day_349_h08v04_mnScaled<- day_349_h08v04_mn*0.0005
Out_day_349<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_349_h08v04_AVG_Scaled.img"
writeRaster (day_349_h08v04_mnScaled, filename=Out_day_349,format="HFA",overwrite=TRUE)
day_350_h08v04_stack<- stack(day_350_h08v04)
nlayers(day_350_h08v04_stack) 
day_350_h08v04_mn<- mean(day_350_h08v04_stack, na.rm=TRUE)
day_350_h08v04_mnScaled<- day_350_h08v04_mn*0.0005
Out_day_350<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_350_h08v04_AVG_Scaled.img"
writeRaster (day_350_h08v04_mnScaled, filename=Out_day_350,format="HFA",overwrite=TRUE)
day_351_h08v04_stack<- stack(day_351_h08v04)
nlayers(day_351_h08v04_stack) 
day_351_h08v04_mn<- mean(day_351_h08v04_stack, na.rm=TRUE)
day_351_h08v04_mnScaled<- day_351_h08v04_mn*0.0005
Out_day_351<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_351_h08v04_AVG_Scaled.img"
writeRaster (day_351_h08v04_mnScaled, filename=Out_day_351,format="HFA",overwrite=TRUE)
day_352_h08v04_stack<- stack(day_352_h08v04)
nlayers(day_352_h08v04_stack) 
day_352_h08v04_mn<- mean(day_352_h08v04_stack, na.rm=TRUE)
day_352_h08v04_mnScaled<- day_352_h08v04_mn*0.0005
Out_day_352<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_352_h08v04_AVG_Scaled.img"
writeRaster (day_352_h08v04_mnScaled, filename=Out_day_352,format="HFA",overwrite=TRUE)
day_353_h08v04_stack<- stack(day_353_h08v04)
nlayers(day_353_h08v04_stack) 
day_353_h08v04_mn<- mean(day_353_h08v04_stack, na.rm=TRUE)
day_353_h08v04_mnScaled<- day_353_h08v04_mn*0.0005
Out_day_353<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_353_h08v04_AVG_Scaled.img"
writeRaster (day_353_h08v04_mnScaled, filename=Out_day_353,format="HFA",overwrite=TRUE)
day_354_h08v04_stack<- stack(day_354_h08v04)
nlayers(day_354_h08v04_stack) 
day_354_h08v04_mn<- mean(day_354_h08v04_stack, na.rm=TRUE)
day_354_h08v04_mnScaled<- day_354_h08v04_mn*0.0005
Out_day_354<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_354_h08v04_AVG_Scaled.img"
writeRaster (day_354_h08v04_mnScaled, filename=Out_day_354,format="HFA",overwrite=TRUE)
day_355_h08v04_stack<- stack(day_355_h08v04)
nlayers(day_355_h08v04_stack) 
day_355_h08v04_mn<- mean(day_355_h08v04_stack, na.rm=TRUE)
day_355_h08v04_mnScaled<- day_355_h08v04_mn*0.0005
Out_day_355<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_355_h08v04_AVG_Scaled.img"
writeRaster (day_355_h08v04_mnScaled, filename=Out_day_355,format="HFA",overwrite=TRUE)
day_356_h08v04_stack<- stack(day_356_h08v04)
nlayers(day_356_h08v04_stack) 
day_356_h08v04_mn<- mean(day_356_h08v04_stack, na.rm=TRUE)
day_356_h08v04_mnScaled<- day_356_h08v04_mn*0.0005
Out_day_356<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_356_h08v04_AVG_Scaled.img"
writeRaster (day_356_h08v04_mnScaled, filename=Out_day_356,format="HFA",overwrite=TRUE)
day_357_h08v04_stack<- stack(day_357_h08v04)
nlayers(day_357_h08v04_stack) 
day_357_h08v04_mn<- mean(day_357_h08v04_stack, na.rm=TRUE)
day_357_h08v04_mnScaled<- day_357_h08v04_mn*0.0005
Out_day_357<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_357_h08v04_AVG_Scaled.img"
writeRaster (day_357_h08v04_mnScaled, filename=Out_day_357,format="HFA",overwrite=TRUE)
day_358_h08v04_stack<- stack(day_358_h08v04)
nlayers(day_358_h08v04_stack) 
day_358_h08v04_mn<- mean(day_358_h08v04_stack, na.rm=TRUE)
day_358_h08v04_mnScaled<- day_358_h08v04_mn*0.0005
Out_day_358<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_358_h08v04_AVG_Scaled.img"
writeRaster (day_358_h08v04_mnScaled, filename=Out_day_358,format="HFA",overwrite=TRUE)
day_359_h08v04_stack<- stack(day_359_h08v04)
nlayers(day_359_h08v04_stack) 
day_359_h08v04_mn<- mean(day_359_h08v04_stack, na.rm=TRUE)
day_359_h08v04_mnScaled<- day_359_h08v04_mn*0.0005
Out_day_359<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_359_h08v04_AVG_Scaled.img"
writeRaster (day_359_h08v04_mnScaled, filename=Out_day_359,format="HFA",overwrite=TRUE)
day_360_h08v04_stack<- stack(day_360_h08v04)
nlayers(day_360_h08v04_stack) 
day_360_h08v04_mn<- mean(day_360_h08v04_stack, na.rm=TRUE)
day_360_h08v04_mnScaled<- day_360_h08v04_mn*0.0005
Out_day_360<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_360_h08v04_AVG_Scaled.img"
writeRaster (day_360_h08v04_mnScaled, filename=Out_day_360,format="HFA",overwrite=TRUE)
day_361_h08v04_stack<- stack(day_361_h08v04)
nlayers(day_361_h08v04_stack) 
day_361_h08v04_mn<- mean(day_361_h08v04_stack, na.rm=TRUE)
day_361_h08v04_mnScaled<- day_361_h08v04_mn*0.0005
Out_day_361<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_361_h08v04_AVG_Scaled.img"
writeRaster (day_361_h08v04_mnScaled, filename=Out_day_361,format="HFA",overwrite=TRUE)
day_362_h08v04_stack<- stack(day_362_h08v04)
nlayers(day_362_h08v04_stack) 
day_362_h08v04_mn<- mean(day_362_h08v04_stack, na.rm=TRUE)
day_362_h08v04_mnScaled<- day_362_h08v04_mn*0.0005
Out_day_362<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_362_h08v04_AVG_Scaled.img"
writeRaster (day_362_h08v04_mnScaled, filename=Out_day_362,format="HFA",overwrite=TRUE)
day_363_h08v04_stack<- stack(day_363_h08v04)
nlayers(day_363_h08v04_stack) 
day_363_h08v04_mn<- mean(day_363_h08v04_stack, na.rm=TRUE)
day_363_h08v04_mnScaled<- day_363_h08v04_mn*0.0005
Out_day_363<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_363_h08v04_AVG_Scaled.img"
writeRaster (day_363_h08v04_mnScaled, filename=Out_day_363,format="HFA",overwrite=TRUE)
day_364_h08v04_stack<- stack(day_364_h08v04)
nlayers(day_364_h08v04_stack) 
day_364_h08v04_mn<- mean(day_364_h08v04_stack, na.rm=TRUE)
day_364_h08v04_mnScaled<- day_364_h08v04_mn*0.0005
Out_day_364<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_364_h08v04_AVG_Scaled.img"
writeRaster (day_364_h08v04_mnScaled, filename=Out_day_364,format="HFA",overwrite=TRUE)
day_365_h08v04_stack<- stack(day_365_h08v04)
nlayers(day_365_h08v04_stack) 
day_365_h08v04_mn<- mean(day_365_h08v04_stack, na.rm=TRUE)
day_365_h08v04_mnScaled<- day_365_h08v04_mn*0.0005
Out_day_365<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_365_h08v04_AVG_Scaled.img"
writeRaster (day_365_h08v04_mnScaled, filename=Out_day_365,format="HFA",overwrite=TRUE)
day_366_h08v04_stack<- stack(day_366_h08v04)
nlayers(day_366_h08v04_stack) 
day_366_h08v04_mn<- mean(day_366_h08v04_stack, na.rm=TRUE)
day_366_h08v04_mnScaled<- day_366_h08v04_mn*0.0005
Out_day_366<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day_366_h08v04_AVG_Scaled.img"
writeRaster (day_366_h08v04_mnScaled, filename=Out_day_366,format="HFA",overwrite=TRUE)