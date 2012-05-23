library (raster)

path<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/ByDate/BadPixelsCorrected_Tiles"

#Find monthly means for h08v04 tiles:
h08v04_inFiles <- list.files(path=path, pattern="*.h08v04.005.ClearDay_corrected.img$")
for (i in 1: length(h08v04_inFiles)){
  h08v04_inFiles[i]<- paste (path,"/",h08v04_inFiles[i],sep="")
}
h08v04_inFiles

#Separate out monthly groupings
Jan_h08v04<- c(1:length (h08v04_inFiles))*NA
for (i in 1: length(h08v04_inFiles)){
  if (length(grep("\\.1\\.1",h08v04_inFiles[i]) !=0)||
    length(grep("\\.1\\.2",h08v04_inFiles[i]) !=0)||
    length(grep("\\.1\\.3",h08v04_inFiles[i]) !=0)||
    length(grep("\\.1\\.4",h08v04_inFiles[i]) !=0)||
    length(grep("\\.1\\.5",h08v04_inFiles[i]) !=0)||
    length(grep("\\.1\\.6",h08v04_inFiles[i]) !=0)||
    length(grep("\\.1\\.7",h08v04_inFiles[i]) !=0)||
    length(grep("\\.1\\.8",h08v04_inFiles[i]) !=0)||
    length(grep("\\.1\\.9",h08v04_inFiles[i]) !=0)){
          Jan_h08v04[i]<- h08v04_inFiles[i]
  } 
}
Jan_h08v04<-Jan_h08v04[!is.na(Jan_h08v04)]
Jan_h08v04

#Create rasters stack, calc means, scale  
Jan_h08v04_stack<- stack(Jan_h08v04) #checked nlayers(), turned by 310: correct
Jan_h08v04_mn<- mean(Jan_h08v04_stack, na.rm=TRUE)
Jan_h08v04_mnScaled<- Jan_h08v04_mn*0.0005

#Write results to new file
Out_Jan<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/Jan_h08v04_AVG_Scaled.img"
writeRaster (Jan_h08v04_mnScaled, filename=Out_Jan,format="HFA",overwrite=TRUE)

Feb_h08v04<- c(1:length (h08v04_inFiles))*NA
for (i in 1: length(h08v04_inFiles)){
  if (length(grep("\\.2\\.1",h08v04_inFiles[i]) !=0)||
    length(grep("\\.2\\.2",h08v04_inFiles[i]) !=0)||
    length(grep("\\.2\\.3",h08v04_inFiles[i]) !=0)||
    length(grep("\\.2\\.4",h08v04_inFiles[i]) !=0)||
    length(grep("\\.2\\.5",h08v04_inFiles[i]) !=0)||
    length(grep("\\.2\\.6",h08v04_inFiles[i]) !=0)||
    length(grep("\\.2\\.7",h08v04_inFiles[i]) !=0)||
    length(grep("\\.2\\.8",h08v04_inFiles[i]) !=0)||
    length(grep("\\.2\\.9",h08v04_inFiles[i]) !=0)){
          Feb_h08v04[i]<- h08v04_inFiles[i]
  } 
}
Feb_h08v04<-Feb_h08v04[!is.na(Feb_h08v04)]
Feb_h08v04

Feb_h08v04_stack<- stack(Feb_h08v04) 
nlayers(Feb_h08v04_stack)
Feb_h08v04_mn<- mean(Feb_h08v04_stack, na.rm=TRUE)
Feb_h08v04_mnScaled<- Feb_h08v04_mn*0.0005

Out_Feb<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/Feb_h08v04_AVG_Scaled.img"
writeRaster (Feb_h08v04_mnScaled, filename=Out_Feb,format="HFA",overwrite=TRUE)

Mar_h08v04<- c(1:length (h08v04_inFiles))*NA
for (i in 1: length(h08v04_inFiles)){
  if (length(grep("\\.3\\.1",h08v04_inFiles[i]) !=0)||
    length(grep("\\.3\\.2",h08v04_inFiles[i]) !=0)||
    length(grep("\\.3\\.3",h08v04_inFiles[i]) !=0)||
    length(grep("\\.3\\.4",h08v04_inFiles[i]) !=0)||
    length(grep("\\.3\\.5",h08v04_inFiles[i]) !=0)||
    length(grep("\\.3\\.6",h08v04_inFiles[i]) !=0)||
    length(grep("\\.3\\.7",h08v04_inFiles[i]) !=0)||
    length(grep("\\.3\\.8",h08v04_inFiles[i]) !=0)||
    length(grep("\\.3\\.9",h08v04_inFiles[i]) !=0)){
          Mar_h08v04[i]<- h08v04_inFiles[i]
  } 
}
Mar_h08v04<-Mar_h08v04[!is.na(Mar_h08v04)]
Mar_h08v04

Mar_h08v04_stack<- stack(Mar_h08v04) 
nlayers(Mar_h08v04_stack)
Mar_h08v04_mn<- mean(Mar_h08v04_stack, na.rm=TRUE)
Mar_h08v04_mnScaled<- Mar_h08v04_mn*0.0005

Out_Mar<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/Mar_h08v04_AVG_Scaled.img"
writeRaster (Mar_h08v04_mnScaled, filename=Out_Mar,format="HFA",overwrite=TRUE)

Apr_h08v04<- c(1:length (h08v04_inFiles))*NA
for (i in 1: length(h08v04_inFiles)){
  if (length(grep("\\.4\\.1",h08v04_inFiles[i]) !=0)||
    length(grep("\\.4\\.2",h08v04_inFiles[i]) !=0)||
    length(grep("\\.4\\.3",h08v04_inFiles[i]) !=0)||
    length(grep("\\.4\\.4",h08v04_inFiles[i]) !=0)||
    length(grep("\\.4\\.5",h08v04_inFiles[i]) !=0)||
    length(grep("\\.4\\.6",h08v04_inFiles[i]) !=0)||
    length(grep("\\.4\\.7",h08v04_inFiles[i]) !=0)||
    length(grep("\\.4\\.8",h08v04_inFiles[i]) !=0)||
    length(grep("\\.4\\.9",h08v04_inFiles[i]) !=0)){
          Apr_h08v04[i]<- h08v04_inFiles[i]
  } 
}
Apr_h08v04<-Apr_h08v04[!is.na(Apr_h08v04)]
Apr_h08v04

Apr_h08v04_stack<- stack(Apr_h08v04) 
nlayers(Apr_h08v04_stack)
Apr_h08v04_mn<- mean(Apr_h08v04_stack, na.rm=TRUE)
Apr_h08v04_mnScaled<- Apr_h08v04_mn*0.0005

Out_Apr<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/Apr_h08v04_AVG_Scaled.img"
writeRaster (Apr_h08v04_mnScaled, filename=Out_Apr,format="HFA",overwrite=TRUE)

May_h08v04<- c(1:length (h08v04_inFiles))*NA
for (i in 1: length(h08v04_inFiles)){
  if (length(grep("\\.5\\.1",h08v04_inFiles[i]) !=0)||
    length(grep("\\.5\\.2",h08v04_inFiles[i]) !=0)||
    length(grep("\\.5\\.3",h08v04_inFiles[i]) !=0)||
    length(grep("\\.5\\.4",h08v04_inFiles[i]) !=0)||
    length(grep("\\.5\\.5",h08v04_inFiles[i]) !=0)||
    length(grep("\\.5\\.6",h08v04_inFiles[i]) !=0)||
    length(grep("\\.5\\.7",h08v04_inFiles[i]) !=0)||
    length(grep("\\.5\\.8",h08v04_inFiles[i]) !=0)||
    length(grep("\\.5\\.9",h08v04_inFiles[i]) !=0)){
          May_h08v04[i]<- h08v04_inFiles[i]
  } 
}
May_h08v04<-May_h08v04[!is.na(May_h08v04)]
May_h08v04

May_h08v04_stack<- stack(May_h08v04) 
nlayers(May_h08v04_stack)
May_h08v04_mn<- mean(May_h08v04_stack, na.rm=TRUE)
May_h08v04_mnScaled<- May_h08v04_mn*0.0005

Out_May<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/May_h08v04_AVG_Scaled.img"
writeRaster (May_h08v04_mnScaled, filename=Out_May,format="HFA",overwrite=TRUE)

Jun_h08v04<- c(1:length (h08v04_inFiles))*NA
for (i in 1: length(h08v04_inFiles)){
  if (length(grep("\\.6\\.1",h08v04_inFiles[i]) !=0)||
    length(grep("\\.6\\.2",h08v04_inFiles[i]) !=0)||
    length(grep("\\.6\\.3",h08v04_inFiles[i]) !=0)||
    length(grep("\\.6\\.4",h08v04_inFiles[i]) !=0)||
    length(grep("\\.6\\.5",h08v04_inFiles[i]) !=0)||
    length(grep("\\.6\\.6",h08v04_inFiles[i]) !=0)||
    length(grep("\\.6\\.7",h08v04_inFiles[i]) !=0)||
    length(grep("\\.6\\.8",h08v04_inFiles[i]) !=0)||
    length(grep("\\.6\\.9",h08v04_inFiles[i]) !=0)){
          Jun_h08v04[i]<- h08v04_inFiles[i]
  } 
}
Jun_h08v04<-Jun_h08v04[!is.na(Jun_h08v04)]
Jun_h08v04

Jun_h08v04_stack<- stack(Jun_h08v04) 
nlayers(Jun_h08v04_stack)
Jun_h08v04_mn<- mean(Jun_h08v04_stack, na.rm=TRUE)
Jun_h08v04_mnScaled<- Jun_h08v04_mn*0.0005

Out_Jun<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/Jun_h08v04_AVG_Scaled.img"
writeRaster (Jun_h08v04_mnScaled, filename=Out_Jun,format="HFA",overwrite=TRUE)

Jul_h08v04<- c(1:length (h08v04_inFiles))*NA
for (i in 1: length(h08v04_inFiles)){
  if (length(grep("\\.7\\.1",h08v04_inFiles[i]) !=0)||
    length(grep("\\.7\\.2",h08v04_inFiles[i]) !=0)||
    length(grep("\\.7\\.3",h08v04_inFiles[i]) !=0)||
    length(grep("\\.7\\.4",h08v04_inFiles[i]) !=0)||
    length(grep("\\.7\\.5",h08v04_inFiles[i]) !=0)||
    length(grep("\\.7\\.6",h08v04_inFiles[i]) !=0)||
    length(grep("\\.7\\.7",h08v04_inFiles[i]) !=0)||
    length(grep("\\.7\\.8",h08v04_inFiles[i]) !=0)||
    length(grep("\\.7\\.9",h08v04_inFiles[i]) !=0)){
          Jul_h08v04[i]<- h08v04_inFiles[i]
  } 
}
Jul_h08v04<-Jul_h08v04[!is.na(Jul_h08v04)]
Jul_h08v04

Jul_h08v04_stack<- stack(Jul_h08v04) 
nlayers(Jul_h08v04_stack)
Jul_h08v04_mn<- mean(Jul_h08v04_stack, na.rm=TRUE)
Jul_h08v04_mnScaled<- Jul_h08v04_mn*0.0005

Out_Jul<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/Jul_h08v04_AVG_Scaled.img"
writeRaster (Jul_h08v04_mnScaled, filename=Out_Jul,format="HFA",overwrite=TRUE)

Aug_h08v04<- c(1:length (h08v04_inFiles))*NA
for (i in 1: length(h08v04_inFiles)){
  if (length(grep("\\.8\\.1",h08v04_inFiles[i]) !=0)||
    length(grep("\\.8\\.2",h08v04_inFiles[i]) !=0)||
    length(grep("\\.8\\.3",h08v04_inFiles[i]) !=0)||
    length(grep("\\.8\\.4",h08v04_inFiles[i]) !=0)||
    length(grep("\\.8\\.5",h08v04_inFiles[i]) !=0)||
    length(grep("\\.8\\.6",h08v04_inFiles[i]) !=0)||
    length(grep("\\.8\\.7",h08v04_inFiles[i]) !=0)||
    length(grep("\\.8\\.8",h08v04_inFiles[i]) !=0)||
    length(grep("\\.8\\.9",h08v04_inFiles[i]) !=0)){
          Aug_h08v04[i]<- h08v04_inFiles[i]
  } 
}
Aug_h08v04<-Aug_h08v04[!is.na(Aug_h08v04)]
Aug_h08v04

Aug_h08v04_stack<- stack(Aug_h08v04) 
nlayers(Aug_h08v04_stack)
Aug_h08v04_mn<- mean(Aug_h08v04_stack, na.rm=TRUE)
Aug_h08v04_mnScaled<- Aug_h08v04_mn*0.0005

Out_Aug<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/Aug_h08v04_AVG_Scaled.img"
writeRaster (Aug_h08v04_mnScaled, filename=Out_Aug,format="HFA",overwrite=TRUE)

Sep_h08v04<- c(1:length (h08v04_inFiles))*NA
for (i in 1: length(h08v04_inFiles)){
  if (length(grep("\\.9\\.1",h08v04_inFiles[i]) !=0)||
    length(grep("\\.9\\.2",h08v04_inFiles[i]) !=0)||
    length(grep("\\.9\\.3",h08v04_inFiles[i]) !=0)||
    length(grep("\\.9\\.4",h08v04_inFiles[i]) !=0)||
    length(grep("\\.9\\.5",h08v04_inFiles[i]) !=0)||
    length(grep("\\.9\\.6",h08v04_inFiles[i]) !=0)||
    length(grep("\\.9\\.7",h08v04_inFiles[i]) !=0)||
    length(grep("\\.9\\.8",h08v04_inFiles[i]) !=0)||
    length(grep("\\.9\\.9",h08v04_inFiles[i]) !=0)){
          Sep_h08v04[i]<- h08v04_inFiles[i]
  } 
}
Sep_h08v04<-Sep_h08v04[!is.na(Sep_h08v04)]
Sep_h08v04

Sep_h08v04_stack<- stack(Sep_h08v04) 
nlayers(Sep_h08v04_stack)
Sep_h08v04_mn<- mean(Sep_h08v04_stack, na.rm=TRUE)
Sep_h08v04_mnScaled<- Sep_h08v04_mn*0.0005

Out_Sep<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/Sep_h08v04_AVG_Scaled.img"
writeRaster (Sep_h08v04_mnScaled, filename=Out_Sep,format="HFA",overwrite=TRUE)

Oct_h08v04<- c(1:length (h08v04_inFiles))*NA
for (i in 1: length(h08v04_inFiles)){
  if (length(grep("\\.10\\.1",h08v04_inFiles[i]) !=0)||
    length(grep("\\.10\\.2",h08v04_inFiles[i]) !=0)||
    length(grep("\\.10\\.3",h08v04_inFiles[i]) !=0)||
    length(grep("\\.10\\.4",h08v04_inFiles[i]) !=0)||
    length(grep("\\.10\\.5",h08v04_inFiles[i]) !=0)||
    length(grep("\\.10\\.6",h08v04_inFiles[i]) !=0)||
    length(grep("\\.10\\.7",h08v04_inFiles[i]) !=0)||
    length(grep("\\.10\\.8",h08v04_inFiles[i]) !=0)||
    length(grep("\\.10\\.9",h08v04_inFiles[i]) !=0)){
          Oct_h08v04[i]<- h08v04_inFiles[i]
  } 
}
Oct_h08v04<-Oct_h08v04[!is.na(Oct_h08v04)]
Oct_h08v04

Oct_h08v04_stack<- stack(Oct_h08v04)
nlayers(Oct_h08v04_stack) 
Oct_h08v04_mn<- mean(Oct_h08v04_stack, na.rm=TRUE)
Oct_h08v04_mnScaled<- Oct_h08v04_mn*0.0005

Out_Oct<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/Oct_h08v04_AVG_Scaled.img"
writeRaster (Oct_h08v04_mnScaled, filename=Out_Oct,format="HFA",overwrite=TRUE)

Nov_h08v04<- c(1:length (h08v04_inFiles))*NA
for (i in 1: length(h08v04_inFiles)){
  if (length(grep("\\.11\\.1",h08v04_inFiles[i]) !=0)||
    length(grep("\\.11\\.2",h08v04_inFiles[i]) !=0)||
    length(grep("\\.11\\.3",h08v04_inFiles[i]) !=0)||
    length(grep("\\.11\\.4",h08v04_inFiles[i]) !=0)||
    length(grep("\\.11\\.5",h08v04_inFiles[i]) !=0)||
    length(grep("\\.11\\.6",h08v04_inFiles[i]) !=0)||
    length(grep("\\.11\\.7",h08v04_inFiles[i]) !=0)||
    length(grep("\\.11\\.8",h08v04_inFiles[i]) !=0)||
    length(grep("\\.11\\.9",h08v04_inFiles[i]) !=0)){
          Nov_h08v04[i]<- h08v04_inFiles[i]
  } 
}
Nov_h08v04<-Nov_h08v04[!is.na(Nov_h08v04)]
Nov_h08v04

Nov_h08v04_stack<- stack(Nov_h08v04) 
nlayers(Nov_h08v04_stack)
Nov_h08v04_mn<- mean(Nov_h08v04_stack, na.rm=TRUE)
Nov_h08v04_mnScaled<- Nov_h08v04_mn*0.0005

Out_Nov<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/Nov_h08v04_AVG_Scaled.img"
writeRaster (Nov_h08v04_mnScaled, filename=Out_Nov,format="HFA",overwrite=TRUE)

Dec_h08v04<- c(1:length (h08v04_inFiles))*NA
for (i in 1: length(h08v04_inFiles)){
  if (length(grep("\\.12\\.1",h08v04_inFiles[i]) !=0)||
    length(grep("\\.12\\.2",h08v04_inFiles[i]) !=0)||
    length(grep("\\.12\\.3",h08v04_inFiles[i]) !=0)||
    length(grep("\\.12\\.4",h08v04_inFiles[i]) !=0)||
    length(grep("\\.12\\.5",h08v04_inFiles[i]) !=0)||
    length(grep("\\.12\\.6",h08v04_inFiles[i]) !=0)||
    length(grep("\\.12\\.7",h08v04_inFiles[i]) !=0)||
    length(grep("\\.12\\.8",h08v04_inFiles[i]) !=0)||
    length(grep("\\.12\\.9",h08v04_inFiles[i]) !=0)){
          Dec_h08v04[i]<- h08v04_inFiles[i]
  } 
}
Dec_h08v04<-Dec_h08v04[!is.na(Dec_h08v04)]
Dec_h08v04

Dec_h08v04_stack<- stack(Dec_h08v04)
nlayers(Dec_h08v04_stack) 
Dec_h08v04_mn<- mean(Dec_h08v04_stack, na.rm=TRUE)
Dec_h08v04_mnScaled<- Dec_h08v04_mn*0.0005

Out_Dec<- "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/Dec_h08v04_AVG_Scaled.img"
writeRaster (Dec_h08v04_mnScaled, filename=Out_Dec,format="HFA",overwrite=TRUE)

