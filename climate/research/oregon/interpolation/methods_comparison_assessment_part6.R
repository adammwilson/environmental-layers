#####################################  METHODS COMPARISON part 6 ##########################################
#################################### Spatial Analysis ############################################
#This script utilizes the R ojbects created during the interpolation phase.                       #
#At this stage the script produces figures of various accuracy metrics and compare methods:       #
#This scripts focuses on a detailed studay of differences in the predictions of CAI_kr and FUsion_Kr                              #
#AUTHOR: Benoit Parmentier                                                                        #
#DATE: 11/30/2012                                                                                 #
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#491 --                                  #
###################################################################################################

###Loading R library and packages                                                      
library(gtools)                                        # loading some useful tools such as mixedsort
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
library(RCurl)
######### Functions used in the script
#loading R objects that might have similar names

download_files_with_pattern_ftp<-function(url_dir,out_path,file_pattern){
  #sadfd
  #adsfd
  #library(RCurl) #modify later using require
  # FTP 
  url<-url_dir
  filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE) 
  file_pattern_rx<-glob2rx(file_pattern)
  # Deal with newlines as \n or \r\n. (BDR) 
  # Or alternatively, instruct libcurl to change \n's to \r\n's for us with crlf = TRUE 
  # filenames = getURL(url, ftp.use.epsv = FALSE, ftplistonly = TRUE, crlf = TRUE) 
  
  filenames = paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "") 
  filenames_all<-filenames
  #Now subset filenames to match wanted files...
  i_file<-grep(file_pattern_rx,filenames_all,value=TRUE) # using grep with "value" extracts the matching names
  pos<-match(i_file,filenames_all)
  
  filenames<-filenames_all[pos]
                                                                 53)})) 
  for(i in 1:length(filenames)){
    out_file<-basename(filenames[i])
    out_file_path<-file.path(out_path,out_file)
    download.file(filenames[i],destfile=out_file_path)
    #creating the file names
  }
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

out_prefix<-"methods_11012012_"

path_wd<-"/home/parmentier/Data/IPLANT_project/methods_interpolation_comparison_10242012"

##### MAIN BODY OF CODE ######

setwd(path_wd)
latlong <- "+init=epsg:4326"
url_dir1<- "ftp://anonymous:test@ftp.wcc.nrcs.usda.gov/data/snow/snotel/snothist/"
url_dir2<- "ftp://anonymous:test@ftp.wcc.nrcs.usda.gov/data/snow/snotel/cards/oregon/"
file_pat<-"*or*.txt"
file_pat1<-"*listor.txt"
file_pat2<-"*all.txt"
output_path<-"/home/parmentier/Data/IPLANT_project/methods_interpolation_comparison_10242012/snotel_data_11302012"
download_files_with_pattern_ftp(url_dir1,path_wd,file_pat1)
download_files_with_pattern_ftp(url_dir1,output_path,file_pat1)
download_files_with_pattern_ftp(url_dir2,output_path,file_pat2)


reg_outline<-readOGR(".",sub(".shp","",infile6))
loc_stat_file<-"listor.txt"
tmp<-read.table(loc_stat_file,sep=" ",skip=4)

colwidths<-c(5,3,4,5,9,9,5,6,6,50)
colab<- c("no", "ST", "CTY", "tyype", "HUC", "station", "Lat_pc", "Long_pc", "elev", "sitename")
loc_tab_snot<-read.fwf(file=loc_stat_file,widths=colwidths,skip=3)
names(loc_tab_snot)<-colab

y<-loc_stations_snot$Lat_pc/100
x<-(loc_stations_snot$Long_pc/100)*-1

tmp<-SpatialPointsDataFrame(coords=cbind(x,y),data=loc_stations_snot)
proj4string(tmp)<-CRS(latlong)
loc_stat_snot<-spTransform(tmp,CRS(proj4string(reg_outline)))

outfile_name<-"loc_stat_snot.shp"
writeOGR(obj=loc_stat_snot,layer="loc_stat_snot",dsn=outfile_name,driver="ESRI Shapefile", overwrite=T)

file_pat2_rx<-glob2rx(file_pat2)
snot_data_filename<-list.files(path=output_path,pattern=file_pat2_rx)
stat_id<-sub("_all.txt","",snot_data_filename)
tmpchr<-trim(levels(loc_stat_snot$station))
loc_stat_snot$station<-tolower(tmpchr)

m<-match(stat_id,loc_stat_snot$station) #This matches the station id location wtih the data files...


#put in a loop
#create giant table/data.frame from all the files...?
snot_data_name<-file.path(output_path,snot_data_filename[1])
snot_data_tab<-read.table(snot_data_name,sep="\t",skip=1)    #This is a tab delimited file...
colab<- c("date", "pill", "prec","tmax", "tmin", "tavg", "prcp")
names(snot_data_tab)<-colab

snot_fnames<-file.path(output_path,snot_data_filename)
                                                                 
                                                                                                                                                                                                               header=TRUE, sep="\t")}))
read_snot_data<-function(files){
  snot_data_tab<-read.table(files,sep="\t",skip=1)    #This is a tab delimited file...
  stat_id_tmp<-sub("_all.txt","",files)
  names(snot_data_tab)<-c("date", "pill", "prec", "tmax", "tmin", "tavg", "prcp")
  snot_data_tab$id<-stat_id_tmp
  return(snot_data_tab)
}
dataset<-do.call("rbind",lapply(file_list, FUN=read_snot_data))

dataset$id<-basename(dataset$id)

snot_loc_data_OR<-merge(loc_stat_snot,dataset,by.x="station",by.y="id")

tmp44<-subset(snot_loc_data_OR,station=="17d02s")
tail(snot_data_tab)
tmp45<-tmp44[tmp44$date=="93012",]
tail(tmp45)
tmp45<-tmp44[tmp44$date=="92912",]

test_data<-subset(snot_loc_data_OR,date=="90112" & station=="17d07s")

#Now subset for specific dates...

date_pat="*10"  #anything in 2010...
date_pat_rx<-glob2rx(date_pat)
#grep(tmp_data$date
date_selected<-grep(date_pat_rx,snot_loc_data_OR$date,value=TRUE) # using grep with "value" extracts the matching names
#pos<-match(i_file,filenames_all)
     
snot_OR_2010<-subset(snot_loc_data_OR,date%in%date_selected)

#tansform in SPDF
coords<-cbind(snot_OR_2010$x,snot_OR_2010$y)
snot_OR_2010_sp<-SpatialPointsDataFrame(coords=coords,data=snot_OR_2010)
length(unique(snot_OR_2010_sp$station)) #Find out about the number of stations...
dim(snot_OR_2010_sp)
#write out in SPDF
outfile_name<-paste("snot_OR_2010_sp2_",out_prefix,".shp",sep="")
writeOGR(obj=snot_OR_2010_sp,layer="snot_OR_2010",dsn=outfile_name,driver="ESRI Shapefile", overwrite=T)

#####END OF SCRIPT
# 
# dataset <- do.call("rbind",lapply(file_list,
#                                   FUN=function(files){read.table(files,
#                                                                  skip=1, sep="\t")}))
#     

 