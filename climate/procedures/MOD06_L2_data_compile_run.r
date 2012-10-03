
setwd("/nobackupp1/awilso10/mod06")
library(multicore)
### get list of files to process
datadir="/nobackupp4/datapool/modis/MOD06_L2.005/"

fs=data.frame(
  path=list.files(datadir,full=T,recursive=T,pattern="hdf"),
  file=basename(list.files(datadir,full=F,recursive=T,pattern="hdf")))
fs$date=as.Date(substr(fs$file,11,17),"%Y%j")
fs$month=format(fs$date,"%m")
fs$year=format(fs$date,"%Y")
fs$time=substr(fs$file,19,22)
fs$datetime=as.POSIXct(strptime(paste(substr(fs$file,11,17),substr(fs$file,19,22)), '%Y%j %H%M'))
fs$dateid=format(fs$date,"%Y%m%d")
fs$path=as.character(fs$path)
fs$file=as.character(fs$file)

## get all unique dates
alldates=unique(fs$dateid)

## load tile information
load(file="modlandTiles.Rdata")
### use MODIS tile as ROI
#modt=readOGR("modgrid","modis_sinusoidal_grid_world",)
#modt@data[,colnames(tb)[3:6]]=tb[match(paste(modt$h,modt$v),paste(tb$ih,tb$iv)),3:6]
#write.csv(modt@data,file="modistile.csv")


## write it out
save(fs,tb,file="allfiles.Rdata")
#save(alldates,file="alldates.Rdata")

## identify which have been completed
outdir="2_daily"
done=alldates%in%substr(list.files(outdir),5,12)
table(done)
notdone=alldates[!done]

#notdone=alldates[1:4]

save(notdone,file="notdone.Rdata")

#write.table(paste("i=",notdone[1:10],sep=""),file="notdone.txt",row.names=F)

## vars
vars=as.data.frame(matrix(c(
  "Cloud_Effective_Radius",              "CER",
  "Cloud_Effective_Radius_Uncertainty",  "CERU",
  "Cloud_Optical_Thickness",             "COT",
  "Cloud_Optical_Thickness_Uncertainty", "COTU",
  "Cloud_Water_Path",                    "CWP",
  "Cloud_Water_Path_Uncertainty",        "CWPU",
  "Cloud_Phase_Optical_Properties",      "CPOP",
  "Cloud_Multi_Layer_Flag",              "CMLF",
  "Cloud_Mask_1km",                      "CM1",
  "Quality_Assurance_1km",               "QA"),
  byrow=T,ncol=2,dimnames=list(1:10,c("variable","varid"))),stringsAsFactors=F)
save(vars,file="vars.Rdata")

library(multicore)
mclapply(1:length(notdone),function(i) system(paste("Rscript --verbose --vanilla /u/awilso10/environmental-layers/climate/procedures/MOD06_L2_data_compile_Pleiades.r i=",i,sep="")))


## finish up and quit R
q("no")
