## Short script to test swtif gridding of various versions
setwd(tempdir())


library(sp)
library(raster)

system("wget ftp://ladsweb.nascom.nasa.gov/allData/6/MOD35_L2/2009/029/MOD35_L2.A2009029.0500.006.2012245113542.hdf")
system("wget ftp://ladsweb.nascom.nasa.gov/allData/6/MOD35_L2/2009/029/MOD35_L2.A2009029.0320.006.2012245113606.hdf")

files=list.files(pattern="hdf")

swtifpath="sudo MRTDATADIR=\"/usr/local/heg/2.12/data\" PGSHOME=/usr/local/heg/2.12/TOOLKIT_MTD PWD=/home/adamw /usr/local/heg/2.12/bin/swtif"
swtifpath="sudo MRTDATADIR=\"/usr/local/heg/2.12b/data\" PGSHOME=/usr/local/heg/2.12b/TOOLKIT_MTD PWD=/home/adamw /usr/local/heg/2.12b/bin/swtif"

## global bounding box
   gbb=cbind(c(-180,-180,180,180,-180),c(-90,90,90,-90,-90))
   gpp = SpatialPolygons(list(Polygons(list(Polygon(gbb)),1)))
   proj4string(gpp)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
 
vars=as.data.frame(matrix(c(
#  "Cloud_Mask",              "CM",       "NN",    1,
#  "Quality_Assurance",       "QA",       "NN",    1,
#  "Solar_Zenith",            "SolZen",   "NN", 1,
  "Sensor_Zenith",           "SenZen",   "CUBIC", 1
  ),
  byrow=T,ncol=4,dimnames=list(1:1,c("variable","varid","method","band"))),stringsAsFactors=F)


## define function that grids swaths
swtif<-function(file,var){
  outfile=paste(tempdir(),"/",var$varid,"_",basename(file),sep="")  #gridded path
   ## First write the parameter file (careful, heg is very finicky!)
   hdr=paste("NUM_RUNS = 1")
grp=paste("
BEGIN
INPUT_FILENAME=",file,"
OBJECT_NAME=mod35
FIELD_NAME=",var$variable,"|
BAND_NUMBER = ",var$band,"
OUTPUT_PIXEL_SIZE_X=926.6
OUTPUT_PIXEL_SIZE_Y=926.6
# MODIS 1km Resolution
SPATIAL_SUBSET_UL_CORNER = ( ",bbox(gpp)[2,2]," ",bbox(gpp)[1,1]," )
SPATIAL_SUBSET_LR_CORNER = ( ",bbox(gpp)[2,1]," ",bbox(gpp)[1,2]," )
RESAMPLING_TYPE =",var$method,"
OUTPUT_PROJECTION_TYPE = SIN
OUTPUT_PROJECTION_PARAMETERS = ( 6371007.181 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )
# projection parameters from http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/newPage.cgi?fileName=sn_gctp
ELLIPSOID_CODE = WGS84
OUTPUT_TYPE = HDFEOS
OUTPUT_FILENAME = ",outfile,"
END
",sep="")
  ## write it to a file
  cat(c(hdr,grp)    , file=paste(tempdir(),"/",basename(file),"_MODparms.txt",sep=""))
  ## now run the swath2grid tool
  ## write the gridded file
  system(paste(swtifpath," -p ",tempdir(),"/",basename(file),"_MODparms.txt -d  -tmpLatLondir ",tempdir(),sep=""),intern=F,ignore.stderr=F)
   print(paste("Finished processing variable",var$variable,"from ",basename(file),"to",outfile))
}

swtifpath="sudo MRTDATADIR=\"/usr/local/heg/2.12/data\" PGSHOME=/usr/local/heg/2.12/TOOLKIT_MTD PWD=/home/adamw /usr/local/heg/2.12/bin/swtif"
swtifpath="sudo MRTDATADIR=\"/usr/local/heg/2.12b/data\" PGSHOME=/usr/local/heg/2.12b/TOOLKIT_MTD PWD=/home/adamw /usr/local/heg/2.12b/bin/swtif"

## run it
  lapply(1:nrow(vars),function(i) swtif(files[1],vars[i,]))
  lapply(1:nrow(vars),function(i) swtif(files[2],vars[i,]))

### make a plot to compare versions
library(rasterVis)

d=stack(list.files(pattern="SenZen.*hdf$"))

png(file="swtifCompareVersions.png",width=2000,height=1000)
levelplot(d,at=seq(0,100,len=100),col.regions=rainbow(100))
dif=d[[1]]-d[[2]]
levelplot(dif,at=seq(min(dif,na.rm=T),100,len=100),col.regions=rainbow(100))
dev.off()
