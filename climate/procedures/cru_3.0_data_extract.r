##
##
##

rm(list=ls(all=TRUE))

setwd("C:\\Users\\Frank\\Documents\\Data\\Climate_data\\CRU_TS_3.0")

library(ncdf)

vrb<-c("pre","tmn","tmp","tmx")
for(v in 4:4){
  # open data file
  nc<-open.ncdf(paste("cru_ts_3_00.1901.2006.",vrb[v],".nc",sep=""))           # open datafile

  nodata<-nc$var$tas$missval                                   # nodata value
  time.dim<-nc$dim$time$len                                    # time dimension
  time.base<-as.numeric(substr(nc$dim$time$units,14,17))       # base year
  time.start<-round(nc$dim$time$vals[1]/12)+time.base         # year simulation started
  time.end<-floor(nc$dim$time$vals[time.dim]/12)+time.base    # year simulation ended

  # build flip matrix
  I<-matrix(0,360,360)
  y<-360
  for(x in 1:360){
    I[x,y]<-1
    y<-y-1
  }

  ## extract data for 1968 to 2006 and output as ESRI ASCII files
  for(i in 1:time.dim){
    # define year and month
    if(trunc(i/12)!=i/12){
      year<-floor(i/12)+time.start
      month<-round((i/12-trunc(i/12))*12)
    }else{
      year<-floor(i/12)+time.start-1
      month<-12
    }
    if(year<2005) next

   # read data
    ncd<-get.var.ncdf(nc,start=c(1,1,i),count=c(720,360,1))

    # flip matrix
    grd<-I%*%(t(ncd))

    # ouput to file in ESRI ASCII format
    esri<-c(paste("ncols",720),
            paste("nrows",360),
            paste("xllcorner",-180),
            paste("yllcorner",-90),
            paste("cellsize",0.5),
            paste("nodata_value",-99.9))
        path2<-paste("cru_ts_3.0_",vrb[v],"_",year,"_",month,".asc",sep="")
        write.table(esri,path2,row.names=F,col.names=F,quote=F,append=F)
        write.table(data.frame(grd),path2,row.names=F,col.names=F,quote=F,append=T)
  }
  close.ncdf(nc) # close datafile
}

