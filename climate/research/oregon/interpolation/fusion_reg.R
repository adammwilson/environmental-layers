#setup
mo=9
day=2
year=2010
datelabel=format(ISOdate(year,mo,day),"%b %d, %Y")
#
#  Step 1 - 10 year monthly averages
#
library(raster)
old<-getwd()
setwd("c:/data/benoit/data_Oregon_stations_Brian_04242012")
l=list.files(pattern="mean_month.*rescaled.tif")
molst<-stack(l)
setwd(old)
molst=molst-273.16  #K->C
idx <- seq(as.Date('2010-01-15'), as.Date('2010-12-15'), 'month')
molst <- setZ(molst, idx)
layerNames(molst) <- month.abb
themolst<-raster(molst,mo)
plot(themolst)
#
# Step 2 - Weather station means across same days
#
# ??? which years & what quality flags???
#select ghcn.id, lat,lon, elev, month, avg(value/10) as "TMax", count(*) as "NumDays" from ghcn, stations where ghcn.id in (select id from stations where state=='OR') and ghcn.id==stations.id and value<>-9999 and year>=2000 and  element=='TMAX' group by stations.id, month;select ghcn.id, lat,lon, elev, month, avg(value/10) as "TMax", count(*) as "NumDays" from ghcn, stations where ghcn.id in (select id from stations where state=='OR') and ghcn.id==stations.id and value<>-9999 and year>=2000 and  element=='TMAX' group by stations.id, month;
#below table from above SQL query
dst<-read.csv('/data/benoit/data_oregon_stations_brian_04242012/station_means.csv',h=T)
modst=dst[dst$month==mo,]
#
# Step 3 - get LST at stations
#
sta_lola=modst[,c("lon","lat")]
library(rgdal)
proj_str="+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs";
lookup<-function(r,lat,lon) {
	xy<-project(cbind(lon,lat),proj_str);
	cidx<-cellFromXY(r,xy);
	return(r[cidx])
}
sta_tmax_from_lst=lookup(themolst,sta_lola$lat,sta_lola$lon)
#
# Step 4 - bias at stations
#
sta_bias=sta_tmax_from_lst-modst$TMax;
bias_xy=project(as.matrix(sta_lola),proj_str)
# windows()
plot(modst$TMax,sta_tmax_from_lst,xlab="Station mo Tmax",ylab="LST mo Tmax")
abline(0,1)
#
# Step 5 - interpolate bias
#
# ?? include covariates like elev, distance to coast, cloud frequency, tree height
library(fields)
#windows()
quilt.plot(sta_lola,sta_bias,main="Bias at stations",asp=1)
US(add=T,col="magenta",lwd=2)
#fitbias<-Tps(bias_xy,sta_bias) #use TPS or krige
fitbias<-Krig(bias_xy,sta_bias,theta=1e5) #use TPS or krige
# windows()
surface(fitbias,col=rev(terrain.colors(100)),asp=1,main="Interpolated bias")
#US(add=T,col="magenta",lwd=2)
#
# Step 6 - return to daily station data & calcualate delta=daily T-monthly T from stations
#
library(RSQLite)
m<-dbDriver("SQLite")
con<-dbConnect(m,dbname='c:/data/ghcn_tmintmax.db')
querystr=paste("select ghcn.id, value as 'dailyTmax' from ghcn where ghcn.id in (select id from stations where state=='OR') and value<>-9999",
   "and year==",year,"and month==",mo,"and day==",day,
               "and element=='TMAX' ")
rs<-dbSendQuery(con,querystr)
d<-fetch(rs,n=-1)
dbClearResult(rs)
dbDisconnect(con)
d$dailyTmax=d$dailyTmax/10 #stored as 1/10 degree C to allow integer storage
dmoday=merge(modst,d,by="id")
# windows()
plot(dailyTmax~TMax,data=dmoday,xlab="Mo Tmax",ylab=paste("Daily for",datelabel),main="across stations in OR")
#
# Step 7 - interpolate delta across space
#
daily_sta_lola=dmoday[,c("lon","lat")] #could be same as before but why assume merge does this - assume not
daily_sta_xy=project(as.matrix(daily_sta_lola),proj_str)
daily_delta=dmoday$dailyTmax-dmoday$TMax
#windows()
quilt.plot(daily_sta_lola,daily_delta,asp=1,main="Station delta for Jan 15")
US(add=T,col="magenta",lwd=2)
#fitdelta<-Tps(daily_sta_xy,daily_delta) #use TPS or krige
fitdelta<-Krig(daily_sta_xy,daily_delta,theta=1e5) #use TPS or krige
# windows()
surface(fitdelta,col=rev(terrain.colors(100)),asp=1,main="Interpolated delta")
#US(add=T,col="magenta",lwd=2)
#
# Step 8 - assemble final answer - T=LST+Bias(interpolated)+delta(interpolated)
#
bias_rast=interpolate(themolst,fitbias)
plot(bias_rast,main="Raster bias")
daily_delta_rast=interpolate(themolst,fitdelta)
plot(daily_delta_rast,main="Raster Daily Delta")
tmax_predicted=themolst+daily_delta_rast-bias_rast
plot(tmax_predicted,main="Predicted daily")
#
# check
#
sta_pred=lookup(tmax_predicted,daily_sta_lola$lat,daily_sta_lola$lon)
RMSE<-function(x,y) {return(mean((x-y)^2)^0.5)}
rmse=RMSE(sta_pred,dmoday$dailyTmax)
plot(sta_pred~dmoday$dailyTmax,xlab=paste("Actual daily for",datelabel),ylab="Pred daily",main=paste("RMSE=",rmse))
abline(0,1)
resid=sta_pred-dmoday$dailyTmax
quilt.plot(daily_sta_lola,resid)
