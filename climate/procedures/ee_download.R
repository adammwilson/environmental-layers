library(doMC)
library(foreach)
registerDoMC(4)

wd="~/acrobates/adamw/projects/cloud"
setwd(wd)

tempdir="tmp"
if(!file.exists(tempdir)) dir.create(tempdir)


### Build Tiles

## bin sizes in degrees
ybin=30
xbin=30

tiles=expand.grid(ulx=seq(-180,180-xbin,by=xbin),uly=seq(90,-90+ybin,by=-ybin))
tiles$h=factor(tiles$ulx,labels=paste("h",sprintf("%02d",1:length(unique(tiles$ulx))),sep=""))
tiles$v=factor(tiles$uly,labels=paste("v",sprintf("%02d",1:length(unique(tiles$uly))),sep=""))
tiles$tile=paste(tiles$h,tiles$v,sep="")
tiles$urx=tiles$ulx+xbin
tiles$ury=tiles$uly
tiles$lrx=tiles$ulx+xbin
tiles$lry=tiles$uly-ybin
tiles$llx=tiles$ulx
tiles$lly=tiles$uly-ybin
tiles$cy=(tiles$uly+tiles$lry)/2
tiles$cx=(tiles$ulx+tiles$urx)/2
tiles=tiles[,c("tile","h","v","ulx","uly","urx","ury","lrx","lry","llx","lly","cx","cy")]

jobs=expand.grid(tile=tiles$tile,year=2000:2012,month=1:12)
jobs[,c("ulx","uly","urx","ury","lrx","lry","llx","lly")]=tiles[match(jobs$tile,tiles$tile),c("ulx","uly","urx","ury","lrx","lry","llx","lly")]

## drop Janurary 2000 from list (pre-modis)
jobs=jobs[!(jobs$year==2000&jobs$month==1),]


## Run the python downloading script
#system("~/acrobates/adamw/projects/environmental-layers/climate/procedures/ee.MOD09.py -projwin -159 20 -154.5 18.5 -year 2001 -month 6 -region test")   
i=1
todo=1:nrow(jobs)

##  Get list of available files
df=data.frame(path=list.files("/mnt/data2/projects/cloud/mod09",pattern="*.tif$",full=T,recur=T),stringsAsFactors=F)
df[,c("region","year","month")]=do.call(rbind,strsplit(basename(df$path),"_|[.]"))[,c(1,2,3)]
df$date=as.Date(paste(df$year,"_",df$month,"_15",sep=""),"%Y_%m_%d")

table(df$year,df$month)


checkcomplete=T
if(checkcomplete&exists("df")){  #if desired (and "df" exists from below) drop complete date-tiles
todo=which(!paste(jobs$tile,jobs$year,jobs$month)%in%paste(df$region,df$year,df$month))
}


writeLines(paste("Tiling options will produce",nrow(tiles),"tiles and ",nrow(jobs),"tile-months.  Current todo list is ",length(todo)))

t=foreach(i=todo,.inorder=FALSE,.verbose=F) %dopar%{
    system(paste("python ~/acrobates/adamw/projects/environmental-layers/climate/procedures/ee.MOD09.py -projwin ",
                      jobs$ulx[i]," ",jobs$uly[i]," ",jobs$urx[i]," ",jobs$ury[i]," ",jobs$lrx[i]," ",jobs$lry[i]," ",jobs$llx[i]," ",jobs$lly[i]," ",
                      "  -year ",jobs$year[i]," -month ",jobs$month[i]," -region ",jobs$tile[i],sep=""))
     }





