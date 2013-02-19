### script to return bounding box for a list of tiles

## import commandline arguments
library(getopt)
## get options
opta <- getopt(matrix(c(
                        'tile', 't', 1, 'character'
                        ), ncol=4, byrow=TRUE))
if(is.null(opta$tile)) stop("Please provide a list of tiles.  For example, -t \"h11v08,h12v09\"")

tiles=sub(" ","",tolower(do.call(c,strsplit(opta$tile,split=","))))

### get tile boundaries
tb=read.table("http://landweb.nascom.nasa.gov/developers/sn_tiles/sn_bound_10deg.txt",skip=6,nrows=648,header=T)
tb$tile=paste("h",sprintf("%02d",tb$ih),"v",sprintf("%02d",tb$iv),sep="")


## get minmax for all tiles
tb2=tb[tb$tile%in%tiles,]

print(tb2)

## print summary
print(paste("Getting bounding box for tiles: ",paste(tb2$tile,collapse=", ")))
print(paste("lon[",min(tb2$lon_min),",",max(tb2$lon_max),"]  lat[",min(tb2$lat_min),",",max(tb2$lat_max),"]"))


