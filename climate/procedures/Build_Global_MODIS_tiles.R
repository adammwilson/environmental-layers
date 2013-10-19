### this script generates empty tifs that align with the MODIS land tiles, but are available globally
library(raster)

######
### proceed by raster using information at http://code.env.duke.edu/projects/mget/wiki/SinusoidalMODIS
maketile=function(tile,outdir="MODTILES"){
  ## check tile
  ## list of tile outside valid region
  nodata=c('h00v00','h01v00','h02v00','h03v00','h04v00','h05v00','h06v00','h07v00','h08v00','h09v00','h10v00',
    'h11v00','h12v00','h13v00','h22v00','h23v00','h24v00','h25v00','h26v00','h27v00','h28v00','h29v00','h30v00','h31v00',
    'h32v00','h33v00','h34v00','h35v00','h00v01','h01v01','h02v01','h03v01','h04v01','h05v01','h06v01','h07v01','h08v01',
    'h09v01','h10v01','h25v01','h26v01','h27v01','h28v01','h29v01','h30v01','h31v01','h32v01','h33v01','h34v01','h35v01',
    'h00v02','h01v02','h02v02','h03v02','h04v02','h05v02','h06v02','h07v02','h08v02','h27v02','h28v02','h29v02',
    'h30v02','h31v02','h32v02','h33v02','h34v02','h35v02','h00v03','h01v03','h02v03','h03v03','h04v03','h05v03','h30v03',
    'h31v03','h32v03','h33v03','h34v03','h35v03','h00v04','h01v04','h02v04','h03v04','h32v04','h33v04','h34v04','h35v04',
    'h00v05','h01v05','h34v05','h35v05','h00v06','h35v06','h00v11','h35v11','h00v12','h01v12','h34v12','h35v12','h00v13',
    'h01v13','h02v13','h03v13','h32v13','h33v13','h34v13','h35v13','h00v14','h01v14','h02v14','h03v14','h04v14','h05v14',
    'h30v14','h31v14','h32v14','h33v14','h34v14','h35v14','h00v15','h01v15','h02v15','h03v15','h04v15','h05v15','h06v15',
    'h07v15','h08v15','h27v15','h28v15','h29v15','h30v15','h31v15','h32v15','h33v15','h34v15','h35v15','h00v16','h01v16',
    'h02v16','h03v16','h04v16','h05v16','h06v16','h07v16','h08v16','h09v16','h10v16','h25v16','h26v16','h27v16','h28v16',
    'h29v16','h30v16','h31v16','h32v16','h33v16','h34v16','h35v16','h00v17','h01v17','h02v17','h03v17','h04v17','h05v17',
    'h06v17','h07v17','h08v17','h09v17','h10v17','h11v17','h12v17','h13v17','h22v17','h23v17','h24v17','h25v17','h26v17',
    'h27v17','h28v17','h29v17','h30v17','h31v17','h32v17','h33v17','h34v17','h35v17') 
  if(tile%in%nodata) {print(paste(tile," not in region with data, skipping"));return(NULL)}
  ##
  h=as.numeric(substr(tile,2,3))
  v=as.numeric(substr(tile,5,6))
  ## Earth Width (m)
  ew=20015109.354*2
  ## Tile width or height = earth width / 36 = (20015109.354 + 20015109.354) / 36 = 1111950.5196666666 m
  tw=ew/36
  ## number of cells (1200 for 1km data)
  nc=1200
  ##1 km Cell size = tile width / cells
  cs=tw/nc
  ##X coordinate of tile lower-left corner = -20015109.354 + horizontal tile number * tile width
  llx= -(ew/2) + (h * tw)
  lly= -(ew/4) + ((17-v) * tw)
  ## projection
  proj="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" 
  ## make extent
  ext=extent(llx,llx+(nc*cs),lly,lly+(nc*cs))
  grid=raster(ext,nrows=nc,ncol=nc,crs=proj)
  names(grid)=tile
  writeRaster(grid,paste(outdir,"/",tile,".tif",sep=""),options=c("COMPRESS=LZW"),datatype="INT1U",overwrite=T)
  return(grid)
}

## run it and save the extents as an Rdata object for easy retrieval
gs=expand.grid(h=0:35,v=0:17)
gs$tile=paste("h",sprintf("%02d",gs$h),"v",sprintf("%02d",gs$v),sep="")
modtiles=lapply(1:nrow(gs),function(i) maketile(gs$tile[i]))
names(modtiles)=gs$tile


## function to confirm that this method results in identical (e same extent, number of rows and columns, projection, resolution, and origin) rasters as MODIS LST data
checkgrid<-function(tile){
  print(tile)
  ## path to MOD11A1 file for this tile to align grid/extent
  gridfile=list.files("/nobackupp4/datapool/modis/MOD11A2.005/2009.01.01",pattern=paste(tile,".*[.]hdf$",sep=""),recursive=T,full=T)[1]
  if(!file.exists(gridfile)) return(NULL)
  td=raster(paste("HDF4_EOS:EOS_GRID:\"",gridfile,"\":MODIS_Grid_8Day_1km_LST:LST_Day_1km",sep=""))
  td2=maketile(tile)
  return(compareRaster(td,td2,res=T,orig=T,rotation=T))
}

testing=F
if(testing){
  ## make the comparison for every tile
  dat=do.call(c,lapply(gs$tile,checkgrid))
  ## summarize the results
  table(dat)
}
