### script to build the list of covariates for use by the "extraction_raster_covariates_study_area.R" script for precipitation
### should be run while in the environment set up by that script...

### Or build list of covariates to include
  covarf=data.frame(files=c(
#                    list.files(pattern="month.*_rescaled.*rst$",full=T),   # LST values
                    list.files(path,pattern="coord.*rst$",full=T,recursive=T),              # coordinates
                    list.files(path,pattern="SRTM.*rst$",full=T,recursive=T),                # topography
                    list.files(path,pattern="W_Layer.*M.rst$",full=T,recursive=T),              # LULC
                    list.files(path,pattern="DIST.rst$",full=T,recursive=T),         # DISTOC
                    list.files(path,pattern="W_Global.*M.rst$",full=T,recursive=T),         #canopy
                     list.files(path,pattern="[_]mean[_]|P20um[_]",full=T,recursive=T) #  MOD06
                    ),stringsAsFactors=F)

  covarf$var=c(
#  paste("mm_",c(10:12,1:9),sep=""),
  "x_OR83M","y_OR83M",
  "aspect","elev","slope",
  paste("LC",c(10,1:9),sep=""),
  "distoc","canheight",
  sub("OR03M_","",sub(".tif","",basename(list.files(path,pattern="[_]mean[_]|P20um[_]",recursive=T))))
  )

### add column to indicate temporally varying covariates (monthly data)
covarf$time=c(rep(0,17),rep(1:12,4))   # vector indicating which of the covar files have a month and which are static (0)


## check it
covarf


#write it out
write.table(covarf,file="covar.txt")
