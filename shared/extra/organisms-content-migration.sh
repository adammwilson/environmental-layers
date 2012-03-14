# Full sequence of (bash) commands used to transform the original
# organisms@jupiter home directory structure to the new layers@atlas
# home directory structure. This was done after first rsyncing the
# entire ~organisms home directory from jupiter onto atlas at
# /home/organisms, and creating a new 'layers' user with home directory
# /home/layers.
#
# Jim Regetz
# NCEAS
# Created on 23-Feb-2012

export ORGANISMS="/home/organisms"
export LAYERS="/home/layers"
export REPO="."

#=======================================================================
# set up some directories
#=======================================================================

# create master home for all datasets
mkdir $LAYERS/data
mkdir $LAYERS/data/climate
mkdir $LAYERS/data/terrain
mkdir $LAYERS/data/land-cover
# create (temporary) home for associated readmes, documentation, etc
mkdir $LAYERS/documentation
mkdir $LAYERS/documentation/climate
mkdir $LAYERS/documentation/terrain
mkdir $LAYERS/documentation/land-cover
# create (temporary) home for processing code
mkdir $LAYERS/code
mkdir $LAYERS/code/climate
mkdir $LAYERS/code/terrain
mkdir $LAYERS/code/land-cover
# create (temporary?) home for everything else
mkdir $LAYERS/commons
mkdir $LAYERS/commons/terrain
mkdir $LAYERS/commons/land-cover
mkdir $LAYERS/commons/cruft

#=======================================================================
# carry out file migration, reorganization, and cleanup
#=======================================================================

#========#
# Marine #
#========#

# easy one ... just delete, as we're retaining this work on the original
# server
rm -rf Marine/


#=====#
# DEM #
#=====#

#
# SRTM 1km 
#

# migrate original rar file
mkdir $LAYERS/data/terrain/dem-cgiar-srtm-1km-ascii
mv $ORGANISMS/DEM/SRTM_1km_ASCII.rar $LAYERS/data/terrain/dem-cgiar-srtm-1km-ascii/srtm_1km_ascii.rar
# migrate clipped/reprojected bil
mv $ORGANISMS/Oregon_SRTM $LAYERS/data/terrain/dem-cgiar-srtm-1km-OR/

#
# CGIAR SRTM 90m
#

# SRTM 90m zips
mv $ORGANISMS/DEM/cgiarSrtm/archive $LAYERS/data/terrain/dem-cgiar-srtm-90m-zips

# SRTM 90m tiles and processed descendents
# first flatten out SRTM 90m directories
mv $ORGANISMS/DEM/cgiarSrtm/SRTM_90m_ASCII_4_1/Tiles_Resampled/Mosaiced \
   $ORGANISMS/DEM/cgiarSrtm/SRTM_90m_ASCII_4_1_Resampled_Mosaiced
mv $ORGANISMS/DEM/cgiarSrtm/SRTM_90m_ASCII_4_1/Tiles_Resampled \
   $ORGANISMS/DEM/cgiarSrtm/SRTM_90m_ASCII_4_1_Resampled
# now migrate to ~layers
mv $ORGANISMS/DEM/cgiarSrtm/SRTM_90m_ASCII_4_1 \
   $LAYERS/data/terrain/dem-cgiar-srtm-90m-asc
mv $ORGANISMS/DEM/cgiarSrtm/SRTM_90m_ASCII_4_1_Resampled \
   $LAYERS/data/terrain/dem-cgiar-srtm-90m-asc-resamp
mv $ORGANISMS/DEM/cgiarSrtm/SRTM_90m_ASCII_4_1_Resampled_Mosaiced \
   $LAYERS/data/terrain/dem-cgiar-srtm-90m-asc-resamp-mosaic

# SRTM 90m tile index (zipped)
mkdir $LAYERS/data/terrain/dem-cgiar-srtm-90m-index-zip
mv $ORGANISMS/DEM/cgiarSrtm/srtm-tile-index_v4.1_shp.zip \
   $LAYERS/data/terrain/dem-cgiar-srtm-90m-index-zip/

# SRTM 90m tile index (unzipped)
mv $ORGANISMS/DEM/cgiarSrtm/index \
   $LAYERS/data/terrain/dem-cgiar-srtm-90m-index

# move readme to deal with later
mv $ORGANISMS/DEM/cgiarSrtm/README.txt $LAYERS/documentation/terrain/cgiar-srtm-90m-readme.txt

# remove now-empty directory
rmdir $ORGANISMS/DEM/cgiarSrtm

#
# ASTER GDEM1
#

# --- first do cleanup ---
# remove duplicate files (confirmed by diffing)
rm $ORGANISMS/DEM/asterGdem/ListOfEurasiaDropouts.txt
rm $ORGANISMS/DEM/asterGdem/BadFileCheckLogVulcanAfterNewFiles.txt
rm $ORGANISMS/DEM/asterGdem/BadFileCheckLogBeforeCorrection.txt
# remove misc trivial/obsolete scripts
rm $ORGANISMS/DEM/asterGdem/translateToImg.sh
rm $ORGANISMS/DEM/asterGdem/incorrectTilesJuly2011/replacedJuly4/copyDownTifs.sh
rm $ORGANISMS/DEM/asterGdem/incorrectTilesJuly2011/checkBadAsterGDEMFiles.sh
# remove file listings and/or gdalinfo output with insufficient context
rm $ORGANISMS/DEM/asterGdem/incorrectTilesJuly2011/BadFileCheckLogVulcanAfterNewFiles.txt
rm $ORGANISMS/DEM/asterGdem/incorrectTilesJuly2011/ListOfEurasiaDropouts.txt
rm $ORGANISMS/DEM/asterGdem/incorrectTilesJuly2011/BadFileCheckLogBeforeCorrection.txt
rm $ORGANISMS/DEM/asterGdem/incorrectTilesJuly2011/replacedJuly4/Juy4NumFiles.txt
rm $ORGANISMS/DEM/asterGdem/incorrectTilesJuly2011/replacedJuly4/July4NumFiles.txt
rm $ORGANISMS/DEM/asterGdem/incorrectTilesJuly2011/replacedJuly4/July4Files.txt
# remove now-empty directory
rmdir $ORGANISMS/DEM/asterGdem/incorrectTilesJuly2011/replacedJuly4
# remove reeves-generated ArcGIS aux/rrd files
find $ORGANISMS/DEM/asterGdem/ -name "*.aux" -exec rm {} \;
find $ORGANISMS/DEM/asterGdem/ -name "*.rrd" -exec rm {} \;
# remove gdal-generated metadata files
find $ORGANISMS/DEM/asterGdem/ -name "*.tif.aux.xml" -exec rm {} \;
find $ORGANISMS/DEM/asterGdem/ -name "*.dem.aux.xml" -exec rm {} \;
# remove img tiles (for which we have the original tifs in all cases)
rm $ORGANISMS/DEM/asterGdem/TilesBelowN59/*.{img,img.aux.xml}

# migrate Robinson R scripts
mv -i $ORGANISMS/DEM/asterGdem/R_files \
      $LAYERS/code/terrain/asterGdem
# migrate Robinson GDEM1 assessment docs (incl QGIS project files)
mkdir $LAYERS/commons/terrain/gdem-v1
mv -i $ORGANISMS/DEM/asterGdem/*.ods \
      $LAYERS/commons/terrain/gdem-v1/
find $ORGANISMS/DEM/asterGdem -name "*.qgs" -exec mv {} \
      $LAYERS/commons/terrain/gdem-v1/ \;
# migrate remaining tree as the GDEM v1 dataset itself
mv $ORGANISMS/DEM/asterGdem \
   $LAYERS/data/terrain/dem-aster-gdem1-30m-orig

#
# ASTER GDEM2
#

# remove gdal-generated metadata files
rm $ORGANISMS/DEM/asterGdem2/*.tif.aux.xml
rm $ORGANISMS/DEM/asterGdem2/90m_NoPixelOffset/*.tif.aux.xml
rm $ORGANISMS/DEM/asterGdem2/90m_NoPixelOffset/Mosaiced/N59to60/*.tif.aux.xml

# first flatten out directories
mv $ORGANISMS/DEM/asterGdem2/90m_NoPixelOffset/Mosaiced/N59to60 \
   $ORGANISMS/DEM/asterGdem2-90m_NoPixelOffset-Mosaiced-N59to60
mv $ORGANISMS/DEM/asterGdem2/90m_NoPixelOffset/Mosaiced \
   $ORGANISMS/DEM/asterGdem2-90m_NoPixelOffset-Mosaiced
mv $ORGANISMS/DEM/asterGdem2/90m_NoPixelOffset \
   $ORGANISMS/DEM/asterGdem2-90m_NoPixelOffset
# now migrate to ~layers
mv $ORGANISMS/DEM/asterGdem2 \
   $LAYERS/data/terrain/dem-aster-gdem2-30m-orig
mv $ORGANISMS/DEM/asterGdem2-90m_NoPixelOffset \
   $LAYERS/data/terrain/dem-aster-gdem2-90m-resamp
mv $ORGANISMS/DEM/asterGdem2-90m_NoPixelOffset-Mosaiced \
   $LAYERS/data/terrain/dem-aster-gdem2-90m-resamp-mosaic-N60to83
mv $ORGANISMS/DEM/asterGdem2-90m_NoPixelOffset-Mosaiced-N59to60 \
   $LAYERS/data/terrain/dem-aster-gdem2-90m-resamp-mosaic-N59to60

#
# CDED
#

# remove duplicate files (confirmed by diffing)
rm $ORGANISMS/DEM/CanadaDED/084b\ \(1\).zip
rm $ORGANISMS/DEM/CanadaDED/Monday/114p\ \(1\).zip 
rm $ORGANISMS/DEM/CanadaDED/mosaicMostCanadaDem.sh
rm $ORGANISMS/DEM/CanadaDED/Monday/mergeCanada104105.{tif,aux,rrd}
# migrate zips
mkdir $LAYERS/data/terrain/dem-cded-zip
mv -i $ORGANISMS/DEM/CanadaDED/0*.zip \
      $LAYERS/data/terrain/dem-cded-zip/
mv -i $ORGANISMS/DEM/CanadaDED/Monday/1*.zip \
      $LAYERS/data/terrain/dem-cded-zip/
# migrate documentation
mv -i $ORGANISMS/DEM/CanadaDED/CanadaDEDTechRefManual.pdf \
      $LAYERS/documentation/terrain/cded-TechRefManual.pdf
# migrate some stuff to experimental, for now...
mkdir $LAYERS/commons/terrain/dem-cded
# ... vrt files, what the heck
mv -i $ORGANISMS/DEM/CanadaDED/*demFile.vrt \
      $LAYERS/commons/terrain/dem-cded/
# ... misc scripts and log files
mv -i $ORGANISMS/DEM/CanadaDED/in*DemFile.txt \
      $LAYERS/commons/terrain/dem-cded/
mv -i $ORGANISMS/DEM/CanadaDED/unzipFiles.sh \
      $LAYERS/commons/terrain/dem-cded/unzipFiles_0.sh
mv -i $ORGANISMS/DEM/CanadaDED/Monday/unzipFiles.sh \
      $LAYERS/commons/terrain/dem-cded/unzipFiles_1.sh
mv -i $ORGANISMS/DEM/CanadaDED/{.,/Monday}/*.sh \
      $LAYERS/commons/terrain/dem-cded/
mv -i $ORGANISMS/DEM/CanadaDED/*.log \
      $LAYERS/commons/terrain/dem-cded/
mv -i $ORGANISMS/DEM/CanadaDED/moscomps \
      $LAYERS/commons/terrain/dem-cded/
mv -i $ORGANISMS/DEM/CanadaDED/Monday/filesToMosaic \
      $LAYERS/commons/terrain/dem-cded/
mv -i $ORGANISMS/DEM/CanadaDED/Monday/*Log \
      $LAYERS/commons/terrain/dem-cded/
# remove extracted data (no plans to use it), after determining that these are
# the set of files matching globs "*.dem" or "*_fgdc_*.{xml,html}"
# # ---- bash code ---#
#  # list out all files contained in the zips
#  cd $LAYERS/data/terrain/dem-cded-zip && ls \
#    | xargs -I+ sh -c 'unzip -l + | grep -e " 200[7-9]-"' \
#    | awk '{print $4}' > /home/layers/cded-extracts.txt
# # ----- R code -----#
#  # read in the extracted file names
#  extracts <- scan("/home/layers/cded-extracts.txt", what="")
#  # read in all CDED files stored in the organisms home dir
#  files <- list.files(path=c("/home/organisms/DEM/CanadaDED",
#       "/home/organisms/DEM/CanadaDED/Monday"))
#  # note that all zip contents had been extracted (...based on filename)
#  all(extracts %in% files)
#  ## [1] TRUE
#  # isolate files *not* extracted from the zips
#  others <- setdiff(files, extracts)
#  # show that all extracted filenames match one of two patterns
#  all(grepl("dem$", extracts) | grepl("_fgdc_", extracts))
#  ## [1] TRUE
#  # and show that no other files match these
#  any(grepl("dem$", others) | grepl("_fgdc_", others))
#  ## [1] FALSE
# # ------------------#
rm $ORGANISMS/DEM/CanadaDED/*.dem
rm $ORGANISMS/DEM/CanadaDED/*_fgdc_*.{xml,html}
rm $ORGANISMS/DEM/CanadaDED/Monday/*.dem
rm $ORGANISMS/DEM/CanadaDED/Monday/*_fgdc_*.{xml,html}
# now remove items of uncertain origin
rm $ORGANISMS/DEM/CanadaDED/*.dem.aux.xml
rm $ORGANISMS/DEM/CanadaDED/Monday/*.dem.aux.xml
rm $ORGANISMS/DEM/CanadaDED/*dem?.{aux,rrd}
rm $ORGANISMS/DEM/CanadaDED/Monday/*dem?.{aux,rrd}
rm $ORGANISMS/DEM/CanadaDED/*demFile.{aux,rrd,vrt.aux,tif.aux.xml,tif,vrt.rrd}
rm $ORGANISMS/DEM/CanadaDED/CDEMPlySRTMDf.aux
rm -r $ORGANISMS/DEM/CanadaDED/cdemplysrtmdf
rm -r $ORGANISMS/DEM/CanadaDED/info
# what the hell, remove other generated tifs because I really can't account for
# the details of their creation...
rm $ORGANISMS/DEM/CanadaDED/CanadaDemMos*.{tif,rrd,aux,aux.xml,tif.xml,img,vat.dbf}
rm $ORGANISMS/DEM/CanadaDED/mergeCanada104105.{tif,aux,rrd}

# remove now-empty directories
rmdir $ORGANISMS/DEM/CanadaDED/Monday
rmdir $ORGANISMS/DEM/CanadaDED


#
# FUSED DEM 
#

# remove gdal-generated metadata files
rm $ORGANISMS/DEM/GlobalProduct/*.tif.aux.xml
# migrate to ~layers
mv $ORGANISMS/DEM/GlobalProduct $LAYERS/data/terrain/dem-fused

#
# GTOPO30
#

# migrate Nunokawa-clipped data -- first version
mkdir $LAYERS/data/terrain/dem-usgs-gtopo30-clipped-nunokawa-v1
find $ORGANISMS/DEM/usgsGTOPO30 -name "*_Clipped.dem" \
     -exec mv {} $LAYERS/data/terrain/dem-usgs-gtopo30-clipped-nunokawa-v1 \;
# migrate Nunokawa-clipped data -- second version
mv -i $ORGANISMS/DEM/Yuni/Data/GTOPO30 \
      $LAYERS/data/terrain/dem-usgs-gtopo30-clipped-nunokawa-v2

# migrate Robinson-clipped data
mkdir $LAYERS/data/terrain/dem-usgs-gtopo30-clipped-robinson
mv $ORGANISMS/DEM/asterGdem/N59to81_E20to59/e020n90/E020N90_clipped.dem \
   $LAYERS/data/terrain/dem-usgs-gtopo30-clipped-robinson/
mv $ORGANISMS/DEM/asterGdem/N59to81_E140to179/e140n90/E140N90_clipped.dem \
   $LAYERS/data/terrain/dem-usgs-gtopo30-clipped-robinson/
mv $ORGANISMS/DEM/asterGdem/N59to81_W140to101/w140n90/W140N90_clipped.dem \
   $LAYERS/data/terrain/dem-usgs-gtopo30-clipped-robinson/
mv $ORGANISMS/DEM/asterGdem/N59to81_E60to99/USGS_ErosDEM_N59to81E60to99/E060N90_clipped.dem \
   $LAYERS/data/terrain/dem-usgs-gtopo30-clipped-robinson/
mv $ORGANISMS/DEM/asterGdem/N59to81_W60to21/w060n90/W060N90_clipped.dem \
   $LAYERS/data/terrain/dem-usgs-gtopo30-clipped-robinson/
mv $ORGANISMS/DEM/asterGdem/N59to81_W100to61/w100n90/W100N90_clipped.dem \
   $LAYERS/data/terrain/dem-usgs-gtopo30-clipped-robinson/
mv $ORGANISMS/DEM/asterGdem/N59to81_E100to139/e100n90/E100N90_clipped.dem \
   $LAYERS/data/terrain/dem-usgs-gtopo30-clipped-robinson/
mv $ORGANISMS/DEM/asterGdem/N59to81_W180to141/w180n90/W180N90_clipped.dem \
   $LAYERS/data/terrain/dem-usgs-gtopo30-clipped-robinson/
mv $ORGANISMS/DEM/asterGdem/N59to81_W20toE19/w020n90/W020N90_clipped.dem \
   $LAYERS/data/terrain/dem-usgs-gtopo30-clipped-robinson/
# remove mysterious extra GeoTIFF (sans extension) with timestamp similar to
# the above, but that was mysteriously the same as the like-named
# clipped-nunokawa-v1 file (created months later)
rm $ORGANISMS/DEM/asterGdem/N59to81_E60to99/USGS_ErosDEM_N59to81E60to99/E060N90_Clipped

# remove gdal-generated metadata file
rm $ORGANISMS/DEM/usgsGTOPO30/e020n90/*.DEM.aux.xml
# remove Mac OS X file metadata cruft
find $ORGANISMS/DEM/usgsGTOPO30 -name ".DS_Store" -exec rm {} \;
find $ORGANISMS/DEM/usgsGTOPO30 -name "._*" -exec rm {} \;
# now migrate original (?) GTOPO30 data
mv $ORGANISMS/DEM/usgsGTOPO30 $LAYERS/data/terrain/dem-usgs-gtopo30
# confirmed that we had identical copies of the above GTOPO DEM data
# # ---- bash code ---#
# for i in `find $ORGANISMS/DEM/asterGdem -name "*.DEM" -exec basename {} \;`
# do
#   diff -q `find $ORGANISMS/DEM/asterGdem -name $i` \
#           `find $LAYERS/data/terrain/dem-usgs-gtopo30 -name $i`
# done
# # ------------------#
# (and in fact manually confirmed that the containing directories themselves
#  were duplicated, albeit with different names)
# ...so remove the duplicates...
find $ORGANISMS/DEM/asterGdem -name "*.DEM" | sed 's/\/[EW].*//' | xargs -I+ rm -r +

#
# GMTED2010
#

# remove gdal-generated metadata files
rm $ORGANISMS/GMTED2010/*.tif.aux.xml
# remove ArcMap(?) schema lock file
rm $ORGANISMS/GMTED2010/GMTED2010_Spatial_Metadata/GMTED2010_Spatial_Metadata.shp.IGSKMNCNWK00726.6284.8844.sr.lock
# migrate original (I think) downloaded data (OR only)
mkdir $LAYERS/data/terrain/dem-gmted2010-mea300-OR-tiles
mv $ORGANISMS/GMTED2010/30N150W_20101117_gmted_mea300.tif \
   $LAYERS/data/terrain/dem-gmted2010-mea300-OR-tiles/
mv $ORGANISMS/GMTED2010/30N120W_20101117_gmted_mea300.tif \
   $LAYERS/data/terrain/dem-gmted2010-mea300-OR-tiles/
# migrate merged OR tiles (i think)
mkdir $LAYERS/data/terrain/dem-gmted2010-mea300-OR-mosaic
mv $ORGANISMS/GMTED2010/OR_Coverage.tif \
   $LAYERS/data/terrain/dem-gmted2010-mea300-OR-mosaic/
# migrate OR clipped/reprojected data
mkdir $LAYERS/data/terrain/dem-gmted2010-mea300-OR-clip-proj-sinu-tif
mv $ORGANISMS/GMTED2010/OR_ClippedToMODIS_InSinu.tif \
   $LAYERS/data/terrain/dem-gmted2010-mea300-OR-clip-proj-sinu-tif/
mkdir $LAYERS/data/terrain/dem-gmted2010-mea300-OR-clip-proj-sinu-bil
mv $ORGANISMS/GMTED2010/OR_ClippedToMODIS_InSinu.{prj,hdr,bil} \
   $LAYERS/data/terrain/dem-gmted2010-mea300-OR-clip-proj-sinu-bil/
# migrate shapefile
mv $ORGANISMS/GMTED2010/GMTED2010_Spatial_Metadata \
   $LAYERS/data/terrain/dem-gmted2010-spatial-metadata
# migrate documentation
mv $ORGANISMS/GMTED2010/of2011-1073.pdf $LAYERS/documentation/terrain/gmted2010-report.pdf
mv $ORGANISMS/GMTED2010/ReadMe $LAYERS/documentation/terrain/gmted2010-readme.txt
# remove now-empty directory
rmdir $ORGANISMS/GMTED2010

#
# Nunokawa content
#

# regetz notes about nunokawa boundary datasets
# aster2/aster2_*_82N.tif             # Int16 59N-82N GDEM2 elevation
# aster2/aster2_*_below.tif           # Int16 59N-60N GDEM2 elevation
# aster2/aster2_*_above.tif           # Int16 60N-61N GDEM2 elevation
# aster2/aster2_*_straddle.tif        # Int16 59N-61N GDEM2 elevation (GDEM2 above, GDEM2 below)
# aster2/aster2_*_straddle_a.tif      # Flt32 59N-61N GDEM2-based aspect
# aster2/aster2_*_straddle_s.tif      # Flt32 59N-61N GDEM2-based slope
# aster2/aster2_*_below_blendgau.tif  # Flt32 59N-60N GDEM2/SRTM blended elevation
# aster2/fused_*_straddle.tif         # Int16 59N-61N GDEM2/SRTM unblended elevation (GDEM2 above, SRTM below)
# aster2/fused_*_straddle_a.tif       # Flt32 59N-61N unblended-based aspect
# aster2/fused_*_straddle_s.tif       # Flt32 59N-61N unblended-based aspect
# aster2/fused_*_blendgau.tif         # Int16 59N-61N GDEM2/SRTM blended elevation (GDEM2 above, blended below)
# aster2/fused_*_blendgau_a.tif       # Flt32 59N-61N blend-based aspect
# aster2/fused_*_blendgau_s.tif       # Flt32 59N-61N blend-based aspect
# srtm/srtm_*_below.tif               # Int16 59N-60N SRTM elevation
# srtm/srtm_*_below_a.tif             # Flt32 59N-60N SRTM-based aspect
# srtm/srtm_*_below_below_s.tif       # Flt32 59N-60N SRTM-based slope
# aster2/aster.vrt                    # VRT - GDEM2 from ~59N-82N (with 1/2-pixel offset)
# srtm/srtm.vrt                       # VRT - SRTM from ~55N-60N (with 1/2-pixel offset)

# remove duplicate file
rm $ORGANISMS/DEM/Yuni/Data/aster2/fused_w180w141
# remove redundant backup files (but leaving some others)
rm $ORGANISMS/DEM/Yuni/scripts/toAnalyzeData/aspect.r~
rm $ORGANISMS/DEM/Yuni/scripts/toAnalyzeData/check.r~
rm $ORGANISMS/DEM/Yuni/scripts/toAnalyzeData/meanElv.r~
rm $ORGANISMS/DEM/Yuni/scripts/toAnalyzeData/meanElv_onlyN59.r~
rm $ORGANISMS/DEM/Yuni/scripts/toAnalyzeData/
rm $ORGANISMS/DEM/Yuni/scripts/toProduceData/assembling.r~
rm $ORGANISMS/DEM/Yuni/scripts/toProduceData/gaussian.r~
rm $ORGANISMS/DEM/Yuni/scripts/toProduceData/mkVrt_Tiff.r~
rm $ORGANISMS/DEM/Yuni/scripts/toProduceData/slope_aspect.r~

# migrate scripts
mv $ORGANISMS/DEM/Yuni/scripts \
   $LAYERS/code/terrain/nunokawa-scripts
# migrate documents
mv -i $ORGANISMS/DEM/Yuni/documents \
      $LAYERS/documentation/terrain/nunokawa-documents
mv -i $ORGANISMS/DEM/Yuni/missing_gdem2_tiles.txt \
      $LAYERS/documentation/terrain/nunokawa-documents/
mv -i $ORGANISMS/DEM/Yuni/metadata.txt \
      $LAYERS/documentation/terrain/nunokawa-documents/
# migrate remaining Data contents to 60N boundary dir
mv -i $ORGANISMS/DEM/Yuni/Data \
      $LAYERS/commons/terrain/north-60

# remove now-empty directories
rmdir $ORGANISMS/DEM/Yuni
rmdir $ORGANISMS/DEM

#
# Remaining DEM content
#

mv $ORGANISMS/DEM/DEM_ProcessingScripts $LAYERS/code/terrain/
mv $ORGANISMS/DEM/ReadMeInProgress.txt $LAYERS/documentation/terrain/misc-partial-readme.txt


rm $ORGANISMS/DEM/checklog
mv $ORGANISMS/DEM/checkBadAsterGDEMFiles.sh $LAYERS/commons/cruft/
mv $ORGANISMS/DEM/CheckGDEMLog.txt $LAYERS/commons/cruft/


#=========#
# CLIMATE #
#=========#

#
# CRU
#

# migrate original gz data from CRU (or so it appears...)
mkdir $LAYERS/data/climate/cru-ts-3.0-1901-2006-gzip
mv $ORGANISMS/CRU_TS_3.0/*.gz $LAYERS/data/climate/cru-ts-3.0-1901-2006-gzip/
# I confirmed that the gunzipped files are identical to the gz contents (by
# gunzipping to /tmp and then diffing):w
# ...so let's delete the uncompressed versions
rm $ORGANISMS/CRU_TS_3.0/cru_ts_3_00.1901.2006.pre.nc
rm $ORGANISMS/CRU_TS_3.0/cru_ts_3_00.1901.2006.tmn.nc
rm $ORGANISMS/CRU_TS_3.0/cru_ts_3_00.1901.2006.tmp.nc
rm $ORGANISMS/CRU_TS_3.0/cru_ts_3_00.1901.2006.tmx.nc
# migrate extracted ASCII grid files
mv $ORGANISMS/CRU_TS_3.0/PRE $LAYERS/data/climate/cru-ts-3.0-1967-2006-asc-pre
mv $ORGANISMS/CRU_TS_3.0/TMN $LAYERS/data/climate/cru-ts-3.0-1967-2006-asc-tmn
mv $ORGANISMS/CRU_TS_3.0/TMP $LAYERS/data/climate/cru-ts-3.0-1967-2006-asc-tmp
mv $ORGANISMS/CRU_TS_3.0/TMX $LAYERS/data/climate/cru-ts-3.0-1967-2006-asc-tmx
# migrate R script for asc extraction
mv -i $ORGANISMS/CRU_TS_3.0/R/cru_3.0_data_extract.r $LAYERS/code/climate/
# rename then migrate readmes
rename 's/(Read_Me)/clim-cru-ts-3.0-\1/' $ORGANISMS/CRU_TS_3.0/Read_Me_*.txt
mv $ORGANISMS/CRU_TS_3.0/*Read_Me_*.txt $LAYERS/documentation/climate/
# now remove empty directory structure
find $ORGANISMS/CRU_TS_3.0 -depth -type d -exec rmdir {} \;

#
# GSOD
#

# for now, just move the whole thing as-is
mv $ORGANISMS/gsod $LAYERS/data/climate/gsod

#
# GHCN
#

# for now, just move the whole thing as-is
mv $ORGANISMS/ghcn $LAYERS/data/climate/ghcn

#
# MODIS LST
#

# migrate MOD11A1 V4 daily 1km L3 LST (March 1-7, 2000)
mv $ORGANISMS/MODIS\ LST/MOD11A1.004 $LAYERS/data/climate/MOD11A1.004-daily-1km-L3-LST/
# migrate MOD11A2 V4 8-day 1km L3 LST (March 5, 2000)
mkdir $LAYERS/data/climate/MOD11A2.004-8day-1km-L3-LST
mv $ORGANISMS/MODIS\ LST/2000.03.05 $LAYERS/data/climate/MOD11A2.004-8day-1km-L3-LST/
# also remove identical copies of the MOD11A2 data (confirmed by diffing)
rm $ORGANISMS/Modis_2000.03.05/MOD11A2.A2000065.*.hdf
rm $ORGANISMS/Modis_2000.03.05/MOD11A2.A2000065.*.hdf.xml
rmdir $ORGANISMS/Modis_2000.03.05
# remove now-empty directory
rmdir $ORGANISMS/MODIS\ LST

#
# MODIS LST -- Oregon
#

# migrate original MODIS OR hdfs (2001-2010?)
mkdir $LAYERS/data/climate/MOD11A1.004-OR-orig
mv $ORGANISMS/MODIS_LST_Oregon/MOD11A1.*.hdf \
   $LAYERS/data/climate/MOD11A1.004-OR-orig/
mv $ORGANISMS/MODIS_LST_Oregon/MOD11A1.*.hdf.xml \
   $LAYERS/data/climate/MOD11A1.004-OR-orig/
# migrate original MODIS OR hdfs (2000, not used)
mkdir $LAYERS/data/climate/MOD11A1.004-OR-orig-2000
mv $ORGANISMS/MODIS_LST_Oregon/2000\ _WithheldFromAVG/MOD11A1.A2000*.hdf \
   $LAYERS/data/climate/MOD11A1.004-OR-orig-2000/
mv $ORGANISMS/MODIS_LST_Oregon/2000\ _WithheldFromAVG/MOD11A1.A2000*.hdf.xml \
   $LAYERS/data/climate/MOD11A1.004-OR-orig-2000/
# migrate ClearDayCov OR img extracts (2001-2010?)
mkdir $LAYERS/data/climate/MOD11A1.004-OR-clearday-img
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/MOD11A1.*.img \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-img/
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/MOD11A1.*.img.aux.xml \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-img/
# migrate ClearDayCov OR img extracts (2000, not used)
mkdir $LAYERS/data/climate/MOD11A1.004-OR-clearday-img-2000
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/2000_WithheldFromAVG/MOD11A1.A2000*.005_ClearDay.img \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-img-2000/
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/2000_WithheldFromAVG/MOD11A1.A2000*.005_ClearDay.img.aux.xml \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-img-2000/

# migrate renamed ClearDayCov OR img extracts (2001-2010?)
mkdir $LAYERS/data/climate/MOD11A1.004-OR-clearday-img-renamed
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/ByDate/*.img \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-img-renamed/
# migrate renamed ClearDayCov OR img extracts (2000, not used)
mkdir $LAYERS/data/climate/MOD11A1.004-OR-clearday-img-renamed-2000
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/ByDate/2000_WithheldFromAVG/MOD11A1.A.2000.*.img \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-img-renamed-2000/

# migrate renamed ClearDayCov OR img extracts with corrected pixels
mkdir $LAYERS/data/climate/MOD11A1.004-OR-clearday-img-renamed-fixed
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/ByDate/BadPixelsCorrected_Tiles/*.img \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-img-renamed-fixed/
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/ByDate/BadPixelsCorrected_Tiles/*.img.aux.xml \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-img-renamed-fixed/

# migrate ClearDayCov OR daily avg tiles
mkdir $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-tiles-img
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day*.img \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-tiles-img/
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/day*.img.aux.xml \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-tiles-img/
# migrate ClearDayCov OR daily avg mosaic imgs
mkdir $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-mosaics-img
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day*_Mosaic_DailyAvg_Scaled.img \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-mosaics-img
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day*_Mosaic_DailyAvg_Scaled.img.aux.xml \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-mosaics-img
# migrate ClearDayCov OR daily avg mosaic bils
mkdir $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-mosaics-bil
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day*_Mosaic_DailyAvg_Scaled.prj \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-mosaics-bil/
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day*_Mosaic_DailyAvg_Scaled.bil \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-mosaics-bil/
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day*_Mosaic_DailyAvg_Scaled.bil.aux.xml \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-mosaics-bil/
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day*_Mosaic_DailyAvg_Scaled.hdr \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-mosaics-bil/

# migrate ClearDayCov OR monthly avg tiles
mkdir $LAYERS/data/climate/MOD11A1.004-OR-clearday-monthlyavg-tiles-img
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/*_AVG_Scaled.img \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-monthlyavg-tiles-img/
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/*_AVG_Scaled.img.aux.xml \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-monthlyavg-tiles-img/
# migrate ClearDayCov OR monthly avg mosaic imgs
mkdir $LAYERS/data/climate/MOD11A1.004-OR-clearday-monthlyavg-mosaics-img
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/*Mosaic_MonthlyAvg_Scaled.img \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-monthlyavg-mosaics-img/
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/*Mosaic_MonthlyAvg_Scaled.img.aux.xml \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-monthlyavg-mosaics-img/
# migrate ClearDayCov OR monthly avg mosaic bils
mkdir $LAYERS/data/climate/MOD11A1.004-OR-clearday-monthlyavg-mosaics-bil
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/*Mosaic_MonthlyAvg_Scaled.prj \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-monthlyavg-mosaics-bil/
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/*Mosaic_MonthlyAvg_Scaled.bil \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-monthlyavg-mosaics-bil/
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/*Mosaic_MonthlyAvg_Scaled.bil.aux.xml \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-monthlyavg-mosaics-bil/
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/*Mosaic_MonthlyAvg_Scaled.hdr \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-monthlyavg-mosaics-bil/

# migrate *old* (wrong?) ClearDayCov OR daily avg tiles
mkdir $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-tiles-img-OLD
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_DailyAverage_Tiles_Old/Day_*_Average_Scaled_*.img \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-tiles-img-OLD/
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_DailyAverage_Tiles_Old/Day_*_Average_Scaled_*.img.aux.xml \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-tiles-img-OLD/
# migrate *old* (wrong?) ClearDayCov OR daily avg mosaics
mkdir $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-mosaics-img-OLD
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_DailyAverage_Mosaic_Old/Day_*_ClearDay_Average.img \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-mosaics-img-OLD/
mv $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_DailyAverage_Mosaic_Old/Day_*_ClearDay_Average.img.aux.xml \
   $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-mosaics-img-OLD/

# remove identical copies of (old) ClearDayCov daily avgs for tile h08v04 
# *Note*: I first confirmed that these are duplicate copies:
# for i in {1..366};
# do
#   diff -q $ORGANISMS/MODIS_LST_Oregon/DailyAvg1_old/Day_"$i"_Average_Scaled.img \
#           $LAYERS/data/climate/MOD11A1.004-OR-clearday-dailyavg-tiles-img-OLD/Day_"$i"_Average_Scaled_h08v04.img
# done
rm $ORGANISMS/MODIS_LST_Oregon/DailyAvg1_old/Day_{1..366}_Average_Scaled.img
rm $ORGANISMS/MODIS_LST_Oregon/DailyAvg1_old/Day_{1..366}_Average_Scaled.img.aux.xml
# remove now-empty directory
rmdir $ORGANISMS/MODIS_LST_Oregon/DailyAvg1_old

# remove misc bil statistics (stx) files)
rm $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day11_Mosaic_DailyAvg_Scaled.stx
rm $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/Day1_Mosaic_DailyAvg_Scaled.stx
rm $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/AprMosaic_MonthlyAvg_Scaled.stx

# remove pyc file
rm $ORGANISMS/MODIS_LST_Oregon/GDAL_Extract_ClearDay_from_HDF_Files.pyc

# move and (if needed) rename documentation files
mv -i $ORGANISMS/MODIS_LST_Oregon/1\ ReadMe\ First.txt \
      $LAYERS/documentation/climate/clim-MOD11A1.004-OR-orig-readme.txt
mv -i $ORGANISMS/MODIS_LST_Oregon/MODIS_Oregon_ProcessingInfo.doc \
      $LAYERS/documentation/climate/
mv -i $ORGANISMS/MODIS_LST_Oregon/2000\ _WithheldFromAVG/ReadMe \
      $LAYERS/documentation/climate/clim-MOD11A1.004-OR-orig-2000-readme.txt
mv -i $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/1\ ReadMe\ First.txt \
      $LAYERS/documentation/climate/clim-MOD11A1.004-OR-clearday-readme.txt
mv -i $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/ByDate/BadPixelsCorrected_Tiles/1\ ReadMe\ First \
      $LAYERS/documentation/climate/clim-MOD11A1.004-OR-clearday-img-renamed-fixed-readme.txt
      
# move scripts
mkdir $LAYERS/code/climate/modis-lst-oregon
mv -i $ORGANISMS/MODIS_LST_Oregon/GDAL_Extract_ClearDay_from_HDF_Files.py \
      $LAYERS/code/climate/modis-lst-oregon/
mv -i $ORGANISMS/MODIS_LST_Oregon/QCTagCheck \
      $LAYERS/code/climate/modis-lst-oregon/QCTagCheck.py
mv -i $ORGANISMS/MODIS_LST_Oregon/ConvertMODIStobil.py \
      $LAYERS/code/climate/modis-lst-oregon/
mv -i $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/DayAVGCalcs_h08v04TIles.r \
      $LAYERS/code/climate/modis-lst-oregon/
mv -i $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Tiles/DayAVGCalcs_h09v04TIles.r \
      $LAYERS/code/climate/modis-lst-oregon/
mv -i $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics/LST_Mosaicing_DailyAvgs.r \
      $LAYERS/code/climate/modis-lst-oregon/
mv -i $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/LST_Mosaicing_MonthlyAvgs.R \
      $LAYERS/code/climate/modis-lst-oregon/
mv -i $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics/Conversion_hdf_to_img.py \
      $LAYERS/code/climate/modis-lst-oregon/
mv -i $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/ByDate/BadPixelsCorrected_Tiles/h09v04Tiles_BadPixelCorrection.r \
      $LAYERS/code/climate/modis-lst-oregon/
mv -i $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/ByDate/BadPixelsCorrected_Tiles/h08v04Tiles_BadPixelCorrection.r \
      $LAYERS/code/climate/modis-lst-oregon/
mv -i $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/Convert\ MODIS\ Day\ of\ Year\ to\ Date\ File\ Name.py \
      $LAYERS/code/climate/modis-lst-oregon/
mv -i $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/MonthlyAvgCalcs_h08v04Tiles.r \
      $LAYERS/code/climate/modis-lst-oregon/
mv -i $ORGANISMS/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Tiles/MonthlyAvgCalcs_h09v04Tiles.r \
      $LAYERS/code/climate/modis-lst-oregon/

# move last few things...
mv -i $ORGANISMS/MODIS_LST_Oregon/hdf.txt \
      $LAYERS/commons/cruft/clim-MOD11A1.004-OR-orig-hdf.txt
mv -i $ORGANISMS/MODIS_LST_Oregon/SDS_PctFills.txt \
      $LAYERS/commons/cruft/clim-MOD11A1.004-OR-orig-SDS_PctFills.txt

# remove all the now-empty directories...
# tree $ORGANISMS/MODIS_LST_Oregon
# /home/organisms/MODIS_LST_Oregon
# .
# ├── 2000 _WithheldFromAVG
# └── ClearDayGDAL
#     ├── ClearDay_DailyAverage_Mosaic_Old
#     ├── ClearDay_DailyAverage_Tiles_Old
#     ├── ClearDay_Original_IMG_Extracts
#     │   ├── 2000_WithheldFromAVG
#     │   └── ByDate
#     │       ├── 2000_WithheldFromAVG
#     │       └── BadPixelsCorrected_Tiles
#     ├── NewClearDay_DailyAvgs_Mosaics
#     ├── NewClearDay_DailyAvgs_Tiles
#     ├── NewClearDay_MonthlyAvgs_Mosaics
#     └── NewClearDay_MonthlyAvgs_Tiles
find $ORGANISMS/MODIS_LST_Oregon -depth -type d -exec rmdir {} \;

# last but not least, deal with top level R directory
# ...move some (old? superceded?) scripts
mv -i $ORGANISMS/R/ReClass Clear Day Rasters h08v04.r \
      $LAYERS/code/climate/modis-lst-oregon/
mv -i $ORGANISMS/R/Calc Clear Day Daily Avg.r \
      $LAYERS/code/climate/modis-lst-oregon/
mv -i $ORGANISMS/R/Mosaic Clear Day Average Grids.r \
      $LAYERS/code/climate/modis-lst-oregon/
mv -i $ORGANISMS/R/Calc Clear Day Monthly Avg.r \
      $LAYERS/code/climate/modis-lst-oregon/
# don't know how this was created, but it's clearly derived data... 
mv -i $ORGANISMS/R/Modis_LSTdaily_climatology $LAYERS/data/clim-modis-lstdaily
# remove now-empty directory
rmdir $ORGANISMS/R

#
# TRMM
#

# this was an *empty* directory! removing...
rmdir $ORGANISMS/TRMM

#
# WorldClim
#

# migrate each one into its own directory
mkdir $LAYERS/data/climate/worldclim-bio-30s-esri-zip
mv $ORGANISMS/WorldClim/bio_30s_esri.zip $LAYERS/data/climate/worldclim-bio-30s-esri-zip/
mkdir $LAYERS/data/climate/worldclim-prec-30s-esri-zip
mv $ORGANISMS/WorldClim/prec_30s_esri.zip $LAYERS/data/climate/worldclim-prec-30s-esri-zip/
mkdir $LAYERS/data/climate/worldclim-tmax-30s-esri-zip
mv $ORGANISMS/WorldClim/tmax_30s_esri.zip $LAYERS/data/climate/worldclim-tmax-30s-esri-zip/
mkdir $LAYERS/data/climate/worldclim-tmean-30s-esri-zip
mv $ORGANISMS/WorldClim/tmean_30s_esri.zip $LAYERS/data/climate/worldclim-tmean-30s-esri-zip/
mkdir $LAYERS/data/climate/worldclim-tmin-30s-esri-zip
mv $ORGANISMS/WorldClim/tmin_30s_esri.zip $LAYERS/data/climate/worldclim-tmin-30s-esri-zip/
# remove now-empty directory
rmdir $ORGANISMS/WorldClim


#============#
# LAND COVER #
#============#

#
# GLCNMO
#

# for now, just move the whole thing as-is
mv $ORGANISMS/GLCNMO $LAYERS/commons/land-cover/

#
# GlobCover
#
# migrate what seems like the original data (?)
mkdir $LAYERS/data/land-cover/GlobCover-v2.2
mv -i $ORGANISMS/GlobCoverv2.2/GLOBCOVER_200412_200606_V2.2_Global_CLA* \
      $LAYERS/data/land-cover/GlobCover-v2.2/
# migrate mysterious derivatives of the above
mkdir $LAYERS/data/land-cover/GlobCover-v2.2-arcgis
mv -i $ORGANISMS/GlobCoverv2.2/globcov22_beh $LAYERS/data/land-cover/GlobCover-v2.2-arcgis/
mv -i $ORGANISMS/GlobCoverv2.2/globcoverv22 $LAYERS/data/land-cover/GlobCover-v2.2-arcgis/
# migrate reference info to documentation area
mv -i $ORGANISMS/GlobCoverv2.2/Globcover_* $LAYERS/documentation/land-cover/
# remove now-empty directory
rmdir $ORGANISMS/GlobCoverv2.2

#
# LandCover (whatever that is)
#

# remove gdal-generated metadata file
rm $ORGANISMS/LandCover/Layer10_Clip1.tif.aux.xml
# migrate original OR landcover tiles
mkdir $LAYERS/data/land-cover/lc-OR-tiles
mv $ORGANISMS/LandCover/Layer*_Clip[1-3].tif \
   $LAYERS/data/land-cover/lc-OR-tiles/
# migrate OR landcover mosaics
mkdir $LAYERS/data/land-cover/lc-OR-mosaic
mv $ORGANISMS/LandCover/Layer*_Mosaic.tif \
   $LAYERS/data/land-cover/lc-OR-mosaic/
# migrate OR clipped/reprojected landcover mosaics
mkdir $LAYERS/data/land-cover/lc-OR-mosaic-clip-proj-sinu
mv $ORGANISMS/LandCover/Layer*_ClippedToMODIS_Sinu.* \
   $LAYERS/data/land-cover/lc-OR-mosaic-clip-proj-sinu/
# migrate OR multiband clipped/reprojected landcover mosaic
mkdir $LAYERS/data/land-cover/lc-OR-mosaic-clip-proj-sinu-multiband
mv $ORGANISMS/LandCover/Final_Multiband_SinuGrid.* \
   $LAYERS/data/land-cover/lc-OR-mosaic-clip-proj-sinu-multiband/
# migrate documentation
mv $ORGANISMS/LandCover/ReadMe $LAYERS/documentation/land-cover/landcover-ReadMe.txt
mv $ORGANISMS/LandCover/LCT\ calculations.xlsx $LAYERS/documentation/land-cover/
mv $ORGANISMS/LandCover/landcover_method_v3.docx $LAYERS/documentation/land-cover/
# migrate scripts
mv $ORGANISMS/LandCover/CheckForNoDataValues_LandCoverFiles.txt $LAYERS/code/land-cover/
# remove now-empty directory
rmdir $ORGANISMS/LandCover

#
# Tree height
#

# migrate $ORGANISMS/Tree_height/Lefsky2010
mkdir $LAYERS/data/land-cover/treeheight-lefsky2010
mv $ORGANISMS/Tree_height/Lefsky2010/x2_080809_global.img \
   $LAYERS/data/land-cover/treeheight-lefsky2010/
mv $ORGANISMS/Tree_height/Lefsky2010/p_080809_global.img* \
   $LAYERS/data/land-cover/treeheight-lefsky2010/
# migrate associated publication do documentation area
mv $ORGANISMS/Tree_height/Lefsky2010/Lefsky\ et\ al.\ 2010\ GeoRLett.pdf \
   $LAYERS/documentation/land-cover/treeheight-Lefsky2010-GeoRLett.pdf
# remove now-empty directory
rmdir $ORGANISMS/Tree_height/Lefsky2010

# migrate $ORGANISMS/Tree_height/Simard2011
mkdir $LAYERS/data/land-cover/treeheight-simard2011
mv $ORGANISMS/Tree_height/Simard2011/Simard_Pinto_3DGlobalVeg_JGR.tif \
   $LAYERS/data/land-cover/treeheight-simard2011/
# remove gdal-generated metadata file
rm $ORGANISMS/Tree_height/Simard2011/Simard_Pinto_3DGlobalVeg_JGR.tif.aux.xml
# migrate clipped/reprojected file (bil)
mkdir $LAYERS/data/land-cover/treeheight-simard2011-OR-clip-proj-sinu-bil
mv $ORGANISMS/Tree_height/Simard2011/GlobalCanopy_ClippedToMODIS_InSinu.* \
   $LAYERS/data/land-cover/treeheight-simard2011-OR-clip-proj-sinu-bil/
# migrate documentation
mv $ORGANISMS/Tree_height/Simard2011/SimardEtAl2011_GlobalTreeHeight_WithLIDAR.pdf \
   $LAYERS/documentation/land-cover/treeheight-SimardEtAl_GlobalTreeHeight_WithLIDAR.pdf
mv $ORGANISMS/Tree_height/Simard2011/ReadMe \
   $LAYERS/documentation/land-cover/treeheight-ReadMe.txt
# remove remaining files as per instructions from Natalie Robinson
rm $ORGANISMS/Tree_height/Simard2011/Test_Clip.tif
rm $ORGANISMS/Tree_height/Simard2011/GDALTranslate_test.*
rm $ORGANISMS/Tree_height/Simard2011/GDALTranslate_DT_int16.*
# remove now-empty directory
rmdir $ORGANISMS/Tree_height/Simard2011

# lastely, remove now-empty Tree_height directory
rmdir $ORGANISMS/Tree_height


#=================#
# Remaining stuff #
#=================#

#
# top level directory
#

# move top level notes
mv $ORGANISMS/Oregon_CaseStudy_FileProcessingInfo $LAYERS/documentation/
mv $ORGANISMS/ReProjection_To_MODIS $LAYERS/documentation/
mv $ORGANISMS/README.txt $LAYERS/documentation/organisms-homedir-readme.txt
mv $ORGANISMS/CHANGES.txt $LAYERS/documentation/organisms-homedir-changes.txt
# move old file extension summary
mv -i $ORGANISMS/file-extension-summary.txt $LAYERS/commons/cruft/

#
# Desktop
#

# move MRT installer to cruft
mv $ORGANISMS/Desktop/MRT_download_Linux $LAYERS/commons/cruft/
# remove orphaned bil metadata file
rm $ORGANISMS/Desktop/ASTER_Test.bil.aux.xml
# remove empty Desktop dir
rmdir $ORGANISMS/Desktop

#
# pyhdf
#

# move pyhdf package to cruft
mv $ORGANISMS/pyhdf $LAYERS/commons/cruft/

#
# GIS/GDD
#

# confirmed that /tmean contains unzipped tmean, except there are additional
# aux files presumably autogenerated by Arc -- I'm find with deleting them
# # ---- bash code ---#
# cd /tmp && unzip $ORGANISMS/GIS/GDD/tmean_30s_esri.zip
# diff -qr /tmp/tmean /home/organisms/GIS/GDD/tmean_30s_esri/tmean/
#  done
# # ------------------#
# delete the uncompressed tmean_30s_esri directory
rm -r $ORGANISMS/GIS/GDD/tmean_30s_esri

# confirmed that WorldClim zips are identical to what we already have elsewhere
# # ---- bash code ---#
# for var in {'prec','tmin','tmax','tmean','bio'}
# do
#   diff -s $ORGANISMS/GIS/GDD/"$var"_30s_esri.zip \
#           $LAYERS/data/climate/worldclim-"$var"-30s-esri-zip/*.zip
# done
#  done
# # ------------------#
# delete the duplicate worldclim zips
rm $ORGANISMS/GIS/GDD/prec_30s_esri.zip
rm $ORGANISMS/GIS/GDD/tmin_30s_esri.zip
rm $ORGANISMS/GIS/GDD/tmax_30s_esri.zip
rm $ORGANISMS/GIS/GDD/tmean_30s_esri.zip
rm $ORGANISMS/GIS/GDD/bio_30s_esri.zip

# confirmed that all MODIS files here are identical to (though in fact they are
# only a subset of) what we already have elsewhere
# # ---- bash code ---#
# for dir in `ls $ORGANISMS/GIS/GDD/MOD11A1.004`
#  do
#    echo checking "$dir"...
#    ls $ORGANISMS/GIS/GDD/MOD11A1.004/"$dir" \
#     | xargs -I+ \
#         diff -q $ORGANISMS/GIS/GDD/MOD11A1.004/"$dir"/+ \
#                 $LAYERS/data/climate/MOD11A1.004-daily-1km-L3-LST/"$dir"/+
#  done
# # ------------------#
# delete the duplicate MODIS data
# note that the last 3 dirs were already empty...
rm $ORGANISMS/GIS/GDD/MOD11A1.004/2000.03.01/MOD11A1.A2000*.{hdf,hdf.xml}
rmdir $ORGANISMS/GIS/GDD/MOD11A1.004/2000.03.01
rm $ORGANISMS/GIS/GDD/MOD11A1.004/2000.03.02/MOD11A1.A2000*.{hdf,hdf.xml}
rmdir $ORGANISMS/GIS/GDD/MOD11A1.004/2000.03.02
rm $ORGANISMS/GIS/GDD/MOD11A1.004/2000.03.03/MOD11A1.A2000*.{hdf,hdf.xml}
rmdir $ORGANISMS/GIS/GDD/MOD11A1.004/2000.03.03
rm $ORGANISMS/GIS/GDD/MOD11A1.004/2000.03.04/MOD11A1.A2000*.{hdf,hdf.xml}
rmdir $ORGANISMS/GIS/GDD/MOD11A1.004/2000.03.04
rmdir $ORGANISMS/GIS/GDD/MOD11A1.004/2000.03.05
rmdir $ORGANISMS/GIS/GDD/MOD11A1.004/2000.03.06
rmdir $ORGANISMS/GIS/GDD/MOD11A1.004/2000.03.07
rmdir $ORGANISMS/GIS/GDD/MOD11A1.004

# migrate remaining text (script?) file
mv $ORGANISMS/GIS/GDD/map\ algebra.txt $LAYERS/code/climate/gdd-worldclim-tmean-map-algebra.txt
# remove empty directory
rmdir $ORGANISMS/GIS/GDD

#
# GIS/terrain
#

# remove empty test directory
rmdir $ORGANISMS/GIS/terrain/test
# for now just migrate everything else to experimental area
mv $ORGANISMS/GIS/terrain $LAYERS/commons/terrain/arcgis

# remove empty directory
rmdir $ORGANISMS/GIS

#
# Oregon
#

# for now just migrate to experimental area
mv $ORGANISMS/Oregon $LAYERS/commons/oregon

#
# steph
#

# delete as per verbal instructions from Stephanie Pau
rm -rf $ORGANISMS/steph

#
# topo
#

# first migrate the whole thing to new commons area
mv $ORGANISMS/topo $LAYERS/commons/
# consolidate first set of dirs
mkdir $LAYERS/commons/topo/v1
mv -i $LAYERS/commons/topo/{experimental,scripts,tiles} \
      $LAYERS/commons/topo/v1/
# consolidate second set of dirs
mv -i $LAYERS/commons/topo/experimental_2/topo \
      $LAYERS/commons/topo/v2
rmdir $LAYERS/commons/topo/experimental_2

# remove ArcToolbox temporary files
rm $LAYERS/commons/topo/v2/topo/xx00003024.s
rm $LAYERS/commons/topo/v2/experimental/xx00013916.x
rm $LAYERS/commons/topo/v2/experimental/xx00003916.s
rm $LAYERS/commons/topo/v2/experimental/xx00013916.s

# consolidate first set of scripts into git repo
mv -i $LAYERS/commons/topo/v1/experimental/layers.aml \
      $REPO/terrain/research/oregon/arcgis/v1/
mv -i $LAYERS/commons/topo/v1/experimental/aggregate.aml \
      $REPO/terrain/research/oregon/arcgis/v1/
mv -i $LAYERS/commons/topo/v1/experimental/multiscalesmooth9a_clean.aml \
      $REPO/terrain/research/oregon/arcgis/v1/
mv -i $LAYERS/commons/topo/v1/experimental/tilemerge_oregon1.aml \
      $REPO/terrain/research/oregon/arcgis/v1/
mv -i $LAYERS/commons/topo/v1/scripts/unpacktiles.aml \
      $REPO/terrain/research/oregon/arcgis/v1/
mv -i $LAYERS/commons/topo/v1/scripts/unziptiles.py \
      $REPO/terrain/research/oregon/arcgis/v1/
mv -i $LAYERS/commons/topo/v1/scripts/mrvbf/mrvbf6g-a3.aml \
      $REPO/terrain/research/oregon/arcgis/v1/
mv -i $LAYERS/commons/topo/v1/scripts/mrvbf/pctl.aml \
      $REPO/terrain/research/oregon/arcgis/v1/
mv -i $LAYERS/commons/topo/v1/scripts/mrvbf/chunkproc.aml \
      $REPO/terrain/research/oregon/arcgis/v1/
mv -i $LAYERS/commons/topo/v1/scripts/mrvbf/pctl-limits.aml \
      $REPO/terrain/research/oregon/arcgis/v1/
mv -i $LAYERS/commons/topo/v1/scripts/mrvbf/gauss3 \
      $REPO/terrain/research/oregon/arcgis/v1/
# for now keep this mysterioud Windows binary around...
mv -i $LAYERS/commons/topo/v1/scripts/mrvbf/pctl.exe \
      $LAYERS/commons/topo/v1/
# remove now-empty scripts directory
rmdir $LAYERS/commons/topo/v1/scripts/mrvbf
rmdir $LAYERS/commons/topo/v1/scripts

# consolidate second set of scripts into git repo
mv -i $LAYERS/commons/topo/v2/experimental/layers.aml \
      $REPO/terrain/research/oregon/arcgis/v2/
mv -i $LAYERS/commons/topo/v2/experimental/aggregate.aml \
      $REPO/terrain/research/oregon/arcgis/v2/
mv -i $LAYERS/commons/topo/v2/experimental/multiscalesmooth9a_clean.aml \
      $REPO/terrain/research/oregon/arcgis/v2/
mv -i $LAYERS/commons/topo/v2/experimental/tilemerge_oregon1.aml \
      $REPO/terrain/research/oregon/arcgis/v2/
mv -i $LAYERS/commons/topo/v2/scripts/unpacktiles.aml \
      $REPO/terrain/research/oregon/arcgis/v2/
mv -i $LAYERS/commons/topo/v2/scripts/unziptiles.py \
      $REPO/terrain/research/oregon/arcgis/v2/
mv -i $LAYERS/commons/topo/v2/topo/mrvbf6g-a3.aml \
      $REPO/terrain/research/oregon/arcgis/v2/
mv -i $LAYERS/commons/topo/v2/topo/pctl.aml \
      $REPO/terrain/research/oregon/arcgis/v2/
mv -i $LAYERS/commons/topo/v2/topo/chunkproc.aml \
      $REPO/terrain/research/oregon/arcgis/v2/
mv -i $LAYERS/commons/topo/v2/topo/pctl-limits.aml \
      $REPO/terrain/research/oregon/arcgis/v2/
mv -i $LAYERS/commons/topo/v2/topo/gauss3 \
      $REPO/terrain/research/oregon/arcgis/v2/


#
# temp_benoit
#

# for now just migrate to experimental area
mv $ORGANISMS/temp_benoit $LAYERS/commons/


#=======================================================================
# fix up permissions and ownership
#=======================================================================

chgrp -R layers $LAYERS/data
find $LAYERS/data -type f -exec chmod 640 {} \;
find $LAYERS/data -type d -exec chmod g-s {} \;
find $LAYERS/data -type d -exec chmod 750 {} \;


#=======================================================================
# now migrate code into git repository clone
#=======================================================================

# nunokawa terrain scripts
mkdir terrain/research/gtopo30
mv -i code/terrain/nunokawa-scripts/toProduceData/clipUSGS.r \
      $REPO/terrain/research/gtopo30/
mv -i code/terrain/nunokawa-scripts/toAnalyzeData/check.r \
      $REPO/terrain/tests/
mv -i code/terrain/nunokawa-scripts/toAnalyzeData/meanElv_OnlyN59.r
      $REPO/terrain/research/north-60/
rm code/terrain/nunokawa-scripts/toAnalyzeData/meanElv.r 
rm code/terrain/nunokawa-scripts/toAnalyzeData/Deltas.r~
mv -i code/terrain/nunokawa-scripts/toAnalyzeData/Deltas.r \
      $REPO/terrain/research/north-60/
rm code/terrain/nunokawa-scripts/toAnalyzeData/rmse_cor.r~ 
mv -i code/terrain/nunokawa-scripts/toAnalyzeData/rmse_cor.r \
      $REPO/terrain/research/north-60/
rm code/terrain/nunokawa-scripts/toAnalyzeData/slope.r~
mv -i code/terrain/nunokawa-scripts/toAnalyzeData/{aspect,slope}.r
      $REPO/terrain/research/north-60/
mv -i code/terrain/nunokawa-scripts/toAnalyzeData/negativeTable.r \
      $REPO/terrain/research/north-60/
mv -i code/terrain/nunokawa-scripts/toProduceData/*.{r,r~} \
      $REPO/terrain/research/north-60/

# robinson terrain scripts
mv -i code/terrain/DEM_ProcessingScripts/Aster_CheckMosaicedTilesExtents.py \
      $REPO/terrain/tests/
mv -i code/terrain/DEM_ProcessingScripts/CheckPixelValuesAtOverlapZones.txt \
      $REPO/terrain/tests/
mv -i code/terrain/DEM_ProcessingScripts/Aster\&SRTM_* \
      $REPO/terrain/tests/
mv -i code/terrain/DEM_ProcessingScripts/Gaussian_Blend.r \
      $REPO/terrain/procedures/
mv -i terrain/tests/Aster\&SRTM_* \
      $REPO/terrain/procedures/
mv -i code/terrain/DEM_ProcessingScripts/SRTM_ClipToN59to60.txt \
      $REPO/terrain/procedures/
mv -i code/terrain/DEM_ProcessingScripts/Mosaicing_AllTiles_East\&WestHemispheres.txt \
      $REPO/terrain/procedures/
mv -i code/terrain/DEM_ProcessingScripts/AsterMosaicingScripts \
      $REPO/terrain/procedures/

# robinson land-cover check
mv -i code/land-cover/CheckForNoDataValues_LandCoverFiles.txt \
      $REPO/land-cover/tests/

# robinson/donoghue Oregon MODIS LST processing code
mkdir climate/research/oregon
mkdir climate/research/oregon/modis-lst
mv -i code/climate/modis-lst-oregon/*.{r,R,py} \
      $REPO/climate/research/oregon/modis-lst/

# misc old (authorless) climate code
mv -i code/climate/cru_3.0_data_extract.r \
      $REPO/climate/procedures/
mv -i code/climate/gdd-worldclim-tmean-map-algebra.txt \
      $REPO/climate/extra/

mv -i $LAYERS/code/terrain/R_files/AsterCheck_demAndnum.r \
      $REPO/terrain/test/
mv -i $LAYERS/code/terrain/R_files/PctNoLand.r \
      $REPO/terrain/research/gtopo30/

# set all file permissions to 644 before committing
find $REPO -type f -exec ^Cmod 644 {} \;

