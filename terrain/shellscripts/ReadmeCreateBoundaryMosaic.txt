May 5, 2011 / Rick Reeves
ReadMe file: Shell scripts to create the mosaic (3 arcsecond resolution) 
of CGIAR/SRTM and ASTER GDEM imagery spanning the 60 Degree North Latitude 
boundary within western Canada

Three shell scripts used:

1) "mosaicCgiarSrtmBdySRTM.sh" : creates mosaic from CGIAR/SRTM 5 degree tiles
    (3 arcsecond resolution) converted to .tif format from original ArcMap ASCII
    format.

2) "mosaicCgiarSrtmMBdyAster.sh" : creates mosaic from ASTER GDEM 1 degree tiles
   (1 arcsecond resolution) converted to .tif format from original ArcMap ASCII
   format. Note: incoming image tiles are resampled to 3 arcsecond resolution
   to create outgoing image mosaic,

3) "mosaicSrtmAsterPartsBdy.sh" : creates mosaic from the outputs from scripts
    1) and 2) (3 arcsecond resolution): 
                                            mergeCgiarAsterBdySRTM_BL.tif 
                                            mergeCgiarAsterBdyASTER_BL.tif

To create the ASTER / CGIAR boundary mosaic: 

1) make current directory '/data/project/organisms/rcr/AsterCgiarMerge/'
2) run the script "mosaicCgiarSrtmBdySRTM.sh"
3) run the script "mosaicSrtmMBdyAster.sh"
4) run the script "mosaicSrtmAsterPartsBdy.sh"

Note: These initial scripts do NOT specify a new image extent for any of the mosaic components.
      (gdalwarp parameter '-te xmin ymin xmax ymax' used to specify the extent)
      So for these initial *test* runs, the output image extent is derived from the input files.
 
      In 'production' runs, the extent of output mosaic tile compoents will be specified using -te;
      we may also set other 'gdalwarp' parameters to properly establish other output image parameters.
      