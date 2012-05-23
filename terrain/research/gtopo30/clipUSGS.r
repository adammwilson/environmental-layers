# Script for clipping each USGS GTOPO30 coverages to extent: N59 to N82 
# degree lat. (to match extent of ASTER GDEM tiles, for the use in 
# Check Part 2)  The clipped data will be saved at
#       "/data/project/organisms/DEM/Yuni/Data/GTOPO30/"
#
# Note: if I knew how to apply gdal_translate in R, the script could be 
#       less redundant.  
#
# December 9th 2011
# Yuina Nunokawa 







export GTOPO="/data/project/organisms/DEM/usgsGTOPO30"
export CLIP="/data/project/organisms/DEM/Yuni/Data/GTOPO30/"

gdal_translate -projwin 20 81.99 59.99 59 $GTOPO/e020n90/E020N90.DEM $CLIP/E020N82_Clipped.dem

gdal_translate -projwin 60 81.99 99.99 59 $GTOPO/e060n90/E060N90.DEM $CLIP/E060N82_Clipped.dem

gdal_translate -projwin 100 81.99 139.99 59 $GTOPO/e100n90/E100N90.DEM $CLIP/E100N82_Clipped.dem

gdal_translate -projwin 140 81.99 179.99 59 $GTOPO/e140n90/E140N90.DEM $CLIP/E140N82_Clipped.dem

gdal_translate -projwin -20 81.99 19.99 59 $GTOPO/w020n90/W020N90.DEM $CLIP/W020N82_Clipped.dem

gdal_translate -projwin -60 81.99 -20.01 59 $GTOPO/w060n90/W060N90.DEM $CLIP/W060N82_Clipped.dem

gdal_translate -projwin -100 81.99 -60.01 59 $GTOPO/w100n90/W100N90.DEM $CLIP/W100N82_Clipped.dem

gdal_translate -projwin -140 81.99 -100.01 59 $GTOPO/w140n90/W140N90.DEM $CLIP/W140N82_Clipped.dem

gdal_translate -projwin -180 81.99 -140.01 59 $GTOPO/w180n90/W180N90.DEM $CLIP/W180N82_Clipped.dem


