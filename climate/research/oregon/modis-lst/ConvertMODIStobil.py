import sys
import string
import os
import osgeo
from osgeo import gdal, gdalconst
import re
import subprocess
from osgeo.gdalconst import *

driver = gdal.GetDriverByName('Ehdr')
driver2=gdal.GetDriverByName('HFA')
driver.Register()
driver2.Register()

# Convert Monthly averages to .bil
sourceDir = "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics"
outputDir = "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_MonthlyAvgs_Mosaics"

# Loop through the folder
for dirname, dirnames, filenames in os.walk(sourceDir):
  for filename in filenames:
     ext = os.path.splitext(filename)[1]
     if ext == ".img":
       infile= sourceDir + "/" + filename
       outfile= re.sub(".img",".bil",infile)
       subprocess.call(["gdalwarp","-s_srs", "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs","-t_srs", "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs", "-ot", "int16", "-srcnodata", "3.39999999999999996e+38", "-dstnodata", "3.39999999999999996e+38", "-of", "EHdr", "-overwrite", infile, outfile])

#subprocess.call(["gdal_translate", "-ot", "int16", "-of", "EHdr", infile, outfile])

# Convert Daily averages to .bil
sourceDir2 = "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics"
outputDir2 = "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/NewClearDay_DailyAvgs_Mosaics"

# Loop through the folder
for dirname, dirnames, filenames in os.walk(sourceDir2):
  for filename in filenames:
     ext = os.path.splitext(filename)[1]
     if ext == ".img":
       infile2= sourceDir2 + "/" + filename
       outfile2= re.sub(".img",".bil",infile2)
       subprocess.call(["gdalwarp","-ot", "int16", "-srcnodata", "3.39999999999999996e+38", "-dstnodata", "3.39999999999999996e+38", "-of", "EHdr", "-overwrite", infile2, outfile2])

#subprocess.call(["gdal_translate", "-ot", "int16", "-of", "EHdr", infile2, outfile2])
       



gdalwarp -ot int16 -srcnodata "3.39999999999999996e+38" -dstnodata "3.39999999999999996e+38" -of EHdr infile2 outfile2 
