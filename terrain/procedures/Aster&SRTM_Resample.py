#This script was used to:
#   1) resample Aster GDEM2 tiles into 90m resolution and shift grids to get rid of #1/2 pixel offset (so that there will be no overlapping pixels between adjacent #tiles, which were found to have different values for many overlapping pixels)
#   2) resample SRTM tiles to get rid of 1/2 pixel offset

#Natalie Robinson 
#Created on Jan. 18, 2012 

import os
import sys
import re
import string
import osgeo
from osgeo import gdal
from osgeo.gdalconst import *
import subprocess
from subprocess import *

#AsterGdem2------------------------------------------------------------------
sourceDir= "/data/project/organisms/DEM/asterGdem2"
outDir= "/data/project/organisms/DEM/asterGdem2/90m_NoPixelOffset"

#separate out .dem files and create lists of file names (input, output, 
#and without fullpath for easy indexing)
for x in os.listdir(sourceDir):
   if re.search("\_dem.tif$",x):
       files.append(x)

for y in range (0,len(files)):
    inName.append (sourceDir + "/" + files[y])
    outName.append (outDir + "/" + files[y][8:15] + "_ReSample.tif")


#Create lists of x_min, x_max, y_min, y_max values for automating gdalwarp:
files = []
inName=[]
outName=[]
lat=[]
lon=[]

#x_min and y_min (lat and lon):
for i in range (0,len(files)):
   lat.append (files[i][9:11])
   lon.append (files[i][11:15])
   for j in range (0,len(lon)):
       if re.search("W",lon[i][0]):
           lon[i]= lon[i].replace("W","-")
       else:
           lon[i]=lon[i].replace("E","")

#x_max and y_max (lat+ 1 and lon+1, but where elements are 
#string format (for correct read into gdalwarp)
lat_PlusOne=[]
lon_PlusOne=[]

for y in range (0,len(lon)):
    lon[y]=int(lon[y])
    lat[y]=int(lat[y])
    lon_PlusOne.append(lon[y]+1)
    lat_PlusOne.append(lat[y]+1)  
    #Convert back to string
    lon_PlusOne=["%s" % el for el in lon_PlusOne]
    lat_PlusOne=["%s" % el for el in lat_PlusOne]
    lon=["%s" % el for el in lon]
    lat=["%s" % el for el in lat]

#Create new files using names from code above
for i in range (0,len(files)):
    subprocess.call (["gdalwarp","-te",lon[i],lat[i],lon_PlusOne[i],lat_PlusOne[i],"-ts","1200","1200","-srcnodata","-9999","-dstnodata","-9999","-r","bilinear",inName[i], outName[i]])

#SRTM 90m---------------------------------------------------------------------
sourceDir= "/data/project/organisms/DEM/cgiarSrtm/SRTM_90m_ASCII_4_1"
outDir= "/data/project/organisms/DEM/cgiarSrtm/SRTM_90m_ASCII_4_1/Tiles_Resampled"

#separate out .asc files and create lists of file names and convert 
#naming scheme into lat/lon info for file creation and naming (input, 
#output, and without fullpath for easy indexing)
files = []
inName=[]
outName=[]
s=[]
t=[]

for x in os.listdir(sourceDir):
   if re.search("\.asc$",x):
       files.append(x)

#Convert SRTM naming scheme to correct lat/lon info
for i in range(len(files)):
   s.append(files[i][5:7])
   t.append(files[i][8:10])
   s[i]=int(s[i])
   t[i]=int(t[i])

#Create lists of x_min, x_max, y_min, y_max values for automating gdalwarp:
lat=[]
lon=[]
lon_PlusFive=[]
lat_PlusFive=[]

#append lon (y_min) w/ correct longitude based on filename, repeat for lat (x_min)
#Seed lon with correct starting longitude, and lat with correct starting latitude
files[0]        # is 03_08, or lat 20, lon -170
lon.append(-170)
lat.append(20)

for k in range (0,len(s)):
   lon.append(lon[k]+ 5 * (s[k+1]-s[k]))
   lat.append(lat[k]- 5 * (t[k+1]-t[k]))

#x_max and y_max (lat+ 5 and lon+5, but where elements are 
#string format (for correct read into gdalwarp)
for y in range (0,len(lon)):
    lon[y]=int(lon[y])
    lat[y]=int(lat[y])
    lon_PlusFive.append(lon[y]+5)
    lat_PlusFive.append(lat[y]+5)  
    #Convert back to string
    lon_PlusFive=["%s" % el for el in lon_PlusFive]
    lat_PlusFive=["%s" % el for el in lat_PlusFive]
    lon=["%s" % el for el in lon]
    lat=["%s" % el for el in lat]

#Create input and output filenames
for y in range (0,len(files)):
    inName.append (sourceDir + "/" + files[y])
    outName.append (outDir + "/" + lat[y] + "Lat" + lon[y] + "Lon" + "_ReSample.tif")

#Create new files
for i in range (0,len(files)):
    subprocess.call (["gdalwarp","-te",lon[i],lat[i],lon_PlusFive[i],lat_PlusFive[i],"-ts","6000","6000","-ot", "Int16", "-srcnodata","-9999","-dstnodata","-9999","-r","bilinear",inName[i], outName[i]])
