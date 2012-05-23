#This script was used to check that the coordinates at the top left corner of each mosaiced Aster tile match those that are expected based on tile name (quick QC check that mosaicing ran correctly)

#Natalie Robinson 
#Created on Jan. 28, 2012 

import os
import osgeo
from osgeo import gdal
from osgeo.gdal import *
import subprocess
import re

path= "/data/project/organisms/DEM/asterGdem2/90m_NoPixelOffset/Mosaiced"
#file= path+ "N60to65E000_005.tif"
#ds=gdal.Open(file, GA_ReadOnly)

FileList=[]
Filex=[]
Filey=[]
x_min=[]
y_max=[]

#Simpler command if no subdirectories (use for path + "/N59to60")
#for dirname, dirnames, filenames in os.walk(path):
#    for file in filenames:
#      ext= os.path.splitext(file)[1]
#      if ext == ".tif":

#If subdirectories, list subdirectories to ignore
ignore='N59to60'
for root, dirs, files in os.walk(path):
   if(ignore in dirs):
      dirs.remove(ignore)
   for file in files:
       FileList.append(path + "/" + file)
       Filex.append(file[7:11])
       Filey.append(file[5:7])
       for i in range (0,len(Filex)):
          if re.search("W",Filex[i][0]):
             Filex[i]= Filex[i].replace("W","-")
          else:
             Filex[i]=Filex[i].replace("E","")
            

for j in range (0,len(Filex)):
   Filex[j]=int(Filex[j])
   Filey[j]=int(Filey[j])

for f in FileList:
       ds=gdal.Open(f)
       geotrans= ds.GetGeoTransform()
       x_min.append(geotrans[0])
       y_max.append(geotrans[3])

for i in range (0,len(Filex)):
    if Filex[i]==x_min[i]:
        print Filex[i], "OK"
    else:
        print Filex[i], x_min

for i in range (0,len(Filey)):
    if Filey[i]==y_max[i]:
        print Filey[i], "OK"
    else:
        print Filey[i], y_max[i]


