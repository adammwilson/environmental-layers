# Extracts SDS 10 (Clear Day) from MODIS HDF files and exports them to Erdas IMAGINE format so they can be used in subsequent Raster Calculations.
# ---------------------------------------------------------------------------
# Developed by John Donoghue II
# 6 May, 2010

import sys
import string
import os

sourceDir = "/data/project/organisms/MODIS_LST_Oregon" # provide a default value if unspecified
outputDir = "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL" # provide a default value if unspecified

# Loop through the folder
for dirname, dirnames, filenames in os.walk(sourceDir):
    for filename in filenames:
        ext = os.path.splitext(filename)[1]
        if ext == ".hdf":
            print "processing " + filename

            # process the file
            inFile = sourceDir + "/" + filename
            outFile = outputDir + "/" + filename[0:27] + "_ClearDay.img"
            
            # remove outfile if present
            if not os.path.exists(outFile) == True:
                #os.remove(outFile)

                # Process: Extract Subdataset
                print "   saving " + filename[0:27] + "_ClearDay.img"
                myCommand = "gdal_translate -of HFA HDF4_EOS:EOS_GRID:" + filename + ":MODIS_Grid_Daily_1km_LST:Clear_day_cov " + outFile
                output = os.popen(myCommand).read() # shows the ouput data of the command
            
print "Finished"
