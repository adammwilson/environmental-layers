# renames MODIS files from a year and day to year.month.day
# ---------------------------------------------------------------------------
# Developed by John Donoghue II
# 620 October, 2010

# Convert MODIS Day of Year to Date File Name.py

import sys
import string
import os
import shutil

sourceDir = "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts"
outputDir = "/data/project/organisms/MODIS_LST_Oregon/ClearDayGDAL/ClearDay_Original_IMG_Extracts/ByDate"

# Loop through the folder
for dirname, dirnames, filenames in os.walk(sourceDir):
    for filename in filenames:
        ext = os.path.splitext(filename)[1]
        if ext == ".img":
		print "processing " + filename

		# process the file
		inFile = sourceDir + "/" + filename
		
		# rename the file to the month and day
		theYear = filename[9:13]
		theDay = filename[13:16]
		prefix = filename[0:9]
		tile = filename[17:27]

		# convert day to month and year
		theYear = int(theYear)
		theDay = int(theDay)
		from datetime import date
		myDate = date.fromordinal(date(theYear, 1, 1).toordinal() + theDay - 1)
		
		myYear = myDate.year
		myMonth = myDate.month
		myDay = myDate.day
				
		outFile = prefix + "."  + str(myYear) + "." + str(myMonth) + "." + str(myDay) + "." + tile + ".ClearDay.img"
		print ".. Writing " + outFile
	
		#print theYear + ' ' + theDay
		outFile = outputDir + "/" + outFile
            
		# remove outfile if present
		if not os.path.exists(outFile) == True:
			#os.remove(outFile)
			shutil.copy2(inFile,outFile)

		# Process: Extract Subdataset
                #print "   saving " + filename[0:27] + "_ClearDay.img"
                #myCommand = "gdal_translate -of HFA HDF4_EOS:EOS_GRID:" + filename + ":MODIS_Grid_Daily_1km_LST:Clear_day_cov " + outFile
                #output = os.popen(myCommand).read() # shows the ouput data of the command
            
print "Finished"
