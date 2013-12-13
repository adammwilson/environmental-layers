#!/usr/bin/env python

## Example script that downloads data from Google Earth Engine using the python API
## MODIS MOD09GA data is processed to extract the MOD09 cloud flag and calculate monthly cloud frequency


## import some libraries
import ee
from ee import mapclient
import ee.mapclient 
import datetime
import wget
import os
import sys
from subprocess import call

import logging
logging.basicConfig(filename='error.log',level=logging.DEBUG)

def Usage():
    print('Usage: ee.MOD9.py -projwin  ulx uly lrx lry -year year -month month -regionname 1') 
    sys.exit( 1 )

ulx = float(sys.argv[2])
uly = float(sys.argv[3])
lrx = float(sys.argv[4])
lry = float(sys.argv[5])
year = int(sys.argv[7])
month = int(sys.argv[9])
regionname = str(sys.argv[11])

#```
#ulx=-159
#uly=20
#lrx=-154.5
#lry=18.5
#year=2001
#month=6
#```
## Define output filename
output=regionname+'_'+str(year)+'_'+str(month)

## set working directory (where files will be downloaded)
os.chdir('/mnt/data2/projects/cloud/mod09')

MY_SERVICE_ACCOUNT = '511722844190@developer.gserviceaccount.com'  # replace with your service account
MY_PRIVATE_KEY_FILE = '/home/adamw/EarthEngine-privatekey.p12'       # replace with you private key file path

ee.Initialize(ee.ServiceAccountCredentials(MY_SERVICE_ACCOUNT, MY_PRIVATE_KEY_FILE))

## set map center to speed up viewing
#ee.mapclient.centerMap(-121.767, 46.852, 11)

#///////////////////////////////////
#// Function to extract cloud flags
def getmod09(img): return(img.select(['state_1km']).expression("((b(0)/1024)%2)>0.5")); 
# added the >0.5 because some values are coming out >1.  Need to look into this further as they should be bounded 0-1...

#////////////////////////////////////////////////////

      # output filename
unzippedfilename=output+".mod09.tif"

      # Check if file already exists and continue if so...
if(os.path.exists(unzippedfilename)):
    sys.exit("File exists:"+output)    


#####################################################
# Processing Function
# MOD09 internal cloud flag for this year-month
      # to filter by a date range:  filterDate(datetime.datetime(yearstart,monthstart,1),datetime.datetime(yearstop,monthstop,31))
mod09 = ee.ImageCollection("MOD09GA").filter(ee.Filter.calendarRange(year,year,"year")).filter(ee.Filter.calendarRange(month,month,"month")).map(getmod09);
#      myd09 = ee.ImageCollection("MYD09GA").filter(ee.Filter.calendarRange(year,year,"year")).filter(ee.Filter.calendarRange(month,month,"month")).map(getmod09);
      # calculate mean cloudiness (%), rename band, multiply by 100, and convert to integer
mod09a=mod09.mean().select([0], ['mod09']).multiply(ee.Image(1000)).int16();
#      myd09a=myd09.mean().select([0], ['MYD09_'+str(year)+'_'+str(month)]).multiply(ee.Image(100)).int8();
## Set data equal to whatver you want downloaded
data=mod09a
######################################################

## define region for download
region=[ulx,lry], [ulx, uly], [lrx, uly], [lrx, lry]  #h11v08
strregion=str(list(region))
# Next few lines for testing only
# print info to confirm there is data
#data.getInfo()

## print a status update
print(output+' Processing....      Coords:'+strregion)


# add to plot to confirm it's working
#ee.mapclient.addToMap(data, {'range': '0,100'}, 'MOD09')
#```

# TODO:  
#  use MODIS projection

      # build the URL and name the object (so that when it's unzipped we know what it is!)
path =mod09a.getDownloadUrl({
        'name': output,  # name the file (otherwise it will be a uninterpretable hash)
        'scale': 926,                              # resolution in meters
        'crs': 'EPSG:4326',                         #  projection
        'region': strregion                        # region defined above
        });

      # Sometimes EE will serve a corrupt zipped file with no error
      # to check this, use a while loop that keeps going till there is an unzippable file.  
      # This has the potential for an infinite loop...

#if(not(os.path.exists(output+".tif"))):
    # download with wget
print("Downloading "+output) 
wget.download(path)
#call(["wget"+path,shell=T])
        # try to unzip it
print("Unzipping "+output)
zipstatus=call("unzip "+output+".zip",shell=True)
         # if file doesn't exists or it didn't unzip, remove it and try again      
if(zipstatus==9):
    sys.exit("File exists:"+output)    
#        print("ERROR: "+output+" unzip-able")
#        os.remove(output+".zip")

## delete the zipped file (the unzipped version is kept)
os.remove(output+".zip")
       
print(output+' Finished!')


