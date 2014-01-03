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
import subprocess
import time

import logging
logging.basicConfig(filename='error.log',level=logging.DEBUG)

def Usage():
    print('Usage: ee.MOD9.py -projwin  ulx uly urx ury lrx lry llx lly -year year -month month -regionname 1') 
    sys.exit( 1 )

ulx = float(sys.argv[2])
uly = float(sys.argv[3])
urx = float(sys.argv[4])
ury = float(sys.argv[5])
lrx = float(sys.argv[6])
lry = float(sys.argv[7])
llx = float(sys.argv[8])
lly = float(sys.argv[9])
year = int(sys.argv[11])
month = int(sys.argv[13])
regionname = str(sys.argv[15])
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
cwd='/mnt/data2/projects/cloud/mod09/'
## Wget always starts with a temporary file called "download", so move to a subdirectory to
## prevent overwrting when parallel downloading 
if not os.path.exists(cwd+output):
    os.makedirs(cwd+output)
os.chdir(cwd+output)

      # output filename
unzippedfilename=output+".mod09.tif"
      # Check if file already exists and continue if so...
if(os.path.exists(unzippedfilename)):
    sys.exit("File exists:"+output)    

## initialize GEE
MY_SERVICE_ACCOUNT = '511722844190@developer.gserviceaccount.com'  # replace with your service account
MY_PRIVATE_KEY_FILE = '/home/adamw/EarthEngine-privatekey.p12'       # replace with you private key file path
ee.Initialize(ee.ServiceAccountCredentials(MY_SERVICE_ACCOUNT, MY_PRIVATE_KEY_FILE))

## set map center to speed up viewing
#ee.mapclient.centerMap(-121.767, 46.852, 11)

#///////////////////////////////////
#// Function to extract cloud flags
def getmod09(img): return(img.select(['state_1km']).expression("((b(0)/1024)%2)").gte(1)); 

#////////////////////////////////////////////////////
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
region=[llx, lly],[lrx, lry],[urx, ury],[ulx,uly]  #h11v08
strregion=str(list(region))

# add to plot to confirm it's working
#ee.mapclient.addToMap(data, {'range': '0,100'}, 'MOD09')

      # build the URL and name the object (so that when it's unzipped we know what it is!)
path =mod09a.getDownloadUrl({
        'crs': 'SR-ORG:6974',          # MODIS Sinusoidal
        'scale': '926.625433055833',   # MODIS ~1km
        'name': output,  # name the file (otherwise it will be a uninterpretable hash)
        'region': strregion                        # region defined above
        });

# print info to confirm there is data
print(data.getInfo())
print(' Processing.... '+output+'     Coords:'+strregion)
print(path)

#test=wget.download(path)
test=subprocess.call(['-c','-q','--timeout=0','--ignore-length','--no-http-keep-alive','-O','mod09.zip',path],executable='wget')
print('download sucess for tile '+output+':   '+str(test))

## Sometimes EE will serve a corrupt zipped file with no error
# try to unzip it
zipstatus=subprocess.call("unzip -o mod09.zip",shell=True)

if zipstatus==0:  #if sucessful, quit
    os.remove("mod09.zip")
    sys.exit("Finished:  "+output)

# if file doesn't exists or it didn't unzip, try again      
if zipstatus!=0:
    print("Zip Error for:  "+output+"...         Trying again (#2)")    
    time.sleep(15)
    test=subprocess.call(['-c','-q','--timeout=0','--ignore-length','--no-http-keep-alive','-O','mod09.zip',path],executable='wget')

zipstatus=subprocess.call("unzip -o mod09.zip",shell=True)

if zipstatus==0:  #if sucessful, quit
    os.remove("mod09.zip")
    sys.exit("Finished:  "+output)

# if file doesn't exists or it didn't unzip, try again      
if zipstatus!=0:
    print("Zip Error for:  "+output+"...         Trying again (#3)")    
    time.sleep(30)
    test=subprocess.call(['-c','-q','--timeout=0','--ignore-length','--no-http-keep-alive','-O','mod09.zip',path],executable='wget')

# try again #4
zipstatus=subprocess.call("unzip -o mod09.zip",shell=True)

if zipstatus==0:  #if sucessful, quit
    os.remove("mod09.zip")
    sys.exit("Finished:  "+output)

# if file doesn't exists or it didn't unzip, try again      
if zipstatus!=0:
    print("Zip Error for:  "+output+"...         Trying again (#4)")    
    time.sleep(30)
    test=subprocess.call(['-c','-q','--timeout=0','--ignore-length','--no-http-keep-alive','-O','mod09.zip',path],executable='wget')

zipstatus=subprocess.call("unzip -o mod09.zip",shell=True)

if zipstatus==0:  #if sucessful, quit
    os.remove("mod09.zip")
    sys.exit("Finished:  "+output)

## delete the zipped file (the unzipped version is kept)
os.remove("mod09.zip")
sys.exit("Zip Error for:  "+output)    


