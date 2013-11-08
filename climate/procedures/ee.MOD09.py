## Example script that downloads data from Google Earth Engine using the python API
## MODIS MOD09GA data is processed to extract the MOD09 cloud flag and calculate monthly cloud frequency


## import some libraries
import ee
from ee import mapclient
import ee.mapclient 
import datetime
import wget
import os
from subprocess import call

#import logging
#logging.basicConfig()

## set working directory (where files will be downloaded)
os.chdir('/home/adamw/acrobates/adamw/projects/cloud/data/mod09')

MY_SERVICE_ACCOUNT = '511722844190@developer.gserviceaccount.com'  # replace with your service account
MY_PRIVATE_KEY_FILE = '/home/adamw/EarthEngine-privatekey.p12'       # replace with you private key file path

#MY_SERVICE_ACCOUNT = '205878743334-4mrtqgu0n5rnsv1vanrvv6atqk6vu8am@developer.gserviceaccount.com'
#MY_PRIVATE_KEY_FILE = '/home/adamw/EarthEngine_Jeremy-privatekey.p12'

ee.Initialize(ee.ServiceAccountCredentials(MY_SERVICE_ACCOUNT, MY_PRIVATE_KEY_FILE))

## set map center to speed up viewing
#ee.mapclient.centerMap(-121.767, 46.852, 11)

#///////////////////////////////////
#// Function to extract cloud flags
def getmod09(img): return(img.select(['state_1km']).expression("((b(0)/1024)%2)>0.5")); 
# added the >0.5 because some values are coming out >1.  Need to look into this further as they should be bounded 0-1...

#// Date ranges
yearstart=2000
yearstop=2012
monthstart=1
monthstop=12

#////////////////////////////////////////////////////
# Loop through months and get monthly % missing data

## set a year-month if you don't want to run the loop (for testing)
#year=2001
#month=2


## define the regions to be processed
regions=['[[-180, -60], [-180, 0], [0, 0], [0, -60]]',  # SW
         '[[-180, 0], [-180, 90], [0, 90], [0, 0]]',    # NW
         '[[0, 0], [0, 90], [180, 90], [180, 0]]',      # NE
         '[[0, 0], [0, -60], [180, -60], [180, 0]]']    # SE
## name the regions (these names will be used in the file names
## must be same length as regions list above
rnames=['SW','NW','NE','SE']

## Loop over regions, years, months to generate monthly timeseries
for r in range(0,len(regions)):                                    # loop over regions
  for year in range(yearstart,yearstop+1):              # loop over years
    for month in range(monthstart,monthstop+1):         # loop over months
      print('Processing '+rnames[r]+"_"+str(year)+'_'+str(month))

      # output filename
      filename='mod09_'+rnames[r]+"_"+str(year)+"_"+str(month)

      # Check if file already exists and continue if so...
      if(os.path.exists(filename+".zip")):
        print("File exists:"+filename)
        continue

      # MOD09 internal cloud flag for this year-month
      # to filter by a date range:  filterDate(datetime.datetime(yearstart,monthstart,1),datetime.datetime(yearstop,monthstop,31))
      mod09 = ee.ImageCollection("MOD09GA").filter(ee.Filter.calendarRange(year,year,"year")).filter(ee.Filter.calendarRange(month,month,"month")).map(getmod09);
      # calculate mean cloudiness (%), rename band, multiply by 100, and convert to integer
      mod09a=mod09.mean().select([0], ['MOD09_'+str(year)+'_'+str(month)]).multiply(ee.Image(100)).byte();
      
      # Next few lines for testing only
      # print info to confirm there is data
      # mod09a.getInfo()
      # add to plot to confirm it's working
      # ee.mapclient.addToMap(mod09a, {'range': '0,100'}, 'MOD09')

      # build the URL and name the object (so that when it's unzipped we know what it is!)
      path =mod09a.getDownloadUrl({
          'name': filename,  # name the file (otherwise it will be a uninterpretable hash)
          'scale': 1000,                              # resolution in meters
          'crs': 'EPSG:4326',                         # MODIS sinusoidal
          'region': regions[r]                        # region defined above
          });

      # Sometimes EE will serve a corrupt zipped file with no error
      # to check this, use a while loop that keeps going till there is an unzippable file.  
      # This has the potential for an infinite loop...

      while(not(os.path.exists(filename+".zip"))):
        # download with wget
        print("Downloading "+filename) 
        wget.download(path)
        # try to unzip it
        print("Unzipping "+filename)
        zipstatus=call("unzip "+filename+".zip",shell=True)
        # if file doesn't exists or it didn't unzip, remove it and try again      
        if(zipstatus==9):
          print("ERROR: "+filename+" unzip-able")
          os.remove(filename)

print 'Finished'


