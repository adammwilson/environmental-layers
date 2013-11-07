## Example script that downloads data from Google Earth Engine using the python API
## MODIS MOD09GA data is processed to extract the MOD09 cloud flag and calculate monthly cloud frequency


## import some libraries
import ee
from ee import mapclient
import ee.mapclient 
import datetime
import wget
import os

#import logging
#logging.basicConfig()

## set working directory (where files will be downloaded)
os.chdir('/home/adamw/acrobates/adamw/projects/cloud/data/mod09')

#MY_SERVICE_ACCOUNT = '511722844190@developer.gserviceaccount.com'  # replace with your service account
#MY_PRIVATE_KEY_FILE = '/home/adamw/EarthEngine-privatekey.p12'       # replace with you private key file path

MY_SERVICE_ACCOUNT = '205878743334-4mrtqgu0n5rnsv1vanrvv6atqk6vu8am@developer.gserviceaccount.com'
MY_PRIVATE_KEY_FILE = '/home/adamw/EarthEngine_Jeremy-privatekey.p12'

ee.Initialize(ee.ServiceAccountCredentials(MY_SERVICE_ACCOUNT, MY_PRIVATE_KEY_FILE))

## set map center to speed up viewing
ee.mapclient.centerMap(-121.767, 46.852, 11)

#///////////////////////////////////
#// Function to extract cloud flags
def getmod09(img): return(img.select(['state_1km']).expression("((b(0)/1024)%2)"));

#// Date ranges
yearstart=2000
yearstop=2010
monthstart=1
monthstop=12

#////////////////////////////////////////////////////
# Loop through months and get monthly % missing data

## set a year-month if you don't want to run the loop (for testing)
year=2001
month=1

for year in range(yearstart,yearstop+1): {
for month in range(monthstart,monthstop); {
    
print('Processing '+str(year)+'_'+str(month))

## MOD09 internal cloud flag for this year-month
## to filter by a date range:  filterDate(datetime.datetime(yearstart,monthstart,1),datetime.datetime(yearstop,monthstop,31))
mod09 = ee.ImageCollection("MOD09GA").filter(ee.Filter.calendarRange(year,year,"year")).filter(ee.Filter.calendarRange(month,month,"month")).map(getmod09);

## calculate mean cloudiness (%), rename band, multiply by 100, and convert to integer
mod09a=mod09.mean().select([0], ['MOD09_'+str(year)+'_'+str(month)]).multiply(ee.Image(100)).byte();

## print info to confirm there is data
#mod09a.getInfo()

## add to plot to confirm it's working
#ee.mapclient.addToMap(mod09a, {'range': '0,100'}, 'MOD09')

## define region for download
region='[[-72, -1], [-72, 11], [-59, 11], [-59, -1]]'  #h11v08
#  '[[-90, -1], [-90, 20], [-49, 20], [-49, -1]]'  // Northern S. America
#  '[[-180, -90], [-180, 90], [180, 90], [180, -90]]'  //global
#  '[[-180, -60], [-180, 90], [180, 90], [180, -60]]'  // Western Hemisphere

## Define tiles


## build the URL and name the object (so that when it's unzipped we know what it is!)
path =mod09a.getDownloadUrl({
  'name': 'mod09_'+str(year)+"_"+str(month),  # name the file (otherwise it will be a uninterpretable hash)
  'scale': 926,                               # resolution in meters
  'crs': 'EPSG:4326',                         # MODIS sinusoidal
  'region': region                            # region defined above
});

## download with wget
wget.download(path)

}}


