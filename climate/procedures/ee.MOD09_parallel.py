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

def Usage():
    print('Usage: ee.MOD9.py -projwin  ulx uly lrx lry -o output   ') 
    sys.exit( 1 )

ulx = float(sys.argv[2])
uly = float(sys.argv[3])
lrx = float(sys.argv[4])
lry = float(sys.argv[5])
output = sys.argv[7]

print ( output , ulx ,uly ,lrx , lry )

#import logging

MY_SERVICE_ACCOUNT = '364044830827-ubb6ja607b8j7t8m9uooi4c01vgah4ms@developer.gserviceaccount.com'  # replace with your service account
MY_PRIVATE_KEY_FILE = '/home/selv/GEE/fe3f13d90031e3eedaa9974baa6994e467b828f7-privatekey.p12'      # replace with you private key file path
ee.Initialize(ee.ServiceAccountCredentials(MY_SERVICE_ACCOUNT, MY_PRIVATE_KEY_FILE))

#///////////////////////////////////
#// Function to extract cloud flags
def getmod09(img): return(img.select(['state_1km']).expression("((b(0)/1024)%2)"));

## set a year-month if you don't want to run the loop (for testing)
year=2001
month=1

print('Processing '+str(year)+'_'+str(month))

## MOD09 internal cloud flag for this year-month
## to filter by a date range:  filterDate(datetime.datetime(yearstart,monthstart,1),datetime.datetime(yearstop,monthstop,31))
mod09 = ee.ImageCollection("MOD09GA").filter(ee.Filter.calendarRange(year,year,"year")).filter(ee.Filter.calendarRange(month,month,"month")).map(getmod09);

## calculate mean cloudiness (%), rename band, multiply by 100, and convert to integer
mod09a=mod09.mean().select([0], ['MOD09_'+str(year)+'_'+str(month)]).multiply(ee.Image(100)).byte();

## print info to confirm there is data
# mod09a.getInfo()

## define region for download
region=[ulx,lry ], [ulx, uly], [lrx, uly], [lrx, lry]  #h11v08
strregion=str(list(region))

## Define tiles
region='[[-72, -1], [-72, 11], [-59, 11], [-59, -1]]'
## build the URL and name the object (so that when it's unzipped we know what it is!)
path =mod09a.getDownloadUrl({
'name': output,  # name the file (otherwise it will be a uninterpretable hash)
'scale': 926,                               # resolution in meters
'crs': 'EPSG:4326',                         # MODIS sinusoidal
'region': strregion                  # region defined above
});

## download with wget
wget.download(path)



