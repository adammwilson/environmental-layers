import ee
from ee import mapclient

import logging
logging.basicConfig()

MY_SERVICE_ACCOUNT = '511722844190@developer.gserviceaccount.com'  # replace with your service account
MY_PRIVATE_KEY_FILE = '/Users/adamw/EarthEngine-privatekey.p12'       # replace with you private key file path

ee.Initialize(ee.ServiceAccountCredentials(MY_SERVICE_ACCOUNT, MY_PRIVATE_KEY_FILE))

#// EVI_Cloud_month

#// Make land mask
#// Select the forest classes from the MODIS land cover image and intersect them
#// with elevations above 1000m.
elev = ee.Image('srtm90_v4');
cover = ee.Image('MCD12Q1/MCD12Q1_005_2001_01_01').select('Land_Cover_Type_1');
blank = ee.Image(0);
#// Where (cover == 0) and (elev > 0), set the output to 1.
land = blank.where(
    cover.neq(0).and(cover.neq(15)),//.and(elev.gt(0)),
    1);

palette = ["aec3d4", // water
               "152106", "225129", "369b47", "30eb5b", "387242", // forest
               "6a2325", "c3aa69", "b76031", "d9903d", "91af40",  // shrub, grass, savanah 
               "111149", // wetlands
               "cdb33b", // croplands
               "cc0013", // urban
               "33280d", // crop mosaic
               "d7cdcc", // snow and ice
               "f7e084", // barren
               "6f6f6f"].join(',');// tundra

#// make binary map for forest/nonforest
forest = blank.where(cover.gte(1).and(cover.lte(5)),1);

#//addToMap(forest, {min: 0, max: 1});
#//addToMap(cover, {min: 0, max: 17, palette:palette});

#// MODIS EVI Collection
collection = ee.ImageCollection("MCD43A4_EVI");

#//define reducers and filters
COUNT = ee.call("Reducer.count");
#// Loop through months and get monthly % cloudiness
#//for (var month = 1; month < 2; month += 1) {
#//month=1;
FilterMonth = ee.Filter(ee.call("Filter.calendarRange",
    start=month,end=month,field="month"));
tmonth=collection.filter(FilterMonth)

n=tmonth.getInfo().features.length; #// Get total number of images
print(n+" Layers in the collection for month "+month)
tcloud=tmonth.reduce(COUNT).float()
c=ee.Image(n);  #// make raster with constant value of n
c1=ee.Image(-1);  #// make raster with constant value of -1 to convert to % cloudy

#/////////////////////////////////////////////////
#// Generate the cloud frequency surface:
#// 1 Calculate the number of days with measurements
#// 2 divide by the total number of layers
#// 3 Add -1 and multiply by -1 to invert to % cloudy
#// 4 Rename to "PCloudy_month"
tcloud = tcloud.divide(c).add(c1).multiply(c1).expression("b()*100").int8().select_([0],["PCloudy_"+month]);
#//if(month==1) {var cloud=tcloud}  // if first year, make new object
#//if(month>1)  {var cloud=cloud.addBands(tcloud)}  // if not first year, append to first year
#//} //end loop over months
# // end loop over years


#//print(evi_miss.stats())
#//addToMap(tcloud,{min:0,max:100,palette:"000000,00FF00,FF0000"});
#//addToMap(elev,{min:0,max:2500,palette:"000000,00FF00,FF0000"});

#//centerMap(-122.418, 37.72, 10);
path = tcloud.getDownloadURL({
  'scale': 1000,
  'crs': 'EPSG:4326',
  'region': '[[-90, 0], [-90, 20], [-50, 0], [-50, 20]]'  //h11v08
#//  'region': '[[-180, -90], [-180, 90], [180, -90], [180, 90]]'
});
print('https://earthengine.googleapis.com' + path);

