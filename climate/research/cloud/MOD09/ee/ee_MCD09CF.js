// MCD09_MCloudFreq
////////////////////////////////////////////////////////////
// Adam.wilson@yale.edu
// Generate a cloud frequency climatology from MOD09 data
// This script does the following
// 1) Extracts daily cloud flag from MOD09GA and MYD09GA and create single collection for each month
// 2) Calculate mean cloud frequency for each month
// 3) Calculate overall mean and sd of monthly mean cloud frequencies for each month
// 4) Export the monthly mean/sd cloud frequency separately (as separate tasks) for each month
////////////////////////////////////////////////////////////

//  Specify destination and run name
var driveFolder="ee_combined";
var run="combined"

// limit overall date range by year (only years in this range will be included)
var yearstart=2000
var yearstop=2013

// limit overall date range by month
var monthstart=1
var monthstop=12

// specify what happens
var verbose=true   // print info about collections along the way (slows things down)
var exportDrive=true  // add exports to task window
var exportGME=false  // add exports to task window and send to GME
var download=false   // show download URL
var drawmap=false    // add image to map


// Get current date as string for file metadata
var currentdate = new Date()
var date= currentdate.getFullYear()+''+currentdate.getMonth()+1+''+currentdate.getDate()

// get array of years to process
var years=Array.apply(0, Array(yearstop-yearstart+1)).map(function (x, y) { return yearstart +y ; });
var nYears=years.length
if(verbose){print('Processing '+years)}

/////////////////////////////////////////////////////////////////////////////
/// Functions
/**
 * Returns an image containing just the specified QA bits.
 *
 * Args:
 *   image - The QA Image to get bits from.
 *   start - The first bit position, 0-based.
 *   end   - The last bit position, inclusive.
 *   name  - A name for the output image.
 */
var getQABits = function(image, start, end, newName) {
    // Compute the bits we need to extract.
    var pattern = 0;
    for (var i = start; i <= end; i++) {
       pattern += Math.pow(2, i);
    }
    return image.select([0], [newName])
                  .bitwise_and(pattern)
                  .right_shift(start);
};

// function to extract MOD09 internal cloud flag
//var getcf=function(img){ return(img.select(['state_1km']).expression("((b(0)/1024)%1)").multiply(ee.Image(100)))};
var getcf=function(img){ return( getQABits(img.select(['state_1km']),10, 10, 'cloud').expression("b(0) == 1").multiply(ee.Image(100)))};

//function to get mean combined (terra + aqua) cloud frequency for a particular year-month
function fMOD09(year,month,collection){
  // define filters
  var getmonth=ee.Filter.calendarRange(month,month,"month");
  var getyear=ee.Filter.calendarRange(year,year,"year");
  // extract values
  var MOD09=ee.ImageCollection(collection).filter(getmonth).filter(getyear).map(getcf).mean();
// get monthly combined mean
  var tMOD09m=MOD09;
return(tMOD09m);
}

// function to return the total number of observations in the collection
var getcount=function(img) {
  return img.select(['num_observations_1km']).select([0],['nObs'])}//.multiply(ee.Image(10))};

// function to return the proportion of days with at least one observation in the collection
var getprop=function(img) {
  return img.select(['num_observations_1km']).gte(1).multiply(ee.Image(100)).select([0],['pObs'])};


////////////////////////////////////////////////////
// Loop through months and get cloud frequency for each month in year range
for (var m = monthstart; m <= monthstop; m ++ ) {

//For this month, build array to hold means for every year
var mod = new Array (nYears);
var myd = new Array (nYears);

// loop over years and and put the mean monthly CF in the array
for (var i=0; i<nYears; i ++) {
  mod[i]= fMOD09(years[i],m,"MOD09GA");
  myd[i]= fMOD09(years[i],m,"MYD09GA");
  }

if(verbose){print('Processing '+nYears+' years of data for Month '+m)}

// build an image collection for all years for this month using the array
var MOD09m=ee.ImageCollection(mod);
var MYD09m=ee.ImageCollection(myd);

if(verbose){print(MOD09m.getInfo())}

/////////////////////////////////////////////////////////////////////////////////////////////////////
///  Process MODIS observation frequency/proportion

// get number of obs and proportion of days with at least one obs
//MOD
var MOD09_pObs = ee.ImageCollection("MOD09GA").
filter(ee.Filter.calendarRange(yearstart,yearstop,"year")).
filter(ee.Filter.calendarRange(m,m,"month")).map(getprop).mean().byte();

var MOD09_nObs = ee.ImageCollection("MOD09GA").
filter(ee.Filter.calendarRange(yearstart,yearstop,"year")).
filter(ee.Filter.calendarRange(m,m,"month")).map(getcount).mean().multiply(ee.Image(4)).byte();

//MYD
var MYD09_pObs = ee.ImageCollection("MYD09GA").
filter(ee.Filter.calendarRange(yearstart,yearstop,"year")).
filter(ee.Filter.calendarRange(m,m,"month")).map(getprop).mean().byte();

var MYD09_nObs = ee.ImageCollection("MYD09GA").
filter(ee.Filter.calendarRange(yearstart,yearstop,"year")).
filter(ee.Filter.calendarRange(m,m,"month")).map(getcount).mean().multiply(ee.Image(4)).byte();

//////////////////////////////////////////////////////////////////////////////////////////////////////
//  Process cloud frequency

// take overall mean and SD across all years for this month for MOD
var MOD09_mean=MOD09m.mean().byte();
var MOD09_sd=MOD09m.reduce(ee.call("Reducer.sampleStdDev")).multiply(ee.Image(4)).byte();


// take overall mean and SD across all years for this month for MYD
var MYD09_mean=MYD09m.mean().byte();
var MYD09_sd=MYD09m.reduce(ee.call("Reducer.sampleStdDev")).multiply(ee.Image(4)).byte();


/////////////////////////////////////////////////////////////////////////////////////////////////////
// Build a single 8-bit image with all bands
var MOD09=MOD09_mean.addBands(MOD09_sd).addBands(MOD09_nObs).addBands(MOD09_pObs);
var MOD09o=MOD09_nObs.addBands(MOD09_pObs); // Strange, include this line or the above line doesn't work
var MOD09o2=MOD09_pObs.addBands(MOD09_nObs); // Strange, include this line or the above line doesn't work

var MYD09=MYD09_mean.addBands(MYD09_sd).addBands(MYD09_nObs).addBands(MYD09_pObs);
var MYD09o=MYD09_nObs.addBands(MYD09_pObs); // Strange, include this line or the above line doesn't work
var MYD09o2=MYD09_pObs.addBands(MYD09_nObs); // Strange, include this line or the above line doesn't work


if(verbose){  print(MOD09.getInfo()) }
//if(verbose){  print(MOD09o.getInfo()) }


// Test with getDownloadURL
if(download){
var path = MOD09.getDownloadURL({
  'name':  date+'_'+run+'_MOD09_'+m,
  'crs': 'SR-ORG:6974', //4326
  'scale': '926.625433055833',
  'region': '[[-18, 0], [-18, 30], [15, 30], [15, 0]]'  // Sahara
});
print(path);
}


if(exportDrive){
exportImage(MOD09, 
 date+'_'+run+'_'+yearstart+yearstop+'_MOD09_'+m,
  {'maxPixels':1000000000,
  'driveFolder':driveFolder,
//  'crs': 'EPSG:4326', //4326
  'crs': 'SR-ORG:6974', //4326
  'scale': '926.625433055833',
//  'crsTransform':[0.008333333333, 0, -180, 0, -0.008333333333, -90],
  'region': '[[-180, -89.9], [-180, 89.9], [180, 89.9], [180, -89.9]]'  //global
//  'region': '[[-18, 0], [-18, 30], [15, 30], [15, 0]]'  // Sahara
});

exportImage(MYD09, 
 date+'_'+run+'_'+yearstart+yearstop+'_MYD09_'+m,
  {'maxPixels':1000000000,
  'driveFolder':driveFolder,
//  'crs': 'EPSG:4326', //4326
  'crs': 'SR-ORG:6974', //4326
  'scale': '926.625433055833',
//  'crsTransform':[0.008333333333, 0, -180, 0, -0.008333333333, -90],
  'region': '[[-180, -89.9], [-180, 89.9], [180, 89.9], [180, -89.9]]'  //global
//  'region': '[[-18, 0], [-18, 30], [15, 30], [15, 0]]'  // Sahara
});
}


// Export to GME
if(exportGME){
exportImage(MCD09, 
 date+'_GME_'+run+'_MCD09_'+m,
  {'maxPixels':1000000000,
//  'crs': 'EPSG:4326', //4326
  'crs': 'SR-ORG:6974', //4326
  'scale': '926.625433055833',
//  'crsTransform':[0.008333333333, 0, -180, 0, -0.008333333333, -90],
//  'region': '[[-180, -89], [-180, 89], [180, 89], [180, -89]]'  //global
  'region': '[[-18, 0], [-18, 30], [15, 30], [15, 0]]',  // Sahara
  'gmeProjectId' : 'MAP OF LIFE', 
  'gmeAttributionName': 'Copyright MAP OF LIFE',
  'gmeMosaic': 'cloud'
});
}


} // close loop through months


// Draw the map?
if(drawmap) {
  var palette="000000,00FF00,FF0000"

  //addToMap(MOD09,{min:0,max:100,palette:palette}, "mod09");
  addToMap(MOD09_mean,{min:0,max:100,palette:palette}, "mean");
  addToMap(MOD09_sd,{min:0,max:200,palette:palette}, "sd");
  addToMap(MOD09_nObs,{min:0,max:20,palette:palette}, "nObs");
  addToMap(MOD09_pObs,{min:0,max:100,palette:palette}, "pObs");

}