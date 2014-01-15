// MODIS LAND Observation Frequency

// Proportion of days in the month with at least 1 MODIS observation
var p_07 = ee.ImageCollection("MOD09GA").
filter(ee.Filter.calendarRange(2011,2012,"year")).
filter(ee.Filter.calendarRange(7,7,"month")).map(function(img) {
  return img.select(['num_observations_1km']).gte(1)}).mean().multiply(ee.Image(100)).select([0],["p07"]); 

// mean number of observations per day
var n_07 = ee.ImageCollection("MOD09GA").
filter(ee.Filter.calendarRange(2011,2012,"year")).
filter(ee.Filter.calendarRange(7,7,"month")).map(function(img) {
  return img.select(['num_observations_1km'])}).mean().select([0],["n07"]);

var n_12 = ee.ImageCollection("MOD09GA").
filter(ee.Filter.calendarRange(2011,2012,"year")).
filter(ee.Filter.calendarRange(12,12,"month")).map(function(img) {
  return img.select(['num_observations_1km']).gte(1)}).mean().multiply(ee.Image(100)).select([0],["p12"]); 

// mean number of observations per day
var p_12 = ee.ImageCollection("MOD09GA").
filter(ee.Filter.calendarRange(2011,2012,"year")).
filter(ee.Filter.calendarRange(12,12,"month")).map(function(img) {
  return img.select(['num_observations_1km'])}).mean().select([0],["n12"]);

// Combine anything into image for download
var image=n_07.addBands(p_07).addBands(n_12).addBands(p_12)
//var reproj = image.reproject('EPSG:4326', [0.008333333333, 0, -180, 0, -0.008333333333, -90])

//var palette="000000,00FF00,FF0000"
//addToMap(image.select([0]),{min:0,max:100,palette:palette},"July");
//addToMap(n_12,{min:0,max:100,palette:palette},"Decenber");


var driveFolder="EarthEngineOutput"

exportImage(image, 
  'PObs',
  {'name': 'PObs',
  'maxPixels':1000000000,
  'driveFolder':driveFolder,
  'crs': 'EPSG:4326', //4326
  'crsTransform':[0.008333333333, 0, -180, 0, -0.008333333333, -90],
//  'scale': 0.008333333333,
  'region': '[[-180, -89], [-180, 89], [180, 89], [180, -89]]'  //global
//  'region': '[[-180, 0], [-180, 10], [180, 10], [180, 0]]'  //
});