import ee
from ee import mapclient

import logging
logging.basicConfig()

MY_SERVICE_ACCOUNT = '511722844190@developer.gserviceaccount.com'  # replace with your service account
MY_PRIVATE_KEY_FILE = '/Users/adamw/EarthEngine-privatekey.p12'       # replace with you private key file path

ee.Initialize(ee.ServiceAccountCredentials(MY_SERVICE_ACCOUNT, MY_PRIVATE_KEY_FILE))



image = ee.Image('srtm90_v4')
map = ee.mapclient.MapClient()
map.addOverlay(mapclient.MakeOverlay(image.getMapId({'min': 0, 'max': 3000})))
