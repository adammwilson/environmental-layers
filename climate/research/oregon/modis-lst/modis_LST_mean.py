#Calculates monthly mean for date range. Uses MOD11A1 tiles in specified directory
#Assumes MOD11A1.005/2000.03.05/MOD11A1.A2000065.h08v04.005.2007176143143.hdf directory structure
#Non optimized version. Could use Cython to compile C++ fuctions to make it faster. 
#Can do single tile or read list from file.
#---------------------------------------------------------------------------	
# Developed by Alberto Guzman
# 09/15/2013

import sys, numpy
import netCDF4, datetime,os
import glob
from optparse import OptionParser
from pyhdf.SD import SD
import gdal

def main():
  usage = "usage: %prog [options] <indDir> <outDir> \n"+\
            "Calculates monthly mean for MOD11A1 tiles in specified directory"
  parser = OptionParser(usage=usage)
  parser.add_option("-s","--yStart",dest="yStart",default=2000,
      help="Start year to process")
  parser.add_option("-e","--yEnd",dest="yEnd",default=2010,
      help="End year to process")
  parser.add_option("-m","--months",dest="months",default='1,2,3,4,5,6,7,8,9,10,11,12',
      help="Comma separated list of months to process (1,2,3...12),default all")
  parser.add_option("-f","--outputFormat",dest="outF",default="flt",
      help="Format of output, currently flt(no projection info) and geotif supported")
  parser.add_option("-d","--daynight",dest="dn",default="0",
      help="Use 0 for both(default),1 for day only,2 for night only")
  parser.add_option("-t","--tile",dest="tile",default="h08v04",
      help="Which modis tile to process by default uses h08v04")
  parser.add_option("-l","--tileList",dest="tiles",default=None,
      help="Get tiles list from file(one per line), overwrites option 't'")
  opts,args=parser.parse_args()
  if len(args)<2:
     sys.exit(parser.print_help())

  inDir = args[0]
  outDir = args[1]

  yStart = int(opts.yStart)
  yEnd = int(opts.yEnd)
  months = opts.months.split(',')
  outFormat = opts.outF
  if (outFormat != "flt") & (outFormat != "tif"):
     print "Error: Only flt or tif output formats supported"
     sys.exit(1)
 
  if int(opts.dn) == 0:
     dayNight = ["Day","Night"]
  elif int(opts.dn) == 1:
     dayNight = ["Day"]
  elif int(opts.dn) == 2:
     dayNight = ["Night"]
  else:
     print "Error: Only 0(both),1(day),2(night) options supported"
     sys.exit(1)

  #Check if in and out directory exist, create out if it doesn't
  if os.path.exists(inDir) is False:
     print "Error: %s directory doesn't exists" % inDir
     sys.exit(1)
  if os.path.exists(outDir) is False:
     print "Warning: %s directory doesn't exists, creating directory" % outDir
  
  if opts.tiles is not None:
     if os.path.exists(opts.tiles) is False:
        print "Error: file %s doesn't exists" % opts.tiles
        sys.exit(1)
     else:
        fPtr = open(opts.tiles)
        if fPtr is None:
          print "Error: Opening file %s" % opts.tiles
          sys.exit(1)
        else:
           tiles = []
           for t in fPtr.readlines(): 
              tiles.append(t.rstrip())
           fPtr.close()
  else:
       tiles = [opts.tile]

  #TEST
  #tile = 'h08v04'
  #dayNight = 'day'

  print tiles
  for tile in tiles:
    #One log file per tile, it only logs errors messages
    logFn = outDir+'/outlog_'+tile+'.log'
    logPtr = open(logFn,"w+")
    if logPtr is None: 
       print "Error: couldn't create log file %s" % logFn
       print "Continuing without log" #should we just exit?

    #Use a tile to create out tif if option selected
    tmpFn = ''

    print "Processing months %s and years %d-%d for tile %s" % (opts.months,yStart,yEnd,tile)
    for dN in dayNight:
      for m in months:
        #Temp monthly arrays
        meanTotal = numpy.zeros((1200,1200),dtype=numpy.float32)
        nobsTotal = numpy.zeros((1200,1200),dtype=numpy.int32)

        mSrc = '%0*d' % (2,int(m)) #setting to two digit format for months < 10 
        for y in range(yStart,yEnd+1):
           #Get list of files assumes MOD11A1.005/2000.03.05/MOD11A1.A2000065.h08v04.005.2007176143143.hdf format
           inFilesSrch = "%s/%d.%s.*/MOD11A1.*.%s.*.hdf" % (inDir,y,mSrc,tile)       
           inFiles = glob.glob(inFilesSrch)

           if len(inFiles) < 1:
             print "No files found"        
           else:
             print "Found %d files" % len(inFiles)
             for f in inFiles:
               print "Processing %s %s" % (f,dN)
               try:
                 ds = SD(f)
                 dayTemp = ds.select('LST_'+dN+'_1km').get()
                 dayTemp = numpy.where(dayTemp > 0,(dayTemp * 0.02) - 273.15,0) # K to C degrees         

                 #We want highest quality so check last two bits for [00]
                 qa = ds.select('QC_'+dN).get() & 0b00000011
                 meanTotal = numpy.where((qa == 0) & (dayTemp > 0),meanTotal+dayTemp,meanTotal)
                 nobsTotal = numpy.where((qa == 0) & (dayTemp > 0),nobsTotal+1,nobsTotal)
                 ds.end()
      
                 meanTotal = numpy.where((nobsTotal != 0) & (meanTotal != 0),meanTotal/nobsTotal,0)
                 meanTotal = numpy.nan_to_num(meanTotal)    
                 tmpFn = f
                 if logPtr is not None:
                      logPtr.write("Processed: "+f+"\n")
               except Exception:
                   print "Error reading or accessing file, adding to log"
                   if logPtr is not None:
                      logPtr.write("Error: "+f+"\n")
                   pass
          
        if outFormat == "flt":  
          #Save to file, outputs to same grid as original tile
          outFn = "%s/mean_LST_%s_1km_%s_%s.flt32" % (outDir,tile,m,dN)
          meanTotal.tofile(outFn)
          outFn = "%s/nobs_LST_%s_1km_%s_%s.int32" % (outDir,tile,m,dN)
          nobsTotal.tofile(outFn)
        elif outFormat == "tif":
          outFn = "%s/mean_LST_%s_1km_%s_%s.tif" % (outDir,tile,m,dN)
          print "Outputing to", outFn
          out2Tif(meanTotal,outFn,tmpFn)
          outFn = "%s/nobs_LST_%s_1km_%s_%s.tif" % (outDir,tile,m,dN)
          out2Tif(nobsTotal,outFn,tmpFn,1,gdal.GDT_UInt32)
    logPtr.close()
  sys.exit(0) 

def out2Tif(outData,outFn,inFn,bands=1,format=gdal.GDT_Float32):
   dims = 1200
   inDsFn = "HDF4_EOS:EOS_GRID:%s:MODIS_Grid_Daily_1km_LST:LST_Day_1km" % inFn
   inDs = gdal.Open(inDsFn,gdal.GA_ReadOnly)
   if inDs is None:
      print "Error opening %s " % inDsFn
      return None
   
   driver = gdal.GetDriverByName("GTiff")
   metadata = driver.GetMetadata()
   if metadata.has_key(gdal.DCAP_CREATE) and metadata[gdal.DCAP_CREATE] == 'YES':
      d = 1
      #print 'Driver GTiff supports Create() method.'
   else:
      print 'Driver GTIFF does not support Create()'
      return None

   geoTrans = inDs.GetGeoTransform()
   geoProj = inDs.GetProjection()
  
   outDs = driver.Create(outFn,dims,dims,bands,format)
   
   if outDs is None:
      print "Error creating output file %s" % outFn
      sys.exit(1)
   outDs.SetGeoTransform(geoTrans)
   outDs.SetProjection(geoProj)

   b1 = outDs.GetRasterBand(1)
   b1.WriteArray(outData)
   
   inDs = None
   outDs = None

   return None  

if __name__ == '__main__':
   main()
