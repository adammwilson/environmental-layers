#This is the python script for checking the quality control and SDS fields of MODIS_Oregon hdf4 tiles.
#File contains scripting for all checks and annotation re: what I was looking for, what I found, etc.
#Created by Natalie Robinson- Oct. 2011

#-------------------------------------------------------------------------------------
#Need to run through original .hdf files.
#Looking for string match: 'AUTOMATICQUALITYFLAG': 'PASSED' 
#                          'CLOUDCONTAMINATED_LST_SCREENED': 'YES'

import gdal
import gdalnumeric
import sys
import string
import os
import re
import numpy 
from pyhdf.SD import *
from numpy import *
import warnings
warnings.simplefilter('ignore', DeprecationWarning)
import glob   #probably don't need
import optparse #probably don't need
import fnmatch #probably don't need
from pyhdf import SD
import scipy
from scipy import *


sourceDir = "/data/project/organisms/MODIS_LST_Oregon"

#Find out how many files in directory for indexing
len= len(os.listdir(sourceDir))  #15274

#Find other QC tags to check- test run on single file

testset= gdal.Open ('/data/project/organisms/MODIS_LST_Oregon/MOD11A1.A2000065.h08v04.005.2007176143143.hdf')
allMeta= testset.GetMetadata ()
dict.keys (allMeta)

#Keys of potential interest- what are OK values?
  
allMeta['QAPERCENTNOTPRODUCEDCLOUD']           #Here= 9
allMeta['QAPERCENTNOTPRODUCEDOTHER']           #Here= 90
allMeta['QAPERCENTMISSINGDATA']                #Here= 0
allMeta['QAPERCENTINTERPOLATEDDATA']           #Here= 0
allMeta['QAPERCENTGOODQUALITY']                #Here= 0
allMeta['QAPERCENTCLOUDCOVER']                 #Here= 9
allMeta['DAYNIGHTFLAG']                        #Here= Both
allMeta['SCIENCEQUALITYFLAG']                  #Here= Not Investigated
allMeta['QAPERCENTOTHERQUALITY']               #Here= 0
allMeta['AUTOMATICQUALITYFLAG']                #Here= Passed
allMeta['CLOUD_CONTAMINATED_LST_SCREENED']     #Here= YES 
#-----------------------------------------------------------------------------------------
#Loop through folder and grab values for each quality control tag- evaluate values

Cloud_contam=[]
Day_Night=[]
Sci_Qual=[]
Auto_flag=[]
for dirname, dirnames, filenames in os.walk(sourceDir):
    for filename in filenames:
        ext = os.path.splitext(filename)[1]
        if ext == ".hdf":
  	  infile= sourceDir + "/" + filename
          ds= gdal.Open(infile)
          hdf_md = ds.GetMetadata()
          for k,v in hdf_md.iteritems():
              if k=='CLOUD_CONTAMINATED_LST_SCREENED':
                 if v== 'YES':
                    Cloud_contam.append ("ok")
                 else: 
                    Cloud_contam.append ("Bad")
              if k=='DAYNIGHTFLAG':
                 if v== 'Both':
                    Day_Night.append ("ok")
                 else: 
                    Day_Night.append ("Check")
              if k=='SCIENCEQUALITYFLAG':
                 if v== 'Not Investigated':
                    Sci_Qual.append ("?")
                 else: 
                    Sci_Qual.append ("Check")
              if k== 'AUTOMATICQUALITYFLAG': 
                 if v== 'Passed':
                    Auto_flag.append("ok")
                 else:
                    Auto_flag.append("Bad")


if "Bad" in Cloud_contam: print ("There's a bad Cloud Contaminated LST Screen")
else: print ("All good on the cloud contaminated LST screen front")


if "Bad" in Auto_flag: print ("There's a bad auto flag")
else: print ("All good on the auto flag front")


if "Check" in Day_Night: print ("Check the day/night flags")
else: print ("All good on the day/night flag front")


if "Check" in Sci_Qual: print ("Check the science quality flags")
else: print ("All good on the science quality flag front?")

#------------------------------------------------------------------------------------------
#Loop through the folder and find ranges for some of the metadata tags

#Percent not produced other
Pct_Other= []    #Create empty list to hold values of specific key  
Pct_Cloud=[]
Pct_Missing= []    #Create empty list to hold values of specific key  
Pct_Interp=[]
Pct_Good=[]
Pct_CC=[]
Pct_OtherQual=[]

for dirname, dirnames, filenames in os.walk(sourceDir):
  for filename in filenames:
        ext = os.path.splitext(filename)[1]
        if ext == ".hdf":
  	  infile= sourceDir + "/" + filename
          ds= gdal.Open(infile)      
          hdf_md = ds.GetMetadata()  
          for k,v in hdf_md.iteritems():
              if k== 'QAPERCENTNOTPRODUCEDOTHER': Pct_Other.append(v)
              if k== 'QAPERCENTNOTPRODUCEDCLOUD': Pct_Cloud.append(v)
              if k== 'QAPERCENTMISSINGDATA': Pct_Missing.append(v)
              if k== 'QAPERCENTINTERPOLATEDDATA': Pct_Interp.append(v)
              if k== 'QAPERCENTGOODQUALITY': Pct_Good.append(v)
              if k== 'QAPERCENTCLOUDCOVER': Pct_CC.append(v)
              if k== 'QAPERCENTOTHERQUALITY': Pct_OtherQual.append(v)


print ("range Pct_Other =" + min (Pct_Other) + " to " + max(Pct_Other))
print ("range Pct_Cloud =" + min (Pct_Cloud) + " to " + max(Pct_Cloud))
print ("range Pct_Missing =" + min (Pct_Missing) + " to " + max(Pct_Missing))
print ("range Pct_Interp =" + min (Pct_Interp) + " to " + max(Pct_Interp))
print ("range Pct_Good =" + min (Pct_Good) + " to " + max(Pct_Good))
print ("range Pct_CC =" + min (Pct_CC) + " to " + max(Pct_CC))
print ("range Pct_OtherQual =" + min (Pct_OtherQual) + " to " + max(Pct_OtherQual))
#-----------------------------------------------------------------------------------------------
#Potential problem file re: Science quality flag explanation (http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/detailInfo.cgi?prod_id=MOD11A1&ver=C4)

ohno= gdal.Open('/data/project/organisms/MODIS_LST_Oregon/MOD11A1.A2003359.h09v04.005.2008163212412.hdf')
ohnoMeta= ohno.GetMetadata()
ohnoMeta['SCIENCEQUALITYFLAG'] #Comes back "Not Investigated" but this tile SHOULD BE "Suspect"
#--------------------------------------------------------------------------------------------
#After checking through known issues for MOD11A1 data (from http://landweb.nascom.nasa.gov/cgi-bin/QA_WWW/getSummary.cgi?esdt=MOD11), there are some versions that are affected and others that aren't.  Check PGEversion of tiles to see if they are affected:

PGE=[]
for dirname, dirnames, filenames in os.walk(sourceDir):
  for filename in filenames:
        ext = os.path.splitext(filename)[1]
        if ext == ".hdf":
  	  infile= sourceDir + "/" + filename
          ds= gdal.Open(infile)      
          hdf_md = ds.GetMetadata()  
          for k,v in hdf_md.iteritems():
              if k== 'PGEVERSION': PGE.append(v)


print ("range PGE =" + min (PGE) + " to " + max(PGE))      #Range= 5.3.6 to 5.5.6


#THIS IS IMPORTANT:
# MOre infromation on QA/QC tags and how to check them available at: https://lpdaac.usgs.gov/products/modis_products_table/land_surface_temperature_emissivity/daily_l3_global_1km/mod11a1
#---------------------------------------------------------------------------------                
#Check out SDS's   

SDS's:                   Desc.                     Acceptable range:    Fill Value:
#      LST_Day_1km      Daily day 1km grid LST       7500-65535              0
#      QC_Day           QC for day LST emiss.           0-255                0 (overlap)
#      Day_view_time    Time: day temp obs.             0-240               255
#      Day_view_angle   Zenith ang. for AM LST          0-130               255
#      LST_Night_1km    Daily PM 1km grid LST        7500-65535              0
#      QC_Night         QC for night LST emiss.         0-255                0 (overlap)
#      Night_view_time  Time: night temp obs.           0-240               255
#      Night_view_angle Zenith ang. for PM LST          0-130               255
#      Emis_31          Band 31 emiss.                  1-255                0
#      Emis_32          Band 32 emiss.                  1-255                0
#      Clear_day_cov    AM clear-sky cov.              0-65535               0 (overlap)
#      Clear_night_cov  PM clear-sky cov.              0-65535               0 (overlap)


#For one file- tester-----------------------------------------------------------------------
file='/data/project/organisms/MODIS_LST_Oregon/MOD11A1.A2000065.h08v04.005.2007176143143.hdf' # The hdf file to read

#Open hdf and read SDS data
hdf=SD.SD(file)
qcday=hdf.select('QC_Day')
QC_Day=qcday.get()
qcnight=hdf.select('QC_Night')
QC_Night=qcnight.get()
lstday1km=hdf.select('LST_Day_1km')
LSTDay1km=lstday1km.get()
dvtime=hdf.select('Day_view_time')
DayViewTime=dvtime.get()
nvtime=hdf.select('Night_view_time')
NightViewTime=nvtime.get()
dvang=hdf.select('Day_view_angl')
DayViewAngle=dvang.get()
nvang=hdf.select('Night_view_angl')
NightViewAngle=nvang.get()
lstnight1km=hdf.select('LST_Night_1km')
LSTNight1km=lstnight1km.get()
clearday=hdf.select('Clear_day_cov')
ClearDayCov=clearday.get()
clearnight=hdf.select('Clear_night_cov')
ClearNightCov=clearnight.get()
emis31=hdf.select('Emis_31')
Emiss_Band31=emis31.get()
emis32=hdf.select('Emis_31')
Emiss_Band32=emis32.get()

print "QC_Day range:", QC_Day.min(), " to ",QC_Day.max()
print "QC_Night range:", QC_Night.min(), " to ",QC_Night.max()
print "LST Day 1km range:",LSTDay1km.min(), " to ",LSTDay1km.max()     #Need to remove 0 results to see minimum real value (0=fill, don't know if values <7500 exist b/c masked by 0
print "LST Night 1km range:",LSTNight1km.min(), " to ",LSTNight1km.max()
print "Day View Time range:",DayViewTime.min(), " to ",DayViewTime.max()
print "Night View Time range:",NightViewTime.min(), " to ",NightViewTime.max()
print "Day View Angle range:",DayViewAngle.min(), " to ",DayViewAngle.max()
print "Night View Angle range:",NightViewAngle.min(), " to ",NightViewAngle.max()
print "Clear Day Coverage range:",ClearDayCov.min(), " to ",ClearDayCov.max()
print "Clear Night Coverage range:",ClearNightCov.min(), " to ",ClearNightCov.max()
print "Emissivity Band 31 range:",Emiss_Band31.min(), " to ",Emiss_Band31.max()
print "Emissivity Band 32 range:",Emiss_Band32.min(), " to ",Emiss_Band32.max()

#For some of these the fill value is the highest or lowest value of the array, so pixels with non-fill values need to still be checked to make sure they are within the valid range

scipy.unique(LSTDay1km)     #Need to see 0 (fill), and all others >=7500
scipy.unique(LSTNight1km)   #Need to see 0 (fill), and all others >=7500
scipy.unique(DayViewTime)    #Need to see 255 (fill), and all others <=240
scipy.unique(NightViewTime)  #Need to see 255 (fill), and all others <=240
scipy.unique(DayViewAngle)   #Need to see 255 (fill), and all others <=130
scipy.unique(NightViewAngle) #Need to see 255 (fill), and all others <=130
scipy.unique(Emiss_Band31)   
scipy.unique(Emiss_Band32)

#Passes check: LSTDay1km, LSTNight1km, DayViewTime, NightViewTime, DayViewAngle, NightViewAngle (barely, highest non-fill value=130), both Emiss Bands.

#Loop through files----------------------------------------------------------------------
#Bad= ANY - value
#Bad QC_Day and QC_Night= max > 255
#Bad ClearDay/ClearNight= max > 65535
#Bad LSTDay1km/LSTNight1km= max > 65535 or scipy.unique(LSTXXX1km)[1] < 7500
#Bad DayViewTime/NightViewTime= max > 255 or scipy.unique(XXXViewTime)[-2] > 240
#Bad DayViewAngle/NightViewAngle= max > 255 or scipy.unique(XXXViewAngle)[-2] > 130
#Bad Emiss_Band31/Emiss_Band32= max > 255

QCDayBad=[]
QCNightBad=[]
ClearDayBad=[]
ClearNightBad=[]
Emiss31Bad=[]
Emiss32Bad=[]
LSTDayBad=[]
LSTNightBad=[]
DayViewTimeBad=[]
NightViewTimeBad=[]
DayViewAngleBad=[]
NightViewAngleBad=[]

for dirname, dirnames, filenames in os.walk(sourceDir):
  for filename in filenames:
        ext = os.path.splitext(filename)[1]
        if ext == ".hdf":
  	  infile= sourceDir + "/" + filename
          hdf4SDS= SD.SD(infile)      
          qcday=hdf4SDS.select('QC_Day')
          QC_Day=qcday.get()
          qcnight=hdf4SDS.select('QC_Night')
          QC_Night=qcnight.get()
          lstday1km=hdf4SDS.select('LST_Day_1km')
          LSTDay1km=lstday1km.get()
          dvtime=hdf4SDS.select('Day_view_time')
          DayViewTime=dvtime.get()
          nvtime=hdf4SDS.select('Night_view_time')
          NightViewTime=nvtime.get()
          dvang=hdf4SDS.select('Day_view_angl')
          DayViewAngle=dvang.get()
          nvang=hdf4SDS.select('Night_view_angl')
          NightViewAngle=nvang.get()
          lstnight1km=hdf4SDS.select('LST_Night_1km')
          LSTNight1km=lstnight1km.get()
          clearday=hdf4SDS.select('Clear_day_cov')
          ClearDayCov=clearday.get()
          clearnight=hdf4SDS.select('Clear_night_cov')
          ClearNightCov=clearnight.get()
          emis31=hdf4SDS.select('Emis_31')
          Emiss_Band31=emis31.get()
          emis32=hdf4SDS.select('Emis_31')
          Emiss_Band32=emis32.get()
	  if QC_Day.min()<0 or QC_Day.max()>255:
              QCDayBad.append (filename)
          if QC_Night.min()<0 or QC_Night.max()>255: 
              QCNightBad.append(filename)
          if ClearDayCov.min()<0 or ClearDayCov.max()>65535:
              ClearDayBad.append (filename)
          if ClearNightCov.min()<0 or ClearNightCov.max()>65535:
              ClearNightBad.append (filename)
          if Emiss_Band31.min()<0 or Emiss_Band31.max()>255:
              Emiss31Bad.append (filename)
          if Emiss_Band32.min()<0 or Emiss_Band32.max()>255:
              Emiss32Bad.append (filename)
          if LSTDay1km.min()<0 or LSTDay1km.max()>65535:
              LSTDayBad.append (filename)
          if LSTNight1km.min()<0 or LSTNight1km.max()>65535:
              LSTNightBad.append (filename)
          if DayViewTime.min()<0 or DayViewTime.max()>255:
              DayViewTimeBad.append (filename)
          if NightViewTime.min()<0 or NightViewTime.max()>255:
              NightViewTimeBad.append (filename)
          if DayViewAngle.min()<0 or DayViewAngle.max()>255:
              DayViewAngleBad.append (filename)
          if NightViewAngle.min()<0 or NightViewAngle.max()>255:
              NightViewAngleBad.append (filename)


print QCDayBad             #None found
print QCNightBad           #None found
print ClearDayBad          #None found
print ClearNightBad        #None found 
print Emiss31Bad           #None found
print Emiss32Bad           #None found
print LSTDayBad            #None found
print LSTNightBad          #None found
print DayViewTimeBad       #None found
print NightViewTimeBad     #None found    
print DayViewAngleBad      #None found
print NightViewAngleBad    #None found

#Do searches for masked (by fill value) min or max values separately------------------------
LSTDayRealMin=[]
LSTNightRealMin=[]
DayViewTimeRealMax=[]
NightViewTimeRealMax=[]
DayViewAngleRealMax=[]
NightViewAngleRealMax=[]

for dirname, dirnames, filenames in os.walk(sourceDir):
  for filename in filenames:
        ext = os.path.splitext(filename)[1]
        if ext == ".hdf":
  	  infile= sourceDir + "/" + filename
          hdf4SDS= SD.SD(infile)  
          lstday1km=hdf4SDS.select('LST_Day_1km')          
          LSTDay1km=lstday1km.get()
          dvtime=hdf4SDS.select('Day_view_time')
          DayViewTime=dvtime.get()
          nvtime=hdf4SDS.select('Night_view_time')
          NightViewTime=nvtime.get()
          dvang=hdf4SDS.select('Day_view_angl')
          DayViewAngle=dvang.get()
          nvang=hdf4SDS.select('Night_view_angl')
          NightViewAngle=nvang.get()
          lstnight1km=hdf4SDS.select('LST_Night_1km')
          LSTNight1km=lstnight1km.get()
          if scipy.unique(LSTDay1km)[1]<100:
              LSTDayRealMin.append (filename)
          #if scipy.unique(LSTNight1km)[1]<7500:        #Only value for pixels=0
          #    LSTNightRealMin.append (filename)
          if scipy.unique(DayViewTime)[-2]>240:
              DayViewTimeRealMax.append (filename)
          #if scipy.unique(NightViewTime)[-2]>240:      #Only value for pixels=255
          #    NightViewTimeRealMax.append (filename)
          if scipy.unique(DayViewAngle)[-2]>130:
              DayViewAngleRealMax.append (filename)
          #if scipy.unique(NightViewAngle)[-2]>130:     #Only value for pixels=255
          #    NightViewAngleRealMax.append (filename)  

print LSTDayRealMin          #Found no bad tiles
print DayViewTimeRealMax     #Found no bad tiles
print DayViewAngleRealMax    #Found no bad tiles
#---------------------------------------------------------------------------------------------
#Find % fill values in each file (this only makes sense for SDS's where fill value
# is NOT within acceptabe range.

sourceDir = "/data/project/organisms/MODIS_LST_Oregon"

#Create empty lists to hold data
Emiss_Band31PCTFill=[]
DayViewTimePCTFill=[]
NightViewTimePCTFill=[]
DayViewAnglePCTFill=[]
NightViewAnglePCTFill=[]
LSTDayPCTFill=[]
LSTNightPCTFill=[]
Emiss_Band32PCTFill=[]

for dirname, dirnames, filenames in os.walk(sourceDir):
  for filename in filenames:
        ext = os.path.splitext(filename)[1]
        if ext == ".hdf":
  	  infile= sourceDir + "/" + filename
          hdf4SDS= SD.SD(infile)                            #Get SDS's
          lstday1km=hdf4SDS.select('LST_Day_1km')           #Select specific SDS
          LSTDay1km=lstday1km.get()   			    #Get data (this is an array)
          LSTDayFlat= LSTDay1km.flatten ([LSTDay1km])	    #Flatten array for searchability
          LSTDaySort=sort(LSTDayFlat)	
          LSTDayFill= LSTDaySort.tolist()	            #Coerce flattened array into list
          LSTDayPCTFill.append("%.5f" % (LSTDayFill.count (0)/1440000.0))  #Count #fills/tot pixels
          lstnight1km=hdf4SDS.select('LST_Night_1km')          
          LSTNight1km=lstnight1km.get()
          LSTNightFlat= LSTNight1km.flatten ([LSTNight1km])
          LSTNightSort=sort(LSTNightFlat)
          LSTNightFill= LSTNightSort.tolist()
          LSTNightPCTFill.append("%.5f" % (LSTNightFill.count (0)/1440000.0))
          dvtime=hdf4SDS.select('Day_view_time')
          DayViewTime=dvtime.get()
          DayViewTimeFlat= DayViewTime.flatten ([DayViewTime])
          DayViewTimeSort=sort(DayViewTimeFlat)
          DayViewTimeFill= DayViewTimeSort.tolist()
          DayViewTimePCTFill.append("%.5f" % (DayViewTimeFill.count (255)/1440000.0))
          nvtime=hdf4SDS.select('Night_view_time')
          NightViewTime=nvtime.get()
          NightViewTimeFlat= NightViewTime.flatten ([NightViewTime])
          NightViewTimeSort=sort(NightViewTimeFlat)
          NightViewTimeFill= NightViewTimeSort.tolist()
          NightViewTimePCTFill.append("%.5f" % (NightViewTimeFill.count (255)/1440000.0))
          dvang=hdf4SDS.select('Day_view_angl')
          DayViewAngle=dvang.get()
          DayViewAngleFlat= DayViewAngle.flatten ([DayViewAngle])
          DayViewAngleSort=sort(DayViewAngleFlat)
          DayViewAngleFill= DayViewAngleSort.tolist()
          DayViewAnglePCTFill.append("%.5f" % (DayViewAngleFill.count (255)/1440000.0))
          nvang=hdf4SDS.select('Night_view_angl')
          NightViewAngle=nvang.get()     
          NightViewAngleFlat= NightViewAngle.flatten ([NightViewAngle])
          NightViewAngleSort=sort(NightViewAngleFlat)
          NightViewAngleFill= NightViewAngleSort.tolist()
          NightViewAnglePCTFill.append("%.5f" % (NightViewAngleFill.count (255)/1440000.0))
          emis31=hdf4SDS.select('Emis_31')
          Emiss_Band31=emis31.get()
          Emiss_Band31Flat= Emiss_Band31.flatten ([Emiss_Band31])
          Emiss_Band31Sort=sort(Emiss_Band31Flat)
          Emiss_Band31Fill= Emiss_Band31Sort.tolist()
          Emiss_Band31PCTFill.append("%.5f" % (Emiss_Band31Fill.count (0)/1440000.0))
          emis32=hdf4SDS.select('Emis_31')
          Emiss_Band32=emis32.get()
          Emiss_Band32Flat= Emiss_Band32.flatten ([Emiss_Band32])
          Emiss_Band32Sort=sort(Emiss_Band32Flat)
          Emiss_Band32Fill= Emiss_Band32Sort.tolist()
          Emiss_Band32PCTFill.append("%.5f" % (Emiss_Band32Fill.count (0)/1440000.0))



SDSFile= open('/data/project/organisms/MODIS_LST_Oregon/Test/SDS_PctFills.txt','w')
#SDSFile.write(" ".join(["%s" % el for el in filenames]))    #Doesn't work- filenames is a list
SDSFile.write("%s\n" % LSTDayPCTFill)
SDSFile.write("%s\n" % LSTNightPCTFill)
SDSFile.write("%s\n" % DayViewTimePCTFill)
SDSFile.write("%s\n" % NightViewTimePCTFill)              
SDSFile.write("%s\n" % DayViewAnglePCTFill)
SDSFile.write("%s\n" % NightViewAnglePCTFill) 
SDSFile.write("%s\n" % Emiss_Band31PCTFill)
SDSFile.write("%s\n" % Emiss_Band32PCTFill)
SDSFile.close()


