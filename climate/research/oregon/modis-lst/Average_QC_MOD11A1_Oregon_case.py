#This script was written by Benoit Parmentier on March 27, 2012.
#This script computes the daily average for MOD11A1 data. The code takes into account the QC and computes the number of valid observation.
#Note that this code will only work with a specific format: Oregon_2003_054_MOD11A1_Reprojected_LST_Day_1km.rst
#This code should be reviewed to use the datetime package in python. As of now, the code relies on the IDRISI software's API.

import os, glob, sys
from osgeo import gdal, gdalconst
import win32com.client
import shutil
from datetime import date
from datetime import datetime
import calendar
import numpy

#variables, input parameters

wd1='H:\Data\IPLANT_project\python_working_dir3\\'  #output folder
wd2='H:\Data\IPLANT_project\outrst_data\\'         #input folder containg the QC and LST files

min_range =7500  #This value can be changed, it correspond to the valid range in LST MOD11A1
max_range =65535
background =0
min_nb_obs =1
nb_years =10
year_start=2001
year_end=2010
bi_sextile_years='2004,2008'

t1 = 2  #minimum threshold to consider a pixel as good quality in the LST QC flag level1
t2 = 3  #minimum threshold to consider a pixel as good quality in the LST QC flag level2

#START OF THE SCRIPT
api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api

#PART 2 Calculating average and number of valid observations

listfiles_wd2 = glob.glob(wd1+'*'+'Reprojected'+'*LST*'+'_masked_'+str(t1)+'_'+str(t2)+'.rst') #list the all the LST files of interest
listfiles_wd2.sort() #sorting the list
listfiles_wd4 = glob.glob(wd1+'*'+'*Reprojected'+'*LST*'+'_masked_bool_'+str(t1)+'_'+str(t2)+'.rst')
listfiles_wd4.sort() #sorting the list

#To find the number of valid points used in the average calculation, one must reclass the pixel values in IDRISI, this is inefficient and
#can be improved a lot later on.

f4 = open( wd1+'output_list'+'.rgf','w')
line0 = str(len(listfiles_wd2))+'\n'
f4.write(line0)
for line in listfiles_wd2:
    line = line[line.rfind('\\')+1:]
    #line = line[line.rfind('\\')+1:-4]
    line=line.replace('.rst','\n')
    f4.write(line) 
f4.close()

#EXTRACTING THE DOY from the files names: Note that this code will only work with a specific format: Oregon_2003_054_MOD11A1_Reprojected_LST_Day_1km.rst

date_start= date(year_start,1,1)  #This creates a date object for the first day of MODIS LST
date_end=date(year_end,12,31)

day=listfiles_wd2[:]
table_data = numpy.ones((12,2))#declaring an array of size stop+1 and initialized with number 1

i=0;
day_no =list(set(day))    #this is the unique set of day in the list. It must be 366 if there are bisextiles years
day_no.sort()

nb_month=12

list_files=list()
list_files[:]=[]
list_files2=list()
list_files2[:]=[]
monthlist=list()
monthlist[:]=[]
monthlist2=list()
monthlist2[:]=[]
i=1

table_data = numpy.ones((12,2)) #declaring an array of size stop+1 and initialized with number 1

for i in range(1,nb_month+1):
    #i=i+1
    #filename = listfiles_wd2[i] #list the files of interest
    monthlist[:]=[] #empty the list
    monthlist2[:]=[] #empty the list
    j=0
    for filename in listfiles_wd2:
        date = filename[filename.rfind('_MOD')-8:filename.rfind('_MOD')]
        d=datetime.strptime(date,'%Y_%j') #This produces a date object from a string extracted from the file name.
        month=d.strftime('%m')
        if int(month)==i:
            monthlist.append(listfiles_wd2[j])
            monthlist2.append(listfiles_wd4[j])
        j=j+1
    list_files.append(monthlist)   #This is a list of a list containing a list for each month
    list_files2.append(monthlist2)
    #NOW Write RGF   
    f4 = open( wd1+'list_month_'+str(i)+'.rgf','w')  #This is the output rgf masked
    line0 = str(len(list_files[i-1]))+'\n' #This line contains the number of files per day
    f4.write(line0)
    for line in list_files[i-1]:
        line = line[line.rfind('\\')+1:]
        #line = line[line.rfind('\\')+1:-4]
        line=line.replace('.rst','\n')
        f4.write(line) 
    f4.close()
    
    f5 = open( wd1+'list_month_valid_obs_'+str(i)+'.rgf','w')  #This is the output rgf masked
    line0 = str(len(list_files2[i-1]))+'\n'
    f5.write(line0)
    for line in list_files2[i-1]:
        line = line[line.rfind('\\')+1:]
        #line = line[line.rfind('\\')+1:-4]
        line=line.replace('.rst','\n')
        f5.write(line) 
    f5.close()
    
#    i=i+1


#Grouping files per month from 1 to 12 
#for day in day_no:
 #   i=i+1
    #table_data[i-1,0]= month
    #table_data[i-1,1]= len(list_files[i-1])    #number of available data for a specific date
   
    # Calulating the average with TSTAT      
    rgf_name = wd1+'list_month_'+str(i)+'.rgf'
    outfilename =wd1+'mean_month_'+str(i)
    
    parameters ='1'+'*'+rgf_name+'*'+outfilename+'*'+str(min_range)+'*'+str(max_range)+'*'+str(background)+'*'+str(min_nb_obs)
    module = 'TStats'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
           print('Error running ' + module + '!')

    rgf_name2 = wd1+'list_month_valid_obs_'+str(i)+'.rgf'
    outfilename2 =wd1+'mean_month_valid_obs_'+str(i)
    
    # Calulating the number of valid observation with TSTAT     
    parameters ='100000'+'*'+rgf_name2+'*'+outfilename2+'*'+str(0)+'*'+str(1)+'*'+str(background)+'*'+str(min_nb_obs)
    module = 'TStats'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
           print('Error running ' + module + '!')           

    # Rescaling the values with scalar
    #H:\Data\IPLANT_project\python_working_dir\Oregon_2008_366_MOD11A1_Reprojected_LST_Day_1km_masked_1_3.rst*H:\Data\IPLANT_project\IPLANT_idrisi_project_03062012\test.rst*3*0.02
    parameters = wd1+'mean_month_'+str(i)+'_Mean.rst'+'*'+wd1+'mean_month'+str(i)+'_rescaled.rst'+'*3*'+'0.02'
    module = 'scalar'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
           print('Error running ' + module + '!')

    i=i+1           

#outfile1=wd1+'table_data.txt'
#numpy.savetxt(outfile1, table_data, fmt='%-7.2f')    

#REPORT ON THE NUMBER OF MISSING VALUES FOR EACH DATE!!! and each pixel??        
#GDALcommand='gdal_translate F:\Data\Paper2_STA_03182012\dem\dem.bil F:\Data\Paper2_STA_03182012\dem_output2.rst -of RST'
#output = os.popen(GDALcommand).read()
        
#print('END OF SCRIPT')
### ==================================== #
### Load GDAL Drivers:
##dst_frmts    = 'rst' #data format driver for gdal
##dst_driver = gdal.GetDriverByName( dst_frmts )
##
##if dst_driver == None:
##    Usage()
##
###src_ds = gdal.Open( inputf1 )
##src_ds = gdal.Open( 'G:\\Benoit_Backup\\STA_PAPER2_02262011\\test5_ID.rst' )
##src_ds = gdal.Open('G:\Benoit_Backup\STA_PAPER2_02262011\GAP_ALB_07182011_filled_t3\\GAP_ALB_07182011_filled_t3_GAP_ALBEDO_16D_MASKED2_FILL4_6_MEAN.rst')
##src_rb = src_ds.GetRasterBand( 1 );
##
##x_size = src_rb.XSize  #nb_cols
##y_size = src_rb.YSize  #nb_rows
##
##src_rb.ReadAsArray(0,6,None,None,24,None)

# today = datetime.datetime.now()
# today.strftime('%j')  Returns the day of the year in a string.
# '083'
#

#THIS CAN BE USED TO CREATE A SEQUENCE
##def date_range(start, end):
##    r = (end+datetime.timedelta(days=1)-start).days
##    return [start+datetime.timedelta(days=i) for i in range(r)]
## 
##start = datetime.date(2007,01,01)
##end = datetime.date(2008,02,01)
##dateList = date_range(start, end)
##print '\n'.join([str(date) for date in dateList])