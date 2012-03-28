#This script was written by Benoit Parmentier on March 27, 2012.
#This script computes the daily average for MOD11A1 data. The code takes into account the QC and computes the number of valid observation.
#Note that this code will only work with a specific format: Oregon_2003_054_MOD11A1_Reprojected_LST_Day_1km.rst
#This code should be reviewed to use the datetime package in python. As of now, the code relies on the IDRISI software's API.

import os, glob, sys
from osgeo import gdal, gdalconst
import win32com.client
import shutil
from datetime import date
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
bi_sextile_years='2004,2008'

t1 = 2  #minimum threshold to consider a pixel as good quality in the LST QC flag level1
t2 = 3  #minimum threshold to consider a pixel as good quality in the LST QC flag level2

#START OF THE SCRIPT
api = win32com.client.Dispatch('idrisi32.IdrisiAPIServer')    # api is the handle for IDRISI api
##wd = api.GetWorkingDir();                                      #Is variable containing the path of the working directory
##wd = 'H:\Benoit_Backup\Paper3_STA_07202011\\'

##PART applies the QC mask to the 
#Idrisi_filename2='Oregon_2004_366_MOD11A1_Reprojected_QC_Day'
#MODIS QC extraction

listfiles_wd1 = glob.glob(wd2+'*'+'Reprojected'+'*LST*.rst') #list the all the LST files of interest
listfiles_wd3 = glob.glob(wd2+'*'+'Reprojected'+'*QC*.rst') #list the all the LST files of interest
nb_files = len(listfiles_wd1)
loop_number3=0
i=0
##
##for i in range(1,nb_files+1):
##            
##    loop_number3 = loop_number3 + 1
##    filename1= listfiles_wd1[i-1]
##    idrisi_filename1= filename1[filename1.rfind('\\')+1:-4] #Remove the path and the extension
##
##    filename3= listfiles_wd3[i-1]
##    idrisi_filename3= filename3[filename3.rfind('\\')+1:-4] #Remove the path and the extension
##
##    #Conversion of MODISQC into quality information: level 1
##    parameters ='2'+'*'+wd2+idrisi_filename3+'.rst'+'*'+wd1+idrisi_filename3+'_L1'+'.rst'+'*1'
##    module = 'MODISQC'
##    print('Running ' + module + ' module with parameters ' + parameters)
##    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
##    if not success:
##           print('Error running ' + module + '!')
##           
##    #Conversion of MODISQC into quality information: level 2
##    parameters ='2'+'*'+wd2+idrisi_filename3+'.rst'+'*'+wd1+idrisi_filename3+'_L2'+'.rst'+'*2'
##    module = 'MODISQC'
##    print('Running ' + module + ' module with parameters ' + parameters)
##    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
##    if not success:
##           print('Error running ' + module + '!')
##
##    #OVERLAY to eliminate bad pixels: this is using QC level 1
##
##    #RECLASS  I*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399_allocating_bool_Class_83overlay_random.rst*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\selection_83.rst*3*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\idrtemp.rcl*1       
##    parameters = 'I'+'*'+wd1+idrisi_filename3+'_L1'+'.rst'+'*'+wd1+idrisi_filename3+'_L1'+'_rec_'+str(t1)+'.rst'+'*2'+'*0*0*'+str(t1)+'*1*'+str(t1)+'*5'+'*-9999*1'  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
##    module = 'reclass'
##    print('Running ' + module + ' module with parameters ' + parameters)
##    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
##    if not success:
##        print('Error running ' + module + '!')
##
##    #OVERLAY to eliminate bad pixels: this is using QC level 2
##    #RECLASS  I*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\OVERLAY_ID_83_399_144_TEST_BURNT_83_144_399_allocating_bool_Class_83overlay_random.rst*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\selection_83.rst*3*C:\Data\Benoit\Clark_University\Python\dissertationunburnt_pixels_selection\idrtemp.rcl*1       
##    parameters = 'I'+'*'+wd1+idrisi_filename3+'_L2'+'.rst'+'*'+wd1+idrisi_filename3+'_L2'+'_rec_'+str(t2)+'.rst'+'*2'+'*0*0*'+str(t2)+'*1*'+str(t2)+'*5'+'*-9999*1'  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
##    module = 'reclass'
##    print('Running ' + module + ' module with parameters ' + parameters)
##    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
##    if not success:
##        print('Error running ' + module + '!')
##            
##    #"This file was created by the MODISQC module with the command line:",2*H:\Data\IPLANT_project\outrst_data\Oregon_2004_366_MOD11A1_Reprojected_QC_Day.rst*H:\Data\IPLANT_project\IPLANT_idrisi_project_03062012\test2.rst*2
##    ###OVERLAY MODULE###    
##    parameters = '3*'+wd1+idrisi_filename3+'_L1'+'_rec_'+str(t1)+'.rst'+'*'+wd1+idrisi_filename3+'_L2'+'_rec_'+str(t2)+'.rst'+'*'+wd1+idrisi_filename3+'_mask_'+str(t1)+'_'+str(t2)+'.rst'
##    module = 'overlay'
##    print('Running ' + module + ' module with parameters ' + parameters)
##    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
##    if not success:
##        print('Error running ' + module + '!')
##
##    #This file was created by the MODISQC module with the command line:",2*H:\Data\IPLANT_project\outrst_data\Oregon_2004_366_MOD11A1_Reprojected_QC_Day.rst*H:\Data\IPLANT_project\IPLANT_idrisi_project_03062012\test2.rst*2
##    ###OVERLAY MODULE###    
##    parameters = '3*'+wd1+idrisi_filename3+'_mask_'+str(t1)+'_'+str(t2)+'.rst'+'*'+wd2+idrisi_filename1+'.rst'+'*'+wd1+idrisi_filename1+'_masked_'+str(t1)+'_'+str(t2)+'.rst'
##    module = 'overlay'
##    print('Running ' + module + ' module with parameters ' + parameters)
##    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
##    if not success:
##        print('Error running ' + module + '!')
##
##    ###Reclass valid values ###    
##    parameters = 'I'+'*'+wd1+idrisi_filename1+'_masked_'+str(t1)+'_'+str(t2)+'.rst'+'*'+wd1+idrisi_filename1+'_masked_bool_'+str(t1)+'_'+str(t2)+'.rst'+'*2'+'*0*0*'+str(min_range)+'*1*'+str(min_range)+'*'+str(max_range)+'*-9999*1'  #option 2 is an avl file with option 1 , count in cells in textfile '*.avl'
##    module = 'reclass'
##    print('Running ' + module + ' module with parameters ' + parameters)
##    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
##    if not success:
##        print('Error running ' + module + '!')
##                     
##
#PART 2 Calculating average and number of valid observations

listfiles_wd2 = glob.glob(wd1+'*'+'Reprojected'+'*LST*'+'_masked_'+str(t1)+'_'+str(t2)+'.rst') #list the all the LST files of interest
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
day=listfiles_wd2[:]
i=0;
for filename in listfiles_wd2:
    #filename = listfiles_wd2[i] #list the files of interest
    day[i] = filename[filename.rfind('_MOD')-3:filename.rfind('_MOD')]
    i=i+1

day_no =list(set(day))    #this is the unique set of day in the list. It must be 366 if there are bisextiles years
day_no.sort()             


table_data = numpy.ones((len(day_no),2)) #declaring an array of size stop+1 and initialized with number 1

i=0
#Grouping files per day from 1 to 366 (or 
for day in day_no:
    i=i+1
    #listfiles_wd3=glob.glob(wd1+'*20**'+day+'*Reprojected'+'*LST*'_masked_'+str(t1)+'_'+str(t2)+'.rst') #list the files of interest: finding the day of each file
    listfiles_wd3=glob.glob(wd1+'*20**'+day+'*Reprojected'+'*LST*'+'_masked_'+str(t1)+'_'+str(t2)+'.rst')
    table_data[i-1,0]= day
    table_data[i-1,1]= len(listfiles_wd3)    #number of available data for a specific date
    f4 = open( wd1+'list_day_'+day+'.rgf','w')  #This is the output rgf masked
    line0 = str(len(listfiles_wd3))+'\n' #This line contains the number of files per day
    f4.write(line0)
    for line in listfiles_wd3:
        line = line[line.rfind('\\')+1:]
        #line = line[line.rfind('\\')+1:-4]
        line=line.replace('.rst','\n')
        f4.write(line) 
    f4.close()

    #listfiles_wd3=glob.glob(wd1+'*20**'+day+'*Reprojected'+'*LST*'_masked_'+str(t1)+'_'+str(t2)+'.rst') #list the files of interest: finding the day of each file
    listfiles_wd4=glob.glob(wd1+'*20**'+day+'*Reprojected'+'*LST*'+'_masked_bool_'+str(t1)+'_'+str(t2)+'.rst')
    #f4 = open( wd2+'ps_'+inputf3+'_'+str(nb_files)+'.rgf','w')
    f5 = open( wd1+'list_day_valid_obs_'+day+'.rgf','w')  #This is the output rgf masked
    line0 = str(len(listfiles_wd4))+'\n'
    f5.write(line0)
    for line in listfiles_wd4:
        line = line[line.rfind('\\')+1:]
        #line = line[line.rfind('\\')+1:-4]
        line=line.replace('.rst','\n')
        f5.write(line) 
    f5.close()    
   
    rgf_name = wd1+'list_day_'+str(day)+'.rgf'
    outfilename =wd1+'mean_day_'+day
   
    # Calulating the average with TSTAT     
    parameters ='1'+'*'+rgf_name+'*'+outfilename+'*'+str(min_range)+'*'+str(max_range)+'*'+str(background)+'*'+str(min_nb_obs)
    module = 'TStats'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
           print('Error running ' + module + '!')

    rgf_name2 = wd1+'list_day_valid_obs_'+str(day)+'.rgf'
    outfilename2 =wd1+'mean_day_valid_obs_'+day
    
    # Calulating the number of valid observation with TSTAT     
    parameters ='100000'+'*'+rgf_name2+'*'+outfilename2+'*'+str(0)+'*'+str(1)+'*'+str(background)+'*'+str(min_nb_obs)
    module = 'TStats'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
           print('Error running ' + module + '!')           

    # Rescaling the values with scalar
    #H:\Data\IPLANT_project\python_working_dir\Oregon_2008_366_MOD11A1_Reprojected_LST_Day_1km_masked_1_3.rst*H:\Data\IPLANT_project\IPLANT_idrisi_project_03062012\test.rst*3*0.02
    parameters = wd1+'mean_day_'+day+'_Mean.rst'+'*'+wd1+'mean_day'+day+'_rescaled.rst'+'*3*'+'0.02'
    module = 'scalar'
    print('Running ' + module + ' module with parameters ' + parameters)
    success = api.RunModule(module, parameters, True, '', '', '', '', True)  #the output of api.runmodule is a value '1' if it is sucessful and '0' if not.
    if not success:
           print('Error running ' + module + '!')

outfile1=wd1+'table_data.txt'
numpy.savetxt(outfile1, table_data, fmt='%-7.2f')    

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