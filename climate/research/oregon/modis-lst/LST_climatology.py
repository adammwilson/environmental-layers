# -*- coding: utf-8 -*-
"""
Created on Fri Oct 26 23:48:57 2012

@author: benoitparmentier
"""

#!/usr/bin/python
#
# Early version of assorted Python code to download MODIS 11A1 (daily)
# data associated with specified dates and tiles, then interface with
# GRASS to calculate temporally averaged LST in a way that accounts for
# QC flags.
#
# TODO:
#  - functionalize to encapsulate high level procedural steps
#  - create a 'main' function to allow script to be run as a command
#    that can accept arguments?
#  - write code to set up and take down a temporary GRASS location/mapset
#  - calculate other aggregate stats? (stddev, ...)
#  - deal with more than just first two bits of QC flags?
#     e.g.: r.mapcalc 'qc_level2 = (qc>>2) & 0x03'   
#     0x03?? that means 0000_0011 and >>2 means shift twice on the right for 
#
# Created by Jim Regetz
# NCEAS on 16-May-2012
# Modified by Benoit Parmentier
#NCEAS on 18-October-2012
#NCEAS on 19-January-2013
#NCEAS on 16-May-2013
# 

import os, glob
import datetime, calendar
import ftplib
import grass.script as gs
import argparse
import shutil

#------------------
# helper functions
#------------------

#Added on January 16, 2012 by Benoit NCEAS
#Add other lists to calculate climatology?? later 
def list_raster_per_month(listmaps):
    """Determine starting and ending numeric day-of-year (doy)
    asscociated with the specified month and year.

    Arguments:
    maplist -- list of raster grass-digit integer year
    month -- integer month (1-12)
    year_range -- list of years

    Returns tuple of start and end doy for that month/year.
    """
    list_maps_month=list()
    nb_month=12
    for i in range(1,nb_month+1):
        #i=i+1
        #filename = listfiles_wd2[i] #list the files of interest
        #monthlist[:]=[] #empty the list
        monthlist=list()
        #monthlist2[:]=[] #empty the list
        j=0  #counter
        for mapname in listmaps:
            #tmp_str=LST[0]
            date_map=mapname.split('_')[1][1:8]
            #date = filename[filename.rfind('_MOD')-8:filename.rfind('_MOD')]
            d=datetime.datetime.strptime(date_map,'%Y%j') #This produces a date object from a string extracted from the file name.
            month=d.strftime('%m') #Extract the month of the current map
            if int(month)==i:
                #monthlist.append(listmaps[j])
                monthlist.append(mapname)
                #monthlist2.append(listmaps[j])
            j=j+1
        list_maps_month.append(monthlist)   #This is a list of a list containing a list for each month
        #list_files2.append(monthlist2)
    return(list_maps_month)    

def calc_clim(maplist, name, overwrite=False):
    """Generate some climatalogies in GRASS based on the input list of
    maps. As usual, current GRASS region settings apply. Produces the
    following output rasters:
      * nobs: count of number of (non-null) values over the input maps
      * mean: arithmetic mean of (non-null) values over the input maps

    Arguments:
    maplist -- list of names of GRASS maps to be aggregated
    name -- template (suffix) for GRASS output map names

    Returns list of names of the output maps created in GRASS.
    """
    denominator = '(%s)' % '+'.join(['if(!isnull(%s))' % m
        for m in maplist])
    gs.mapcalc('nobs_%s = %s' % (name, denominator), overwrite=overwrite)
    numerator = '(%s)' % '+'.join(['if(isnull(%s), 0, %s)' % (m, m)
        for m in maplist])
    gs.mapcalc('mean_%s = round(float(%s)/nobs_%s)' % (name, numerator, name),
        overwrite=overwrite)
    return ['%s_%s' % (s, name) for s in ['nobs', 'mean']]

def load_qc_adjusted_lst(hdf):
    """Load LST_Day_1km into GRASS from the specified hdf file, and
    nullify any cells for which QA flags indicate anything other than
    high quality.

    Argument:
    hdf -- local path to the 11A1 HDF file

    Returns the name of the QC-adjusted LST map created in GRASS.
    """
    lstname =  'LST_' + '_'.join(os.path.basename(hdf).split('.')[1:3])               
    # read in lst grid
    gs.run_command('r.in.gdal',
        input='HDF4_EOS:EOS_GRID:%s:MODIS_Grid_Daily_1km_LST:LST_Day_1km' % hdf,
        output=lstname)            # No new location created since it has already been done with r.in.gdal before!!!

    # read in qc grid
    gs.run_command('r.in.gdal',
        input = 'HDF4_EOS:EOS_GRID:%s:MODIS_Grid_Daily_1km_LST:QC_Day' % hdf,
        output = 'qc_this_day')
    gs.run_command('g.region', rast=lstname)
    # null out any LST cells for which QC is not high [00]  #THIS WHERE WE MIGHT NEED TO CHANGE...
    gs.mapcalc('${lst} = if((qc_this_day & 0x03)==0, ${lst}, null())',
        lst=lstname, overwrite=True)
    # clean up
    gs.run_command('g.remove', rast='qc_this_day')
    # return name of qc-adjusted LST raster in GRASS
    return lstname

def main():
    #--------------------------------------------
    # Download and Calculate monthly climatology for daily LST time series for a specific area
    #--------------------------------------------

    # TODO: set up a (temporary?) GRASS database to use for processing? code
    # currently assumes it's being run within an existing GRASS session
    # using an appropriately configured LOCATION...
    #
    # note the following trick to fix datum for modis sinu;
    # TODO: check that this works properly...compare to MRT output?
    # gs.run_command('g.proj', flags='c',
    #     proj4='+proj=sinu +a=6371007.181 +b=6371007.181 +ellps=sphere')
    ##    proj4='+proj=sinu +R=6371007.181 +nadgrids=@null +wktext')

    ########## START OF SCRIPT ##############
    #### Modified by Benoit on May 13, 2013 
    
    ### INPUT Parameters
    #Inputs from R?? there are 9 parameters
   #tiles = ['h11v08','h11v07','h12v07','h12v08','h10v07','h10v08'] #These tiles correspond to Venezuela.
    #tiles= ['h08v04','h09v04']    
    #start_year = 2001
    #end_year = 2010
    #start_month=1
    #end_month=12
    #hdfdir =  '/home/layers/commons/modis/MOD11A1_tiles' #destination file where hdf files are stored locally after download.
    #hdfdir =  '/home/parmentier/Data/IPLANT_project/MOD11A1_tiles' #destination file where hdf files are stored locally after download.
    #night=1    # if 1 then produce night climatology
    #out_suffix="_03192013"
    #download=1  # if 1 then download files
   
    #Passing arguments from the shell...using positional assignment
    parser = argparse.ArgumentParser()
    parser.add_argument("tiles", type=str, help="list of Modis tiles")
    parser.add_argument("start_year", type=int, help="start year")
    parser.add_argument("end_year", type=int, help="end year")
    parser.add_argument("start_month", type=int, help="start month")
    parser.add_argument("end_month", type=int, help="end month")
    parser.add_argument("hdfdir", type=str, help="destination/source directory for hdf file")
    parser.add_argument("night", type=int, help="night")
    parser.add_argument("download", type=int, help="out_suffix")
    parser.add_argument("out_suffix", type=str, help="out_suffix")

    myargs = parser.parse_args()

    tiles = myargs.tiles #These tiles correspond to Venezuela.
    start_year = myargs.start_year
    end_year = myargs.end_year 
    end_month = myargs.end_month #not used...to be removed
    start_month= myargs.start_month #not used...to be removed
    hdfdir =  myargs.hdfdir
    night= myargs.night    # if 1 then produce night climatology
    out_suffix= myargs.out_suffix #"_03192013"
    download=myargs.download# if 1 then download files
    
    tiles =tiles.split(",") #Create a list from string
    #need to add on the fly creation of folder for each tile!!
    
    ################## First step: download data ###################
    # Added tile loop 
    year_list=range(start_year,end_year+1) #list of year to loop through
   
    # modify loop to take into account "hdfs", list of hdf files needed in the next steps…  
    ################# Second step: compute climatology #################
    ## Process tile by tile...
    var_name = ['LST_Day_1km','LST_Night_1km']
    if night==1:
        lst_var = var_name[1]
    if night==0:
        lst_var = var_name[0]
        
    #start = time.clock()    
    for tile in tiles:        
        # Set up a temporary GRASS data base   
        tmp_location = 'tmp' + "_"+ tile + "_"+ str(os.getpid())        # Name of the temporary GRASS data base 
        orig_location = gs.gisenv()['LOCATION_NAME']   #Current location  of GRASS DATABASE at start up (config grassgrc?)
        orig_mapset = gs.gisenv()['MAPSET']   #Current mapset of GRASS DATABASE at start up (grassgrc config)
        gs.os.environ['GRASS_OVERWRITE'] = '1'         # Allow for overwrite?? GRASS data base  
            
        #path_file = hdfdir #change
        path_file = os.path.join(hdfdir,tile)
        
        os.chdir(path_file)    #set working directory
        os.getcwd()            #get current working directory
        listfiles_wd2 = glob.glob('*'+tile+'*'+'.hdf') #list the all the hdf modis LST files of interest
        name_of_file = listfiles_wd2[0]    
        #Create the name of the layer to extract from the hdf file used for definition of the database
        lst1day = 'HDF4_EOS:EOS_GRID:%s/%s:%s' % (
        path_file,
        name_of_file,
        'MODIS_Grid_Daily_1km_LST:'+ lst_var)

        #now import one image and set the location, mapset and database !!create name per tile
        gs.run_command('r.in.gdal', input=lst1day, output='LST_1day_'+tile,
               location=tmp_location)
    
        # Now that the new location has been created, switch to new location
        gs.run_command('g.gisenv', set='LOCATION_NAME=%s' % tmp_location)
        gs.run_command('g.gisenv', set='MAPSET=PERMANENT')
    
        # set GRASS the projection system and GRASS region to match the extent of the file
        gs.run_command('g.proj', flags='c',
        proj4='+proj=sinu +a=6371007.181 +b=6371007.181 +ellps=sphere') #This should be set in the parameters
        gs.run_command('g.region', rast='LST_1day_'+tile)
    
        #generate monthly pixelwise mean & count of high-quality daytime LST
        gs.os.environ['GRASS_OVERWRITE'] = '1'
    
        #Provide a list of file "hdfs" to process…this should be in a loop...
        #tile= 'h12v08'
        fileglob = 'MOD11A1.A*.%s*hdf' % (tile)
        pathglob = os.path.join(path_file, fileglob)
        files = glob.glob(pathglob)
        hdfs = files
        os.chdir(hdfdir)    #set working directory to the general path to modis files...
        
        ### LST values in images must be masked out if they have low quality, determined from QC flags stored in hdf files
        LST = [load_qc_adjusted_lst(hdf) for hdf in hdfs]
        #load qc takes about 8 minutes for about 350 files        
        ### load_qc is a list that contains the name of the new lst files in the GRASS format. The files are stored in the temporary GRASS database.
        list_maps_month=list_raster_per_month(LST)    
        list_maps_name=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
        ##Now calculate clim per month, do a test for year1
        nb_month = 12
        for i in range(1,nb_month+1):
            clims = calc_clim(list_maps_month[i-1],lst_var+'_'+tile+'_'+list_maps_name[i-1]+'_'+str(i-1))
            for j in range(1, len(clims)+1):
                if j-1 ==0:
                    gs.run_command('r.out.gdal', input= clims[j-1], output=clims[j-1]+ out_suffix +'.tif', type='Float64')
                if j-1 ==1:
                    gs.mapcalc(' clim_rescaled = ('+ clims[j-1 ]+ ' * 0.02) -273.15')  
                    gs.run_command('r.out.gdal', input= 'clim_rescaled', output=clims[j-1]+ out_suffix+'.tif', type='Float64')
        #clims = calc_clim(LST, 'LST_%s_%d_%02d' % (tile, year, month))
        
        # clean up GRASS DATABASE: ok working
        
        gs.run_command('g.remove', rast=','.join(LST))
        ## get list of remaining files...
        maps_list = gs.mlist_strings(type = 'rast') #list all remaining maps in the location
        gs.run_command('g.remove', rast=','.join(maps_list)) #remove remaning maps
        gs.os.environ['GRASS_OVERWRITE'] = '0'
        
        #clean up temporary location and set the original GRASS database parameters back
        gs.run_command('g.gisenv', set='LOCATION_NAME=%s' % orig_location)
        gs.run_command('g.gisenv', set='MAPSET=%s' % orig_mapset)
        shutil.rmtree(os.path.join(gs.gisenv()['GISDBASE'], tmp_location))

    return None
    
#Need to add this to run
if __name__ == '__main__':
    main()


