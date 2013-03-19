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
#  - put downloaded data somewhere other than working directory?
#  - write code to set up and take down a temporary GRASS location/mapset
#  - write code to export climatology rasters to file (geotiff? compressed?)
#  - calculate other aggregate stats? (stddev, ...)
#  - deal with nightly LST too?
#  - deal with more than just first two bits of QC flags?
#     e.g.: r.mapcalc 'qc_level2 = (qc>>2) & 0x03'   
#     0x03?? that means 0000_0011 and >>2 means shit twice on the right for 
#  - record all downloads to a log file?
#
# Jim Regetz
# NCEAS
# Created on 16-May-2012
# Modified by Benoit Parmentier
# NCEAS on 18-October-2012
# 

import os, glob
import datetime, calendar
import ftplib
import grass.script as gs

#from datetime import date
#from datetime import datetime


#------------------
# helper functions
#------------------

def yj_to_ymd(year, doy):
    """Return date as e.g. '2000.03.05' based on specified year and
    numeric day-of-year (doy) """
    date = datetime.datetime.strptime('%d%03d' % (year, doy), '%Y%j')
    return date.strftime('%Y.%m.%d')

def get_doy_range(year, month):
    """Determine starting and ending numeric day-of-year (doy)
    asscociated with the specified month and year.

    Arguments:
    year -- four-digit integer year
    month -- integer month (1-12)

    Returns tuple of start and end doy for that month/year.
    """
    last_day_of_month = calendar.monthrange(year, month)[1]
    start_doy = int(datetime.datetime.strptime('%d.%02d.%02d' % (year,
        month, 1), '%Y.%m.%d').strftime('%j'))
    end_doy = int(datetime.datetime.strptime('%d.%02d.%02d' % (year,
        month, last_day_of_month), '%Y.%m.%d').strftime('%j'))
    return (int(start_doy), int(end_doy))

#added by Benoit, to create a list of raster images for particular month (2001-2010)

def date_sequence(year_start,month_start,day_start,year_end,month_end,day_end):
    """Create a date sequence for a start date and end date defined by year, month, date
    Arguments:
    year_start -- starting year
 
    Returns list of absolute paths to the downloaded HDF files.Note the format yyyydoy
    """
    from datetime import date
    from dateutil.rrule import rrule, DAILY

    a = date(year_start, month_start,day_start)
    b = date(year_end, month_end, day_end)
    date_seq=list()
    for dt in rrule(DAILY, dtstart=a, until=b):
        print dt.strftime("%Y%j")    
        date_seq.append(dt.strftime("%Y%j"))
        
    return(date_seq)

#Added on January 16, 2012 by Benoit NCEAS
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
    
# quick function to return list of dirs in wd
def list_contents(ftp):
    """Parse ftp directory listing into list of names of the files
    and/or directories contained therein. May or may not be robust in
    general, but seems to work fine for LP DAAP ftp server."""
    listing = []
    ftp.dir(listing.append)
    contents = [item.split()[-1] for item in listing[1:]]
    return contents

def download_mod11a1(destdir, tile, start_doy, end_doy, year, ver=5):
    """Download into destination directory the MODIS 11A1 HDF files
    matching a given tile, year, and day range. If for some unexpected
    reason there are multiple matches for a given day, only the first is
    used. If no matches, the day is skipped with a warning message.

    Arguments:
    destdir -- path to local destination directory
    tile -- tile identifier (e.g., 'h08v04')
    start_doy -- integer start of day range (0-366)
    end_doy -- integer end of day range (0-366)
    year -- integer year (>=2000)
    ver -- MODIS version (4 or 5)

    Returns list of absolute paths to the downloaded HDF files.
    """
    # connect to data pool and change to the right directory
    ftp = ftplib.FTP('e4ftl01.cr.usgs.gov')
    ftp.login('anonymous', '')
    ftp.cwd('MOLT/MOD11A1.%03d' % ver)
    # make list of daily directories to traverse
    available_days = list_contents(ftp)
    desired_days = [yj_to_ymd(year, x) for x in range(start_doy, end_doy+1)]
    days_to_get = filter(lambda day: day in desired_days, available_days)
    if len(days_to_get) < len(desired_days):
        missing_days = [day for day in desired_days if day not in days_to_get]
        print 'skipping %d day(s) not found on server:' % len(missing_days)
        print '\n'.join(missing_days)
    # get each tile in turn
    hdfs = list()
    for day in days_to_get:
        ftp.cwd(day)
        files_to_get = [file for file in list_contents(ftp)
            if tile in file and file[-3:]=='hdf']
        if len(files_to_get)==0:
            # no file found -- print message and move on
            print 'no hdf found on server for tile', tile, 'on', day
            ftp.cwd('..') #added by Benoit
            continue
        elif 1<len(files_to_get):
            # multiple files found! -- just use the first...
            print 'multiple hdfs found on server for tile', tile, 'on', day
        file = files_to_get[0]
        local_file = os.path.join(destdir, file)
        ftp.retrbinary('RETR %s' % file, open(local_file, 'wb').write)
        hdfs.append(os.path.abspath(local_file))
        ftp.cwd('..')
    # politely disconnect
    ftp.quit()
    # return list of downloaded paths
    return hdfs

def get_hdf_paths(hdfdir, tile, start_doy, end_doy, year):
    """Look in supplied directory to find the MODIS 11A1 HDF files
    matching a given tile, year, and day range. If for some unexpected
    reason there are multiple matches for a given day, only the first is
    used. If no matches, the day is skipped with a warning message.

    Arguments:
    hdfdir -- path to directory containing the desired HDFs
    tile -- tile identifier (e.g., 'h08v04')
    start_doy -- integer start of day range (0-366)
    end_doy -- integer end of day range (0-366)
    year -- integer year (>=2000)

    Returns list of absolute paths to the located HDF files.
    """
    hdfs = list()
    for doy in range(start_doy, end_doy+1):
        fileglob = 'MOD11A1.A%d%03d.%s*hdf' % (year, doy, tile)
        pathglob = os.path.join(hdfdir, fileglob)
        files = glob.glob(pathglob)
        if len(files)==0:
            # no file found -- print message and move on
            print 'cannot access %s: no such file' % pathglob
            continue
        elif 1<len(files):
            # multiple files found! -- just use the first...
            print 'multiple hdfs found for tile', tile, 'on', day
        hdfs.append(os.path.abspath(files[0]))
    return hdfs

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

 
    #### MOdified by Benoit in March 2013 
    
    #Parameters
    #Inputs from R??
    tiles = ['h11v08','h11v07','h12v07','h12v08','h10v07','h10v08'] #These tiles correspond to Venezuela.
    start_year = 2001
    end_year = 2010
    end_month=12
    start_month=1
    hdfdir =  '/home/parmentier/Data/benoit_test' #destination file where hdf files are stored locally after download.
    night=1    # if 1 then produce night climatology
    out_suffix="_03192013"
    download=1  # if 1 then download files
    
    ################## First step: download data ###################
    # Added tile loop 
    year_list=range(start_year,end_year+1) #list of year to loop through
    #for testing...
    #year_list=[2001,2002]
    #hdfdir = '/home/parmentier/Data/benoit_test2' 
    #tiles = ['h10v07','h10v08','h12v07']
    if download==1:
        for tile in tiles:
            for year in year_list:
                start_doy = 1
                end_doy = 365
                if calendar.isleap(year):
                    end_doy=366
                    
                hdfs = download_mod11a1(hdfdir, tile, start_doy, end_doy, year)
    
    # modify loop to take into account "hdfs", list of hdf files needed in the next steps…  
    ################# Second step: compute climatology #################
    ## Process tile by tile...
    ##start loop for tile
    #tile= 'h12v08'
    #tiles = ['h12v07','h12v08']
    var_name = ['LST_Day_1km','LST_Night_1km']
    if night==1:
        lst_var = var_name[1]
    if night==0:
        lst_var = var_name[0]
        
    #start = time.clock()    
    for tile in tiles:        
        # Set up a temporary GRASS data base   
        tmp_location = 'tmp' + "_"+ tile + "_"+ str(os.getpid())        # Name of the temporary GRASS data base 
        orig_location = gs.gisenv()['LOCATION_NAME']
        orig_mapset = gs.gisenv()['MAPSET']
        gs.os.environ['GRASS_OVERWRITE'] = '1'         # Allow for overwrite?? GRASS data base  
            
        path_file = hdfdir
        os.chdir(path_file)    #set working directory
        os.getcwd()            #get current working directory
        listfiles_wd2 = glob.glob('*'+tile+'*'+'.hdf') #list the all the LST files of interest
        name_of_file = listfiles_wd2[0]    
        #Create the name of the layer to extract from the hdf file used for definition of the database
        lst1day = 'HDF4_EOS:EOS_GRID:%s/%s:%s' % (
        path_file,
        name_of_file,
        'MODIS_Grid_Daily_1km_LST:'+ lst_var)
        #'LST_Night_1km'
        #change the above line later to allow night LST for tmin
         
        #now import one image and set the location, mapset and database !!create name per tile
        gs.run_command('r.in.gdal', input=lst1day, output='LST_1day_'+tile,
               location=tmp_location)
    
        # Now that the new location has been create, switch to new location
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
        ### LST values in images must be masked out if they have low quality, determined from QC flags stored in hdf files
        LST = [load_qc_adjusted_lst(hdf) for hdf in hdfs]
        ### LST is a list that contains the name of the new lst files in the GRASS format. The files are stored in the temporary GRASS database.
        list_maps_month=list_raster_per_month(LST)    
        list_maps_name=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
        ##Now calculate clim per month, do a test for year1
        nb_month = 12
        for i in range(1,nb_month+1):
            clims = calc_clim(list_maps_month[i-1],lst_var+'_'+tile+'_'+list_maps_name[i-1]+'_'+str(i-1))
            for j in range(1, len(clims)+1):
                if j-1 ==0:
                    gs.run_command('r.out.gdal', input= clims[j-1], output=clims[j-1]+ out_suffix +'.tif', type='Float32')
                if j-1 ==1:
                    gs.mapcalc(' clim_rescaled = ('+ clims[j-1 ]+ ' * 0.02) -273.15')  
                    gs.run_command('r.out.gdal', input= 'clim_rescaled', output=clims[j-1]+ out_suffix+'.tif', type='Float32')
        #clims = calc_clim(LST, 'LST_%s_%d_%02d' % (tile, year, month))
        # clean up  if necessary
        
        #gs.run_command('g.remove', rast=','.join(LST))
        #gs.os.environ['GRASS_OVERWRITE'] = '0'
        
        # clean up
        #gs.run_command('g.gisenv', set='LOCATION_NAME=%s' % orig_location)
        #gs.run_command('g.gisenv', set='MAPSET=%s' % orig_mapset)
        #shutil.rmtree(os.path.join(gs.gisenv()['GISDBASE'], tmp_location))

    return None
