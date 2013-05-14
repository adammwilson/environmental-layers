# -*- coding: utf-8 -*-
"""
Created on Mon May 13 10:02:02 2013

@author: parmentier
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
#  - record all downloads to a log file?
#
# Created by Jim Regetz
# NCEAS on 16-May-2012
# Modified by Benoit Parmentier
#NCEAS on 18-October-2012
#NCEAS on 19-January-2013
#NCEAS on 14-May-2013
# 

import os, glob
import datetime, calendar
import ftplib
#import grass.script as gs
import argparse

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


def main():
    #from sys import argv                       # example client code
    #myargs = getopts(argv)
    
    import argparse
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
    end_year = myargs.start_year 
    end_month = myargs.end_month
    start_month= myargs.start_month
    hdfdir =  myargs.hdfdir
    night= myargs.night    # if 1 then produce night climatology
    out_suffix= myargs.out_suffix #"_03192013"
    download=myargs.download# if 1 then download files
    
    ################## First step: download data ###################
    # Added tile loop 
    year_list=range(start_year,end_year+1) #list of year to loop through
    #print 'Number of arguments:', len(myargs), 'arguments.'
    #print 'Argument List:', str(myargs)
    
    tiles =tiles.split(",") #Create a list from string
    print 'this is the result',str(year_list)
    print(myargs)
    print(tiles)

    #tiles = ['h11v08','h11v07','h12v07','h12v08','h10v07','h10v08'] #These tiles correspond to Venezuela.
    #start_year = 2001
    #end_year = 2010
    #end_month=12
    #start_month=1
    #hdfdir =  '/home/parmentier/Data/benoit_test' #destination file where hdf files are stored locally after download.
    #night=1    # if 1 then produce night climatology
    #out_suffix="_03192013"
    #download=1  # if 1 then download files
    
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

if __name__ == '__main__':
    main()
#    from sys import argv                       # example client code
#    myargs = getopts(argv)
#    if '-a' in myargs:
#        print(myargs['-a'])
#    if '-b' in myargs:
#        print(myargs['-b'])
#    print(myargs)
    
#i = 1
#while i < arg_length:
#    arg = argv[i]
#    if arg =='-a':
#        a     = argv[i]
#    elif arg =='-b':
#        b     = argv[i]

#x = a + b

#print x