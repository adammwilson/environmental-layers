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
#  - record all downloads to a log file?
#
# Jim Regetz
# NCEAS
# Created on 16-May-2012

import os, glob
import datetime, calendar
import ftplib
import grass.script as gs

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
        output=lstname)
    # read in qc grid
    gs.run_command('r.in.gdal',
        input = 'HDF4_EOS:EOS_GRID:%s:MODIS_Grid_Daily_1km_LST:QC_Day' % hdf,
        output = 'qc_this_day')
    gs.run_command('g.region', rast=lstname)
    # null out any LST cells for which QC is not high [00]
    gs.mapcalc('${lst} = if((qc_this_day & 0x03)==0, ${lst}, null())',
        lst=lstname, overwrite=True)
    # clean up
    gs.run_command('g.remove', rast='qc_this_day')
    # return name of qc-adjusted LST raster in GRASS
    return lstname


#--------------------------------------------
# test procedures mostly for timing purposes
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

# (1) download then aggregate for one month

tile = 'h09v04'
year = 2005
month = 1
hdfdir = '.'

# determine range of doys for the specified month
start_doy, end_doy = get_doy_range(year, month)
# download data
### [atlas 17-May-2012] Wall time: 111.62 s
hdfs = download_mod11a1(hdfdir, tile, start_doy, end_doy, year)
# generate monthly pixelwise mean & count of high-quality daytime LST
### [atlas 17-May-2012] Wall time: 53.79 s
gs.os.environ['GRASS_OVERWRITE'] = '1'
LST = [load_qc_adjusted_lst(hdf) for hdf in hdfs]
clims = calc_clim(LST, 'LST_%s_%d_%02d' % (tile, year, month))
# clean up
gs.run_command('g.remove', rast=','.join(LST))
gs.os.environ['GRASS_OVERWRITE'] = '0'


# (2) aggregate all 12 months in one year, using local data

tile = 'h09v04'
year = 2005
hdfdir = '/home/layers/data/climate/MOD11A1.004-OR-orig'

### [atlas 17-May-2012] Wall time: 802.86 s
gs.os.environ['GRASS_OVERWRITE'] = '1'
for month in range(1, 12+1):
    start_doy, end_doy = get_doy_range(year, month)
    hdfs = get_hdf_paths(hdfdir, tile, start_doy, end_doy, year)
    LST = [load_qc_adjusted_lst(hdf) for hdf in hdfs]
    clims = calc_clim(LST, 'LST_%s_%d_%02d' % (tile, year, month))
    gs.run_command('g.remove', rast=','.join(LST))
gs.os.environ['GRASS_OVERWRITE'] = '0'
