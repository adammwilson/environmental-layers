{\rtf1\ansi\ansicpg1252\cocoartf1138\cocoasubrtf320
{\fonttbl\f0\fnil\fcharset0 Menlo-Regular;}
{\colortbl;\red255\green255\blue255;\red0\green116\blue0;\red170\green13\blue145;\red196\green26\blue22;
\red28\green0\blue207;\red63\green105\blue30;}
\margl1440\margr1440\vieww28360\viewh15140\viewkind0
\deftab560
\pard\tx560\pardeftab560\pardirnatural

\f0\fs28 \cf2 \CocoaLigature0 #!/usr/bin/python\cf0 \
\cf2 #\cf0 \
\cf2 # Early version of assorted Python code to download MODIS 11A1 (daily)\cf0 \
\cf2 # data associated with specified dates and tiles, then interface with\cf0 \
\cf2 # GRASS to calculate temporally averaged LST in a way that accounts for\cf0 \
\cf2 # QC flags.\cf0 \
\cf2 #\cf0 \
\cf2 # TODO:\cf0 \
\cf2 #  - functionalize to encapsulate high level procedural steps\cf0 \
\cf2 #  - create a 'main' function to allow script to be run as a command\cf0 \
\cf2 #    that can accept arguments?\cf0 \
\cf2 #  - put downloaded data somewhere other than working directory?\cf0 \
\cf2 #  - write code to set up and take down a temporary GRASS location/mapset\cf0 \
\cf2 #  - write code to export climatology rasters to file (geotiff? compressed?)\cf0 \
\cf2 #  - calculate other aggregate stats? (stddev, ...)\cf0 \
\cf2 #  - deal with nightly LST too?\cf0 \
\cf2 #  - deal with more than just first two bits of QC flags?\cf0 \
\cf2 #     e.g.: r.mapcalc 'qc_level2 = (qc>>2) & 0x03'   \
#     0x03?? that means 0000_0011 and >>2 means shit twice on the right for \cf0 \
\cf2 #  - record all downloads to a log file?\cf0 \
\cf2 #\cf0 \
\cf2 # Jim Regetz\cf0 \
\cf2 # NCEAS\cf0 \
\cf2 # Created on 16-May-2012\
# Modified by Benoit Parmentier\
# NCEAS on 18-October-2012\
# \cf0 \
\
\cf3 import\cf0  os, glob\
\cf3 import\cf0  datetime, calendar\
\cf3 import\cf0  ftplib\
\cf3 import\cf0  grass.script \cf3 as\cf0  gs\
\
\cf2 #------------------\cf0 \
\cf2 # helper functions\cf0 \
\cf2 #------------------\cf0 \
\
\cf3 def\cf0  yj_to_ymd(year, doy):\
    \cf4 """Return date as e.g. '2000.03.05' based on specified year and\
    numeric day-of-year (doy) """\cf0 \
    date = datetime.datetime.strptime(\cf5 '%d%03d'\cf0  % (year, doy), \cf5 '%Y%j'\cf0 )\
    \cf3 return\cf0  date.strftime(\cf5 '%Y.%m.%d'\cf0 )\
\
\cf3 def\cf0  get_doy_range(year, month):\
    \cf4 """Determine starting and ending numeric day-of-year (doy)\
    asscociated with the specified month and year.\
\
    Arguments:\
    year -- four-digit integer year\
    month -- integer month (1-12)\
\
    Returns tuple of start and end doy for that month/year.\
    """\cf0 \
    last_day_of_month = calendar.monthrange(year, month)[\cf5 1\cf0 ]\
    start_doy = int(datetime.datetime.strptime(\cf5 '%d.%02d.%02d'\cf0  % (year,\
        month, \cf5 1\cf0 ), \cf5 '%Y.%m.%d'\cf0 ).strftime(\cf5 '%j'\cf0 ))\
    end_doy = int(datetime.datetime.strptime(\cf5 '%d.%02d.%02d'\cf0  % (year,\
        month, last_day_of_month), \cf5 '%Y.%m.%d'\cf0 ).strftime(\cf5 '%j'\cf0 ))\
    \cf3 return\cf0  (int(start_doy), int(end_doy))\
\
\cf2 # quick function to return list of dirs in wd\cf0 \
\cf3 def\cf0  list_contents(ftp):\
    \cf4 """Parse ftp directory listing into list of names of the files\
    and/or directories contained therein. May or may not be robust in\
    general, but seems to work fine for LP DAAP ftp server."""\cf0 \
    listing = []\
    ftp.dir(listing.append)\
    contents = [item.split()[-\cf5 1\cf0 ] \cf3 for\cf0  item \cf3 in\cf0  listing[\cf5 1\cf0 :]]\
    \cf3 return\cf0  contents\
\
\cf3 def\cf0  download_mod11a1(destdir, tile, start_doy, end_doy, year, ver=\cf5 5\cf0 ):\
    \cf4 """Download into destination directory the MODIS 11A1 HDF files\
    matching a given tile, year, and day range. If for some unexpected\
    reason there are multiple matches for a given day, only the first is\
    used. If no matches, the day is skipped with a warning message.\
\
    Arguments:\
    destdir -- path to local destination directory\
    tile -- tile identifier (e.g., 'h08v04')\
    start_doy -- integer start of day range (0-366)\
    end_doy -- integer end of day range (0-366)\
    year -- integer year (>=2000)\
    ver -- MODIS version (4 or 5)\
\
    Returns list of absolute paths to the downloaded HDF files.\
    """\cf0 \
    \cf2 # connect to data pool and change to the right directory\cf0 \
    ftp = ftplib.FTP(\cf5 'e4ftl01.cr.usgs.gov'\cf0 )\
    ftp.login(\cf5 'anonymous'\cf0 , \cf5 ''\cf0 )\
    ftp.cwd(\cf5 'MOLT/MOD11A1.%03d'\cf0  % ver)\
    \cf2 # make list of daily directories to traverse\cf0 \
    available_days = list_contents(ftp)\
    desired_days = [yj_to_ymd(year, x) \cf3 for\cf0  x \cf3 in\cf0  range(start_doy, end_doy+\cf5 1\cf0 )]\
    days_to_get = filter(\cf3 lambda\cf0  day: day \cf3 in\cf0  desired_days, available_days)\
    \cf3 if\cf0  len(days_to_get) < len(desired_days):\
        missing_days = [day \cf3 for\cf0  day \cf3 in\cf0  desired_days \cf3 if\cf0  day \cf3 not\cf0  \cf3 in\cf0  days_to_get]\
        \cf3 print\cf0  \cf5 'skipping %d day(s) not found on server:'\cf0  % len(missing_days)\
        \cf3 print\cf0  \cf5 '\\n'\cf0 .join(missing_days)\
    \cf2 # get each tile in turn\cf0 \
    hdfs = list()\
    \cf3 for\cf0  day \cf3 in\cf0  days_to_get:\
        ftp.cwd(day)\
        files_to_get = [file \cf3 for\cf0  file \cf3 in\cf0  list_contents(ftp)\
            \cf3 if\cf0  tile \cf3 in\cf0  file \cf3 and\cf0  file[-\cf5 3\cf0 :]==\cf5 'hdf'\cf0 ]\
        \cf3 if\cf0  len(files_to_get)==\cf5 0\cf0 :\
            \cf2 # no file found -- print message and move on\cf0 \
            \cf3 print\cf0  \cf5 'no hdf found on server for tile'\cf0 , tile, \cf5 'on'\cf0 , day\
            ftp.cwd(\cf5 '..'\cf0 ) \cf2 #added by Benoit\cf0 \
            \cf3 continue\cf0 \
        \cf3 elif\cf0  \cf5 1\cf0 <len(files_to_get):\
            \cf2 # multiple files found! -- just use the first...\cf0 \
            \cf3 print\cf0  \cf5 'multiple hdfs found on server for tile'\cf0 , tile, \cf5 'on'\cf0 , day\
        file = files_to_get[\cf5 0\cf0 ]\
        local_file = os.path.join(destdir, file)\
        ftp.retrbinary(\cf5 'RETR %s'\cf0  % file, open(local_file, \cf5 'wb'\cf0 ).write)\
        hdfs.append(os.path.abspath(local_file))\
        ftp.cwd(\cf5 '..'\cf0 )\
    \cf2 # politely disconnect\cf0 \
    ftp.quit()\
    \cf2 # return list of downloaded paths\cf0 \
    \cf3 return\cf0  hdfs\
\
\cf3 def\cf0  get_hdf_paths(hdfdir, tile, start_doy, end_doy, year):\
    \cf4 """Look in supplied directory to find the MODIS 11A1 HDF files\
    matching a given tile, year, and day range. If for some unexpected\
    reason there are multiple matches for a given day, only the first is\
    used. If no matches, the day is skipped with a warning message.\
\
    Arguments:\
    hdfdir -- path to directory containing the desired HDFs\
    tile -- tile identifier (e.g., 'h08v04')\
    start_doy -- integer start of day range (0-366)\
    end_doy -- integer end of day range (0-366)\
    year -- integer year (>=2000)\
\
    Returns list of absolute paths to the located HDF files.\
    """\cf0 \
    hdfs = list()\
    \cf3 for\cf0  doy \cf3 in\cf0  range(start_doy, end_doy+\cf5 1\cf0 ):\
        fileglob = \cf5 'MOD11A1.A%d%03d.%s*hdf'\cf0  % (year, doy, tile)\
        pathglob = os.path.join(hdfdir, fileglob)\
        files = glob.glob(pathglob)\
        \cf3 if\cf0  len(files)==\cf5 0\cf0 :\
            \cf2 # no file found -- print message and move on\cf0 \
            \cf3 print\cf0  \cf5 'cannot access %s: no such file'\cf0  % pathglob\
            \cf3 continue\cf0 \
        \cf3 elif\cf0  \cf5 1\cf0 <len(files):\
            \cf2 # multiple files found! -- just use the first...\cf0 \
            \cf3 print\cf0  \cf5 'multiple hdfs found for tile'\cf0 , tile, \cf5 'on'\cf0 , day\
        hdfs.append(os.path.abspath(files[\cf5 0\cf0 ]))\
    \cf3 return\cf0  hdfs\
\
\cf3 def\cf0  calc_clim(maplist, name, overwrite=False):\
    \cf4 """Generate some climatalogies in GRASS based on the input list of\
    maps. As usual, current GRASS region settings apply. Produces the\
    following output rasters:\
      * nobs: count of number of (non-null) values over the input maps\
      * mean: arithmetic mean of (non-null) values over the input maps\
\
    Arguments:\
    maplist -- list of names of GRASS maps to be aggregated\
    name -- template (suffix) for GRASS output map names\
\
    Returns list of names of the output maps created in GRASS.\
    """\cf0 \
    denominator = \cf5 '(%s)'\cf0  % \cf5 '+'\cf0 .join([\cf5 'if(!isnull(%s))'\cf0  % m\
        \cf3 for\cf0  m \cf3 in\cf0  maplist])\
    gs.mapcalc(\cf5 'nobs_%s = %s'\cf0  % (name, denominator), overwrite=overwrite)\
    numerator = \cf5 '(%s)'\cf0  % \cf5 '+'\cf0 .join([\cf5 'if(isnull(%s), 0, %s)'\cf0  % (m, m)\
        \cf3 for\cf0  m \cf3 in\cf0  maplist])\
    gs.mapcalc(\cf5 'mean_%s = round(float(%s)/nobs_%s)'\cf0  % (name, numerator, name),\
        overwrite=overwrite)\
    \cf3 return\cf0  [\cf5 '%s_%s'\cf0  % (s, name) \cf3 for\cf0  s \cf3 in\cf0  [\cf5 'nobs'\cf0 , \cf5 'mean'\cf0 ]]\
\
\cf3 def\cf0  load_qc_adjusted_lst(hdf):\
    \cf4 """Load LST_Day_1km into GRASS from the specified hdf file, and\
    nullify any cells for which QA flags indicate anything other than\
    high quality.\
\
    Argument:\
    hdf -- local path to the 11A1 HDF file\
\
    Returns the name of the QC-adjusted LST map created in GRASS.\
    """\cf0 \
    lstname =  \cf5 'LST_'\cf0  + \cf5 '_'\cf0 .join(os.path.basename(hdf).split(\cf5 '.'\cf0 )[\cf5 1\cf0 :\cf5 3\cf0 ])               \
    \cf2 # read in lst grid\cf0 \
    gs.run_command(\cf5 'r.in.gdal'\cf0 ,\
        input=\cf5 'HDF4_EOS:EOS_GRID:%s:MODIS_Grid_Daily_1km_LST:LST_Day_1km'\cf0  % hdf,\
        output=lstname)            \cf2 # No new location created since it has already been done with r.in.gdal before!!!\cf0 \
\
    \cf2 # read in qc grid\cf0 \
    gs.run_command(\cf5 'r.in.gdal'\cf0 ,\
        input = \cf5 'HDF4_EOS:EOS_GRID:%s:MODIS_Grid_Daily_1km_LST:QC_Day'\cf0  % hdf,\
        output = \cf5 'qc_this_day'\cf0 )\
    gs.run_command(\cf5 'g.region'\cf0 , rast=lstname)\
    \cf2 # null out any LST cells for which QC is not high [00]  #THIS WHERE WE MIGHT NEED TO CHANGE...\cf0 \
    gs.mapcalc(\cf5 '$\{lst\} = if((qc_this_day & 0x03)==0, $\{lst\}, null())'\cf0 ,\
        lst=lstname, overwrite=True)\
    \cf2 # clean up\cf0 \
    gs.run_command(\cf5 'g.remove'\cf0 , rast=\cf5 'qc_this_day'\cf0 )\
    \cf2 # return name of qc-adjusted LST raster in GRASS\cf0 \
    \cf3 return\cf0  lstname\
\
\
\cf3 def\cf0  main():\
    \cf2 #--------------------------------------------\cf0 \
    \cf2 # Download and Calculate monthly climatology for daily LST time series for a specific area\cf0 \
    \cf2 #--------------------------------------------\cf0 \
\
    \cf2 # TODO: set up a (temporary?) GRASS database to use for processing? code\cf0 \
    \cf2 # currently assumes it's being run within an existing GRASS session\cf0 \
    \cf2 # using an appropriately configured LOCATION...\cf0 \
    \cf2 #\cf0 \
    \cf2 # note the following trick to fix datum for modis sinu;\cf0 \
    \cf2 # TODO: check that this works properly...compare to MRT output?\cf0 \
    \cf2 # gs.run_command('g.proj', flags='c',\cf0 \
    \cf2 #     proj4='+proj=sinu +a=6371007.181 +b=6371007.181 +ellps=sphere')\cf0 \
    \cf2 ##    proj4='+proj=sinu +R=6371007.181 +nadgrids=@null +wktext')\cf0 \
\cf5 \
 \
    \cf6 #### Added by Benoit on 18 October 2012\
    \
    #Parameters\cf5 \
    \cf0 tiles = ['h11v08','h11v07','h12v08']\
    start_year = 2001\
    end_year = 2010\
    end_month=12\
    start_month=1\
    \
    \cf2 ################## First step: download data ###################\cf0 \
    \cf2 # Added tile loop \cf0 \
    \cf3 for\cf0  tile \cf3 in\cf0  tiles:	\
        \cf3 for\cf0  year \cf3 in\cf0  range(start_year, end_year+\cf5 1\cf0 ):\
            		if year==end_year:\
            				start_doy, end_doy = get_doy_range(year, end_month)\
                			start_doy=1  #Fix problem later\
                			hdfs = download_mod11a1(hdfdir, tile, start_doy, end_doy, year)\
\
            		\cf3 elif\cf0  year==start_year:\
                			start_doy, end_doy = get_doy_range(year, start_month)\
                			end_doy=365\
                	if calendar.isleap(year):\
                			end_doy=366\
                			hdfs = download_mod11a1(hdfdir, tile, start_doy, end_doy, year)\
            \
            		\cf3 elif\cf0  (year!=start_year and year!=end_year):\
                			start_doy=1\
                			end_doy=365\
                			if calendar.isleap(year):\
                					end_doy=366\
                			hdfs = download_mod11a1(hdfdir, tile, start_doy, end_doy, year)\
    \
    \cf2 # modify loop to take into account "hdfs", list of hdf files needed in the next steps\'85  \cf0 \
\
    \cf2 ################# Second step: compute climatology #################\
\cf0 \
    \
    \cf2 # Set up a temporary GRASS data base \
    
\fs22 \cf0 \

\fs28     tmp_location = \cf5 'tmp'\cf0  + str(os.getpid())        \cf2 # Name of the temporary GRASS data base \cf0 \
    orig_location = gs.gisenv()[\cf5 'LOCATION_NAME'\cf0 ]\
    orig_mapset = gs.gisenv()[\cf5 'MAPSET'\cf0 ]
\fs22 \

\fs28     gs.os.environ[\cf5 'GRASS_OVERWRITE'\cf0 ] = \cf5 '1'         \cf2 # Allow for overwrite?? GRASS data base\cf5   \cf0 \
\cf2     \
\
    #load a file that will define the region of interest and location at the same time\cf0 \
	name_of_file = \cf5 'MOD11A1.A2003364.h12v08.005.2008163211649.hdf'\cf0 \
\cf2      # first load daytime LST, by accessing specific layers in the hdd??\
    #SUBDATASET_1_NAME=HDF4_EOS:EOS_GRID:"MOD11A1.A2003364.h12v08.005.2008163211649.hdf":MODIS_Grid_Daily_1km_LST:LST_Day_1km\
\cf0     path_file = os.getcwd() \
	lst1day = \cf5 'HDF4_EOS:EOS_GRID:%s/%s:%s'\cf0  % (\
    path_file,\
    name_of_file,\
     \cf5 '\cf2 MODIS_Grid_Daily_1km_LST:LST_Day_1km\cf5 '\cf0 )\
	 gs.run_command(\cf5 'r.in.gdal'\cf0 , input=lst1day, output=\cf5 'LST_1day_h12v08'\cf0 ,\
           location=tmp_location)\
\
   \cf2 	# Now that the new location has been create, switch to new location\cf0 \
	gs.run_command(\cf5 'g.gisenv'\cf0 , set=\cf5 'LOCATION_NAME=%s'\cf0  % tmp_location)\
	gs.run_command(\cf5 'g.gisenv'\cf0 , set=\cf5 'MAPSET=PERMANENT'\cf0 )\
\
\cf2 	# set GRASS the projection system and GRASS region to match the extent of the file\cf0 \
	gs.run_command(\cf5 'g.proj'\cf0 , flags=\cf5 'c'\cf0 ,\
    proj4=\cf5 '+proj=sinu +a=6371007.181 +b=6371007.181 +ellps=sphere'\cf0 )\
	gs.run_command(\cf5 'g.region'\cf0 , rast=\cf5 'LST_1day_h12v08'\cf0 )\
\cf2 \
    \
    #generate monthly pixelwise mean & count of high-quality daytime LST\
\cf0    \
    gs.os.environ[\cf5 'GRASS_OVERWRITE'\cf0 ] = \cf5 '1'\
\
    \cf2 #Provide a list of file "hdfs" to process\'85\
\cf0     tile= 'h12v08'\cf2 \
\cf0     fileglob = \cf5 'MOD11A1.A*.%s*hdf'\cf0  % (tile)\
    pathglob = os.path.join(path_file, fileglob)\
    files = glob.glob(pathglob)\
    hdfs = files\
    \cf2 ### LST values in images must be masked out if they have low quality: this is done using QC flags from the layer subset in hdf files\cf0 \
    LST = [load_qc_adjusted_lst(hdf) \cf3 for\cf0  hdf \cf3 in\cf0  hdfs]\
    \cf2 ### LST is a list that contains the name of the new lst files in the GRASS format. The files are stored in the temporary GRASS database.\cf0 \
    clims = calc_clim(LST, \cf5 'LST_%s_%d_%02d'\cf0  % (tile, year, month))\
    \cf2 # clean up\cf0 \
    gs.run_command(\cf5 'g.remove'\cf0 , rast=\cf5 ','\cf0 .join(LST))\
    gs.os.environ[\cf5 'GRASS_OVERWRITE'\cf0 ] = \cf5 '0'\cf0 \
    \
    \cf2 # clean up\cf0 \
    gs.run_command(\cf5 'g.gisenv'\cf0 , set=\cf5 'LOCATION_NAME=%s'\cf0  % orig_location)\
    gs.run_command(\cf5 'g.gisenv'\cf0 , set=\cf5 'MAPSET=%s'\cf0  % orig_mapset)\
    shutil.rmtree(os.path.join(gs.gisenv()[\cf5 'GISDBASE'\cf0 ], tmp_location))
\fs22 \

\fs28 \
    \cf3 return\cf0  \cf3 None\cf0 \
}