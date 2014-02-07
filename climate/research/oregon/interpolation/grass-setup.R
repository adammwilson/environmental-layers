######################################## Access  GRASS In Python #######################################

#This script configures the system shell to allow access to GRASS in python through a system call in R.
#AUTHOR: Jim Regetz                                                                      
#DATE: 05/15/2013                                                                                
#PROJECT: NCEAS INPLANT: Environment and Organisms --TASK#491--                                  

#-------------------------------------------------------------
# set necessary environment variables
# see: http://grass.osgeo.org/grass64/manuals/variables.html

GISBASE <- "/usr/lib/grass64"
PATH <- Sys.getenv("PATH")
PATH <- paste(PATH, file.path(GISBASE, "bin"), file.path(GISBASE,
                                                         "scripts"), sep=":")
LD_LIBRARY_PATH <- Sys.getenv("LD_LIBRARY_PATH")
LD_LIBRARY_PATH <- paste(LD_LIBRARY_PATH, file.path(GISBASE, "/lib"),
                         sep=":")
GISRC <- file.path(path.expand("~"), ".grassrc6")
GIS_LOCK <- Sys.getpid()
PYTHONPATH <- file.path(GISBASE, "etc/python/")

Sys.setenv(PATH=PATH)
Sys.setenv(LD_LIBRARY_PATH=LD_LIBRARY_PATH)
Sys.setenv(GIS_LOCK=GIS_LOCK)
Sys.setenv(GISRC=GISRC)
Sys.setenv(GISBASE=GISBASE)
Sys.setenv(PYTHONPATH=PYTHONPATH)
#-------------------------------------------------------------
