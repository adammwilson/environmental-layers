# Quick R code to identify which days are missing from our MODIS11A1.004
# holdings for Oregon (tiles h08v04 and h09v04).
#
# Note that the code below is currently useful only when run
# interactively, in which case the output of interest is printed to the
# console.
#
# TODO:
#  * explicitly check data holdings with respect to both tiles (which I
#    did manually, but not in the code below)
#  * make this more 'testlike' by having the script result in an error
#    (with useful reporting) if holdings are incomplete relative to a
#    prespecified range of expected dates
#
# Jim Regetz
# NCEAS
# Created 07-Mar-2012

# specify current data directory located on atlas.nceas.ucsb.edu
datadir <- "/home/layers/data/climate/MOD11A1.004-OR-orig"

# get names of all *.hdf files in the directory of interest
modis <- list.files(datadir, pattern="*.hdf$")

# parse date info out of filenames, and convert to Date object
dates <- as.Date(sub("^.*[.]A([^.]*).*$", "\\1", x), format="%Y%j")

# show range of dates -- note that the last one is 11-Oct-2010
range(dates)
## [1] "2001-01-01" "2010-10-11"

# create vector of all calendar dates within that range
actual.dates <- seq(from=dates[1], to=dates[length(dates)], by="1 day")

# report which dates are missing
# note: need to coerce to character before setdiffing
setdiff(as.character(actual.dates), as.character(dates))
##  [1] "2001-06-15" "2001-06-16" "2001-06-17" "2001-06-18" "2001-06-19"
##  [6] "2001-06-20" "2001-06-21" "2001-06-22" "2001-06-23" "2001-06-24"
## [11] "2001-06-25" "2001-06-26" "2001-06-27" "2001-06-28" "2001-06-29"
## [16] "2001-06-30" "2001-07-01" "2001-07-02" "2002-03-19" "2002-03-20"
## [21] "2002-03-21" "2002-03-22" "2002-03-23" "2002-03-24" "2002-03-25"
## [26] "2002-03-26" "2002-03-27" "2002-03-28" "2002-04-15" "2003-02-01"
## [31] "2003-12-17" "2003-12-18" "2003-12-19" "2003-12-20" "2003-12-21"
## [36] "2003-12-22" "2003-12-23" "2006-11-01" "2008-12-20" "2008-12-21"
## [41] "2008-12-22" "2010-07-01"

