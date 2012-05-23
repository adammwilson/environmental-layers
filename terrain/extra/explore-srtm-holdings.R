# Quick exploration and file listing-based comparison of the CGIAR SRTM
# tiles available in the following places:
#
#   - those downloaded by Ming
#       jupiter.nceas.ucsb.edu:~organisms/SRTM_90m_ASCII_v4.1/
#
#   - those downloaded by Rick
#       jupiter.nceas.ucsb.edu:~organisms/CgiarSrtmAll/5_5x5_ascii/
#
#   - those currently available at CGIAR (ASCII format)
#       ftp://srtm.csi.cgiar.org/SRTM_v41/SRTM_Data_ArcASCII/
#
# Note that in addition to the comparisons made below, I also noticed
# that a few timestamps on zips Ming downloaded are newer than the
# timestamps on the CGIAR ftp site. It appears that these are bad zips
# on the CGIAR site; in subsequent email exchange with Ming, he said he
# believes he replaced them by downloading the corresponding GeoTIFFs
# from CGIAR and converting them to ASCII.
#
# Jim Regetz
# NCEAS
# Created on 21-May-2011

# list of tiles Ming downloaded onto jupiter
cmd <- "cd ~organisms/SRTM_90m_ASCII_v4.1 && ls -l srtm_*zip"
files <- system(cmd, intern=TRUE)
jup.v4.1 <- with(read.table(textConnection(files)), data.frame(tile=V8,
    size=V5, date=as.Date(V6, format="%Y-%m-%d"), time=V7))

# list of tiles Rick downloaded onto jupiter
cmd <- "cd ~organisms/CgiarSrtmAll/5_5x5_ascii && ls -l srtm_*zip"
files <- system(cmd, intern=TRUE)
jup.5_5x5 <- with(read.table(textConnection(files)), data.frame(tile=V8,
    size=V5, date=as.Date(V6, format="%Y-%m-%d"), time=V7))

# current list of tiles at CGIAR ftp site
cmd <- "curl ftp://srtm.csi.cgiar.org/SRTM_v41/SRTM_Data_ArcASCII/"
files <- system(cmd, intern=TRUE)
cgiar <- with(read.table(textConnection(files)), data.frame(tile=V4,
    size=V3, date=as.Date(V1, format="%m-%d-%y"), time=V2))

# list CGIAR tiles not found in jup.5_5x5
setdiff(cgiar$tile, jup.5_5x5$tile)
##  "srtm_32_05.zip" "srtm_68_03.zip"

# list jup.5_5x5 tiles not found in CGIAR (none!)
setdiff(jup.5_5x5$tile, cgiar$tile)
##  character(0)

# list CGIAR tiles not found in jup.v4.1
setdiff(cgiar$tile, jup.v4.1$tile)
##  "srtm_65_01.zip" "srtm_65_05.zip" "srtm_65_06.zip" "srtm_65_07.zip"
##  "srtm_65_08.zip" "srtm_65_10.zip" "srtm_65_11.zip" "srtm_65_13.zip"
##  "srtm_65_15.zip" "srtm_65_16.zip"

# list jup.v4.1 tiles not found in CGIAR (none!)
setdiff(jup.v4.1$tile, cgiar$tile)
##  character(0)

# show that all common tiles between CGIAR and jup.v4.1 have the same file
# size
common <- merge(cgiar, jup.v4.1, by="tile")
identical(common$size.x, common$size.y)
##  [1] TRUE

# but note that three files in jup.v4.1 have different (newer, as it
# turns out) timestamps
identical(common$date.x, common$date.y)
##  [1] FALSE
common$tile[common$date.x!=common$date.y]
##  "srtm_19_06.zip" "srtm_21_14.zip" "srtm_42_14.zip"

