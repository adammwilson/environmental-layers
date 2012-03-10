# R script for creating tables of negative values and its frequency for each 
# for each nine region. For region 6 and 7, the list (table) were too long 
# to show at once, resulting in separating first part of the table and 
# the latter part of the table to print.
# 
#
# 9-Dec- 2011
# Yuina Nunokawa





library(raster) 

v1 <- values(raster("/data/project/organisms/DEM/Yuni/Data/aster2/aster2_e020e059_82N.tif"))
t1 <- as.data.frame(table(v1[v1 < 0]))

v2 <- values(raster("/data/project/organisms/DEM/Yuni/Data/aster2/aster2_e060e099_82N.tif"))
t2 <- as.data.frame(table(v2[v2 < 0]))

v3 <- values(raster("/data/project/organisms/DEM/Yuni/Data/aster2/aster2_e100e139_82N.tif"))
t3 <- as.data.frame(table(v3[v3 < 0]))

v4 <- values(raster("/data/project/organisms/DEM/Yuni/Data/aster2/aster2_e140e179_82N.tif"))
t4 <- as.data.frame(table(v4[v4 < 0]))

v5 <- values(raster("/data/project/organisms/DEM/Yuni/Data/aster2/aster2_w020e019_82N.tif"))
t5 <- as.data.frame(table(v5[v5 < 0]))

v6 <- values(raster("/data/project/organisms/DEM/Yuni/Data/aster2/aster2_w060w021_82N.tif"))
t6 <- as.data.frame(table(v6[v6 < 0]))


v7 <- values(raster("/data/project/organisms/DEM/Yuni/Data/aster2/aster2_w100w061_82N.tif"))
t7 <- as.data.frame(table(v7[v7 < 0]))

v8 <- values(raster("/data/project/organisms/DEM/Yuni/Data/aster2/aster2_w140w101_82N.tif"))
t8 <- as.data.frame(table(v8[v8 < 0]))

v9 <- values(raster("/data/project/organisms/DEM/Yuni/Data/aster2/aster2_w180w141_82N.tif"))
t9 <- as.data.frame(table(v9[v9 < 0]))



# print the table 

t1
t2
t3
t4
t5
t6[1:100]
t6
head(t7, 41)
t7
t8
t9
