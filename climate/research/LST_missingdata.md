LST Climatology Evaluation
====



### Adam M. Wilson (Compiled on Tue Mar  4 14:57:04 2014  using code version (git hash): 0b07113)

A short script to visualize and explore the updated Land Surface Climatology algorithm that 'lowers the standards' in some areas to increase the number of available observations.  


```
## Warning: 'x' is NULL so the result will be NULL
```

```
## Error: replacement has length zero
```



## Download data from ECOcast and convert to raster stacks

```r
download = F
if (download) system("wget -e robots=off -L -r -np -nd -p 20140304_LST -nc -A tif http://ecocast.arc.nasa.gov/data/pub/climateLayers/LST_new/")

## organize file names
f = data.frame(full = T, path = list.files("20140304_LST", pattern = "tif$", 
    full = T), stringsAsFactors = F)
f$month = as.numeric(do.call(rbind, strsplit(basename(f$path), "_|[.]"))[, 7])
f$type = do.call(rbind, strsplit(basename(f$path), "_|[.]"))[, 1]
f = f[order(f$month), ]
f$mn = month.name[f$month]

## create raster stacks
lst_mean = stack(f$path[f$type == "mean"])
names(lst_mean) = f$mn[f$type == "mean"]
NAvalue(lst_mean) = 0

lst_nobs = stack(f$path[f$type == "nobs"])
names(lst_nobs) = f$mn[f$type == "nobs"]

lst_qa = stack(f$path[f$type == "qa"])
names(lst_qa) = f$mn[f$type == "qa"]

## define a function to summarize data
fst = function(x, na.rm = T) c(mean = mean(x, na.rm = T), min = min(x, na.rm = T), 
    max = max(x, na.rm = T))
rfst = function(r) cellStats(r, fst)
```


## Mean Monthly LST

Map of LST by month (with white indicating missing data).  Note that many inland regions have missing data (white) in some months (mostly winter).

```r
colramp = colorRampPalette(c("blue", "orange", "red"))
dt_mean = rfst(lst_mean)
levelplot(lst_mean, col.regions = c(colramp(99)), at = seq(0, 65, len = 99), 
    main = "Mean Land Surface Temperature", sub = "Tile H08v05 (California and Northern Mexico)")
```

![plot of chunk unnamed-chunk-4](http://i.imgur.com/LTEp1xT.png) 



Table of mean, min, and maximum LST for this tile by month.

```r
print(xtable(dt_mean), type = "html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Tue Mar  4 14:19:55 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> January </TH> <TH> February </TH> <TH> March </TH> <TH> April </TH> <TH> May </TH> <TH> June </TH> <TH> July </TH> <TH> August </TH> <TH> September </TH> <TH> October </TH> <TH> November </TH> <TH> December </TH>  </TR>
  <TR> <TD align="right"> mean </TD> <TD align="right"> 15.01 </TD> <TD align="right"> 18.41 </TD> <TD align="right"> 25.81 </TD> <TD align="right"> 32.05 </TD> <TD align="right"> 39.49 </TD> <TD align="right"> 44.18 </TD> <TD align="right"> 44.70 </TD> <TD align="right"> 41.96 </TD> <TD align="right"> 38.59 </TD> <TD align="right"> 30.84 </TD> <TD align="right"> 21.77 </TD> <TD align="right"> 14.33 </TD> </TR>
  <TR> <TD align="right"> min </TD> <TD align="right"> 1.43 </TD> <TD align="right"> 1.98 </TD> <TD align="right"> 1.08 </TD> <TD align="right"> 1.28 </TD> <TD align="right"> 2.37 </TD> <TD align="right"> 5.03 </TD> <TD align="right"> 12.04 </TD> <TD align="right"> 12.96 </TD> <TD align="right"> 12.72 </TD> <TD align="right"> 6.90 </TD> <TD align="right"> 2.77 </TD> <TD align="right"> 2.76 </TD> </TR>
  <TR> <TD align="right"> max </TD> <TD align="right"> 31.31 </TD> <TD align="right"> 36.74 </TD> <TD align="right"> 45.88 </TD> <TD align="right"> 52.29 </TD> <TD align="right"> 58.66 </TD> <TD align="right"> 60.88 </TD> <TD align="right"> 61.15 </TD> <TD align="right"> 60.99 </TD> <TD align="right"> 57.07 </TD> <TD align="right"> 48.82 </TD> <TD align="right"> 39.17 </TD> <TD align="right"> 29.42 </TD> </TR>
   </TABLE>


###  Boxplot of Monthly Mean LST

```r
lst_tmean = melt(unlist(as.matrix(lst_mean)))
colnames(lst_tmean) = c("cell", "month", "value")
lst_tmean = lst_tmean[!is.na(lst_tmean$value), ]
lst_tmean$month = factor(lst_tmean$month, levels = month.name, ordered = T)
bwplot(value ~ month, data = lst_tmean, ylab = "Mean LST (c)", xlab = "Month")
```

![plot of chunk unnamed-chunk-6](http://i.imgur.com/5Knr2zX.png) 



## Total number of available observations

This section details the spatial and temporal distribution of the number of LST observations that were not masked by quality control (see section below).  Note that the regions with no data in the map above have missing data (nobs=0) here as well, but also the areas surrounding those regions have low numbers of observations in some months (blue colors).  

```r
dt_nobs = rfst(lst_nobs)
levelplot(lst_nobs, col.regions = c("grey", colramp(99)), at = c(-0.5, 0.5, 
    seq(1, 325, len = 99)), main = "Sum Available Observations", sub = "Tile H08v05 (California and Northern Mexico) \n Grey indicates zero observations")
```

![plot of chunk unnamed-chunk-7](http://i.imgur.com/n7HfvPt.png) 


Table of mean, min, and maximum number of observations for this tile by month.
<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Tue Mar  4 14:21:49 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> January </TH> <TH> February </TH> <TH> March </TH> <TH> April </TH> <TH> May </TH> <TH> June </TH> <TH> July </TH> <TH> August </TH> <TH> September </TH> <TH> October </TH> <TH> November </TH> <TH> December </TH>  </TR>
  <TR> <TD align="right"> mean </TD> <TD align="right"> 107.97 </TD> <TD align="right"> 100.06 </TD> <TD align="right"> 140.35 </TD> <TD align="right"> 152.19 </TD> <TD align="right"> 177.02 </TD> <TD align="right"> 178.22 </TD> <TD align="right"> 156.86 </TD> <TD align="right"> 169.87 </TD> <TD align="right"> 180.29 </TD> <TD align="right"> 167.68 </TD> <TD align="right"> 136.12 </TD> <TD align="right"> 102.86 </TD> </TR>
  <TR> <TD align="right"> min </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> <TD align="right"> 0.00 </TD> </TR>
  <TR> <TD align="right"> max </TD> <TD align="right"> 272.00 </TD> <TD align="right"> 238.00 </TD> <TD align="right"> 281.00 </TD> <TD align="right"> 293.00 </TD> <TD align="right"> 319.00 </TD> <TD align="right"> 304.00 </TD> <TD align="right"> 319.00 </TD> <TD align="right"> 324.00 </TD> <TD align="right"> 310.00 </TD> <TD align="right"> 305.00 </TD> <TD align="right"> 271.00 </TD> <TD align="right"> 260.00 </TD> </TR>
   </TABLE>


###  Boxplot of Number of Observations
The seasonal cycle of missing data is quite noisy, though there tend to be fewer observations in winter months (DJF).  

```r
lst_tnobs = melt(unlist(as.matrix(lst_nobs)))
colnames(lst_tnobs) = c("cell", "month", "value")
lst_tnobs = lst_tnobs[!is.na(lst_tnobs$value), ]
lst_tnobs$month = factor(lst_tnobs$month, levels = month.name, ordered = T)
bwplot(value ~ month, data = lst_tnobs, ylab = "Number of availble observations", 
    xlab = "Month")
```

![plot of chunk unnamed-chunk-9](http://i.imgur.com/G9g9qQw.png) 



## Quality Assessment level used

Map of the Quality Assessment (QA) level used to fill the pixels. It goes from 0 (highest quality) to 3(low). For h08v05 all pixels are filled with either 0 or 1. So red indicates areas with the lower quality data (most of the tile).

```r
levelplot(lst_qa, col.regions = c("grey", "red"), at = c(-0.5, 0.5, 1.5), cuts = 2, 
    main = "Quality Assessment Filter", sub = "Tile H08v05 (California and Northern Mexico)")
```

![plot of chunk unnamed-chunk-10](http://i.imgur.com/LWLYRXJ.png) 



Proportion of cells in each month with QA=1 (including cells in the Pacific Ocean)
<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Tue Mar  4 14:23:35 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> January </TH> <TH> February </TH> <TH> March </TH> <TH> April </TH> <TH> May </TH> <TH> June </TH> <TH> July </TH> <TH> August </TH> <TH> September </TH> <TH> October </TH> <TH> November </TH> <TH> December </TH>  </TR>
  <TR> <TD align="right"> ProportionQA_1 </TD> <TD align="right"> 0.47 </TD> <TD align="right"> 0.47 </TD> <TD align="right"> 0.42 </TD> <TD align="right"> 0.40 </TD> <TD align="right"> 0.38 </TD> <TD align="right"> 0.37 </TD> <TD align="right"> 0.42 </TD> <TD align="right"> 0.40 </TD> <TD align="right"> 0.38 </TD> <TD align="right"> 0.38 </TD> <TD align="right"> 0.43 </TD> <TD align="right"> 0.47 </TD> </TR>
   </TABLE>



## Questions

A few open questions/comments (in my mind):

  1. Why are there only two QA classes for this tile (0 and 1) rather than 4 (0-3)?  There are still missing data in some months, is the plan to do it or was there another reason to not consider all classes for this tile?
  2. How exactly are the different QA levels selected?  If QA=0 results in <33 obs, go to QA=1, etc.?  
  3.  Please name monthly output in a way that it sorts chronologically  (e.g. mean_LST_Day_1km_h08v05_04.tif instead of mean_LST_Day_1km_h08v05_apr_4.tif )  
  4.  Please name directories on ECOcast with dates rather than "new".  E.g. LST/20140304/*   That will make it easier to see which is the new version.
  5.  Should we consider also saving the SD of the observations in each pixel (in addition to the mean and n of observations)?

