
LST Temporal Aggregation Evaluation
====



### Adam M. Wilson and Giuseppe Amatulli (Compiled on Mon Mar 31 16:57:05 2014  using code hash: 3af3057)

A short script to explore the implications of a 3-month moving window Land Surface Temperature Climatology.


```
## $eval
## [1] TRUE
## 
## $echo
## [1] TRUE
## 
## $results
## [1] "markup"
## 
## $tidy
## [1] TRUE
## 
## $cache
## [1] FALSE
## 
## $dependson
## NULL
## 
## $cache.path
## [1] "cache/"
## 
## $cache.vars
## NULL
## 
## $ref.label
## NULL
## 
## $child
## NULL
## 
## $engine
## [1] "R"
## 
## $prompt
## [1] FALSE
## 
## $comment
## [1] "##"
## 
## $autodep
## [1] FALSE
## 
## $fig.keep
## [1] "high"
## 
## $fig.show
## [1] "asis"
## 
## $fig.align
## [1] "default"
## 
## $fig.path
## [1] "figure/"
## 
## $fig.ext
## NULL
## 
## $dev
## [1] "png"
## 
## $dpi
## [1] 72
## 
## $dev.args
## NULL
## 
## $fig.width
## [1] 12
## 
## $fig.height
## [1] 8
## 
## $fig.env
## [1] "figure"
## 
## $fig.cap
## NULL
## 
## $fig.scap
## NULL
## 
## $fig.lp
## [1] "fig:"
## 
## $fig.pos
## [1] ""
## 
## $out.width
## NULL
## 
## $out.height
## NULL
## 
## $out.extra
## NULL
## 
## $resize.width
## NULL
## 
## $resize.height
## NULL
## 
## $external
## [1] TRUE
## 
## $sanitize
## [1] FALSE
## 
## $purl
## [1] TRUE
## 
## $highlight
## [1] TRUE
## 
## $size
## [1] "normalsize"
## 
## $warning
## [1] TRUE
## 
## $error
## [1] TRUE
## 
## $message
## [1] TRUE
## 
## $background
## [1] "#F7F7F7"
## 
## $split
## [1] FALSE
## 
## $include
## [1] TRUE
## 
## $interval
## [1] 1
## 
## $aniopts
## [1] "controls,loop"
```

```
## Loading required package: plyr
## 
## Attaching package: 'reshape'
## 
## The following objects are masked from 'package:plyr':
## 
## rename, round_any
## 
## Loading required package: RColorBrewer Loading required package: lattice
```


# Data
The following data were extracted from the shapefiles used to fit the tiled interpolations that Alberto has already run. They are limited to the CONUS region and include mean monthly maximum temperature observed at the station locations, as well as the LST from the month before, the month of, and the month following.  Here's what the data look like:

|station      |month    |    TMax|    lst|  lstp1|  lstm1|tile         |
|:------------|:--------|-------:|------:|------:|------:|:------------|
|USC00100528  |January  |  0.9592|  1.880|  3.406|  1.303|50.0_-115.0  |
|USC00100667  |January  |  2.5101|  1.630|  2.264|  0.340|50.0_-115.0  |
|USC00101079  |January  |  0.8731|  1.598|  3.976|  3.330|50.0_-115.0  |
|USC00101363  |January  |  1.5249|  2.682|  3.616|  1.030|50.0_-115.0  |
|USC00101956  |January  |  2.6857|  2.354|  4.175|  3.123|50.0_-115.0  |
|USC00102159  |January  |  2.6410|  4.978|  5.748|  3.740|50.0_-115.0  |



# TMax~LST relationship seasonal variability 

First let's explore the variability of the TMax~LST relationship by month, grouped by tile.  In this plot grey lines indicate a Tmax~LST regression within each tile (stations may be present in multiple tiles). Variability among grey lines represents spatial variability in the fitted lapse rate and the heavy black line is overall mean relationship (by month).
![plot of chunk unnamed-chunk-4](http://i.imgur.com/ubD0LTM.png) 



# Comparison of monthly means with 3-month moving window
Here we compare the monthly means with a three month moving window (e.g. January mean LST with December-February mean LST).  Note that the relationship is very good (R^2 >0.95) but slightly weaker in winter months, likely due primarily to seasonal minimums.  Heavy black line is 1:1, and thin line is the fitted regression.

![plot of chunk unnamed-chunk-5](http://i.imgur.com/objPR16.png) 


# Comparison of monthly means with 2-month moving window that does not include the month
Here we compare the monthly means with a two month moving window that does not include the month of interest (e.g. January mean LST with December and February mean LST, but not including the January LST).  Heavy black line is 1:1, and thin line is the fitted regression.

![plot of chunk unnamed-chunk-6](http://i.imgur.com/XyLMM4e.png) 


Now let's look at the differences between the 1-month and 2-month LST values.  These represent a measure of how wrong we would be if we only had data from the two surrounding months and not the month in question.  


```r
histogram(dlst2$lst - dlst2$lst2m, col = "grey", xlab = "Anomolies (1 month - 2 month means)")
```

![plot of chunk unnamed-chunk-7](http://i.imgur.com/8EwDnLv.png) 

The 95th quantile of that is only 4.2, so the differences are quite small. From this analysis, it appears that broadening the temporal window window will maintain relativly consistent estimate of LST (R^2 ranged from 0.88-0.9) even when using only data from the surrounding months.

Let's see how the seasonal cycle is represented by these proxies for a few randomly selected stations.  Here the red line is the observed TMax data, the heavy black line represents the mean LST in that month, and the green line is a three-month moving window, while the purple line is the 2-month window (not including the month of interest).


```r
dlst2l = melt(dlst2, id.vars = c("station", "month"), measure.vars = c("TMax", 
    "lst", "lst3m", "lst2m"))
ss = sample(dlst2l$station, 10)
```


![plot of chunk unnamed-chunk-9](http://i.imgur.com/hJpXINT.png) 



#  Remaining Questions  

1. How does this variability compare to spatial variability (e.g. which is worse, spatial or temporal smoothing)?
2. How do the 1-3 month differences vary spatially?
