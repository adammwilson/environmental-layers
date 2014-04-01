
LST Temporal Aggregation Evaluation
====



### Adam M. Wilson and Giuseppe Amatulli
(Compiled on Tue Apr  1 13:22:11 2014  using code hash: 5600791)

A short script to explore the implications of a 3-month moving window Land Surface Temperature Climatology.




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
![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 



# Comparison of monthly means with 3-month moving window
Here we compare the monthly means with a three month moving window (e.g. January mean LST with December-February mean LST).  Note that the relationship is very good (R^2 >0.95) but slightly weaker in winter months, likely due primarily to seasonal minimums.  Heavy black line is 1:1, and thin line is the fitted regression.

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


# Comparison of monthly means with 2-month moving window that does not include the month
Here we compare the monthly means with a two month moving window that does not include the month of interest (e.g. January mean LST with December and February mean LST, but not including the January LST).  Heavy black line is 1:1, and thin line is the fitted regression.

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


Now let's look at the differences between the 1-month and 2-month LST values.  These represent a measure of how wrong we would be if we only had data from the two surrounding months and not the month in question.  


```r
histogram(dlst2$lst - dlst2$lst2m, col = "grey", xlab = "Anomolies (1 month - 2 month means)")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

The 95th quantile of the absolute value is only 4.2, so the differences are quite small. From this analysis, it appears that broadening the temporal window will maintain a relatively consistent estimate of LST (R^2 ranged from 0.88-0.9) even when using only data from the surrounding months.

Let's see how the seasonal cycle is represented by these proxies for a few randomly selected stations.  Here the red line is the observed TMax data, the heavy black line represents the mean LST in that month, and the green line is a three-month moving window, while the purple line is the 2-month window (not including the month of interest).


```r
dlst2l = melt(dlst2, id.vars = c("station", "month"), measure.vars = c("TMax", 
    "lst", "lst3m", "lst2m"))
ss = sample(dlst2l$station, 10)
```


![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


# Processing Options

## Solution 1 (relatively easy)

1. Spatial interpolation: fill in LST when another observation is available within  some small distance (3? 5? 7? kilometers)
2. Temporal interpolation: Estimate the monthly value at the 15th day by linear interpolation (in the temporal domain) considering the temporally closest observations in the previous and following month.

This is more robust than the simple mean of 3 months.  For example, imagine using a simple mean in the following case:  
* month -1: no observation 
* month  0: no observation                                     
* month +1: observation in the end of the month

Would lead to a prediction = month +1.

Or, alternatively, one in which:
* month -1: has data for the full month
* month 0: has no data
* month +1: has only data near end of month

In this case a mean would be 'pulled' towards the mean value of month-1.  Using a fixed number of observations in t-1 and t+1 and a linear interpolation that accounts for the time of those observations would prevent 

## Solution 2 (complex - based on Kalman filter and ancillary model)

We could also re-imagine the interpolation of daily missing pixel values based on the temporal trend of previous and future observation driven by an ancillary modelled data [http://dx.doi.org/10.1016/j.rse.2007.07.007].  The modelled data can be obtained from external climate models such as NCEP Reanalysis (at >0.5 degree of resolution). For example:

![title](http://i.imgur.com/qZs0KZn.png)

However, this would require a major reorganization of how we are approaching the problem.

#  Remaining Questions  

1. How does this variability compare to spatial variability (e.g. which is worse, spatial or temporal smoothing)?
2. How do the anomolies from different temporal windows vary spatially?  
