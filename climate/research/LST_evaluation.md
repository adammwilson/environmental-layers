
LST Temporal Aggregation Evaluation
====



### Adam M. Wilson and Giuseppe Amatulli

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
![plot of chunk unnamed-chunk-4](http://i.imgur.com/RZbujnI.png) 



# Comparison of monthly means with 3-month moving window
Here we compare the monthly means with a three month moving window (e.g. January mean LST with December-February mean LST).  Note that the relationship is very good (R^2 >0.95) but slightly weaker in winter months, likely due primarily to seasonal minimums.  Heavy black line is 1:1, and thin line is the fitted regression.

![plot of chunk unnamed-chunk-5](http://i.imgur.com/aAmbMaz.png) 


# Comparison of monthly means with 2-month moving window that does not include the month
Here we compare the monthly means with a two month moving window that does not include the month of interest (e.g. January mean LST with December and February mean LST, but not including the January LST).  Heavy black line is 1:1, and thin line is the fitted regression.

![plot of chunk unnamed-chunk-6](http://i.imgur.com/AeJ0AFq.png) 


Now let's look at the differences between the 1-month and 2-month LST values.  These represent a measure of how wrong we would be if we only had data from the two surrounding months and not the month in question.  


```r
histogram(dlst2$lst - dlst2$lst2m, col = "grey", xlab = "Anomolies (1 month - 2 month means)")
```

![plot of chunk unnamed-chunk-7](http://i.imgur.com/qIlsdWo.png) 

The 95th quantile of the absolute value is only 4.2, so the differences are quite small. From this analysis, it appears that broadening the temporal window will maintain a relatively consistent estimate of LST (R^2 ranged from 0.88-0.9) even when using only data from the surrounding months.

Let's see how the seasonal cycle is represented by these proxies for a few randomly selected stations.  Here the red line is the observed TMax data, the heavy black line represents the mean LST in that month, and the green line is a three-month moving window, while the purple line is the 2-month window (not including the month of interest).




![plot of chunk unnamed-chunk-8](http://i.imgur.com/Ve0mC1C.png) 


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

## New Solution 2 (also relatively easy)

1. Create 12 monthly LST climatologies if at least 5 or 10 observations are available for each pixel-month.
2. Fill in missing months using temporal interpolation:
   * Fit function through all available months (rather than just +/- 1 month) to capture seasonal variabilty.
   * Use some function that can capture the cyclical nature (sin, spline, polynomial, etc.)


Remember from above that the LST curves are fairly smooth (caveat: we've only looked at locations in the CONUS dataset).  So let's try fitting a simple sin function to estimate missing months:

```r
### Curve fitting function Set parameter starting values and limits
parms = list(phi = 5, A = 0.05)  #starting parameters
low = list(phi = -999, A = 0)  #lower bounds - constrain s3 to be positive 
high = list(phi = 999, A = 999)  #lower bounds - constrain s3 to be positive

## define a simple model estimating a sin function with variable shift and
## amplitude
fit.model <- function(parms, x) {
    fit = sin(parms[["phi"]] + (x * 2 * pi)) * abs(parms[["A"]])
    fit[fit == Inf] = .Machine$double.xmax  #if Inf, replace with maximum value
    fit[fit == -Inf] = .Machine$double.xmin  #if Inf, replace with maximum value
    list(fit = fit)
}
## function to minimize the RMSE of predictions
minimize.model <- function(parms, x, y) {
    ss = sum((y - fit.model(parms, x)$fit)^2)
    ss = min(ss, .Machine$double.xmax)  #if Inf, replace with maximum value
    ss = max(ss, .Machine$double.xmin)  #if -Inf, replace with maximum value
    list(ss = ss)
}

## Process station data and make predictions for each month
makepreds = function(dat, drop = NULL) {
    dat = na.omit(dat)
    dat = dat[dat$variable == "lst", ]
    ## drop
    if (nrow(dat) == 0) {
        return(NULL)
    }
    ## drop some proportion of data if drop is specified
    if (!is.null(drop)) 
        dat = dat[sample(1:nrow(dat), round(nrow(dat) * (1 - drop))), ]
    dat = dat[order(dat$month), ]
    dat$time = (1:nrow(dat))/nrow(dat)
    xbar = mean(dat$value, na.rm = T)
    fit = optim(unlist(parms), minimize.model, x = dat$time, y = dat$value - 
        xbar, control = list(trace = 0, maxit = 10000), method = "L-BFGS-B", 
        lower = low, upper = high)
    
    dat$pred = xbar + sin(fit$par[["phi"]] + (dat$time * 2 * pi)) * fit$par[["A"]]
    dat$A = fit$par[["A"]]
    dat$phi = fit$par[["phi"]]
    dat
}
## apply the function
d4 = ddply(dlst2ls, .(station), .fun = makepreds)
d4l = melt(d4, id.vars = c("station", "month"), measure.vars = c("value", "pred"))
```


Let's see what that looks  for the 10 example stations above. The blue line is the observed LST value and the pink line is the predicted LST from the sinusoidal function.
![plot of chunk unnamed-chunk-10](http://i.imgur.com/oNteXlf.png) 

And a histogram of the residuals for these stations:
![plot of chunk unnamed-chunk-11](http://i.imgur.com/1z4IHQs.png) 


Not bad... Now let's drop 25% of the observations from each station and do it again:
![plot of chunk unnamed-chunk-12](http://i.imgur.com/6zkd5CB.png) 


And the  of residuals for these 10 stations with 25% of the observations removed:
![plot of chunk unnamed-chunk-13](http://i.imgur.com/c1Wyh9J.png) 


### Apply this function to the full CONUS dataset described above.
Now look at the distributions of the residuals for the full conus dataset.
![plot of chunk unnamed-chunk-14](http://i.imgur.com/od4iR2S.png) 

And summarize the residuals into RMSE's by station:
![plot of chunk unnamed-chunk-15](http://i.imgur.com/KIV2rBk.png) 

So the vast majority of stations will have a RMSE of <5 using this simple method even if 25% of the data are missing.

# Next steps  
We should pick 3-4 problematic tiles and 1 tile with little missing data and explore how these various options differ in infilling LST.  It would also be ideal if we could complete the interpolation using these different datasets and compare the resulting estimates of TMax over these tiles. 

#  Remaining Questions  

1. How does this variability compare to spatial variability (e.g. which is worse, spatial or temporal smoothing)?
2. How do the anomolies from different temporal windows vary spatially?  
