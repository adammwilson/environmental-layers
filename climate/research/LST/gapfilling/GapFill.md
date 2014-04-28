Assessment of gap-filling procedures
========================================================




```r
library(reshape)
library(knitr)

d = read.csv("gapfillvalidation.csv")
d$Month = factor(d$Month, levels = month.name[6:9], ordered = T)
```


You can also embed plots, for example:


```r
kable(cast(d, Type ~ Month, value = "value", fun.aggregate = function(x) paste("R2=", 
    x[1], " (β=", x[2], ")", sep = "")))
```

|id      |June              |July              |August            |September        |
|:-------|:-----------------|:-----------------|:-----------------|:----------------|
|poly    |R2=0.58 (β=1.17)  |R2=0.46 (β=0.68)  |R2=0.78 (β=0.8)   |R2=0.78 (β=0.8)  |
|sin     |R2=0.55 (β=1.16)  |R2=0.4 (β=0.54)   |R2=0.64 (β=0.62)  |R2=0.45 (β=0.5)  |
|spline  |R2=0.56 (β=1.19)  |R2=0.41 (β=0.59)  |R2=0.77 (β=0.84)  |R2=0.8 (β=0.86)  |


  
