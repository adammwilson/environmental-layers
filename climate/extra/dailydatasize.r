### Brief script to explore the total size of the 1km daily (1970-2010) dataset

## Time
dates=seq(as.Date("1970-01-01"),as.Date("2010-01-01"),by=1)
n.dates=length(dates)

## Total area
n.globe=510072000 #km2, from http://en.wikipedia.org/wiki/Earth

## Land Area
n.land=148940000 #km2, from http://en.wikipedia.org/wiki/Earth

## total values
n.landtime=n.dates*n.land
n.globetime=n.dates*n.globe

n.landtime
n.globetime

formatC(n.landtime, format="f",digits=0, big.mark=',')


### approximate storage size for various data types
types=as.data.frame(matrix(c(
  "Short integer",-32768, 32767,2,
  "Long integer", -2147483648,2147483647,4,
  "Single-precision floating-point", -3.4e38,1.2e38,4,
  "Double-precision floating-point", -2.2e308,1.8e308,8
                         ),ncol=4,byrow=T,dimnames=list(1:4,c("type","min","max","bytes"))),stringsAsFactors=F)
types$min=as.numeric(types$min);types$max=as.numeric(types$max);types$bytes=as.numeric(types$bytes)
types

### now estimate TB storage for total values (not full grid, just the values)

types$storage.landtime.TB=n.landtime*types$bytes/2^40
types$storage.globetime.TB=n.globetime*types$bytes/2^40; types

