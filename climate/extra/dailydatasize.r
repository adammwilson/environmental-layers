### Brief script to explore the total size of the 1km daily (1970-2010) dataset

## Time
dates=seq(as.Date("1970-01-01"),as.Date("2010-01-01"),by=1)
n.dates=length(dates)

## Total area
n.globe=510072000 #km2, from http://en.wikipedia.org/wiki/Earth

## Land Area
n.land=148940000 #km2, from http://en.wikipedia.org/wiki/Earth

## total values (space x time)
n.landtime=n.dates*n.land
n.globetime=n.dates*n.globe

n.landtime
n.globetime

formatC(n.landtime, format="f",digits=0, big.mark=',')


### approximate storage size for various data types
types=as.data.frame(matrix(c(
  "short int","Short integer",-32768, 32767,2,
  "long int","Long integer", -2147483648,2147483647,4,
  "single","Single-precision floating-point", -3.4e38,1.2e38,4,
  "double","Double-precision floating-point", -2.2e308,1.8e308,8
                         ),ncol=5,byrow=T,dimnames=list(1:4,c("type","longtype","min","max","bytes"))),stringsAsFactors=F)
types$min=as.numeric(types$min);types$max=as.numeric(types$max);types$bytes=as.numeric(types$bytes)
types

### now estimate TB storage for total values

types$storage.landtime.TB=round(n.landtime*types$bytes/2^40,1)
types$storage.globetime.TB=round(n.globetime*types$bytes/2^40,1); types

types[,-c(2:4)]
