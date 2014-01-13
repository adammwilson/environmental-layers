#### Assessing relevance of MODIS cloud data for SDM
library(dismo)

##  Species
sp=c("Spondias radlkoferi","Tapirira mexicana","Lozanella enantiophylla")
spl=strsplit(sp," ")

tsp=spl[[3]]
td=gbif(genus=tsp[1],species=tsp[2],geo=TRUE, sp=FALSE, removeZeros=FALSE, download=TRUE, getAlt=TRUE, start=1, end=NULL, feedback=3)


td

