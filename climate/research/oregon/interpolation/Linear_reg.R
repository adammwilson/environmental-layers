#This script interpolates station value for the Oregon case study. This program loads the station data from a csv file and perform two types  of regression: multiple linear model and general additive model (GAM). Note that this program 1)assumes that the csv file is in the current working 2)extract relevant variables from raster images before performing the regressions. The user must provide the list of raster images
#in  a textile.
#Created by Benoit Parmentier on February 12, 2012.

###Loading r library and packages
library(raster)                                                                              # loading the raster package
library(gtools)                                                                              l# loading ...
library(sp)
library(mgcv)

###Parameters and arguments
infile1<-"ghcn_or_b_02122012_OR83M.csv"
inlistf<-"list_files.txt"
path<-getwd()
#list_var<-list.files()                                                                        # displaying the files in the current folder which should contain only the relevant raster images.
#dump("list_var", file="list_var.txt")                                                # Saving the values of list_var in a ascii text file.

#inlistvar<-"ghcn_var_02122012.txt"

###Reading the station data
ghcn<-read.csv(paste(path,"/",infile1, sep=""), header=TRUE)    #The "paste" function concatenates the path and file name in common string.
ghcn<-read.csv(infile1)                                                                 #Use read.table if the input file is space delimited or read.table(infile1, headername=TRUE, sep=',')
names(ghcn)                                                                                 #Checking that the columns are correctly labelled.

###Extracting the variables values from the raster files
coords<- ghcn[,c('x_OR83M','y_OR83M')]                                    #Selecting all rows for the x and y columns and packaging the output in a data frame.
lines <-readLines(inlistf)
inlistvar<-paste(path,"/",lines,sep="")

#s_raster<- stack(inlistvar)                                                              #Creating a stack of raster images from the list of variables.
#stat_val<- extract(s_raster, coords)                                               #Extracting values from the raster stack for every point location in coords data frame.
#create a shape file and data_frame with names??


###Regression part 1: linear models
lm_PRISM1=lm(tmax~lat+lon+ELEV_SRTM+ASPECT+DISTOC, data=ghcn)
lm_ANUSPLIN1=lm(tmax~lat+lon+ELEV_SRTM, data=ghcn)
summary(lm_ANUSPLIN1)
summary(lm_PRISM1)

#Regression part2: linear model on a specific date.
ghcn1507 <-subset(ghcn,ghcn$date_=="20100715")
lm_PRISM2=lm(tmax~lat+lon+ELEV_SRTM+ASPECT+DISTOC, data=ghcn1507)
lm_ANUSPLIN2=lm(tmax~lat+lon+ELEV_SRTM, data=ghcn1507)


###Regression part 3: GAM models
GAM1<-gam(tmax~ s(lat) + s (lon) + s (elev), data=ghcn1507)


#use the s() for smoothing function

###Compare the models
#Show AIC, Cook distance, p values and residuals plot
###Access the R2 and significance to give a report

AIC (lm_ANUSPLIN1,lm_ANUSPLIN2,GAM1) #list the AIC and for the results
anova(lm_ANUSPLIN1, lm_ANUSPLIN2,GAM1,test="F") #compare the different models in terms of F; a reference model should be set for comparison.
#GAM plot of partial residuals
gam.check(GAM1)   #This will produce basic plots of residuals

###Creating a validation dataset by creating training and testing datasets (%30)
n<-nrow(ghcn1507)
ns<-n-round(n*0.3)  #Create a sample from the data frame with 70% of the rows
ind.training <- sample(nrow(ghcn1507), size=ns, replace=FALSE) #This selects the index position for 70% of the rows taken randomly
ind.testing <- setdiff(1:nrow(ghcn1507), ind.training)
ghcn1507_s <- ghcn1507[ind.training, ]
ghcn1507_v <- ghcn1507[ind.testing, ]


#ghcn1507_s<- sample(ghcn1507,size=ns, replace=T)
ghcn1507_s<- ghcn1507[sample(nrow(ghcn1507), size=ns), ]
#ghcn1507_v <- ghcn1507$STAT_ID[ !ghcn1507_s$STAT_ID ] #dataset for validation that can be used to predict values.



##End of script




 
