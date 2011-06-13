##############################################################################################
#
# CreateImageDiffPlots
# 
# R script generates collection of nine plots that display the distributions of  populations of 
# three elevation pixel pairs in proximity to the border between ASTER and STRM data in mosaic 
# images# under development for the Environment and Organisms project.
# 
#
# Inputs: 
#   1) Comma Separated Value (CSV) table containing randomly-sampled elevation value pairs,
#      extracted from ASTER/CGIAR mosaic and CDEM images, created by the R script: makeImagePairTable.r
#      Note: At present, be sure that this file has been sorted by column 1 (ColumnID) in Excel
#      before using in this program. 
#      NOTE: This filename is set in the body of the script.
#
#   2) sampling factor: integer (range=1 to number of recors in input table. Determines
#      the size of randomly-selected sampe of total table record 'triplets' (north, border,
#      and south) rows of each column sample) to be displayed in the plots: 
#           sampling factor = 1  : plot every table record
#                             10 : plot a random sample containing 1/10th of records
#                            100 : plot random sample containing 1/100th of records.
#   3) JpgPlotFileFlag: Set to TRUE to have *individual* plots written to JPG files.
#
# To run:
#
#  1) place this file and the input file "tableForMark4000_5_8_SortColID.csv" in folder
#  2) start R
#  3) > source("CreateImageDiffPlotsOverlay.r")
#  4) > CreateImageDiffPlots(sampling factor,JpegFlag)
#     < press <cr> key to view plots sequentially
#  
# TODO: add symbol legend to each plot.
# Author: Rick Reeves, NCEAS
# May 14, 2011
# May 17, 2011: This version generates 'delta scatterplots' specified by Mark and Jim.
# May 18, 2011: If JpgPlotFlag set, write individual plot files to JPG files.
# May 22, 2011: add Lowess lines to center of scatterplot distribution
##############################################################################################
CreateImageDiffPlotsNewLL <- function(plotSampFact = 1,JpgPlotFileFlag = TRUE)
{

# Check plotSampleFact rangels -l *.tif

   if (plotSampFact < 1)
      plotSampFact = 1
      
# Read input table that was sorted OFFLINE in Excel on the first column (ColumnID)

   pointTable <-read.csv("pixelPairs36000_5_8EvenSortCol1.csv")
#   pointTable <-read.csv("pixelPairs36000_Exa.csv")
# Table created by randomly sampling from two superimposed 
# image: 
#  1) DOM mosaic image comprised of ASTER and SRTM components.
#  2) 'Baseline' Canadian DEM (CDEM) image.
# 
# The input table contains three rows for each randomly-selected
# pixel pair from both images. Each row contains two pixel pairs,
# the first pair drawn from the image mosaic, the second pair
# drawn from the CDEM image: 
#   First pair: North pixel, South pixel (ASTER/SRTM mosaic)
#   Second pair: North pixel, South pixel (CDEM)
#
#  The first row of each 'triplet' contains pixel pairs North of border,
#  The second row contains pixel pairs spanning  border,
#  The third row contains pixel pairs South of border,
#
# This script generates a series of plots that display
# differences between:
#    1) The mosaic and CDEM images
#    2) Image pixels on and away from the ASTER / SRTM boundary.
# 
   northRowIndexes = seq(from=1, to=(nrow(pointTable) - 3),by=3)
   borderRowIndexes = seq(from=2, to=(nrow(pointTable) - 1),by=3)   
   southRowIndexes = seq(from=3, to=(nrow(pointTable)),by=3)
#
# calculate and append the difference between elevations
# and CDEM
# these lines create the inputs for differnce image plots for each of three
# pixel pair subsets: North of border (All Aster), border (combo Aster/Srtm),
#                     South of border (all Srtm)
# 
# First, add the 'difference columns' to the entire table
#
   pointTable <-cbind(pointTable,(pointTable$elevNorth - pointTable$elevSouth))
   pointTable <-cbind(pointTable,(pointTable$cdemNorth - pointTable$cdemSouth))
   pointTable <-cbind(pointTable,(pointTable$elevNorth - pointTable$cdemNorth))
   pointTable <-cbind(pointTable,(pointTable$elevSouth - pointTable$cdemSouth))
#   
   colnames(pointTable)[6] <- "diffMosaicNorthSouth"
   colnames(pointTable)[7] <- "diffCDEMNorthSouth"
   colnames(pointTable)[8] <- "diffNorthMosaicCDEM"
   colnames(pointTable)[9] <- "diffSouthMosaicCDEM"   
   
# add a placeholder for the 'boundary' value, across each table

#  pointTable <-cbind(pointTable,(-1))
   
#  colnames(pointTable)[10] <- "deltaAcrossBorder"

# Difference between Mosaic (ASTER or CGIAR or border) 
# and CDEM elevation as pertentage of the mosaic elevation

   pointTable <-cbind(pointTable,(pointTable$diffNorthMosaicCDEM/pointTable$elevNorth * 100)) 
   pointTable <-cbind(pointTable,(pointTable$diffSouthMosaicCDEM/pointTable$elevSouth * 100)) 
   
   colnames(pointTable)[10] <- "magDiffMosaicCDEMNorthPct"
   colnames(pointTable)[11] <- "magDiffMosaicCDEMSouthPct"   

# For the plots, subdivide the table into three segments: 
#     rows north of border
#     rows crossing border
#     rows south of border

   northRowTblAll = pointTable[northRowIndexes,]
   borderRowTblAll = pointTable[borderRowIndexes,]
   southRowTblAll = pointTable[southRowIndexes,]

   subset <- 1:nrow(northRowTblAll)
   
   randSub <- sample(subset,as.integer(length(subset) / plotSampFact)) 
   
   northRowTbl <- northRowTblAll[randSub,]
   borderRowTbl <- borderRowTblAll[randSub,]
   southRowTbl <- southRowTblAll[randSub,]   
   
message("hit key to create each plot...")
browser()

# Three plotting characters used

   plotCh1 <- 17 # 'north' (aster) points
   plotCh2 <- 18 # 'border' (aster+srtm) points 
   plotCh3 <- 20 # 'south' (srtm) points
   lowessLineColor <- "yellow"
# NEW: Three plots: The difference between Mosaic and CDEM for pixels along
# each of three border edges: North (ASTER), Border, South (SRTM)
# for now, north, border, south have separate plots

# We need to tailor the plot 'triplet' X and Y axes to the dynamic ranges of all three data sets.

# Create values for the three 'delta across pixel boundaries' plots: 
#  1) Y-Axis: columnIDs 
#  2) Y-Axis: CDEM elevations
#  3) Y-Axis: 

   deltaBoundaryASTER <- northRowTbl$diffNorthMosaicCDEM - northRowTbl$diffSouthMosaicCDEM
   deltaBoundaryBorder <- borderRowTbl$diffNorthMosaicCDEM - borderRowTbl$diffSouthMosaicCDEM  
   deltaBoundarySRTM <- southRowTbl$diffNorthMosaicCDEM - southRowTbl$diffSouthMosaicCDEM   
   
   normDeltaBoundaryASTER <-  (deltaBoundaryASTER / northRowTbl$cdemNorth * 100)
   normDeltaBoundaryBorder <- (deltaBoundaryBorder / borderRowTbl$cdemNorth * 100)  
   normDeltaBoundarySRTM <-   (deltaBoundarySRTM / southRowTbl$cdemNorth * 100)
   
   par(mfrow=c(1,3)) # Create a set of three column multi-plots
   
   commonXAxis <- c(0,max(pointTable$ColumnID))   
   commonYAxis <- range(c(deltaBoundarySRTM,deltaBoundaryBorder,deltaBoundaryASTER),na.rm=TRUE)   
   
   xAxisLbl <- sprintf("M/SD: ASTER: %.2f / %.2f",mean(deltaBoundaryASTER,na.rm=TRUE),sd(deltaBoundaryASTER,na.rm=TRUE))
   plot(northRowTbl$ColumnID,deltaBoundaryASTER,xlim=commonXAxis, ylim=commonYAxis,main="Delta: ASTER - CDEM",xlab=xAxisLbl,col="red",pch=plotCh1)
   vv <- cbind(northRowTbl$ColumnID,deltaBoundaryASTER)   
#   vx <- vv[-(which(is.na(vv[,"deltaBoundaryASTER"]))),]   
   vx <- vv[(which(!is.na(vv[,"deltaBoundaryASTER"]))),]   
   lines(stats::lowess(vx),col=lowessLineColor)
  
   xAxisLbl <- sprintf("M/SD: BORDER: %.2f / %.2f",mean(deltaBoundaryBorder,na.rm=TRUE),sd(deltaBoundaryBorder,na.rm=TRUE))
   plot(northRowTbl$ColumnID,deltaBoundaryBorder,xlim=commonXAxis, ylim=commonYAxis,main="Delta: Bdry Mix - CDEM",sub="X Axis: West-East Columns",xlab=xAxisLbl,col="darkgreen",pch=plotCh2)
   vv <- cbind(northRowTbl$ColumnID,deltaBoundaryBorder)   
   vx <- vv[(which(!is.na(vv[,"deltaBoundaryBorder"]))),]   
   lines(stats::lowess(vx),col=lowessLineColor)
   
   xAxisLbl <- sprintf("M/SD: SRTM: %.2f / %.2f",mean(deltaBoundarySRTM,na.rm=TRUE),sd(deltaBoundarySRTM,na.rm=TRUE))
   plot(northRowTbl$ColumnID,deltaBoundarySRTM,main="Delta: SRTM - CDEM",xlim=commonXAxis, ylim=commonYAxis,xlab=xAxisLbl,col="blue",pch=plotCh3)
   vv <- cbind(northRowTbl$ColumnID,deltaBoundarySRTM)   
   vx <- vv[(which(!is.na(vv[,"deltaBoundarySRTM"]))),]   
   lines(stats::lowess(vx),col=lowessLineColor)   
   
message("ColumnID-X-Axis is done")
browser()
#dev.new()
#   par(mfrow=c(3,1)) # Create a three column multi-plot pointTable$diffNorthMosaicCDEM/pointTable$elevNorth * 100))
   
   commonXAxis <- c(0,max(pointTable$cdemNorth,na.rm=TRUE))   
   commonYAxis <- range(c(deltaBoundarySRTM,deltaBoundaryBorder,deltaBoundaryASTER),na.rm=TRUE)   
 
#   northDelta.lo <- loess(deltaBoundaryASTER ~ deltaBoundaryASTER,northRowTbl)
   xAxisLbl <- sprintf("M/SD: ASTER: %.2f / %.2f",mean(deltaBoundaryASTER,na.rm=TRUE),sd(deltaBoundaryASTER,na.rm=TRUE))
   plot(northRowTbl$cdemNorth,deltaBoundaryASTER,xlim=commonXAxis, ylim=commonYAxis,main="Delta: ASTER - CDEM",xlab=xAxisLbl,col="red",pch=plotCh1)
   vv <- cbind(northRowTbl$cdemNorth,deltaBoundaryASTER)   
   vx <- vv[(which(!is.na(vv[,"deltaBoundaryASTER"]))),]   
   lines(stats::lowess(vx),col=lowessLineColor)
   
   xAxisLbl <- sprintf("M/SD: BORDER: %.2f / %.2f",mean(deltaBoundaryBorder,na.rm=TRUE),sd(deltaBoundaryBorder,na.rm=TRUE))
   plot(northRowTbl$cdemNorth,deltaBoundaryBorder,xlim=commonXAxis, ylim=commonYAxis,main=" Delta: Bdy Mix - CDEM",sub="X Axis: CDEM Elevation",xlab=xAxisLbl,col="darkgreen",pch=plotCh2)
   vv <- cbind(northRowTbl$cdemNorth,deltaBoundaryBorder)   
   vx <- vv[(which(!is.na(vv[,"deltaBoundaryBorder"]))),]   
   lines(stats::lowess(vx),col=lowessLineColor)
    
   xAxisLbl <- sprintf("M/SD: SRTM: %.2f / %.2f",mean(deltaBoundarySRTM,na.rm=TRUE),sd(deltaBoundarySRTM,na.rm=TRUE))
   plot(northRowTbl$cdemNorth,deltaBoundarySRTM,main="Delta: SRTM - CDEM",xlim=commonXAxis, ylim=commonYAxis,xlab=xAxisLbl,col="blue",pch=plotCh3)
   vv <- cbind(northRowTbl$cdemNorth,deltaBoundarySRTM)   
   vx <- vv[(which(!is.na(vv[,"deltaBoundarySRTM"]))),]   
   lines(stats::lowess(vx),col=lowessLineColor)
message("ColumnID-CDEM Elevation done")
browser()
   commonXAxis <- c(0,max(pointTable$cdemNorth,na.rm=TRUE))   
   commonYAxis <- range(c(normDeltaBoundarySRTM,normDeltaBoundaryBorder,normDeltaBoundaryASTER),na.rm=TRUE)   
   
   xAxisLbl <- sprintf("M/SD: ASTER: %.2f / %.2f",mean(normDeltaBoundaryASTER,na.rm=TRUE),sd(normDeltaBoundaryASTER,na.rm=TRUE))
   plot(northRowTbl$cdemNorth,normDeltaBoundaryASTER,xlim=commonXAxis, ylim=commonYAxis,main="Delta Norm Elev: ASTER",xlab=xAxisLbl,col="red",pch=plotCh1)
   vv <- cbind(northRowTbl$cdemNorth,normDeltaBoundaryASTER)   
   vx <- vv[(which(!is.na(vv[,"normDeltaBoundaryASTER"]))),]   
   lines(stats::lowess(vx),col=lowessLineColor)
   
   xAxisLbl <- sprintf("M/SD: BORDER: %.2f / %.2f",mean(normDeltaBoundaryBorder,na.rm=TRUE),sd(normDeltaBoundaryBorder,na.rm=TRUE))
   plot(northRowTbl$cdemNorth,normDeltaBoundaryBorder,xlim=commonXAxis, ylim=commonYAxis,main="Delta Norm Elev: Border",sub="X Axis: CDEM Elevation",xlab=xAxisLbl,col="darkgreen",pch=plotCh2)
   vv <- cbind(northRowTbl$cdemNorth,normDeltaBoundaryBorder)   
   vx <- vv[(which(!is.na(vv[,"normDeltaBoundaryBorder"]))),]   
   lines(stats::lowess(vx),col=lowessLineColor)
   
   xAxisLbl <- sprintf("M/SD: SRTM: %.2f / %.2f",mean(normDeltaBoundarySRTM,na.rm=TRUE),sd(normDeltaBoundarySRTM,na.rm=TRUE))
   plot(northRowTbl$cdemNorth,normDeltaBoundarySRTM,main="Delta Norm Elev: SRTM",xlim=commonXAxis, ylim=commonYAxis,xlab=xAxisLbl,col="blue",pch=plotCh3)
   vv <- cbind(northRowTbl$cdemNorth,normDeltaBoundarySRTM)   
   vx <- vv[(which(!is.na(vv[,"normDeltaBoundarySRTM"]))),]   
   lines(stats::lowess(vx),col=lowessLineColor)
message("NORMALIZED ColumnID-CDEM Elevation done")
browser()
   commonXAxis <- c(0,max(pointTable$cdemNorth,na.rm=TRUE))   
   commonYAxis <- range(c(northRowTbl$magDiffMosaicCDEMNorthPct,borderRowTbl$magDiffMosaicCDEMNorthPct,southRowTbl$magDiffMosaicCDEMNorthPct),na.rm=TRUE)   
   
   xAxisLbl <- sprintf("M/SD: ASTER: %.2f / %.2f",mean(northRowTbl$magDiffMosaicCDEMNorthPct,na.rm=TRUE),sd(northRowTbl$magDiffMosaicCDEMNorthPct,na.rm=TRUE))
   plot(northRowTbl$cdemNorth,northRowTbl$magDiffMosaicCDEMNorthPct,xlim=commonXAxis, ylim=commonYAxis,main="Delta Vs Elev Mag: ASTER",xlab=xAxisLbl,col="red",pch=plotCh1)
   vv <- cbind(northRowTbl$cdemNorth,northRowTbl$magDiffMosaicCDEMNorthPct)   
   vx <- vv[(which(!is.na(vv[,2]))),]   
   lines(stats::lowess(vx),col=lowessLineColor)
   
   xAxisLbl <- sprintf("M/SD: BORDER: %.2f / %.2f",mean(borderRowTbl$magDiffMosaicCDEMNorthPct,na.rm=TRUE),sd(borderRowTbl$magDiffMosaicCDEMNorthPct,na.rm=TRUE))
   plot(northRowTbl$cdemNorth,borderRowTbl$magDiffMosaicCDEMNorthPct,xlim=commonXAxis, ylim=commonYAxis,main="Delta Vs Elev Mag:: Border",sub="E-W Col",xlab=xAxisLbl,col="darkgreen",pch=plotCh2)
   vv <- cbind(northRowTbl$cdemNorth,deltaBoundarySRTM)   
   vx <- vv[(which(!is.na(vv[,2]))),]   
   lines(stats::lowess(vx),col=lowessLineColor)
   
   xAxisLbl <- sprintf("M/SD: SRTM: %.2f / %.2f",mean(southRowTbl$magDiffMosaicCDEMNorthPct,na.rm=TRUE),sd(southRowTbl$magDiffMosaicCDEMNorthPct,na.rm=TRUE))
   plot(northRowTbl$cdemNorth,southRowTbl$magDiffMosaicCDEMNorthPct,main="Delta Vs Elev Mag:: SRTM",xlim=commonXAxis, ylim=commonYAxis,xlab=xAxisLbl,col="blue",pch=plotCh3)
   vv <- cbind(northRowTbl$cdemNorth,southRowTbl$magDiffMosaicCDEMNorthPct)   
   vx <- vv[(which(!is.na(vv[,2]))),]   
   lines(stats::lowess(vx),col=lowessLineColor)
message("Delta as Fcn Elev Mag-X-Axis is done...")

# if 'plot flag' is set, send the plots to a JPG file. 

if (JpgPlotFileFlag)
{
  message("creating JPG plot files...")
  browser()
jpeg(file="RowBoundaryDeltaColIDXAxisASTER.jpg")
   xAxisLbl <- sprintf("M/SD: ASTER: %.2f / %.2f",mean(deltaBoundaryASTER,na.rm=TRUE),sd(deltaBoundaryASTER,na.rm=TRUE))
   plot(northRowTbl$ColumnID,deltaBoundaryASTER,xlim=commonXAxis, ylim=commonYAxis,main="Row Boundary Delta: ASTER",xlab=xAxisLbl,col="red",pch=plotCh1)
dev.off()
jpeg(file="RowBoundaryDeltaColIDXAxisBORDER.jpg")   
   xAxisLbl <- sprintf("M/SD: BORDER: %.2f / %.2f",mean(deltaBoundaryBorder,na.rm=TRUE),sd(deltaBoundaryBorder,na.rm=TRUE))
   plot(northRowTbl$ColumnID,deltaBoundaryBorder,xlim=commonXAxis, ylim=commonYAxis,main="Row Boundary Delta: Border",sub="E-W Col",xlab=xAxisLbl,col="darkgreen",pch=plotCh2)
dev.off()
jpeg(file="RowBoundaryDeltaColIDXAxisSRTM.jpg")   
   xAxisLbl <- sprintf("M/SD: SRTM: %.2f / %.2f",mean(deltaBoundarySRTM,na.rm=TRUE),sd(deltaBoundarySRTM,na.rm=TRUE))
   plot(northRowTbl$ColumnID,deltaBoundarySRTM,main="Row Boundary Delta: SRTM",xlim=commonXAxis, ylim=commonYAxis,xlab=xAxisLbl,col="blue",pch=plotCh3)
message("ColumnID-X-Axis plots are  done")
#dev.new()
   commonXAxis <- c(0,max(pointTable$cdemNorth))   
   commonYAxis <- range(c(deltaBoundarySRTM,deltaBoundaryBorder,deltaBoundaryASTER),na.rm=TRUE)   
jpeg(file="RowBoundaryDeltaCDEMElevXAxisASTER.jpg")
   xAxisLbl <- sprintf("M/SD: ASTER: %.2f / %.2f",mean(deltaBoundaryASTER,na.rm=TRUE),sd(deltaBoundaryASTER,na.rm=TRUE))
   plot(northRowTbl$cdemNorth,deltaBoundaryASTER,xlim=commonXAxis, ylim=commonYAxis,main="Boundary Delta (CDEM Elev): ASTER",xlab=xAxisLbl,col="red",pch=plotCh1)
dev.off()
jpeg(file="RowBoundaryDeltaCDEMElevXAxisBORDER.jpg")
   xAxisLbl <- sprintf("M/SD: BORDER: %.2f / %.2f",mean(deltaBoundaryBorder,na.rm=TRUE),sd(deltaBoundaryBorder,na.rm=TRUE))
   plot(northRowTbl$cdemNorth,deltaBoundaryBorder,xlim=commonXAxis, ylim=commonYAxis,main="Boundary Delta (CDEM Elev): Border",sub="vs Elev",xlab=xAxisLbl,col="darkgreen",pch=plotCh2)
dev.off()
jpeg(file="RowBoundaryDeltaCDEMElevXAxisCDEM.jpg")
   xAxisLbl <- sprintf("M/SD: SRTM: %.2f / %.2f",mean(deltaBoundarySRTM,na.rm=TRUE),sd(deltaBoundarySRTM,na.rm=TRUE))
   plot(northRowTbl$cdemNorth,deltaBoundarySRTM,main="Boundary Delta (CDEM Elev): SRTM",xlim=commonXAxis, ylim=commonYAxis,xlab=xAxisLbl,col="blue",pch=plotCh3)
dev.off()
message("ColumnID-CDEM Elevation Plots are done")
#browser()
   commonXAxis <- c(0,max(pointTable$cdemNorth))   
   commonYAxis <- range(c(normDeltaBoundarySRTM,normDeltaBoundaryBorder,normDeltaBoundaryASTER),na.rm=TRUE)   
jpeg(file="NormRowBoundaryDeltaColIDXAxisASTER.jpg")
   xAxisLbl <- sprintf("M/SD: ASTER: %.2f / %.2f",mean(normDeltaBoundaryASTER,na.rm=TRUE),sd(normDeltaBoundaryASTER,na.rm=TRUE))
   plot(northRowTbl$cdemNorth,normDeltaBoundaryASTER,xlim=commonXAxis, ylim=commonYAxis,main="Norm Bdry Delta (CDEM Elev): ASTER",xlab=xAxisLbl,col="red",pch=plotCh1)
dev.off()
jpeg(file="NormRowBoundaryDeltaColIDXAxisBORDER.jpg")
   xAxisLbl <- sprintf("M/SD: BORDER: %.2f / %.2f",mean(normDeltaBoundaryBorder,na.rm=TRUE),sd(normDeltaBoundaryBorder,na.rm=TRUE))
   plot(northRowTbl$cdemNorth,normDeltaBoundaryBorder,xlim=commonXAxis, ylim=commonYAxis,main="Norm Bdry Delta (CDEM Elev): Border",sub="vs Elev",xlab=xAxisLbl,col="darkgreen",pch=plotCh2)
dev.off()
jpeg(file="NormRowBoundaryDeltaColIDXAxisSRTM.jpg")
   xAxisLbl <- sprintf("M/SD: SRTM: %.2f / %.2f",mean(normDeltaBoundarySRTM,na.rm=TRUE),sd(normDeltaBoundarySRTM,na.rm=TRUE))
   plot(northRowTbl$cdemNorth,normDeltaBoundarySRTM,main="Norm Bdry Delta (CDEM Elev): SRTM",xlim=commonXAxis, ylim=commonYAxis,xlab=xAxisLbl,col="blue",pch=plotCh3)
dev.off()
message("NORMALIZED ColumnID-CDEM Elevation Plots are done")
browser()
   commonXAxis <- c(0,max(pointTable$ColumnID))   
   commonYAxis <- range(c(normDeltaBoundarySRTM,normDeltaBoundaryBorder,normDeltaBoundaryASTER),na.rm=TRUE)   
jpeg(file="NormRowBoundaryDeltaCDEMElevXAxisASTER.jpg")   
   xAxisLbl <- sprintf("M/SD: ASTER: %.2f / %.2f",mean(normDeltaBoundaryASTER,na.rm=TRUE),sd(normDeltaBoundaryASTER,na.rm=TRUE))
   plot(northRowTbl$ColumnID,normDeltaBoundaryASTER,xlim=commonXAxis, ylim=commonYAxis,main="Norm Bdry Delta: ASTER",xlab=xAxisLbl,col="red",pch=plotCh1)
dev.off()
jpeg(file="NormRowBoundaryDeltaCDEMElevXAxisBORDER.jpg")
   xAxisLbl <- sprintf("M/SD: BORDER: %.2f / %.2f",mean(normDeltaBoundaryBorder,na.rm=TRUE),sd(normDeltaBoundaryBorder,na.rm=TRUE))
   plot(northRowTbl$ColumnID,normDeltaBoundaryBorder,xlim=commonXAxis, ylim=commonYAxis,main="Norm Bdry Delta: Border",sub="E-W Col",xlab=xAxisLbl,col="darkgreen",pch=plotCh2)
dev.off()
jpeg(file="NormRowBoundaryDeltaCDEMElevXAxisSRTM.jpg")
   xAxisLbl <- sprintf("M/SD: SRTM: %.2f / %.2f",mean(normDeltaBoundarySRTM,na.rm=TRUE),sd(normDeltaBoundarySRTM,na.rm=TRUE))
   plot(northRowTbl$ColumnID,normDeltaBoundarySRTM,main="Norm Bdry Delta: SRTM",xlim=commonXAxis, ylim=commonYAxis,xlab=xAxisLbl,col="blue",pch=plotCh3)
dev.off()   
message("NORMALIZED ColumnID-X-Axis Plot files have been created..")
}
message("...All plots created - hit key to delete them...")
browser()
graphics.off()
} 