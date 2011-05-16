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
#
#   2) sampling factor: integer (range=1 to number of recors in input table. Determines
#      the size of randomly-selected sampe of total table record 'triplets' (north, border,
#      and south) rows of each column sample) to be displayed in the plots: 
#           sampling factor = 1  : plot every table record
#                             10 : plot a random sample containing 1/10th of records
#                            100 : plot random sample containing 1/100th of records.
# 
# To run:
#
#  1) place this file and the input file "tableForMark4000_5_8_SortColID.csv" in folder
#  2) start R
#  3) > source("CreateImageDiffPlotsOverlay.r")
#  4) > CreateImageDiffPlots(sampling factor)
#     < press <cr> key to view plots sequentially
#  
# TODO: add symbol legend to each plot.
# Author: Rick Reeves, NCEAS
# May 14, 2011
##############################################################################################
CreateImageDiffPlots <- function(plotSampFact = 100)
{
# Check plotSampleFact range  

   if (plotSampFact < 1)
      plotSampFact = 1
      
   if (plotSampFact > 100)
      plotSampFact = 100
 
# Read input table that was sorted OFFLINE in Excel on the first column (ColumnID)

   pointTable <-read.csv("tableForMark4000_5_8_SortColID.csv")

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

   plotCh1 <- 17 # 'north' (Aster) points
   plotCh2 <- 18 # 'border' (Aster+srtm) points 
   plotCh3 <- 20 # 'south' (srtm) points
   
# First plot pair: North/South Mosaic Image pixel elevation for North/South subsets 'distance East from Western edge'.

   xAxisLbl <- sprintf("M/SD: AST: %.2f / %.2f BRD: %.2f / %.2f STM: %.2f / %.2f",mean(northRowTbl$elevNorth,na.rm=TRUE),sd(northRowTbl$elevNorth,na.rm=TRUE),
                                                                            mean(borderRowTbl$elevNorth,na.rm=TRUE),sd(borderRowTbl$elevNorth,na.rm=TRUE),
                                                                            mean(southRowTbl$elevNorth,na.rm=TRUE),sd(southRowTbl$elevNorth,na.rm=TRUE))
   plot(northRowTbl$ColumnID,northRowTbl$elevNorth,main="North Mosaic Pixel Elev: ASTER (red) Border (green) SRTM (blue)",xlab=xAxisLbl,col="red",pch=plotCh1)
   points(borderRowTbl$ColumnID,borderRowTbl$elevNorth,col="darkgreen",pch=plotCh2)
   points(southRowTbl$ColumnID,southRowTbl$elevNorth,col="blue",pch=plotCh3)
browser()
dev.new()
   xAxisLbl <- sprintf("M/SD: AST: %.2f / %.2f BRD: %.2f / %.2f STM: %.2f / %.2f",mean(northRowTbl$elevSouth,na.rm=TRUE),sd(northRowTbl$elevSouth,na.rm=TRUE),
                                                                            mean(borderRowTbl$elevSouth,na.rm=TRUE),sd(borderRowTbl$elevSouth,na.rm=TRUE),
                                                                            mean(southRowTbl$elevSouth,na.rm=TRUE),sd(southRowTbl$elevSouth,na.rm=TRUE))
   plot(northRowTbl$ColumnID,northRowTbl$elevSouth,main="South Mosaic Pixel Elev: ASTER (red) Border (green) SRTM (blue)",xlab=xAxisLbl,col="red",pch=plotCh1)
   points(borderRowTbl$ColumnID,borderRowTbl$elevSouth,col="darkgreen",pch=plotCh2)
   points(southRowTbl$ColumnID,southRowTbl$elevSouth,col="blue",pch=plotCh3)
browser()
dev.new()
# Second plot pair: North/South CDEM pixel elevation for North/South subsets 'distance East from Western edge'.

   xAxisLbl <- sprintf("M/SD: AST: %.2f / %.2f BRD: %.2f / %.2f STM: %.2f / %.2f",mean(northRowTbl$cdemNorth,na.rm=TRUE),sd(northRowTbl$cdemNorth,na.rm=TRUE),
                                                                            mean(borderRowTbl$cdemNorth,na.rm=TRUE),sd(borderRowTbl$cdemNorth,na.rm=TRUE),
                                                                            mean(southRowTbl$cdemNorth,na.rm=TRUE),sd(southRowTbl$cdemNorth,na.rm=TRUE))
   plot(northRowTbl$ColumnID,northRowTbl$cdemNorth,main="North CDEM Pixel Elev ASTER (r) Border (g) SRTM (b)",xlab=xAxisLbl,col="red",pch=plotCh1)
   points(borderRowTbl$ColumnID,borderRowTbl$cdemNorth,col="darkgreen",pch=plotCh2)
   points(southRowTbl$ColumnID,southRowTbl$cdemNorth,col="blue",pch=plotCh3)
browser()   
dev.new()
   xAxisLbl <- sprintf("M/SD: AST: %.2f / %.2f BRD: %.2f / %.2f STM: %.2f / %.2f",mean(northRowTbl$cdemSouth,na.rm=TRUE),sd(northRowTbl$cdemSouth,na.rm=TRUE),
                                                                            mean(borderRowTbl$cdemSouth,na.rm=TRUE),sd(borderRowTbl$cdemSouth,na.rm=TRUE),
                                                                            mean(southRowTbl$cdemSouth,na.rm=TRUE),sd(southRowTbl$cdemSouth,na.rm=TRUE))   
   plot(northRowTbl$ColumnID,northRowTbl$cdemSouth,main="South CDEM Pixel Elev ASTER (r) Border (g) SRTM (b)",xlab=xAxisLbl,col="red",pch=plotCh1)
   points(borderRowTbl$ColumnID,borderRowTbl$cdemSouth,col="darkgreen",pch=plotCh2)
   points(southRowTbl$ColumnID,southRowTbl$cdemSouth,col="blue",pch=plotCh3)
browser()
dev.new()
# Third plot pair: difference between North and South pixels in pair.

   xAxisLbl <- sprintf("M/SD: AST: %.2f / %.2f BRD: %.2f / %.2f STM: %.2f / %.2f",mean(northRowTbl$diffMosaicNorthSouth,na.rm=TRUE),sd(northRowTbl$diffMosaicNorthSouth,na.rm=TRUE),
                                                                            mean(borderRowTbl$diffMosaicNorthSouth,na.rm=TRUE),sd(borderRowTbl$diffMosaicNorthSouth,na.rm=TRUE),
                                                                            mean(southRowTbl$diffMosaicNorthSouth,na.rm=TRUE),sd(southRowTbl$diffMosaicNorthSouth,na.rm=TRUE))
   plot(northRowTbl$ColumnID,northRowTbl$diffMosaicNorthSouth,main="Mosaic N/S Pixel Diff: ASTER (r) Border (g) SRTM (b)",xlab=xAxisLbl,col="red",pch=plotCh1)
   points(borderRowTbl$ColumnID,borderRowTbl$diffMosaicNorthSouth,col="darkgreen",pch=plotCh2)
   points(southRowTbl$ColumnID,southRowTbl$diffMosaicNorthSouth,col="blue",pch=plotCh3)   
browser()  
dev.new()
   xAxisLbl <- sprintf("M/SD: AST: %.2f / %.2f BRD: %.2f / %.2f STM: %.2f / %.2f",mean(northRowTbl$diffCDEMNorthSouth,na.rm=TRUE),sd(northRowTbl$diffCDEMNorthSouth,na.rm=TRUE),
                                                                            mean(borderRowTbl$diffCDEMNorthSouth,na.rm=TRUE),sd(borderRowTbl$diffCDEMNorthSouth,na.rm=TRUE),
                                                                            mean(southRowTbl$diffCDEMNorthSouth,na.rm=TRUE),sd(southRowTbl$diffCDEMNorthSouth,na.rm=TRUE))
   plot(northRowTbl$ColumnID,northRowTbl$diffCDEMNorthSouth,main="CDEM N/S Pixel Diff: ASTER (r) Border (g) SRTM (b)",xlab=xAxisLbl,col="red",pch=plotCh1)
   points(borderRowTbl$ColumnID,borderRowTbl$diffCDEMNorthSouth,col="darkgreen",pch=plotCh2)
   points(southRowTbl$ColumnID,southRowTbl$diffCDEMNorthSouth,col="blue",pch=plotCh3)
browser()
dev.new()

# Fourth plot pair: the Mosaic/CDEM difference for North and South pixels

   xAxisLbl <- sprintf("M/SD: AST: %.2f / %.2f BRD: %.2f / %.2f STM: %.2f / %.2f",mean(northRowTbl$diffNorthMosaicCDEM,na.rm=TRUE),sd(northRowTbl$diffNorthMosaicCDEM,na.rm=TRUE),
                                                                            mean(borderRowTbl$diffNorthMosaicCDEM,na.rm=TRUE),sd(borderRowTbl$diffNorthMosaicCDEM,na.rm=TRUE),
                                                                            mean(southRowTbl$diffNorthMosaicCDEM,na.rm=TRUE),sd(southRowTbl$diffNorthMosaicCDEM,na.rm=TRUE))
   plot(northRowTbl$ColumnID,northRowTbl$diffMosaicNorthSouth,main="Mosaic / CDEM Pixel Diff (North): ASTER (r) Border (g) SRTM (b)",xlab=xAxisLbl,col="red",pch=plotCh1)
   points(borderRowTbl$ColumnID,borderRowTbl$diffMosaicNorthSouth,col="darkgreen",pch=plotCh2)
   points(southRowTbl$ColumnID,southRowTbl$diffMosaicNorthSouth,col="blue",pch=plotCh3)   
browser()
dev.new()
   xAxisLbl <- sprintf("M/SD: AST: %.2f / %.2f BRD: %.2f / %.2f STM: %.2f / %.2f",mean(northRowTbl$diffSouthMosaicCDEM,na.rm=TRUE),sd(northRowTbl$diffSouthMosaicCDEM,na.rm=TRUE),
                                                                            mean(borderRowTbl$diffSouthMosaicCDEM,na.rm=TRUE),sd(borderRowTbl$diffSouthMosaicCDEM,na.rm=TRUE),
                                                                            mean(southRowTbl$diffSouthMosaicCDEM,na.rm=TRUE),sd(southRowTbl$diffSouthMosaicCDEM,na.rm=TRUE))
   plot(northRowTbl$ColumnID,northRowTbl$diffCDEMNorthSouth,main="Mosaic / CDEM Pixel Diff (South): ASTER (r) Border (g) SRTM (b)",xlab=xAxisLbl,col="red",pch=plotCh1)
   points(borderRowTbl$ColumnID,borderRowTbl$diffCDEMNorthSouth,col="darkgreen",pch=plotCh2)
   points(southRowTbl$ColumnID,southRowTbl$diffCDEMNorthSouth,col="blue",pch=plotCh3)
browser()
dev.new()
# Fifth plot pair: the Mosaic/CDEM difference divided by the (ASTER or SRTM) elevation

  xAxisLbl <- sprintf("M/SD: AST: %.2f / %.2f BRD: %.2f / %.2f STM: %.2f / %.2f",mean(northRowTbl$magDiffMosaicCDEMNorthPct,na.rm=TRUE),sd(northRowTbl$magDiffMosaicCDEMNorthPct,na.rm=TRUE),
                                                                            mean(borderRowTbl$magDiffMosaicCDEMNorthPct,na.rm=TRUE),sd(borderRowTbl$magDiffMosaicCDEMNorthPct,na.rm=TRUE),
                                                                            mean(southRowTbl$magDiffMosaicCDEMNorthPct,na.rm=TRUE),sd(southRowTbl$magDiffMosaicCDEMNorthPct,na.rm=TRUE))  
  plot(northRowTbl$ColumnID,northRowTbl$magDiffMosaicCDEMNorthPct,main="Pixel Elev Diff as % of Mosaic Elev (North) - ASTER (r) Border (g) SRTM (b)",xlab=xAxisLbl,col="red",pch=plotCh1)
  points(borderRowTbl$ColumnID,borderRowTbl$magDiffMosaicCDEMNorthPct,col="darkgreen",pch=plotCh2)  
  points(southRowTbl$ColumnID,southRowTbl$magDiffMosaicCDEMNorthPct,col="blue",pch=plotCh3)  
browser()
dev.new()
  xAxisLbl <- sprintf("M/SD: AST: %.2f / %.2f BRD: %.2f / %.2f STM: %.2f / %.2f",mean(northRowTbl$magDiffMosaicCDEMSouthPct,na.rm=TRUE),sd(northRowTbl$magDiffMosaicCDEMSouthPct,na.rm=TRUE),
                                                                            mean(borderRowTbl$magDiffMosaicCDEMSouthPct,na.rm=TRUE),sd(borderRowTbl$magDiffMosaicCDEMSouthPct,na.rm=TRUE),
                                                                             mean(southRowTbl$magDiffMosaicCDEMSouthPct,na.rm=TRUE),sd(southRowTbl$magDiffMosaicCDEMSouthPct,na.rm=TRUE))
  plot(northRowTbl$ColumnID,northRowTbl$magDiffMosaicCDEMSouthPct,main="Pixel Elev Diff as % of Mosaic Elev (South) - ASTER (r) Border (g) SRTM (b)",xlab=xAxisLbl,col="red",pch=plotCh1)
  points(borderRowTbl$ColumnID,borderRowTbl$magDiffMosaicCDEMSouthPct,col="darkgreen",pch=plotCh2)  
  points(southRowTbl$ColumnID,southRowTbl$magDiffMosaicCDEMSouthPct,col="blue",pch=plotCh3)  
browser()
dev.new()
# Final plot pair: Mosaic vs CDEM difference vs elevation (north or south)

xAxisLbl <- sprintf("M/SD: AST: %.2f / %.2f BRD: %.2f / %.2f STM: %.2f / %.2f",mean(northRowTbl$diffMosaicNorthSouth,na.rm=TRUE),sd(northRowTbl$diffMosaicNorthSouth,na.rm=TRUE),
                                                                          mean(borderRowTbl$diffMosaicNorthSouth,na.rm=TRUE),sd(borderRowTbl$diffMosaicNorthSouth,na.rm=TRUE),
                                                                          mean(southRowTbl$diffMosaicNorthSouth,na.rm=TRUE),sd(southRowTbl$diffMosaicNorthSouth,na.rm=TRUE))
plot(northRowTbl$elevNorth,northRowTbl$diffMosaicNorthSouth,main="Mosaic Elev N/S Diff ASTER (r) Border (g) SRTM (b)",xlab=xAxisLbl,col="red",pch=plotCh1)
points(borderRowTbl$elevNorth,borderRowTbl$diffMosaicNorthSouth,col="darkgreen",pch=plotCh2)
points(southRowTbl$elevNorth,southRowTbl$diffMosaicNorthSouth,col="blue",pch=plotCh3)
browser()
dev.new()
  xAxisLbl <- sprintf("M/SD: AST: %.2f / %.2f BRD: %.2f / %.2f STM: %.2f / %.2f",mean(northRowTbl$diffCDEMNorthSouth,na.rm=TRUE),sd(northRowTbl$diffCDEMNorthSouth,na.rm=TRUE),
                                                                            mean(borderRowTbl$diffCDEMNorthSouth,na.rm=TRUE),sd(borderRowTbl$diffCDEMNorthSouth,na.rm=TRUE),
                                                                            mean(southRowTbl$diffCDEMNorthSouth,na.rm=TRUE),sd(southRowTbl$diffCDEMNorthSouth,na.rm=TRUE))
  plot(northRowTbl$elevNorth,northRowTbl$diffCDEMNorthSouth,main="CDEM Elev N/S Diff ASTER (r) Border (g) SRTM (b)",xlab=xAxisLbl,col="red",pch=plotCh1)
  points(borderRowTbl$elevNorth,borderRowTbl$diffCDEMNorthSouth,col="darkgreen",pch=plotCh2)
  points(southRowTbl$elevNorth,southRowTbl$diffCDEMNorthSouth,col="blue",pch=plotCh3)
message("All plots created - hit key to delete them...")
browser()
graphics.off()
} 