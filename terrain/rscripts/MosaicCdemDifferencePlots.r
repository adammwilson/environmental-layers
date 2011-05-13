#setwd("f:/Projects/NCEAS/EnviromentAndOrganisms")
CreateDiffPlots <- function()
{
   pointTable <-read.csv("tableForMark4000_5_8_SortColID.csv")
#
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

   pointTable <-cbind(pointTable,pointTable$diffNorthMosaicCDEM/pointTable$elevNorth) * 100
   pointTable <-cbind(pointTable,pointTable$diffSouthMosaicCDEM/pointTable$elevSouth) * 100 
   
   colnames(pointTable)[10] <- "magDiffMosaicCDEMNorthPct"
   colnames(pointTable)[11] <- "magDiffMosaicCDEMSouthPct"   

# For the plots, subdivide the table into three segments: 
#     rows north of border
#     rows crossing border
#     rows south of border

   northRowTbl = pointTable[northRowIndexes,]
   borderRowTbl = pointTable[borderRowIndexes,]
   southRowTbl = pointTable[southRowIndexes,]
message("hit key to create plots...")
browser()
#
# Create plots
#
# First, plot North/South pixel elevation for North/Border/South subsets 'distance East from Western edge'.
#
   plot(northRowTbl$ColumnID,northRowTbl$elevNorth,main="North (ASTER) Mosaic North Pixel Elevation",col="red",pch=".")
   plot(borderRowTbl$ColumnID,borderRowTbl$elevNorth,main="Border Mosaic North Elevation",col="darkgreen",pch=".")
   plot(southRowTbl$ColumnID,southRowTbl$elevNorth,main="South (SRTM) Mosaic North Elevation",col="blue",pch=".")
   
   plot(northRowTbl$ColumnID,northRowTbl$elevNorth,main="North (ASTER) CDEM North Pixel Elevation",col="red",pch=".")
   plot(borderRowTbl$ColumnID,borderRowTbl$elevNorth,main="Border CDEM North Pixel Elevation",col="darkgreen",pch=".")
   plot(southRowTbl$ColumnID,southRowTbl$elevNorth,main="South (SRTM) CDEM North Elevation",col="blue",pch=".")

   plot(northRowTbl$ColumnID,northRowTbl$elevSouth,main="North (ASTER) Mosaic South Pixel Elevation",col="red",pch=".")
   plot(borderRowTbl$ColumnID,borderRowTbl$elevSouth,main="Border Mosaic South Elevation",col="darkgreen",pch=".")
   plot(southRowTbl$ColumnID,southRowTbl$elevSouth,main="South (SRTM) Mosaic South Elevation",col="blue",pch=".")

   plot(northRowTbl$ColumnID,northRowTbl$elevSouth,main="North (ASTER) CDEM South Pixel Elevation",col="red",pch=".")
   plot(borderRowTbl$ColumnID,borderRowTbl$elevSouth,main="Border CDEM South Pixel Elevation",col="darkgreen",pch=".")
   plot(southRowTbl$ColumnID,southRowTbl$elevSouth,main="South (SRTM) CDEM South Elevation",col="blue",pch=".")   
#
# Next, (for now) difference between North and South pixels in pair, for various regions
# 
   plot(northRowTbl$ColumnID,northRowTbl$diffMosaicNorthSouth,main="North (ASTER) Mosaic North/South Diff",col="red",pch=".")
   plot(borderRowTbl$ColumnID,borderRowTbl$diffMosaicNorthSouth,main="Border Mosaic North/South Diff",col="darkgreen",pch=".")
   plot(southRowTbl$ColumnID,southRowTbl$diffMosaicNorthSouth,main="South (SRTM) Mosaic North/South Diff",col="darkgreen",pch=".")   
#   
   plot(northRowTbl$ColumnID,northRowTbl$diffCDEMNorthSouth,main="North (ASTER) CDEM North/South Diff",col="red",pch=".")
   plot(borderRowTbl$ColumnID,borderRowTbl$diffCDEMNorthSouth,main="Border CDEM North/South Diff",col="darkgreen",pch=".")
   plot(southRowTbl$ColumnID,southRowTbl$diffCDEMNorthSouth,main="South (SRTM) CDEM North/South Diff",col="blue",pch=".")

# Next: the Mosaic/CDEM difference divided by the (ASTER or SRTM) elevation

  plot(northRowTbl$ColumnID,northRowTbl$magDiffMosaicCDEMNorthPct,col="red",pch=".",main="Elev Diff as PerCent of Mosaic Elev (North)")
  plot(borderRowTbl$ColumnID,borderRowTbl$magDiffMosaicCDEMNorthPct,col="darkgreen",pch=".",main="Elev Diff as PerCent of Mosaic Elev (Border)")  
  plot(southRowTbl$ColumnID,southRowTbl$magDiffMosaicCDEMSouthPct,col="blue",pch=".",main="Elev Diff as PerCent of Mosaic Elev (South)")  
  
# Finally: Mosaic vs CDEM difference vs elevation (north or south)

  plot(northRowTbl$elevNorth,northRowTbl$diffCDEMNorthSouth,col="red",pch=".",main="ASTER Elev Diff vs Mosaic Elev (North Pixel)")
# plot(northRowTbl$elevSouth,southRowTbl$diffCDEMNorthSouth,col="darkgreen",pch=".",main="ASTER Elev Diff vs Mosaic Elev (South Pixel)")  
  
  plot(borderRowTbl$elevNorth,borderRowTbl$diffCDEMNorthSouth,col="darkgreen",pch=".",main="Border Elev Diff vs Mosaic Elev (North Pixel)")
#  plot(borderRowTbl$elevSouth,borderRowTbl$diffCDEMNorthSouth,col="darkgreen",pch=".",main="Border Elev Diff vs Mosaic Elev (South Pixel)")  
  
  plot(southRowTbl$elevNorth,southRowTbl$diffCDEMNorthSouth,col="blue",pch=".",main="SRTM Elev Diff vs Mosaic Elev (North Pixel)")
#  plot(southRowTbl$elevSouth,southRowTbl$diffCDEMNorthSouth,col="darkgreen",pch=".",main="SRTM Elev Diff vs Mosaic Elev (South Pixel)")  
  





}