
library(ggplot2)
library(reshape2)
library(gridExtra)
library(GGally)
# Note that tidyr and raster both have an extract tool, so loading tidyr first means that
# the tidyr extract is masked by the raster extract
library(tidyr)
library(raster)
library(rgdal)
library(stringr)
library(sp)
library(maptools)
library(foo)
library(tools)



GetRasterStatBasedOnPoly <- function(RasterFilePath, ShapeFilePath, Function) {
  polygons <- readOGR(dsn=dirname(ShapeFilePath),
                      layer=file_path_sans_ext(basename(ShapeFilePath)),
                      pointDropZ=TRUE)
  r <- raster(RasterFilePath)
  
  ExtractMyData <- extract(r, polygons, small=TRUE, fun=get(Function), na.rm=TRUE, df=FALSE,  nl=1, sp=TRUE)
  Out <- ExtractMyData@data
  names(Out)[names(Out) == file_path_sans_ext(basename(RasterFilePath))] <- sprintf("%s.%s.%s", file_path_sans_ext(basename(ShapeFilePath)),
                                                                                    file_path_sans_ext(basename(RasterFilePath)),
                                                                                    Function)
  return(Out)
}





#####################################
# Collect raster data from shapes
#########################################

my.list <- vector("list", 3)

i <- 1
for (radius in c("2_5","5","10")) {
  shape <- sprintf("C:/Dropbox (ASU)/M2NEON/Paper_2/DATA/VECTOR/SensorPoints/Circle_Radius%sm.shp", radius)
  raster <- "C:/Dropbox (ASU)/M2NEON/Paper_2/DATA/RASTER/LAS/CanopyDensity_2m.tif"
  func <- "mean"
  
  my.list[[i]] <- GetRasterStatBasedOnPoly(raster, shape, func)
  
  i <- i + 1
}

dfPolygon <- Reduce(function(x, y) merge(x, y, by="loc_ID"), my.list)

write.csv(dfPolygon, "C:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/MicrositeLevel_CircleCanopy.csv", row.names=FALSE)


######################################
######################################


######################################
# Collect raster data from points
######################################
my.list <- vector("list", 1)

i <- 1
for (radius in c(30)) {
  shape <- sprintf("C:/Dropbox (ASU)/M2NEON/Paper_2/DATA/VECTOR/SensorPoints/SiteLevel_Circle_Radius%sm.shp", radius)
  raster <- "C:/Dropbox (ASU)/M2NEON/Paper_2/DATA/RASTER/DEM/DEM_2m.tif"
  func <- "mean"
  
  my.list[[i]] <- GetRasterStatBasedOnPoly(raster, shape, func)
  
  i <- i + 1
}

dfPoly <- Reduce(function(x, y) merge(x, y, by="site_ID"), my.list)

dfMerge <- merge(dfPolygon, dfPoly, by="site_ID")

######################################
######################################



######################################
# Calculate relative elevation (elevation at point minus min elevation within radius)
######################################

for (radius in c(150)) {
  dfMerge[,sprintf("RelativeElevation.SiteLevel_Circle_Radius%sm", radius)] <- dfMerge[,"SiteLevel_Circle_Radius30m.DEM_2m.mean"] - dfMerge[,sprintf("SiteLevel_Circle_Radius%sm.DEM_2m.min", radius)]
}
dfMerge <- dfMerge[,c("site_ID","RelativeElevation.SiteLevel_Circle_Radius150m")]

write.csv(dfMerge, "C:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/SiteLevel_RelativeElevation.csv", row.names=FALSE)


######################################
######################################




######################################
# Get wider radius canopy density data
######################################


my.list <- vector("list", 2)

i <- 1

for (radius in c(30,90)) {
  shape <- sprintf("C:/Dropbox (ASU)/M2NEON/Paper_2/DATA/VECTOR/SensorPoints/SiteLevel_Circle_Radius%sm.shp", radius)
  raster <- "C:/Dropbox (ASU)/M2NEON/Paper_2/DATA/RASTER/LAS/CanopyDensity_2m.tif"
  func <- "mean"
  
  my.list[[i]] <- GetRasterStatBasedOnPoly(raster, shape, func)
  
  i <- i + 1
}

dfPolygon <- Reduce(function(x, y) merge(x, y, by="site_ID"), my.list)

write.csv(dfPolygon, "C:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/SiteLevel_CanopyDensity.csv", row.names=FALSE)



######################################
######################################


















