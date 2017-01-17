library(raster)
library(rgdal)


dir.values <- "E:/D17_GIS/PAPER_1_ANALYSIS/R_test/inRASTER"
dir.zones <- "E:/D17_GIS/PAPER_1_ANALYSIS/R_test/inVECTOR"

# Doesn't need to exist
dir.output <- "E:/D17_GIS/PAPER_1_ANALYSIS/R_test/outFolder"


dir.create(dir.output)

merge.all <- function(x, y) {
  merge(x, y, all=TRUE, by="zone")
}


for (str.zones in list.files(dir.zones)) {
  
  if (substr(str.zones, nchar(str.zones)-3, nchar(str.zones)) == ".shp") {
    cat(paste(str.zones, "\n"))
    
    str.zones <- substr(str.zones, 1, nchar(str.zones)-4)
    shp.zones <- readOGR(dir.zones, str.zones) # Load zones shapefile
    listDF <- NULL
    listDF <- list() #empty list to store dataframe
    
    i <- 1
    for (str.values in list.files(dir.values)) {
      
      if (substr(str.values, nchar(str.values)-3, nchar(str.values)) == ".tif") {
        cat(paste(str.values, "\n"))
        
        pth.values <- file.path(dir.values, str.values)
        rst.values <- raster(pth.values) # Load values raster
        rst.zones <- rasterize(shp.zones, rst.values) # Convert zones shapefile to raster
        
        # Create dataframe of stats
        df <- Reduce(merge.all, lapply(c('count','sum','min','mean','max','sd'),
                                       function(x)
                                         data.frame(zonal(rst.values,rst.zones,fun=x, digits=0, na.rm=TRUE))))
        
        # Rename columns to incorporate value raster name
        for (name in colnames(df[colnames(df) != "zone"])) {
          colnames(df)[colnames(df) == name] <- sprintf("%s.%s",
                                                        substr(str.values, 1, nchar(str.values)-4), name)
        }
        
        listDF[[i]] <- df
        i <- i + 1
      }
    }
    
    dfOUT <- Reduce(merge.all, listDF)
    write.csv(dfOUT, file.path(dir.output, sprintf("%s.csv", str.zones)), row.names=FALSE)
    
  }
}









