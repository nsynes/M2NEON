library(tidyr)
library(plyr)

source("C:/Dropbox/Work/ASU/GitHub/M2NEON/Synes/SensorDataCleaning/M2NEON_Rfunctions.R")

TopDir <- "C:/Dropbox/Work/ASU/Paper_2/ANALYSIS/NestedModel/Results/7_Complete"

MainDir <- sprintf("%s/SiteLevel/ModelDirs", TopDir)

setwd(MainDir)

ModelDirs <- list.dirs(full.names=FALSE, recursive=FALSE)


my.list <- vector("list", length(ModelDirs))

i <- 1
for (site in c("Sierra foothills","Sierra montane")) {
  for (day in 1:365) {
    for (metric in c("Max","Min")) {
      
      depvar <- sprintf("Sensor.Day%s.%s", day, metric)
      SimDir <- sprintf("Site(s)=%s_y=%s", site, depvar)
      
      
      fPath <- sprintf("%s/%s/Predictions.csv", MainDir, SimDir)
      if (file.exists(fPath)) {
        cat(paste(SimDir, "\n"))
        
        foo <- NULL
        
        foo <- read.csv(fPath)
        foo$Point.Garden <- as.numeric(substr(foo$loc_ID,7,7))
        foo <- subset(foo, foo$Point.Garden <= 6)
        foo$DepVar <- as.factor(depvar)
        foo$Point.Site <- as.factor(site)
        foo$X <- NULL
        foo$ObsValue <- NULL
        foo$model <- NULL
        foo$dataType <- NULL
        foo$object <- NULL
        foo$GardenMedian <- foo$obs
        foo$obs <- NULL
        foo$GardenMedianPred <- foo$pred
        foo$pred <- NULL
        foo$loc_IDofMedian <- foo$loc_ID
        foo$loc_ID <- NULL
        
        my.list[[i]] <- foo
    
        i <- i + 1
      }
    }
  }
}
dfModelled <- do.call('rbind', my.list)








year <- 2013
dfBACKUP <- read.csv(sprintf("C:/Dropbox/Work/ASU/SensorData/Merged_RasterAndSensorData_%s.csv", year))
dfBACKUP$Point.Site <- substr(dfBACKUP$Point.loc_ID,5,6)
dfBACKUP$Point.Site <- ifelse(dfBACKUP$Point.Site == "sf", "Sierra foothills", dfBACKUP$Point.Site)
dfBACKUP$Point.Site <- ifelse(dfBACKUP$Point.Site == "sm", "Sierra montane", dfBACKUP$Point.Site)
dfBACKUP$Point.Site <- as.factor(dfBACKUP$Point.Site)
dfBACKUP$Point.Garden <- as.numeric(substr(dfBACKUP$Point.loc_ID, 7,7))

dfGardens <- subset(dfBACKUP, Point.Garden %in% 1:6)
dfBACKUP <- NULL

dfGardens <- dfGardens[,c("Point.loc_ID","Point.Garden","Point.Site", paste0("Sensor.Day",1:365, ".Max"),
                                                                      paste0("Sensor.Day",1:365, ".Min"),
                                                                      paste0("Sensor.Day",1:365, ".DiurnalRange"))]

dfGardensLONG <- gather_(dfGardens, "DepVar", "value", c(paste0("Sensor.Day",1:365, ".Max"),
                                                         paste0("Sensor.Day",1:365, ".Min"),
                                                         paste0("Sensor.Day",1:365, ".DiurnalRange")))



dfGardenResid <- merge(dfGardensLONG, dfModelled, by=c("Point.Site","Point.Garden","DepVar"))


# Add any missing levels to the column for loc_IDs that were garden medians
levels(dfGardenResid$loc_IDofMedian) <- c(levels(dfGardenResid$loc_IDofMedian),
                                          levels(dfGardenResid$Point.loc_ID)[!(levels(dfGardenResid$Point.loc_ID) %in% levels(dfGardenResid$loc_IDofMedian))])
# Remove sensors that were used as the median value for each garden
# NOT SURE I NEED TO REMOVE THESE SENSORS??
#dfGardenResid <- subset(dfGardenResid, Point.loc_ID != loc_IDofMedian)

# Residual = observed - predicted value
dfGardenResid$Residual <- dfGardenResid$value - dfGardenResid$GardenMedianPred


dfGardenResidWIDE <- spread(dfGardenResid[,c("DepVar","Point.loc_ID","Residual")], DepVar, Residual)

dfIndVars <- read.csv("C:/Dropbox/Work/ASU/Paper_2/ANALYSIS/NestedModel/Input/MicroSiteLevel_IndepVars.csv")


dfOut <- merge(dfGardenResidWIDE, dfIndVars, by=c("Point.loc_ID"))


write.csv(dfOut, sprintf("%s/Merged_RasterAndResidualSensorData_%s.csv", TopDir, year), row.names=FALSE)








