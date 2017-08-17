library(tidyr)
library(plyr)


source("D:/Dropbox (ASU)/M2NEON/GitHub/M2NEON/Synes/SensorDataCleaning/M2NEON_Rfunctions.R")

year <- "2013"

dfBACKUP <- read.csv(sprintf("D:/Dropbox (ASU)/M2NEON/SensorData/Merged_RasterAndSensorData_%s.csv", year))
dfBACKUP$Point.Site <- substr(dfBACKUP$Point.loc_ID,5,6)
dfBACKUP$Point.Site <- ifelse(dfBACKUP$Point.Site == "sf", "Sierra foothills", dfBACKUP$Point.Site)
dfBACKUP$Point.Site <- ifelse(dfBACKUP$Point.Site == "sm", "Sierra montane", dfBACKUP$Point.Site)
dfBACKUP$Point.Site <- as.factor(dfBACKUP$Point.Site)
dfBACKUP$Point.Garden <- as.factor(substr(dfBACKUP$Point.loc_ID, 7,7))



lVars <- c(paste0("Sensor.Day",1:365, ".Max"), paste0("Sensor.Day",1:365, ".Min"), paste0("Sensor.Day",1:365, ".DiurnalRange"))

dfGardens <- subset(dfBACKUP, Point.Garden %in% 1:6)

my.list <- vector("list", length(lVars))

i <- 1
for (day in 1:365) {
  for (metric in c("Max","Min","DiurnalRange")) {
    
    var <- sprintf("Sensor.Day%s.%s", day, metric)
    
    cat(paste(var, "\n"))
    my.list[[i]] <- getMedian(dfGardens, measurevar=var, IDvar="Point.loc_ID", groupvars=c("Point.Site","Point.Garden"))
    my.list[[i]]$Day <- day
    my.list[[i]]$Metric <- metric
  
    i <- i + 1
  
  }
}
dfDailyMedian <- do.call('rbind', my.list)

# change the data from long to wide format, to match GBM input requirements
dfDailyMedianWIDE <- spread(dfDailyMedian[,c("var","ID","Point.Garden","median")], var, median)
dfDailyMedianWIDE$Point.Garden <- NULL
dfDailyMedianWIDE <- rename(dfDailyMedianWIDE, c("ID" = "Point.loc_ID"))

# Remove any rows that are entirely NAs
dfDailyMedianWIDE <- dfDailyMedianWIDE[rowSums(is.na(dfDailyMedianWIDE)) != ncol(dfDailyMedianWIDE),]

dfLandscapes <- subset(dfBACKUP, Point.Garden == 7)
dfLandscapes <- dfLandscapes[,c("Point.loc_ID",paste0("Sensor.Day",1:365, ".Max"),
                                paste0("Sensor.Day",1:365, ".Min"), paste0("Sensor.Day",1:365, ".DiurnalRange"))]

dfAllMedian <- rbind.fill(dfDailyMedianWIDE, dfLandscapes)

write.csv(dfAllMedian, "D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/SiteLevel_DepVars.csv", row.names=FALSE)

dfIndep <- read.csv("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/SiteLevel_IndepVars.csv")



dfOut <- merge(dfIndep, dfAllMedian, by="Point.loc_ID")
write.csv(dfOut, "D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/SiteLevel_AllVars.csv", row.names=FALSE)







