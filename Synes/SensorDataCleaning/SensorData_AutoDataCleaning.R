
library(ggplot2)
library(reshape2)
library(tidyr)
library(plyr)
library(gridExtra)
library(parallel)
library(lubridate)


setwd("C:/Dropbox (ASU)/M2NEON/SensorData")

# M2NEON data specific functions, as used below
source("C:/Dropbox (ASU)/M2NEON/GitHub/M2NEON/Synes/SensorDataCleaning/M2NEON_Rfunctions.R")

# Values that are outside the garden max min (excluding the current sensor) by more than this
# threshold will be removed
BadDataThreshold <- 5
# Where daily max and min temperatures remain within +-threshold of zero degrees, the
# sensor's day's values will be removed as it suggests they were under snow
SnowThreshold <- 2

SensorType <- "temp5cm" # temp1m, temp2m, temp4m, temp5cm, tempmax5cm, tempmin5cm, temps

sink("CleaningConsoleOutput.txt")

cat(paste("Bad data threshold: ", BadDataThreshold, "\nSnow threshold: ", SnowThreshold,"\n"))

df_snow <- data.frame()

###############################
# Sensor data file information
###############################
#for (Site in c("sm","sf","tf","tm")) {
#  for (y in c(2013,2014,2015)) {
for (Site in c("tf")) {
  for (y in c(2014)) {    
    
    #SensorFilePath <- sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/FromFTP/%s/sitewide/level3/%s/%s",
    #                      Site,
    #                      SensorType,
    #                      sprintf("%s_%s_%s0101-%s1231.csv", SensorType, Site, y, y))
    SensorFilePath <- sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/CLEANED/%s_%s_%s0101-%s1231.csv", SensorType, Site, y, y) 
    
    cat(paste("################\n################\nCurrentFile:", SensorFilePath, "\n"))
    
    df_Original <- GetSensorData(SensorFilePath, DateFormat = "%Y-%m-%d %H:%M:%S")
    ######################################
    
    
    ######################################
    # While loop unti lno more values are removed
    # loop is required since bad data in one sensor may allow bad data from another sensor to remain
    # although loop cannot completely fix issue, e.g. if two sensors have bad data in same day with the threshold of each other
    df_day_pre <- df_Original
    cat(paste("----------\nRemoving extreme values\n----------\n"))
    TotalValuesRemoved <- 1
    while (TotalValuesRemoved > 0) {
      # Remove extreme values that are more than 5 degrees from the gardens daily max min (excluding each sensor in turn) 
      MyList <- RemoveExtremeValues(df_day_pre, "Date", BadDataThreshold)
      TotalValuesRemoved <- MyList[[1]]
      df_day_post <- MyList[[2]]
      cat(paste("Total values removed =", TotalValuesRemoved, "\n"))
      # Set the previous result data frame as the new data frame to be processed
      df_day_pre <- df_day_post[colnames(df_day_pre)]
    }
    ######################################
    
    
    ######################################
    # Remove data that remains in +- the threshold of 0 degrees (these readings suggest that the sensor is under snow)
    cat(paste("----------\nRemoving values where sensors submerged in snow\n----------\n"))
    MySnowList <- RemoveSnowCoverValues(df_day_post, SnowThreshold)
    # Data frame indicating which days had snow for each sensor
    df_SiteSnow <- MySnowList[[1]]
    df_snow <- rbind(df_snow, df_SiteSnow)
    # the updated site dataframe with snow day data removed
    df_day <- MySnowList[[2]]
    ######################################
    
    
    ######################################
    # Format ready for exporting
    df_day[,1] <- strftime(df_day[,1], "%Y-%m-%d %H:%M:%S")
    df_day_wide <- spread(df_day[,c("DateAndTime","loc_ID","value")], loc_ID, value)
    write.csv(df_day_wide, sprintf("CLEANED2/%s_%s_%s0101-%s1231.csv", SensorType, Site, y, y), na="", row.names=FALSE)
    df_Original <- NULL
    df_day_post <- NULL
    df_day_pre <- NULL
    df_day_wide <- NULL
    ######################################
  }
}
write.csv(df_snow, "SnowDays_1stPass.csv", na="", row.names=FALSE)


# CODE TO RERUN THE "SUBMERGED BENEATH SNOW" CHECK
if (FALSE) {
  setwd("C:/Dropbox (ASU)/M2NEON/SensorData")
  # M2NEON data specific functions, as used below
  source("M2NEON_Rfunctions.R")
  
  SensorType <- "temp5cm"
  SnowThreshold <- 2
  
  for (Site in c("sf","sm","tf","tm")) {
    for (y in c(2013,2014,2015)) {
      SensorFilePath <- sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/CLEANED/%s_%s_%s0101-%s1231.csv", SensorType, Site, y, y)
      df_Original <- GetSensorData(SensorFilePath, DateFormat = "%Y-%m-%d %H:%M:%S")
      MySnowList <- RemoveSnowCoverValues(df_Original, SnowThreshold)
      df_day <- MySnowList[[2]]
      df_day[,1] <- strftime(df_day[,1], "%Y-%m-%d %H:%M:%S")
      df_day_wide <- spread(df_day[,c("DateAndTime","loc_ID","value")], loc_ID, value)
      write.csv(df_day_wide, sprintf("CLEANED2/%s_%s_%s0101-%s1231.csv", SensorType, Site, y, y), na="", row.names=FALSE)
    }
  }
  
}


