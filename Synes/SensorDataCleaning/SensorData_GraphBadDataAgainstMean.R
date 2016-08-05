
library(ggplot2)
library(reshape2)
library(plyr)
library(gridExtra)
library(xlsx)
library(grid)
library(scales)


# Get the functions which I have stored in a separate file
setwd("C:/Dropbox (ASU)/M2NEON/SensorData")

# M2NEON data specific functions, as used below
source("M2NEON_Rfunctions.R")

###############################
# Sensor data file information
###############################
for (Site in c("sm","sf","tf","tm")) {
  
    SensorType <- "temp5cm" # temp1m, temp2m, temp4m, temp5cm, tempmax5cm, tempmin5cm, temps
    
    #SensorFilePath <- sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/FromFTP/%s/level3/%s/%s",
    #                      toupper(Site),
    #                      SensorType,
    #                      sprintf("%s_%s_%s0101-%s1231.csv", SensorType, Site, y, y))
    
    df <- data.frame()
    for (y in c(2013,2014,2015)) {
      SensorFilePath <- sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/CLEANED2/%s",
                                 sprintf("%s_%s_%s0101-%s1231.csv", SensorType, Site, y, y))
      df_year <- GetSensorData(SensorFilePath)
      df <- rbind(df, df_year)
    }
    df_year <- NULL
    
    
    ###############################
    # HOBO Issues
    ###############################
    issuelist <- c("1 = funnel replaced",
                   "2 = sensor replaced",
                   "3 = sensor repositioned",
                   "4 = sensor bad; removed, not replaced",
                   "5 = cap replaced (not likely to affect readings; more of a download issue)",
                   "6 = battery replaced (mostly fall 2014, currently not recorded in these files)",
                   "7 = animal activity or minor damage; no maintenance performed",
                   "8 = funnel badly damaged or gone; not replaced",
                   "9 = trouble downloading, but eventually worked",
                   "10 = sensor removed for happy retirement (no malfunction)")
    issuetext <- paste0("",unlist(issuelist),collapse="\n")
    
    
    dfIssues <- read.xlsx(sprintf("MetaData/%s_HoboIssuesCoded.xlsx", toupper(Site)), sheetIndex=1, stringsAsFactors=FALSE)
    colnames(dfIssues)[1] <- "Date"
    # Remove the "x." that appears in row names from xlsx sheet (not sure why this happens)
    for (i in 3:length(colnames(dfIssues))) {
      colnames(dfIssues)[i] <- substring(colnames(dfIssues)[i],3)
    }
    dfIssues$Date <- as.Date(dfIssues$Date, tz="MST")
    # Get all issues data for this site, including 2013 and prior to 2016
    dfIssues <- subset(dfIssues, Date >= "2013-01-01" & Date < "2016-01-01")
    
    # Get list of sensors that have had issues
    # Select only columns (loc_IDs) that have had an issue in the given time period
    temp <- subset(dfIssues, Checked == "SensorsChecked")
    temp$Date <- as.Date(temp$Date, format="%d/%m/%Y", tz = "MST")
    temp <- temp[colSums(!is.na(temp)) > 0]
    SensorsWithIssues <- colnames(temp)[4:length(colnames(temp))]
    temp <- NULL
    
    dfIssuesOnly <- SubsetSensorsWithIssues(dfIssues)
    
    for (loc in unique(df$loc_ID)) {
      
      loc_issues <- subset(dfIssuesOnly, loc_ID == loc)
      total_issues <- nrow(loc_issues)
      # If there were no issues with this sensor than get the checked dates from another
      # sensor, but remove the issues
      if (total_issues == 0) {
        garden <- substring(loc, 5, 7)
        loc_checked <- unique(subset(dfIssuesOnly, GardenID == garden)$loc_ID)[1]
        loc_issues <- subset(dfIssuesOnly, loc_ID == loc_checked)
        loc_issues$value <- ""
      }
      
      dfBad <- df[df$loc_ID == loc,]
      dfGood <- df[df$loc_ID != loc & df$GardenID == substring(loc,5,7),]
      
      dfBadSummary <- summarySE(dfBad, measurevar = "value", groupvars = c("loc_ID","GardenID","Date"), na.rm=TRUE)
      # Remove the Inf and -Inf values given to max and min values on dates with no data
      dfBadSummary$Sensor.min <- ifelse(!is.finite(dfBadSummary$Sensor.min), NA, dfBadSummary$Sensor.min)
      dfBadSummary$Sensor.max <- ifelse(!is.finite(dfBadSummary$Sensor.max), NA, dfBadSummary$Sensor.max)
      dfGoodSummary <- summarySE(dfGood, measurevar = "value", groupvars = c("GardenID","Date"), na.rm=TRUE)
      dfGoodSummary$Sensor.min <- ifelse(!is.finite(dfGoodSummary$Sensor.min), NA, dfGoodSummary$Sensor.min)
      dfGoodSummary$Sensor.max <- ifelse(!is.finite(dfGoodSummary$Sensor.max), NA, dfGoodSummary$Sensor.max)
      
      p1<-GetPlot(loc, loc_issues, legend=TRUE)
      # Only display the list of possible issues if the sensor had issues
      if (total_issues > 0) {
      # Empty plot, so I can use title to display the list of potential issues
        p2 <- ggplot() + 
          labs(title = issuetext) + theme(plot.title = element_text(hjust = 0))
      }
      else {
        p2 <- ggplot() +
          labs(title = "") + theme(plot.title = element_text(hjust = 0))
      }
      
      # Arrange the two plots together, heights allows the main graph to take more space than the issues list
      plot <- grid.arrange(arrangeGrob(p1,p2, heights=c(3, 1), ncol=1),
                   ncol=1)
      
      if (total_issues > 0) {
        ggsave(file=sprintf("Issues/%s_Issues.pdf",loc), plot, width=14,height=8, dpi=500)
        ggsave(file=sprintf("Issues/%s_Issues.png",loc), plot, width=14,height=8, dpi=500)
      }
      else {
        ggsave(file=sprintf("NoIssues/%s_NoIssues.pdf",loc), plot, width=14,height=8, dpi=500)
        ggsave(file=sprintf("NoIssues/%s_NoIssues.png",loc), plot, width=14,height=8, dpi=500)
      }
      
      dfBad <- NULL
      dfGood <- NULL
      dfBadSummary <- NULL
      dfGoodSummary <- NULL
    }
}









