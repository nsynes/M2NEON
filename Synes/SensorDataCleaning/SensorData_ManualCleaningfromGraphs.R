
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


for (Site in c("sm","sf","tf","tm")) {

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
  
  # write to CSV so that any logged issues that are not a problem can be deleted
  # any issues left can then be used to direct final data cleaning
  write.csv(dfIssuesOnly, sprintf("%s_issues.csv", Site))
  
  
}
# AFTER RUNNING THE SECTINO ABOVE, NEED TO LOOK THROUGH EXCEL FILES AND THE GRAPHS AND IDENTIFY PERIODS OF TIME THAT
# STILL CONTAIN BAD DATA

# ONCE THE BAD PERIODS HAVE BEEN REMOVED< SHOULD RERUN THE MAX MIN CHECKS SINCE SOME (e.g. tf120) have extreme data points which were not cleaned
# because a another sensor in the garden was bad (but should be clean after this process)


dfDataToRemove <- read.csv("C:/Dropbox (ASU)/M2NEON/SensorData/CLEANED/graphs/IssuesIdentifiedFromGraphs/AllIssues.csv")
SensorType <- "temp5cm"
for (site in c("sm","sf","tf","tm")) {
  dfRemoveBySite <- subset(dfDataToRemove, Site == site)
  df <- data.frame()
  
  for (y in c(2013,2014,2015)) {
    SensorFilePath <- sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/CLEANED/%s",
                              sprintf("%s_%s_%s0101-%s1231.csv", SensorType, Site, y, y))
    df_year <- GetSensorData(SensorFilePath)
    df <- rbind(df, df_year)
  }
  df_year <- NULL
  levels(dfRemoveBySite$loc_ID) <- levels(df$loc_ID)
  foo1 <- adply(dfRemoveBySite, 1, function(x) {
    df$value <- ifelse(df$loc_ID == x$loc_ID & df$Date >= x$FromDate & df$Date <= x$ToDate, NA, df$value)
  })
}



