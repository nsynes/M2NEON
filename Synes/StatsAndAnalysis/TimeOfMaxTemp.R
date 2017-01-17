
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
library(plyr)
library(scales)
library(lattice)
library(grid)
library(rgl)




# Get the functions which I have stored in a separate file
source("D:/Dropbox (ASU)/M2NEON/GitHub/M2NEON/Synes/SensorDataCleaning/M2NEON_Rfunctions.R")

setwd("D:/Dropbox (ASU)/M2NEON/SensorData")

###############################
# Sensor data file information
###############################
SensorType <- "temp5cm" # temp1m, temp2m, temp4m, temp5cm, tempmax5cm, tempmin5cm, temps

y <- 2013

sfFilePath2013 <- sprintf("D:/Dropbox (ASU)/M2NEON/SensorData/CleanPass2_FINAL/%s_%s_%s0101-%s1231.csv",
                          SensorType, "sf", y, y)

smFilePath2013 <- sprintf("D:/Dropbox (ASU)/M2NEON/SensorData/CleanPass2_FINAL/%s_%s_%s0101-%s1231.csv",
                          SensorType, "sm", y, y)

y <- 2014

sfFilePath2014 <- sprintf("D:/Dropbox (ASU)/M2NEON/SensorData/CleanPass2_FINAL/%s_%s_%s0101-%s1231.csv",
                          SensorType, "sf", y, y)

smFilePath2014 <- sprintf("D:/Dropbox (ASU)/M2NEON/SensorData/CleanPass2_FINAL/%s_%s_%s0101-%s1231.csv",
                          SensorType, "sm", y, y)

y <- 2015

sfFilePath2015 <- sprintf("D:/Dropbox (ASU)/M2NEON/SensorData/CleanPass2_FINAL/%s_%s_%s0101-%s1231.csv",
                          SensorType, "sf", y, y)

smFilePath2015 <- sprintf("D:/Dropbox (ASU)/M2NEON/SensorData/CleanPass2_FINAL/%s_%s_%s0101-%s1231.csv",
                          SensorType, "sm", y, y)
###############################




dfAllData <- rbind(GetSensorData(sfFilePath2013, GroupLabels=c("Month","Year")),
                   GetSensorData(smFilePath2013, GroupLabels=c("Month","Year")),
                   GetSensorData(sfFilePath2014, GroupLabels=c("Month","Year")),
                   GetSensorData(smFilePath2014, GroupLabels=c("Month","Year")),
                   GetSensorData(sfFilePath2015, GroupLabels=c("Month","Year")),
                   GetSensorData(smFilePath2015, GroupLabels=c("Month","Year")))


bar1 <- summarySE(dfAllData, measurevar="value", groupvars=c("Date", "loc_ID"), na.rm=TRUE)
bar1 <- subset(bar1, No > 0.9* 144)
bar2 <- merge(dfAllData, bar1, by=c("Date","loc_ID"))
dfMaxTemp<-subset(bar2, value == max)
dfMaxTemp$value<-NULL
dfMaxTemp$min <- NULL
dfMaxTemp$mean <- NULL
dfMaxTemp$sd <- NULL
dfMaxTemp$se <- NULL
dfMaxTemp$ci <- NULL
bar1<-NULL
bar2<-NULL

# Add a time column to the dataframe
dfMaxTemp$Time <- strftime(dfMaxTemp[,"DateAndTime"], format="%H:%M:%S")
# Add a date to the time
dfMaxTemp$TimeFormat <- as.POSIXct(dfMaxTemp$Time, format="%H:%M:%S")

dfMaxTemp$MidMonthDate <- as.Date(sprintf("%s-%s-%s", dfMaxTemp$Year, dfMaxTemp$Month, 15))

dfMeanDailyMax<-ddply(dfMaxTemp, ~MidMonthDate+loc_ID, summarise, MeanDailyMax = mean(max), MeanTime = mean(TimeFormat))

dfMeanDailyMax$Year <- as.POSIXlt(dfMeanDailyMax[,1], tz = "MST")$year + 1900


dfSAC <- read.csv("D:/Dropbox (ASU)/M2NEON/SensorData/SlopeAspectCanopy.csv")
dfSAC$loc_ID <- dfSAC$Point.loc_ID
dfSAC$Point.loc_ID <- NULL




###################
#PLOTTING
###################
Sensor <- "tdl_sm220_0"
dfSub <- subset(dfMaxTemp, loc_ID == Sensor)
dfSubMonth <- subset(dfMeanDailyMax, loc_ID == Sensor)

pTime <- ggplot() +
           geom_line(data=dfSub, aes(x=Date, y=TimeFormat), color="black", linetype=2) +
           geom_line(data=dfSubMonth, aes(x=MidMonthDate, y=MeanTime), size=2) +
           scale_y_datetime(date_breaks = "2 hour",
                            labels = date_format("%H:%M", tz="MST")) +
           labs(y="Time of max temperature") +
           theme_bw() +
           theme(text = element_text(size=20))

pMaxTemp <- ggplot() +
              geom_line(data=dfSub, aes(x=Date, y=max), color="red", linetype=2) +
              geom_line(data=dfSubMonth, aes(x=MidMonthDate, y=MeanDailyMax), color="red", size=2) +
              labs(y="Max temperature\n.") +
              theme_bw() +
  theme(text = element_text(size=20))
  
grid.arrange(top=textGrob(Sensor,gp=gpar(fontsize=20)), pTime, pMaxTemp)
###########################


###############
# 3D
###############
setwd("D:/Dropbox (ASU)/M2NEON")

for (s in c("SF","SM")) {
  for (y in c(2013)) {
    for (view in c("Front","Rear")) {
      
      dfMerged <- merge(dfMeanDailyMax, dfSAC, by=c("loc_ID"))
      dfMerged <- subset(dfMerged, Point.Site == s & Year == y)
      dfMerged <- dfMerged[order(dfMerged$MidMonthDate, dfMerged$Raster.SlopeCosAspect),]
      
      if (view == "Front") screenview = list(z = 50, x = -75, y = 0)
      if (view == "Rear") screenview = list(z = 220, x = -75, y = 0)
      
      
      p <- wireframe(as.numeric(MeanDailyMax) ~ as.numeric(MidMonthDate) * Raster.SlopeCosAspect, dfMerged,
                    #shade = TRUE, 
                    drape=TRUE, aspect = c(1, 1),
                    xlab="Month", ylab="Slope * Northness", zlab="Mean\ndaily\nmax",
                    #light.source = c(10,10,10),
                    main = sprintf("Site: %s, Year: %s", paste0(unique(dfMerged$Point.Site), sep=" "),
                                                         paste0(unique(dfMerged$Year), sep=",")),
                    col.regions = colorRampPalette(c("blue", "red"))(100),
                    scales = list(x=list(at=unique(as.numeric(dfMerged$MidMonthDate)), labels=c('J','F','M','A','M','J','J','A','S','O','N','D')),
                                  z.ticks=5,arrows=FALSE, col="black", font=3, tck=1, cex=1.2),
                    screen = screenview)
      
      png(filename=sprintf("Plot3d_%s%s_%sView.png", s, y, view), width=800, height=800)
      plot(p)
      dev.off()
    }
  }
}


####
# OR.....
######


c = dfMerged$loc_ID
c = cut(c, breaks=length(unique(dfMerged$loc_ID)))
cols = rainbow(length(unique(dfMerged$loc_ID)))[as.numeric(c)]

plot3d(dfMerged$MidMonthDate, dfMerged$Raster.SlopeCosAspect, dfMerged$MeanDailyMax)





