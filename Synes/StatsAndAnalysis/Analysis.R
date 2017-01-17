
library(ggplot2)
library(gtools)
#library(dismo)
library(gbm)
library(tidyr)
library(caret)
library(gridExtra)




# Get the functions which I have stored in a separate file
source("C:/Dropbox (ASU)/M2NEON/GitHub/M2NEON/Synes/SensorDataCleaning/M2NEON_Rfunctions.R")

setwd("C:/Dropbox (ASU)/M2NEON/SensorData")

df <- read.csv("Merged_RasterAndSensorData.csv")
df$Point.Site <- substr(df$Point.loc_ID,5,6)
df$Point.Site <- ifelse(df$Point.Site == "sf", "Sierra foothills", df$Point.Site)
df$Point.Site <- ifelse(df$Point.Site == "sm", "Sierra montane", df$Point.Site)
df$Point.Site <- as.factor(df$Point.Site)

# Generate temperature range values by month and quarter from max and min sensor values
for (period in c("Month","Quarter")) {
  if (period == "Month") Quantity <- 1:12
  if (period == "Quarter") Quantity <- 1:4
  listSensorPeriod <- lapply(paste0(period, Quantity), function(p) sprintf("Sensor.%s", p))
  for (SensorPeriod in listSensorPeriod) {
    df[sprintf("%s.TempRange", SensorPeriod)] <- df[[sprintf("%s.max", SensorPeriod)]] - df[[sprintf("%s.min", SensorPeriod)]]
  }
}
dfBACKUP <- df

####################
# Reformat to 'long format' data
dfLong <- melt(df, measure.vars = colnames(df[ , !(names(df) %in% c("Point.loc_ID","Point.coords.x1","Point.coords.x2"))]),
                   variable.name = "variable")
dfLong$VarName <- dfLong$variable
####################


####################
# Generate plots of raster data against sensor data by month and quarter
for (period in c("Month","Quarter")) {
  for (measure in c(paste0("CDD", c(5,10,15,20,25,30)),c("max","min","mean","TempRange"))) {
    for (radiation in c("DirectDuration","DirectRadiation","SolarRadiation",
                        "CanopyFracDirectDuration.LASdns","CanopyFracDirectRadiation.LASdns","CanopyFracSolarRadiation.LASdns",
                        "CanopyFracDirectDuration.LAScov","CanopyFracDirectRadiation.LAScov","CanopyFracSolarRadiation.LAScov")) {
      if (period == "Month") Quantity <- 1:12
      if (period == "Quarter") Quantity <- 1:4
      
      listSensorVars <- lapply(paste0(period, Quantity), function(p) sprintf("Sensor.%s.%s", p, measure))
      
      dfSensor <- melt(df, id.vars = c("Point.loc_ID", "Point.Site"), measure.vars = listSensorVars, variable.name = "variable")
      dfSensor <- separate(data = dfSensor, col = variable, into = c("Sensor", "Period", "Measure"), sep = "\\.")
      dfSensor$SensorValue <- dfSensor$value
      dfSensor$value <- NULL
      
      listRasterVars <- lapply(paste0(period, Quantity), function(p) sprintf("Raster.%s.Solar.%s", p, radiation))
  
      dfRaster <- melt(df, id.vars = c("Point.loc_ID", "Point.Site"), measure.vars = listRasterVars, variable.name = "variable")
      dfRaster <- separate(data = dfRaster, col = variable, into = c("Sensor", "Period", "Solar", "RadiationType"), sep = "\\.")
      dfRaster$RasterValue <- dfRaster$value
      dfRaster$RadiationType <- as.factor(dfRaster$RadiationType)
      dfRaster$value <- NULL
      
      foo <- merge(dfSensor, dfRaster, by=c("Point.loc_ID", "Period", "Point.Site"))
      foo$Period <- as.factor(foo$Period)
      foo$Period <- factor(foo$Period, levels = mixedsort(levels(foo$Period)))
      dfSensor <- NULL
      dfRaster <- NULL
      
      plot <- ggplot(data = foo) +
        geom_point(aes(x=RasterValue, y=SensorValue, color=Point.Site), size=1.5) +
        theme_bw() +
        scale_colour_manual(values = c("red","blue")) +
        labs(title=sprintf("%s ~ %s (each point represents one sensor location)", unique(foo$RadiationType), unique(foo$Measure)),
             x=sprintf("Raster value: %s", unique(foo$RadiationType)),
             y=sprintf("Sensor value: %s", unique(foo$Measure))) +
        facet_wrap(~Period)
      
      ggsave(file=sprintf("OUT/%s~%s (by %s).png", unique(foo$RadiationType), unique(foo$Measure), period), plot, width=12,height=8, dpi=500)

    }
  }
}
####################




