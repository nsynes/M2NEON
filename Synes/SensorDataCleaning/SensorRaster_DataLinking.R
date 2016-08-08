
library(ggplot2)
library(reshape2)
library(gridExtra)
library(GGally)
library(raster)
library(rgdal)

# Get the functions which I have stored in a separate file
setwd("C:/Dropbox (ASU)/M2NEON/SensorData")
source("M2NEON_Rfunctions.R")


###############################
# Sensor data file information
###############################
y <- 2013
SensorType <- "temp5cm" # temp1m, temp2m, temp4m, temp5cm, tempmax5cm, tempmin5cm, temps

Site <- "sf"
sfFilePath <- sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/Cleaned/%s/level3/%s/%s",
                      toupper(Site),
                      SensorType,
                      sprintf("%s_%s_%s0101-%s1231.csv", SensorType, Site, y, y))

# NOTE sm data not yet cleaned
Site <- "sm"
smFilePath <- sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/FromFTP/%s/level3/%s/%s",
                      toupper(Site),
                      SensorType,
                      sprintf("%s_%s_%s0101-%s1231.csv", SensorType, Site, y, y))

dfSensorAnnual <- rbind(GetSensorSummary(sfFilePath, "Year"), GetSensorSummary(smFilePath, "Year"))

dfSensorMonthly <- rbind(GetSensorSummary(sfFilePath, c("Year","Month")), GetSensorSummary(smFilePath, c("Year","Month")))

###############################





###############################
# Extract raster data to the sensor point data
###############################

SensorsShapeFile <- readOGR(dsn="C:/Dropbox (ASU)/M2NEON/DGPS/CleanedLinked",
                            layer="SJER_TEAK_Sensors",
                            pointDropZ=TRUE)

RasterDir <- "C:/Dropbox (ASU)/M2NEON/Rasters"

Rasters <- c(CHM=raster(sprintf("%s/CHM/CHM_ALL_AREAS.tif", RasterDir)),
             CoAspect=raster(sprintf("%s/CoAspect/CoAspect_1m.tif", RasterDir)),
             DEM=raster(sprintf("%s/DEM/DEM_ALL_AREAS_v2.tif", RasterDir)),
             TWI=raster(sprintf("%s/TWI/TWI_ALL_AREAS.tif", RasterDir)))

RastersBySite <- c(Solar.1=raster(sprintf("%s/Solar/Solar_CanopyRemoved_SJER_1m.tif", RasterDir)),
                   Solar.2=raster(sprintf("%s/Solar/Solar_CanopyRemoved_TEAK_1m.tif", RasterDir)),
                   CWD.1=raster(sprintf("%s/Climate_Models/cwd_wy20131.tif", RasterDir)),
                   CWD.2=raster(sprintf("%s/Climate_Models/cwd_wy20132.tif", RasterDir)),
                   PET.1=raster(sprintf("%s/Climate_Models/TEAK_pet_wy20131.tif", RasterDir)),
                   PET.2=raster(sprintf("%s/Climate_Models/SJER_pet_wy20131.tif", RasterDir)),
                   TMax.1=raster(sprintf("%s/Climate_Models/TEAK_tmx2012_asc1.tif", RasterDir)),
                   TMax.2=raster(sprintf("%s/Climate_Models/SJER_tmx2012_asc1.tif", RasterDir)))

dfBySite <- data.frame(coordinates(SensorsShapeFile),
                       loc_ID=SensorsShapeFile$loc_ID,
                       lapply(RastersBySite, function(raster) {extract(raster, coordinates(SensorsShapeFile)[,1:2])}))

dfBySite <- MergeBySite(dfBySite)

dfAcrossSites <- data.frame(coordinates(SensorsShapeFile),
                             SensorsShapeFile,
                             lapply(Rasters, function(raster) {extract(raster, coordinates(SensorsShapeFile)[,1:2])}))

dfRaster <- merge(dfBySite, dfAcrossSites, by=c("coords.x1", "coords.x2", "loc_ID"))

# Get rid of redundant data
SensorsShapeFile <- NULL
Rasters <- NULL
RastersBySite <- NULL
dfBySite <- NULL
dfAcrossSites <- NULL
###############################




###############################
# Merge the Sensor Data with the Raster Data
###############################
# Annual
dfAnnual <- merge(dfSensorAnnual, dfRaster, by = c("loc_ID"))

dfAnnualLong <- melt(dfAnnual, measure.vars = c("Sensor.max","Sensor.min","Sensor.mean","Sensor.sd",
                                     "Sensor.se","Sensor.ci","CWD","PET","TMax",
                                     "CHM","CoAspect","DEM","TWI"),
                                     variable.name = "Group")

# Monthly
dfMonthly <- merge(dfSensorMonthly, dfRaster, by = c("loc_ID"))

dfMonthlyLong <- melt(dfMonthly, measure.vars = c("Sensor.max","Sensor.min","Sensor.mean","Sensor.sd",
                                                  "Sensor.se","Sensor.ci","CWD","PET","TMax",
                                                  "CHM","CoAspect","DEM","TWI"),
                      variable.name = "Group")
###############################



dfLong <- dfAnnualLong
###############################
# Plot of all main sensor and raster data against the solar data
# utilising the long format data frame for facet_wrap
###############################
ggplot(data=dfLong) +
  geom_point(aes(x=Solar, y=value, color=Site)) +
  facet_wrap(~Group, scales="free")
###############################



df <- dfMonthly
#######################
# Solar compared to sensor data
#######################
pmin <- ggplot(data = df) +
  geom_point(aes(x=Solar, y=Sensor.min, color=Site)) +
  facet_wrap(~Month)

pmean <- ggplot(data = df) +
  geom_point(aes(x=Solar, y=Sensor.mean, color=Site)) +
  facet_wrap(~Month)

pmax <- ggplot(data = df) +
  geom_point(aes(x=Solar, y=Sensor.max, color=Site)) +
  facet_wrap(~Month)

psd <- ggplot(data = df) +
  geom_point(aes(x=Solar, y=Sensor.sd, color=Site)) +
  facet_wrap(~Month)

grid.arrange(pmin,pmean,pmax,psd)
##########################


df <- dfAnnual
#######################
# Solar compared to sensor data
#######################
pmin <- ggplot(data = df) +
  geom_point(aes(x=Solar, y=Sensor.min, color=Site))

pmean <- ggplot(data = df) +
  geom_point(aes(x=Solar, y=Sensor.mean, color=Site))

pmax <- ggplot(data = df) +
  geom_point(aes(x=Solar, y=Sensor.max, color=Site))

psd <- ggplot(data = df) +
  geom_point(aes(x=Solar, y=Sensor.sd, color=Site))

grid.arrange(pmin,pmean,pmax,psd)
##########################


##########################
# Solar compared to other raster data
##########################
pcwd <- ggplot(data = df) +
  geom_point(aes(x=Solar, y=CWD, color=Site))

ptwi <- ggplot(data = df) +
  geom_point(aes(x=Solar, y=TWI, color=Site))

pdem <- ggplot(data = df) +
  geom_point(aes(x=Solar, y=DEM, color=Site))

ppet <- ggplot(data = df) +
  geom_point(aes(x=Solar, y=PET, color=Site))

grid.arrange(pcwd, ptwi, pdem, ppet)


ggpairs(df, columns=c("Solar","DEM","TWI","CWD","PET","TMax","Sensor.max","Sensor.min","Sensor.mean","Sensor.sd","CoAspect"))










##################################
##################################
# Perhaps this can be used later...
# Not enough data yet for these polynomial models?
#SJER.mod1 = lm(SolarDSM ~ mean, data = dfg2)
SJER.mod2 = lm(SolarDSM ~ mean + I(mean^2), data = dfg2)
#SJER.mod3 = lm(SolarDSM ~ mean + I(mean^2) + I(mean^3), data = dfg2)
#SJER.mod4 = lm(SolarDSM ~ mean + I(mean^2) + I(mean^3) + I(mean^4), data = dfg2)
#AIC(SJER.mod1, SJER.mod2, SJER.mod3, SJER.mod4)
summary(SJER.mod2)






###############################
# HOBO Issues
###############################
#dfIssues <- read.csv("C:/Dropbox (ASU)/M2NEON/SF_SM_HoboIssues2013.csv")
#df4 <- merge(df3, dfIssues, by=c("loc_ID"))
###############################


###############################
# Facet plotting all sensors over a time period
###############################
#ggplot(data=df3) +
#  geom_ribbon(aes(x = Month, ymax = max, ymin = min), alpha = 1, fill = "grey") +
#  geom_line(aes(x = Month, y = mean, group = loc_ID, colour = "green")) +
#  geom_line(aes(x = Month, y = max, group = loc_ID, colour = "red")) +
#  geom_line(aes(x = Month, y = min, group = loc_ID, colour = "blue")) +
#  facet_grid(GardenID~WithinGardenID)
###############################


###############################
# Plot all sensors averaged over entire year
###############################
#ggplot(data=df2) +
#  geom_boxplot(aes(x = factor(WithinGardenID), y = value, fill = factor(WithinGardenID))) +
#  facet_wrap(~GardenID)
#ggsave(file=sprintf("C:/Dropbox (ASU)/M2NEON/Year%s.png", y), width=12, height=8, dpi=300)
###############################


###############################
# Plotting sensor by sensor
###############################
#for (sensor in SensorsOnly) {
#  ggplot(data=subset(df3, loc_ID == sensor)) +
#    geom_ribbon(aes(x = Month, ymax = max, ymin = min), alpha = 1, fill = "grey") +
#    geom_line(aes(x = Month, y = mean, group = 1), colour = "green") +
#    geom_line(aes(x = Month, y = max, group = 1), colour = "red") +
#    geom_line(aes(x = Month, y = min, group = 1), colour = "blue")
#  ggsave(file=sprintf("C:/Dropbox (ASU)/M2NEON/%s_Month%s.png", sensor, y), width=10, height=5, dpi=300)
#}
###############################



