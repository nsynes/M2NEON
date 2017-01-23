
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
library(Hmisc)



# Get the functions which I have stored in a separate file
source("D:/Dropbox (ASU)/M2NEON/GitHub/M2NEON/Synes/SensorDataCleaning/M2NEON_Rfunctions.R")

setwd("D:/Dropbox (ASU)/M2NEON")

# THESE SENSORS RECORDED AT 20 MINUTE INTERVALS:
Sensors20Min <- c("tdl_sf400_0",
                  "tdl_sf500_0",
                  "tdl_sf600_0",
                  "tdl_sf707_0",
                  "tdl_sf708_0",
                  "tdl_sf709_0",
                  "tdl_sf710_0",
                  "tdl_sf711_0",
                  "tdl_sf712_0",
                  "tdl_sm100_0",
                  "tdl_sm300_0")

y<-2013
DaysInMonth <- data.frame(Year=y, Month=1:12, days=monthDays(as.Date(paste0(y,"-",1:12,"-01"))))


###############################
# Extract raster data to the sensor point data
###############################

SensorsShapeFile <- readOGR(dsn="DGPS/CleanedLinked",
                            layer="SJER_TEAK_Sensors",
                            pointDropZ=TRUE)

RasterDir <- "Paper_2/DATA/RASTER"
# For rasters that exist as one for all sites 
Rasters <- c(DEM.2m=raster(sprintf("%s/DEM/DEM_2m.tif", RasterDir)),
             SinSlopeCosAspect.2m=raster(sprintf("%s/SinSlopeCosAspect/SinSlopeCosAspect_2m.tif", RasterDir)),
             Curvature.Plan.30m=raster(sprintf("%s/Curvature/CurvCalcOut/DEM_ALL_AREAS_v2_rs_30m_plan.tif", RasterDir)),
             Curvature.Prof.30m=raster(sprintf("%s/Curvature/CurvCalcOut/DEM_ALL_AREAS_v2_rs_30m_pro.tif", RasterDir)),
             Curvature.Plan.100m=raster(sprintf("%s/Curvature/CurvCalcOut/DEM_ALL_AREAS_v2_rs_100m_plan.tif", RasterDir)),
             Curvature.Prof.100m=raster(sprintf("%s/Curvature/CurvCalcOut/DEM_ALL_AREAS_v2_rs_100m_pro.tif", RasterDir)),
             TWI.2m=raster(sprintf("%s/TWI/TWI_ALL_AREAS_2m.tif", RasterDir)),
             TWI.30m=raster(sprintf("%s/TWI/TWI_ALL_AREAS_30m.tif", RasterDir)),
             WBI.2m=raster(sprintf("%s/WATER_INDICIES/WBI_2m.tif", RasterDir)))

VegRasters <- c(Canopy.p90.1m=raster(sprintf("%s/LAS/p90_1m.tif", RasterDir)))
                #Canopy.p75.1m=raster(sprintf("%s/LAS/p75_1m.tif", RasterDir)),
                #Canopy.p90.2m=raster(sprintf("%s/LAS/p90_2m.tif", RasterDir)),
                #Canopy.p75.2m=raster(sprintf("%s/LAS/p75_2m.tif", RasterDir)),
                #Canopy.p90.5m=raster(sprintf("%s/LAS/p90_5m.tif", RasterDir)),
                #Canopy.p75.5m=raster(sprintf("%s/LAS/p75_5m.tif", RasterDir)),
                #Shrub.1m=raster(sprintf("%s/SHRUB_LAYER/Shrub_1m.tif", RasterDir)),
                #Shrub.2m=raster(sprintf("%s/SHRUB_LAYER/Shrub_2m.tif", RasterDir)),
                #Shrub.5m=raster(sprintf("%s/SHRUB_LAYER/Shrub_5m.tif", RasterDir)))
             

# For rasters that exist as one for each site
# 1 is SJER (sf), 2 is TEAK (sm)
RastersBySite <- list()


# BiWeekly solar rasters
for (hm in 1:24){
  for (solartype in c("SolarRadiation")){
    for (s in c("SJER","TEAK")) {
      RastersBySite[[sprintf("HM%s%s.%s", hm, solartype, s)]] = raster(
        sprintf("%s/DSM_BiWeeklySolar/%s/%s_hm%s.tif", RasterDir, s, solartype, hm))
    }
  }
}
#############################
# Test for grabbing values from south of each point
dfVegBySite <- data.frame(coordinates(SensorsShapeFile),
                          loc_ID=SensorsShapeFile$loc_ID,
                          lapply(VegRasters, function(raster) {extract(raster,
                                                                       coordinates(SensorsShapeFile)[,1:2],
                                                                       small = TRUE)}))
###############################


dfBySite <- data.frame(coordinates(SensorsShapeFile),
                       loc_ID=SensorsShapeFile$loc_ID,
                       lapply(RastersBySite, function(raster) {extract(raster, coordinates(SensorsShapeFile)[,1:2])}))

dfBySite <- MergeBySite(dfBySite)
dfBySite <- RenameColumns(dfBySite, "Raster", colnames(dfBySite[ , !(names(dfBySite) %in% c("coords.x1","coords.x2","loc_ID"))]))

dfAcrossSites <- data.frame(coordinates(SensorsShapeFile),
                             loc_ID=SensorsShapeFile$loc_ID,
                             lapply(Rasters, function(raster) {extract(raster, coordinates(SensorsShapeFile)[,1:2])}))
dfAcrossSites <- RenameColumns(dfAcrossSites, "Raster", colnames(dfAcrossSites[ , !(names(dfAcrossSites) %in% c("coords.x1","coords.x2","loc_ID"))]))


dfRaster <- merge(dfBySite, dfAcrossSites, by=c("coords.x1", "coords.x2", "loc_ID"))


# Merge the raster data with the sensor location data
dfSensorLocTable <- merge(dfRaster,SensorsShapeFile, by=c("coords.x1", "coords.x2", "loc_ID"))
#Remove unnecessary variables
dfSensorLocTable$Comment <- NULL
dfSensorLocTable$Vert_Prec <- NULL
dfSensorLocTable$GNSS_Heigh <- NULL
dfSensorLocTable$Std_Dev <- NULL
dfSensorLocTable$logger_SN <- NULL
dfSensorLocTable$Notes <- NULL
# Label the non raster columns as point
colnames(dfSensorLocTable)[!(substr(colnames(dfSensorLocTable),1,6) == "Raster")] =
  sprintf("Point.%s", colnames(dfSensorLocTable)[!(substr(colnames(dfSensorLocTable),1,6) == "Raster")])


# Get rid of redundant data
SensorsShapeFile <- NULL
Rasters <- NULL
RastersBySite <- NULL
dfBySite <- NULL
dfAcrossSites <- NULL
dfRaster <- NULL
###############################


############################################
# Mean daily max and min for each month
##########################################
Daily10MinCount <- 6 * 24
DailyProportionRequired <- 0.9
HalfMonthlyProportionRequired <- 0.5
y <- 2013
df_DailySummary <- rbind(read.csv(sprintf("SensorData/sf%s_DailySummary.csv",y)), read.csv(sprintf("SensorData/sm%s_DailySummary.csv",y)))


# remove data for days below the DailyProportionRequired of sensor readings
df_DailySummary <- subset(df_DailySummary, (!(loc_ID %in% Sensors20Min) & Sensor.No >= Daily10MinCount * DailyProportionRequired) | 
                                            ((loc_ID %in% Sensors20Min) & Sensor.No >= (Daily10MinCount/2) * DailyProportionRequired))
df_DailySummary$Sensor.sd <- NULL
df_DailySummary$Sensor.se <- NULL
df_DailySummary$Sensor.ci <- NULL

df_DailySummary$Sensor.range <- df_DailySummary$Sensor.max - df_DailySummary$Sensor.min

df_DailySummary$Year <- as.POSIXlt(df_DailySummary[,1], format="%Y-%m-%d", tz = "MST")$year + 1900
df_DailySummary$Month <- as.POSIXlt(df_DailySummary[,1], format="%Y-%m-%d", tz = "MST")$mon + 1
df_DailySummary$Day <- as.POSIXlt(df_DailySummary[,1], format="%Y-%m-%d", tz = "MST")$yday + 1

# Manual definitions of bi-monthly time periods
df_DailySummary$HM <- NA
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 1 & df_DailySummary$Day <= 15, 1, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 16 & df_DailySummary$Day <= 31, 2, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 32 & df_DailySummary$Day <= 45, 3, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 46 & df_DailySummary$Day <= 59, 4, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 60 & df_DailySummary$Day <= 75, 5, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 76 & df_DailySummary$Day <= 90, 6, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 91 & df_DailySummary$Day <= 105, 7, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 106 & df_DailySummary$Day <= 120, 8, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 121 & df_DailySummary$Day <= 136, 9, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 137 & df_DailySummary$Day <= 151, 10, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 152 & df_DailySummary$Day <= 166, 11, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 167 & df_DailySummary$Day <= 181, 12, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 182 & df_DailySummary$Day <= 197, 13, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 198 & df_DailySummary$Day <= 212, 14, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 213 & df_DailySummary$Day <= 227, 15, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 228 & df_DailySummary$Day <= 242, 16, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 243 & df_DailySummary$Day <= 257, 17, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 258 & df_DailySummary$Day <= 272, 18, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 273 & df_DailySummary$Day <= 288, 19, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 289 & df_DailySummary$Day <= 303, 20, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 304 & df_DailySummary$Day <= 318, 21, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 319 & df_DailySummary$Day <= 333, 22, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 334 & df_DailySummary$Day <= 349, 24, df_DailySummary$HM)
df_DailySummary$HM <- ifelse(df_DailySummary$Day >= 350 & df_DailySummary$Day <= 365, 24, df_DailySummary$HM)

foomax <- summarySE(df_DailySummary, measurevar="Sensor.max", groupvars=c("HM","loc_ID","GardenID","WithinGardenID"))

# Remove halfmonthly data with less than the MonthlyProportionRequired of recorded days
barmax <- data.frame()
for (hm in 1:24) {
  barmax <- rbind(barmax,
               subset(foomax, HM == hm & No >= (HalfMonthlyProportionRequired * (subset(DaysInMonth, Year == y & Month == ceiling(hm/2))$days / 2))))
}
barmax$Year <- y
barmax$No <- NULL
barmax$max <- NULL
barmax$min <- NULL
barmax$sd <- NULL
barmax$se <- NULL
barmax$ci <- NULL
barmax$MeanDailyMax <- barmax$mean
barmax$mean <- NULL

foomin <- summarySE(df_DailySummary, measurevar="Sensor.min", groupvars=c("HM","loc_ID","GardenID","WithinGardenID"))

# Remove monthly data with less than the HalfMonthlyProportionRequired of recorded days
barmin <- data.frame()
for (hm in 1:24) {
  barmin <- rbind(barmin,
                  subset(foomin, HM == hm & No >= (HalfMonthlyProportionRequired * (subset(DaysInMonth, Year == y & Month == ceiling(hm/2))$days / 2))))
}
barmin$Year <- y
barmin$No <- NULL
barmin$max <- NULL
barmin$min <- NULL
barmin$sd <- NULL
barmin$se <- NULL
barmin$ci <- NULL
barmin$MeanDailyMin <- barmin$mean
barmin$mean <- NULL

foomean <- summarySE(df_DailySummary, measurevar="Sensor.mean", groupvars=c("HM","loc_ID","GardenID","WithinGardenID"))

# Remove monthly data with less than the HalfMonthlyProportionRequired of recorded days
barmean <- data.frame()
for (hm in 1:24) {
  barmean <- rbind(barmean,
                  subset(foomean, HM == hm & No >= (HalfMonthlyProportionRequired * (subset(DaysInMonth, Year == y & Month == ceiling(hm/2))$days / 2))))
}
barmean$Year <- y
barmean$No <- NULL
barmean$max <- NULL
barmean$min <- NULL
barmean$sd <- NULL
barmean$se <- NULL
barmean$ci <- NULL
barmean$Mean <- barmean$mean
barmean$mean <- NULL

foorange <- summarySE(df_DailySummary, measurevar="Sensor.range", groupvars=c("HM","loc_ID","GardenID","WithinGardenID"))

# Remove monthly data with less than the HalfMonthlyProportionRequired of recorded days
barrange <- data.frame()
for (hm in 1:24) {
  barrange <- rbind(barrange,
                   subset(foorange, HM == hm & No >= (HalfMonthlyProportionRequired * (subset(DaysInMonth, Year == y & Month == ceiling(hm/2))$days / 2))))
}
barrange$Year <- y
barrange$No <- NULL
barrange$max <- NULL
barrange$min <- NULL
barrange$sd <- NULL
barrange$se <- NULL
barrange$ci <- NULL
barrange$MeanDiurnalRange <- barrange$mean
barrange$mean <- NULL

bar <- merge(barmax, barmin, by=c("HM","loc_ID","GardenID","WithinGardenID","Year"))
bar <- merge(bar, barmean, by=c("HM","loc_ID","GardenID","WithinGardenID","Year"))
bar <- merge(bar, barrange, by=c("HM","loc_ID","GardenID","WithinGardenID","Year"))
bar$GardenID <- NULL
bar$WithinGardenID <- NULL
dfMaxMinhm <- subset(OneRowPerSensor(bar, prefix="Sensor", name = "HM", c("MeanDailyMax", "MeanDailyMin", "Mean", "MeanDiurnalRange")), Year == y)
dfMaxMinhm$Year <- NULL
dfMaxMinhm$Point.loc_ID <- dfMaxMinhm$loc_ID
dfMaxMinhm$loc_ID <- NULL

foomax <- NULL
foomean <- NULL
foomin <- NULL
foorange <- NULL
#################################################
#################################################


listCDDhm <- list()
i<-1
for (CDD_base in c(5,10)) {
  cat(paste(CDD_base), "\n")
  
  dfCDDhm <- GetCDD(df_DailySummary, dailymean = "Sensor.mean", base = CDD_base, interval = "HM", SubsetYear=y)
  dfCDDhm <- OneRowPerSensor(dfCDDhm, prefix="Sensor", name = "HM", sprintf("CDD%s", CDD_base))
  
  listCDDhm[[i]] <- dfCDDhm
  
  dfCDDhm <- NULL
  
  i<-i+1
}

dfCDDhm <- Reduce(function(x, y) merge(x, y, all=TRUE, by="loc_ID"), listCDDhm)
dfCDDhm$Point.loc_ID <- dfCDDhm$loc_ID
dfCDDhm$loc_ID <- NULL
###############################



###############################
# Merge the Sensor Data with the Raster Data
###############################
listSensorDataframes <- list(dfCDDhm,
                             dfMaxMinhm)
dfSensorTable <- merge(dfSensorLocTable,
                       Reduce(function(x, y) merge(x, y, all=TRUE, by="Point.loc_ID"), listSensorDataframes),
                       by = "Point.loc_ID")
###############################



write.csv(dfSensorTable, sprintf("Merged_RasterAndSensorData_%s.csv",y), row.names=FALSE)
#dfSensorTable <- read.csv("Merged_RasterAndSensorData.csv")






if (FALSE) {
  
  ###############################
  # Create a long table format
  # and create group and type columns to classify each variable
  ###############################
  dfSensorTableLong <- melt(dfSensorTable,
                                      measure.vars = colnames(dfSensorTable[ , !(names(dfSensorTable) %in% "loc_ID")]),
                                      variable.name = "variable")
  
  dfSensorTableLong<-separate(data = dfSensorTableLong, col = variable, into = c("group", "type","foo"), sep = "\\.")
  dfSensorTableLong$type <- ifelse(!is.na(dfSensorTableLong$foo),
                                   sprintf("%s.%s",dfSensorTableLong$type, dfSensorTableLong$foo),
                                   dfSensorTableLong$type)
  dfSensorTableLong$foo <- NULL
  ###############################
  
  
  
  ###############################
  # Plot of all main sensor and raster data against the solar data
  # utilising the long format data frame for facet_wrap
  ###############################
  listVars <- list()
  for (month in paste0("Quarter",1:4)) {
    for (cdd in paste0("CDD",c(5,10,15,20,25,30))) {
      listVars <- append(sprintf("%s.%s", month, cdd), listVars)
    }
  }
  foo <- melt(dfSensorTable, id.vars = c("loc_ID", "Raster.Solar", "Site"),
              measure.vars = listVars,
                             variable.name = "variable")
  foo <- separate(data = foo, col = variable, into = c("period", "cdd"), sep = "\\.")
  foo$cdd <- factor(foo$cdd, levels = c("CDD5",
                                        "CDD10",
                                        "CDD15",
                                        "CDD20",
                                        "CDD25",
                                        "CDD30"))
  
  plot <- ggplot(data=foo) +
    geom_point(aes(x=Raster.Solar, y=value, color=Site), size=0.5) +
    theme_bw() +
    scale_colour_manual(values = c("red","blue")) +
    labs(title="Annual solar radiation ~ cumulative degree days (from sensor data)",y="Sensor.CDD") +
    facet_grid(period~cdd)
  ggsave(file="Radiation~QuarterCDD.png", plot, width=12,height=8, dpi=500)
  
  ###############
  ###############
  
  listVars <- list()
  for (month in paste0("Month",1:12)) {
    for (cdd in paste0("CDD",c(5,10,15,20,25,30))) {
      listVars <- append(sprintf("%s.%s", month, cdd), listVars)
    }
  }
  foo <- melt(dfSensorTable, id.vars = c("loc_ID", "Raster.Solar", "Site"),
              measure.vars = listVars,
              variable.name = "variable")
  foo <- separate(data = foo, col = variable, into = c("period", "cdd"), sep = "\\.")
  foo$cdd <- factor(foo$cdd, levels = c("CDD5",
                                        "CDD10",
                                        "CDD15",
                                        "CDD20",
                                        "CDD25",
                                        "CDD30"))
  foo$period <- factor(foo$period, levels = c("Month1",
                                              "Month2",
                                              "Month3",
                                              "Month4",
                                              "Month5",
                                              "Month6",
                                              "Month7",
                                              "Month8",
                                              "Month9",
                                              "Month10",
                                              "Month11",
                                              "Month12"))
  
  plot <- ggplot(data=foo) +
    geom_point(aes(x=Raster.Solar, y=value, color=Site), size=0.5) +
    theme_bw() +
    scale_colour_manual(values = c("red","blue")) +
    labs(title="Annual solar radiation ~ cumulative degree days (from sensor data)",y="Sensor.CDD") +
    facet_grid(period~cdd)
  ggsave(file="Radiation~MonthCDD.png", plot, width=12,height=8, dpi=500)
  ###############################
  
  ggpairs(df, columns=c("Solar","DEM","TWI","CWD","PET","TMax","Sensor.max","Sensor.min","Sensor.mean","Sensor.sd","CoAspect"))
  
  
  
}





