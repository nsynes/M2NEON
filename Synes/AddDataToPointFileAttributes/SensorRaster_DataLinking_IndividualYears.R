
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
source("C:/Dropbox (ASU)/M2NEON/GitHub/M2NEON/Synes/SensorDataCleaning/M2NEON_Rfunctions.R")

setwd("C:/Dropbox (ASU)/M2NEON/SensorData")

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
DaysInMonth <- rbind(data.frame(Year=2013, Month=1:12, days=monthDays(as.Date(paste0(2013,"-",1:12,"-01")))),
                     data.frame(Year=2014, Month=1:12, days=monthDays(as.Date(paste0(2014,"-",1:12,"-01")))),
                     data.frame(Year=2015, Month=1:12, days=monthDays(as.Date(paste0(2015,"-",1:12,"-01")))),
                     data.frame(Year=2016, Month=1:12, days=monthDays(as.Date(paste0(2016,"-",1:12,"-01")))))

###############################
# Sensor data file information
###############################
SensorType <- "temp5cm" # temp1m, temp2m, temp4m, temp5cm, tempmax5cm, tempmin5cm, temps

y <- 2013

sfFilePath <- sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/CleanPass2_FINAL/%s_%s_%s0101-%s1231.csv",
                          SensorType, "sf", y, y)

smFilePath <- sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/CleanPass2_FINAL/%s_%s_%s0101-%s1231.csv",
                          SensorType, "sm", y, y)
###############################



###############################
# Extract raster data to the sensor point data
###############################

SensorsShapeFile <- readOGR(dsn="C:/Dropbox (ASU)/M2NEON/DGPS/CleanedLinked",
                            layer="SJER_TEAK_Sensors",
                            pointDropZ=TRUE)

RasterDir <- "C:/Dropbox (ASU)/M2NEON/Rasters"

# For rasters that exist as one for all sites 
Rasters <- c(CHM=raster(sprintf("%s/CHM/CHM_ALL_AREAS.tif", RasterDir)),
             CoAspect=raster(sprintf("%s/CoAspect/CoAspect_1m.tif", RasterDir)),
             DEM=raster(sprintf("%s/DEM/DEM_ALL_AREAS_v2.tif", RasterDir)),
             TWI=raster(sprintf("%s/TWI/TWI_ALL_AREAS_2m.tif", RasterDir)))

for (x in c("cov","dns","p25","p75","p90","std")) {
  Rasters[[sprintf("CanopyFrac.2mLAS%s", x)]] = raster(
    sprintf("%s/LAS/outTIF/mosaics/2m/SJER_TEAK_%s.tif", RasterDir, x))
  Rasters[[sprintf("CanopyFrac.3mLAS%s", x)]] = raster(
    sprintf("%s/LAS/outTIF/mosaics/3m/SJER_TEAK_%s.tif", RasterDir, x))
  Rasters[[sprintf("CanopyFrac.5mLAS%s", x)]] = raster(
    sprintf("%s/LAS/outTIF/mosaics/5m/SJER_TEAK_%s.tif", RasterDir, x))
} 

# For rasters that exist as one for each sit
# 1 is SJER (sf), 2 is TEAK (sm)
RastersBySite <- c(Annual.CWD.sjer=raster(sprintf("%s/CLIMATE_MODELS/cwd_wy20131.tif", RasterDir)),
                   Annual.CWD.teak=raster(sprintf("%s/CLIMATE_MODELS/cwd_wy20132.tif", RasterDir)),
                   Annual.PET.sjer=raster(sprintf("%s/CLIMATE_MODELS/TEAK_pet_wy20131.tif", RasterDir)),
                   Annual.PET.teak=raster(sprintf("%s/CLIMATE_MODELS/SJER_pet_wy20131.tif", RasterDir)),
                   Annual.TMax2012.sjer=raster(sprintf("%s/CLIMATE_MODELS/TEAK_tmx2012_asc1.tif", RasterDir)),
                   Annual.TMax2012.teak=raster(sprintf("%s/CLIMATE_MODELS/SJER_tmx2012_asc1.tif", RasterDir)),
                   ARVI.sjer=raster(sprintf("%s/VEG_INDICIES/SJER/SJER_ARVI_FULL.tif", RasterDir)),
                   ARVI.teak=raster(sprintf("%s/VEG_INDICIES/TEAK/TEAK_ARVI_FULL_v1.tif", RasterDir)),
                   EVI.sjer=raster(sprintf("%s/VEG_INDICIES/SJER/SJER_EVI_FULL.tif", RasterDir)),
                   EVI.teak=raster(sprintf("%s/VEG_INDICIES/TEAK/TEAK_EVI_FULL.tif", RasterDir)),
                   NDLI.sjer=raster(sprintf("%s/VEG_INDICIES/SJER/SJER_NDLI_FULL.tif", RasterDir)),
                   NDLI.teak=raster(sprintf("%s/VEG_INDICIES/TEAK/TEAK_NDLI_FULL.tif", RasterDir)),
                   NDNI.sjer=raster(sprintf("%s/VEG_INDICIES/SJER/SJER_NDNI_FULL.tif", RasterDir)),
                   NDNI.teak=raster(sprintf("%s/VEG_INDICIES/TEAK/TEAK_NDNI_FULL.tif", RasterDir)),
                   NDVI.sjer=raster(sprintf("%s/VEG_INDICIES/SJER/SJER_NDVI_FULL.tif", RasterDir)),
                   NDVI.teak=raster(sprintf("%s/VEG_INDICIES/TEAK/TEAK_NDVI_FULL.tif", RasterDir)),
                   PRI.sjer=raster(sprintf("%s/VEG_INDICIES/SJER/SJER_PRI_FULL.tif", RasterDir)),
                   PRI.teak=raster(sprintf("%s/VEG_INDICIES/TEAK/TEAK_PRI_FULL.tif", RasterDir))
                   #CanopyFrac.BeerLambert.sjer=raster(sprintf("%s/CANOPY_FRACTION/tif/teak_5m_tscan_out_BL.tif", RasterDir)),
                   #CanopyFrac.BeerLambert.teak=raster(sprintf("%s/CANOPY_FRACTION/tif/sjer_5m_tscan_out_BL_v3.tif", RasterDir))
                   )

for (x in c("cov","dns","p25","p75","p90","std")) {
  RastersBySite[[sprintf("CanopyFrac.1mLAS%s.sjer", x)]] = raster(
    sprintf("%s/LAS/outTIF/mosaics/1m/SJER/SJER_20m_buffers_ht_%s.tif", RasterDir, x))
  RastersBySite[[sprintf("CanopyFrac.1mLAS%s.teak", x)]] = raster(
    sprintf("%s/LAS/outTIF/mosaics/1m/TEAK/TEAK_20m_buffers_ht_%s.tif", RasterDir, x))
} 

# Commented out as currently ignoring quarters
if (FALSE) {
# Quarterly solar rasters
for (q in 1:4){
  for (solartype in c("DirectDuration","DirectRadiation","SolarRadiation","DiffuseRadiation")){
    for (s in c("sjer","teak")) {
      RastersBySite[[sprintf("Quarter%s.Solar.%s.%s", q, solartype, s)]] = raster(
        sprintf("%s/QUARTERLY_SOLAR/tif/%s/%s_q%s.tif", RasterDir, s, solartype, q))
      #for (LAStype in c("cov","dns")) {
      #  RastersBySite[[sprintf("Quarter%s.Solar.CanopyFrac%s.LAS%s.%s", q, solartype, LAStype, s)]] = raster(
      #    sprintf("%s/CanopyFractionAdjustedSolar/quarterly/%s/LAS_%s_%s_q%s.tif", RasterDir, s, LAStype, solartype, q))
      #}
    }
  }
}
}

# Monthly solar rasters
for (m in 1:12){
  for (solartype in c("DirectDuration","DirectRadiation","SolarRadiation","DiffuseRadiation")){
    for (s in c("sjer","teak")) {
      RastersBySite[[sprintf("Month%s.Solar.%s.%s", m, solartype, s)]] = raster(
        sprintf("%s/MONTHLY_SOLAR/tif/%s/%s_m%s.tif", RasterDir, s, solartype, m))
      #for (LAStype in c("cov","dns")) {
      #  RastersBySite[[sprintf("Month%s.Solar.CanopyFrac%s.LAS%s.%s", m, solartype, LAStype, s)]] = raster(
      #    sprintf("%s/CanopyFractionAdjustedSolar/monthly/%s/LAS_%s_%s_m%s.tif", RasterDir, s, LAStype, solartype, m))      
      #}
    }
  }
}



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

# Get the names of the rasters that have been included
#IncludedRasters <- colnames(dfRaster[,!(names(dfRaster) %in% c("coords.x1","coords.x2","loc_ID"))])

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



###############################
# Collect sensor summary data
###############################
ColumnsToRename <- c("No","max","min","mean","sd","se","ci")

dfAllData <- rbind(GetSensorData(sfFilePath, GroupLabels=c("Month","Year")),
                   GetSensorData(smFilePath, GroupLabels=c("Month","Year")))

#dfSensorAnnual <- GetSensorSummary(dfAllData, SummaryVar="Year")
#dfSensorAnnual <- subset(RenameColumns(dfSensorAnnual, "Sensor.Annual", ColumnsToRename), Year == y)

dfSensorMonth <- GetSensorSummary(dfAllData, SummaryVar=c("Month", "Year"))
dfSensorMonth <- subset(OneRowPerSensor(dfSensorMonth, prefix="Sensor", name = "Month", ColumnsToRename), Year == y)

#dfSensorQuarter <- GetSensorSummary(dfAllData, SummaryVar="Quarter")
#dfSensorQuarter <- OneRowPerSensor(dfSensorQuarter, prefix="Sensor", name = "Quarter", ColumnsToRename)

dfAllData <- NULL

############################################
# Mean daily max and min for each month
##########################################
Daily10MinCount <- 6 * 24
DailyProportionRequired <- 0.9
MonthlyProportionRequired <- 0.5
df_DailySummary <- rbind(read.csv(sprintf("sf%s_DailySummary.csv",y)), read.csv(sprintf("sm%s_DailySummary.csv",y)))

# remove data for days below the DailyProportionRequired of sensor readings
df_DailySummary <- subset(df_DailySummary, (!(loc_ID %in% Sensors20Min) & Sensor.No >= Daily10MinCount * DailyProportionRequired) | 
                                            ((loc_ID %in% Sensors20Min) & Sensor.No >= (Daily10MinCount/2) * DailyProportionRequired))

df_DailySummary$Year <- as.POSIXlt(df_DailySummary[,1], format="%Y-%m-%d", tz = "MST")$year + 1900
df_DailySummary$Month <- as.POSIXlt(df_DailySummary[,1], format="%Y-%m-%d", tz = "MST")$mon + 1

foomax <- summarySE(df_DailySummary, measurevar="Sensor.max", groupvars=c("Month","loc_ID","GardenID","WithinGardenID"))

# Remove monthly data with less than the MonthlyProportionRequired of recorded days
barmax <- data.frame()
for (m in 1:12) {
  barmax <- rbind(barmax,
               subset(foomax, Month == m & No >= (MonthlyProportionRequired * subset(DaysInMonth, Year == y & Month == m)$days)))
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

foomin <- summarySE(df_DailySummary, measurevar="Sensor.min", groupvars=c("Month","loc_ID","GardenID","WithinGardenID"))

# Remove monthly data with less than the MonthlyProportionRequired of recorded days
barmin <- data.frame()
for (m in 1:12) {
  barmin <- rbind(barmin,
                  subset(foomin, Month == m & No >= (MonthlyProportionRequired * subset(DaysInMonth, Year == y & Month == m)$days)))
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

bar <- merge(barmax, barmin, by=c("Month","loc_ID","GardenID","WithinGardenID","Year"))
bar$GardenID <- NULL
bar$WithinGardenID <- NULL
dfSensorMeanDailyMaxMin <- subset(OneRowPerSensor(bar, prefix="Sensor", name = "Month", c("MeanDailyMax", "MeanDailyMin")), Year == y)
dfSensorMeanDailyMaxMin$Year <- NULL
dfSensorMeanDailyMaxMin$Point.loc_ID <- dfSensorMeanDailyMaxMin$loc_ID
dfSensorMeanDailyMaxMin$loc_ID <- NULL
#################################################
#################################################


df_DailySummary <- rbind(read.csv(sprintf("sf%s_DailySummary.csv",y)), read.csv(sprintf("sm%s_DailySummary.csv",y)))
# Cumulative degree days
listCDDy <- list()
listCDDm <- list()
listCDDq <- list()
i<-1
for (CDD_base in c(5,10,15,20,25,30)) {
  cat(paste(CDD_base))
  #dfCDDy <- GetCDD(df_DailySummary, dailymean = "Sensor.mean", base = CDD_base, interval = "Year", SubsetYear=y)
  #dfCDDy <- RenameColumns(dfCDDy, "Sensor.Annual", sprintf("CDD%s", CDD_base))
  dfCDDm <- GetCDD(df_DailySummary, dailymean = "Sensor.mean", base = CDD_base, interval = "Month", SubsetYear=y)
  dfCDDm <- OneRowPerSensor(dfCDDm, prefix="Sensor", name = "Month", sprintf("CDD%s", CDD_base))
  #dfCDDq <- GetCDD(df_DailySummary, dailymean = "Sensor.mean", base = CDD_base, interval = "Quarter", QStartYear = 2013, Quarters=c(33,126,219,312,37))
  #dfCDDq <- OneRowPerSensor(dfCDDq, prefix="Sensor", name = "Quarter", sprintf("CDD%s", CDD_base))
  listCDDy[[i]] <- dfCDDy
  listCDDm[[i]] <- dfCDDm
  #listCDDq[[i]] <- dfCDDq
  dfCDDy <- NULL
  dfCDDm <- NULL
  #dfCDDq <- NULL
  i<-i+1
}
#dfCDDAnnual <- Reduce(function(x, y) merge(x, y, all=TRUE, by="loc_ID"), listCDDy)
dfCDDMonth <- Reduce(function(x, y) merge(x, y, all=TRUE, by="loc_ID"), listCDDm)
#dfCDDQuarter <- Reduce(function(x, y) merge(x, y, all=TRUE, by="loc_ID"), listCDDq)
df_DailySummary <- NULL
###############################



###############################
# Merge the Sensor Data with the Raster Data
###############################
#colnames(dfSensorAnnual)[!(substr(colnames(dfSensorAnnual),1,6) == "Sensor")] <- sprintf("Point.%s", colnames(dfSensorAnnual))
colnames(dfSensorMonth)[!(substr(colnames(dfSensorMonth),1,6) == "Sensor")] <- sprintf("Point.%s", colnames(dfSensorMonth))
#colnames(dfSensorQuarter)[!(substr(colnames(dfSensorQuarter),1,6) == "Sensor")] <- sprintf("Point.%s", colnames(dfSensorQuarter))
#colnames(dfCDDAnnual)[!(substr(colnames(dfCDDAnnual),1,6) == "Sensor")] <- sprintf("Point.%s", colnames(dfCDDAnnual))
colnames(dfCDDMonth)[!(substr(colnames(dfCDDMonth),1,6) == "Sensor")] <- sprintf("Point.%s", colnames(dfCDDMonth))
#colnames(dfCDDQuarter)[!(substr(colnames(dfCDDQuarter),1,6) == "Sensor")] <- sprintf("Point.%s", colnames(dfCDDQuarter))

#dfSensorAnnual$Point.Year <- NULL
dfSensorMonth$Point.Year <- NULL
#dfSensorQuarter$Point.Year <- NULL
#dfSensorAnnual$Point.GardenID <- NULL
dfSensorMonth$Point.GardenID <- NULL
#dfSensorQuarter$Point.GardenID <- NULL
#dfSensorAnnual$Point.WithinGardenID <- NULL
dfSensorMonth$Point.WithinGardenID <- NULL
#dfSensorQuarter$Point.WithinGardenID <- NULL

listSensorDataframes <- list(#dfSensorAnnual,
                             dfSensorMeanDailyMaxMin,
                             dfSensorMonth,
                             #dfSensorQuarter,
                             #dfCDDAnnual,
                             dfCDDMonth)
                             #dfCDDQuarter)
dfSensorTable <- merge(dfSensorLocTable,
                       Reduce(function(x, y) merge(x, y, all=TRUE, by="Point.loc_ID"), listSensorDataframes),
                       by = "Point.loc_ID")
###############################




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





