
library(plyr)
library(foreign)


# Dbf files for point based solar
dfTEAK <- read.dbf("D:/Dropbox (ASU)/M2NEON/Paper_2/DATA/VECTOR/PointBasedSolar/Solar_TEAK_DEM2013.dbf", as.is = FALSE)
dfSJER <- read.dbf("D:/Dropbox (ASU)/M2NEON/Paper_2/DATA/VECTOR/PointBasedSolar/Solar_SJER_DEM2013.dbf", as.is = FALSE)
dfNew <- rbind.fill(dfSJER, dfTEAK)

for (hm in 1:24) {
  colnames(dfNew)[which(names(dfNew) == sprintf("HM%s", hm))] <- sprintf("Raster.HM%s.DEMSolarRadiation", hm)
}
for (day in 1:365) {
  colnames(dfNew)[which(names(dfNew) == sprintf("Day%s", day))] <- sprintf("Raster.Day%s.DEMSolarRadiation", day)
}
colnames(dfNew)[which(names(dfNew) == "loc_ID")] <- "Point.loc_ID"


# Existing sensor and raster csv file
dfAll <- read.csv("D:/Dropbox (ASU)/M2NEON/SensorData/Merged_RasterAndSensorData_2013.csv")


df <- merge(dfAll, dfNew, by = "Point.loc_ID")


write.csv(df, "D:/Dropbox (ASU)/M2NEON/SensorData/Merged_RasterAndSensorData_2013NEW.csv", row.names=FALSE)



