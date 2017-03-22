
library(plyr)
library(foreign)


dirSolar <- "D:/Dropbox (ASU)/M2NEON/Paper_2/DATA/SolarDays"
pthLocID <- "D:/Dropbox (ASU)/M2NEON/SensorData/Merged_RasterAndSensorData_2013.csv"

df <- read.csv(pthLocID)
listLocID <- df[,"Point.loc_ID"]
df <- NULL


setwd(dirSolar)



my.loclist <- vector('list', length(listLocID))
i <- 1

for (LocID in listLocID) {
  my.daylist <- vector('list', 346)
  site <- substr(LocID, 5, 6)
  if (site == "sm") {
    cat(paste(LocID, "\n"))
    for (day in c(1:346)) {
      fname <- sprintf("Solar_%s_%s_CutDSM_2013Day%s.dbf", LocID, site, day)
      dfDay <- read.dbf(fname, as.is = FALSE)
      if ("T0" %in% colnames(dfDay)) {
        dfDay[,sprintf("Day%s", day)] <- dfDay$T0
        dfDay$T0 <- NULL
      }
      my.daylist[[day]] <- dfDay
    }
    df <- do.call('cbind', my.daylist)
    my.daylist <- NULL
    df$loc_ID <- LocID
    my.loclist[[i]] <- df
    i <- i + 1
  }
}
my.loclist <- my.loclist[!sapply(my.loclist, is.null)]
dfAll <- do.call('rbind', my.loclist)


dfNew <- rbind.fill(dfSF, dfSM)
dfNew$Point.loc_ID <- dfNew$loc_ID
dfNew$loc_ID <- NULL
for (day in 1:365) {
  colnames(dfNew)[which(names(dfNew) == sprintf("Day%s", day))] <- sprintf("Raster.Day%s.DEMDSMSolarRadiation", day)
}


# Existing sensor and raster csv file
dfExisting <- read.csv("D:/Dropbox (ASU)/M2NEON/SensorData/Merged_RasterAndSensorData_2013.csv")

df <- merge(dfExisting, dfNew, by = "Point.loc_ID")


write.csv(df, "D:/Dropbox (ASU)/M2NEON/SensorData/Merged_RasterAndSensorData_2013NEW.csv", row.names=FALSE)





