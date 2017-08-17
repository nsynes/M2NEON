
library(plyr)
library(foreign)
library(rgdal)
library(sp)
library(maptools)
library(tools)


###########################################
# Rename columns in each daily solar shapefile
###########################################


Site <- "TEAK"
Scale <- "Microsite"
dirSolar <- sprintf("D:/Dropbox (ASU)/M2NEON/Paper_2/DATA/VECTOR/%sLevel_PointBasedSolar/Solar%s_DEM", Scale, Site)

if (Scale == "Microsite") {
  pthSitePoints <- sprintf("D:/Dropbox (ASU)/M2NEON/Paper_2/DATA/VECTOR/SensorPoints/%s_Point_Sensors.shp", Site)
} else if (Scale == "Site") {
  pthSitePoints <- sprintf("D:/Dropbox (ASU)/M2NEON/Paper_2/DATA/VECTOR/SensorPoints/%s_SiteLevel.shp", Site)
}


for (day in 1:365) {
  cat(paste("Day:", day, "\n"))
  fname <- sprintf("%s/Solar_%s_DEM_2013Day%s.shp", dirSolar, Site, day)
  shpDay <- readOGR(dsn=dirname(fname),
                   layer=file_path_sans_ext(basename(fname)),
                   pointDropZ=TRUE)
  shpDay <- subset(shpDay, !duplicated(shpDay@coords))
  names(shpDay)[names(shpDay) == 'T0'] <- sprintf("Day%s", day)
  
  writeOGR(shpDay, dirname(fname), file_path_sans_ext(basename(fname)), driver="ESRI Shapefile", overwrite_layer=TRUE)
  
}

################################################
################################################


################################################
# Merge dbf solar files (by loc_ID or site_ID)
################################################


Site <- "SJER"
Scale <- "Site"
dirSolar <- sprintf("D:/Dropbox (ASU)/M2NEON/Paper_2/DATA/VECTOR/%sLevel_PointBasedSolar_SAMEAtmosTrans/Solar%s_DEM", Scale, Site)


setwd(dirSolar)

my.DayList <- vector('list', 365) #346)

for (day in 1:365) { #346) {
  cat(paste("Day:", day, "\n"))
  fname <- sprintf("%s/Solar_%s_DEM_2013Day%s.dbf", dirSolar, Site, day)
  
  my.DayList[[day]] <- read.dbf(fname, as.is=FALSE)
}
df <- Reduce(function(x, y) merge(x, y, by="site_ID", all=TRUE), my.DayList)

#dfSJER <- df
#dfTEAK <- df
#df <- NULL

dfMerge <- rbind.fill(dfSJER, dfTEAK)
dfSJER<-NULL
dfTEAK<-NULL

for (day in 1:365) {
  colnames(dfSolarConst)[which(names(dfSolarConst) == sprintf("Day%s", day))] <- sprintf("Indep.Day%s.SolarRadiationConstAtmosTrans30m", day)
}

################################################
################################################

dfBothSolar <- merge(dfSolar, dfSolarConst, by="site_ID", all=TRUE)
colnames(dfBothSolar)[which(names(dfBothSolar) == "site_ID")] <- "Point.site_ID"

df <- read.csv("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/SiteLevel_IndepVars.csv")

dfOut <- merge(df, dfBothSolar, by="Point.site_ID")

write.csv(dfOut, sprintf("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/SiteLevel_All.csv"), row.names=FALSE)






