
library(tools)
library(foreign)
library(tidyr)
library(plyr)



setwd("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/IndividualFiles")

lFiles <- list.files()

my.list <- vector('list', 4)
i <- 1

for (f in lFiles) {
  
   if (file_ext(f) == "csv") {
     my.list[[i]] <- read.csv(f)
     
     i <- i + 1
   }
}

df <- Reduce(function(x, y) merge(x, y, by="site_ID", all=TRUE), my.list)
df$Point.site_ID <- df$site_ID
df$site_ID <- NULL



pthSensorPoints <- sprintf("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Merged_RasterAndMedianSensorData_2013.csv")
dfSensors <- read.csv(pthSensorPoints)
dfSensors <- dfSensors[c("Point.loc_ID","Point.Site")]
dfSensors$Point.Garden <- substr(dfSensors$Point.loc_ID, 7,7)

dfSensors$Point.site_ID <- ifelse(dfSensors$Point.Garden < 7, substr(dfSensors$Point.loc_ID, 5,7), substr(dfSensors$Point.loc_ID, 5,9))


dfIndepVars <- merge(dfSensors, df, by="Point.site_ID")

#write.csv(dfIndepVars, "D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/SiteLevel_AllIndepVars.csv", row.names=FALSE)
dfIndepVars <- read.csv("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/SiteLevel_AllIndepVars.csv")




dfDepVars <- read.csv("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/SiteLevel_DepVars.csv")


dfOut <- merge(dfIndepVars, dfDepVars, by=c("Point.loc_ID"))
write.csv(dfOut, sprintf("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/SiteLevel_MergedVars.csv"), row.names=FALSE)














df <- read.csv("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/MicroSiteLevel_IndepVars.csv")
dfSolar <- read.csv("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/MicrositeLevel_SolarNoAtmosTrans.csv")

dfOut <- merge(df, dfSolar, by=c("Point.loc_ID"))
write.csv(dfOut, sprintf("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/MicrositeLevel_.csv"), row.names=FALSE)





