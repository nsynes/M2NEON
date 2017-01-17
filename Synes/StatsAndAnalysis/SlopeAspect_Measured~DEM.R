
library(ggplot2)


df <- read.csv("C:/Dropbox (ASU)/M2NEON/Original GPS/logger_attributes.csv")
df[df$slope_deg == "?",]$slope_deg = NA
df$slope_deg <- as.numeric(df$slope_deg)
df <- subset(df, prime_site %in% c("Sierra Foothill (San Joaquin Experimental Range)", "Sierra Montane (Teakettle Experimental Forest)"))
df$GardenID <- ifelse(substr(df$loc_ID, 7,7) != "7", sprintf("Garden %s", substr(df$loc_ID, 7,7)), "Landscape")
dfMeasuredSA <- df[c("loc_ID","aspect","slope_deg", "prime_site", "GardenID")]
dfMeasuredSA <- unique(dfMeasuredSA)
dfMeasuredSA$Aspect <- dfMeasuredSA$aspect
dfMeasuredSA$aspect <- NULL
dfMeasuredSA$Slope <- dfMeasuredSA$slope_deg
dfMeasuredSA$slope_deg <- NULL
dfMeasuredSA$Site <- dfMeasuredSA$prime_site
dfMeasuredSA$prime_site <- NULL
dfMeasuredSA$Measurement <- "On the ground"
df<-NULL



df <- read.csv("C:/Dropbox (ASU)/M2NEON/SensorData/Merged_RasterAndSensorData_2013.csv")
dfDEMSA <- df[c("Point.loc_ID","Raster.AspectDegree","Raster.SlopeDegree", "Point.Site")]
dfDEMSA$GardenID <- ifelse(substr(dfDEMSA$Point.loc_ID, 7,7) != "7", sprintf("Garden %s", substr(dfDEMSA$Point.loc_ID, 7,7)), "Landscape")
dfDEMSA$loc_ID <- dfDEMSA$Point.loc_ID
dfDEMSA$Point.loc_ID <- NULL
dfDEMSA <- unique(dfDEMSA)
dfDEMSA$Aspect <- dfDEMSA$Raster.AspectDegree
dfDEMSA$Raster.AspectDegree <- NULL
dfDEMSA$Slope <- dfDEMSA$Raster.SlopeDegree
dfDEMSA$Raster.SlopeDegree <- NULL
dfDEMSA$Site <- dfDEMSA$Point.Site
dfDEMSA$Point.Site <- NULL
dfDEMSA$Site <- ifelse(dfDEMSA$Site == "SF", "Sierra Foothill (San Joaquin Experimental Range)", "Sierra Montane(Teakettle Experimental Forest)")
dfDEMSA$Site <- as.factor(dfDEMSA$Site)
dfDEMSA$Measurement <- "DEM"
df<-NULL

dfMeasuredSA <- dfMeasuredSA[dfMeasuredSA$loc_ID %in% unique(dfDEMSA$loc_ID),]

df <- rbind(dfDEMSA, dfMeasuredSA)
df$Site <- revalue(df$Site, c("Sierra Foothill (San Joaquin Experimental Range)"  = "Sierra Foothill",
                              "Sierra Montane (Teakettle Experimental Forest)"  = "Sierra Montane"))


ggplot() +
  geom_point(data=df, aes(x=Aspect, y = Slope, color=Measurement), size=3) +
  facet_grid(Site~GardenID) +
  theme_bw() +
  theme(text = element_text(size=20),
        legend.position="top")






