library(reshape2)
library(ggplot2)
library(gridExtra)

setwd("C:/Dropbox (ASU)/M2NEON/SensorData")



df <- read.csv("Merged_RasterAndSensorData_2013.csv")

dfRasterWide <- df[(substr(colnames(df),1,7) == "Raster." | substr(colnames(df),1,6) == "Point.")]

dfRaster <- melt(dfRasterWide, id.vars=c("Point.loc_ID", "Point.coords.x1", "Point.coords.x2", "Point.Site"))
dfRaster$value <- as.numeric(dfRaster$value)
dfRaster$Point.GardenNo <- as.numeric(substr(dfRaster$Point.loc_ID, 7,7))
dfRaster$Point.Garden <- ifelse(dfRaster$Point.GardenNo < 7, sprintf("Garden %s", dfRaster$Point.GardenNo), "Landscapes")
dfRaster$Point.Sensor <- as.numeric(substr(dfRaster$Point.loc_ID, 8,9))

for (i in 2:7) {
  dfRaster$Point.Sensor <- ifelse(dfRaster$Point.GardenNo == i, dfRaster$Point.Sensor + (i - 1) * 21, dfRaster$Point.Sensor)
}

dfRaster$variable <- as.character(dfRaster$variable)
if (TRUE) {
dfVars <- subset(dfRaster, variable %in% c("Raster.DEM.2m",
                                          "Raster.SinSlopeCosAspect.2m",
                                          "Raster.Curvature.Plan.100m",
                                          "Raster.Curvature.Prof.100m",
                                          "Raster.Canopy.p90.2m",
                                          "Raster.TWI.30m",
                                          "Raster.Shrub.2m"))
}
if (FALSE) {
dfVars <- subset(dfRaster, substr(variable, 1,9) == "Raster.HM")
dfVars$variable <- factor(dfVars$variable, levels=paste0("Raster.HM", 1:24, ".DSMSolarRadiation"))
}
dfVars$variable <- as.factor(dfVars$variable)
dfRaster$variable <- as.factor(dfRaster$variable)

dfSub <- subset(dfVars, Point.Site == "SM")

p1 <- ggplot(data=dfSub) +
  geom_point(aes(x=Point.Sensor, y=value, color=factor(Point.Garden))) +
  facet_wrap(~variable, scales="free") + #, ncol=1) +
  labs(title=sprintf("Site: %s", unique(dfSub$Point.Site)[1]), x = "Sensor", color = "Gardens (and landscapes)") +
  scale_color_brewer(palette="Dark2") +
  theme_bw()

dfSub <- subset(dfVars, Point.Site == "SF")

p2 <- ggplot(data=dfSub) +
  geom_point(aes(x=Point.Sensor, y=value, color=factor(Point.Garden))) +
  facet_wrap(~variable, scales="free") + #, ncol=1) +
  labs(title=sprintf("Site: %s", unique(dfSub$Point.Site)[1]), x = "Sensor", color = "Gardens (and landscapes)") +
  scale_color_brewer(palette="Dark2") +
  theme_bw()


plot <- grid.arrange(p1,p2, top=textGrob("Independent Variable Values at each sensor",gp=gpar(fontsize=20,font=3)))

ggsave(file="IndependentVariable_SensorValues.png", plot, width=20,height=12, dpi=500)







