library(reshape2)
library(ggplot2)
library(gridExtra)
library(plotly)

setwd("C:/Dropbox (ASU)/M2NEON/SensorData")

##########################################
# Slope aspect polar plots
##############################################
df <- read.csv("SlopeAspectCanopy.csv")
df$Point.Garden <- as.factor(substr(df$Point.loc_ID,7,7))
df$Point.Site <- as.factor(substr(df$Point.loc_ID,5,6))
df$Point.Garden <- ifelse(df$Point.Garden %in% c("1","2","3","4","5","6"), sprintf("Garden %s", df$Point.Garden), "Landscapes")
df$Point.Garden <- as.factor(df$Point.Garden)

dfSJER <- subset(df, df$Point.Site == "sf")
dfTEAK <- subset(df, df$Point.Site == "sm")

fontdef <- list(
  size = 20,
  color = 'black')
m <- list(l = 10, r = 50, b = 50, t = 50, pad = 4)

plot_ly(dfSJER, r = ~Raster.SlopeDegree, t = ~Raster.AspectDegree, color = ~Point.Garden, type = "scatter") %>%
  layout(title = 'Site: Sierra foothills (r = Slope, t = Aspect)', font=fontdef, orientation = -90,
         radialaxis = list(ticksuffix = "°"), angularaxis = list(ticksuffix = "°", tickcolor = toRGB("darkgrey")),
         autosize = F, width = 1000, height = 800, margin=m)

plot_ly(dfTEAK, r = ~Raster.SlopeDegree, t = ~Raster.AspectDegree, color = ~Point.Garden, type = "scatter") %>%
  layout(title = 'Site: Sierra montane (r = Slope, t = Aspect)', font=fontdef, orientation = -90,
         radialaxis = list(ticksuffix = "°"), angularaxis = list(ticksuffix = "°", tickcolor = toRGB("darkgrey")),
         autosize = F, width = 1000, height = 800, margin=m)

if (FALSE) {
ggplot(df) +
  geom_point(data=df, aes(x=Raster.AspectDegree, y=Raster.SlopeDegree, color=Point.Garden)) +
  facet_wrap(~Point.Site) +
  scale_x_continuous(limits=c(0, 360), breaks=c(0,30,60,90,120,150,180,210,240,270,300,330)) +
  theme_bw() +
  coord_polar()
}

###########################################
#########################################



df <- read.csv("Merged_RasterAndSensorData_2013.csv")
df$Point.Garden <- as.factor(substr(df$Point.loc_ID,7,7))

## for scatter plots of raster var ~ raster var.
ggplot() +
  geom_point(data=df, aes(x=Raster.DEM.2m, y=Raster.Curvature.Plan.100m, color=Point.Garden),size=4) +
  facet_wrap(~Point.Site, scales="free_x") +
  theme_bw()

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







