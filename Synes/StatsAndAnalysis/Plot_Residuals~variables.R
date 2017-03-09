library(ggplot2)


dfResiduals <- read.csv(sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/14_TEST/Residuals/CSVs/Residuals.csv"))

dfVariables <- read.csv("C:/Dropbox (ASU)/M2NEON/SensorData/Merged_RasterAndSensorData_2013.csv")



DepVar <- "Max"
var <- "Raster.Canopy.Density.SouthRad10m"

dfVar <- dfVariables[c("Point.loc_ID",var)]


df <- merge(dfVar, dfResiduals, by.x="Point.loc_ID", by.y="loc_ID")
df$Site <- substr(df$Point.loc_ID,5,6)
df$Garden <- substr(df$Point.loc_ID,7,7)



ggplot() +
  geom_point(data=df, aes(x=get(var), y=Residual), size=0.2) +
  geom_hline(yintercept=0) +
  facet_wrap(~Site, ncol=1, scales="free_x") +
  labs(title=sprintf("Dependent variable: %s", DepVar)) +
  theme_bw()





listVars <- c("Raster.DEM.2m","Raster.Curvature.Plan.100m","Raster.Curvature.Prof.100m",
              "Raster.TWI.30m","Raster.Canopy.Density.SouthRad2.5m",       
              "Raster.Canopy.Density.SouthRad2.5m","Raster.Shrub.SouthRad5m")

setwd("C:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/14_TEST/Residuals/variables~residuals")


for (var in listVars) {
  for (day in 1:365) {

    p <- ggplot() +
      geom_point(data=df, aes(x=get(var), y=get(sprintf("Day%sResidual", day)), color=Garden), size=4) +
      geom_hline(yintercept=0) +
      facet_wrap(~Site, ncol=1, scales="free_x") +
      labs(title=sprintf("Dependent variable: %s", DepVar), x=var, y=sprintf("Day %s residuals", day)) +
      theme_bw()
    
    ggsave(file=sprintf("DepVar=%s_Day%s_%s.png", DepVar, day, var),
           p, width=6,height=6, dpi=300)
    
  }
}






