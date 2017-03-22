
library(ggplot2)
library(reshape)
library(gridExtra)
library(grid)

source("C:/Dropbox (ASU)/M2NEON/GitHub/M2NEON/Synes/SensorDataCleaning/M2NEON_Rfunctions.R")
year <- 2013
dfBACKUP <- read.csv(sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/Merged_RasterAndSensorData_%s.csv", year))

dfTransSF <- read.csv("C:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/AtmosphericTransmittance/SJER_2013.csv")
dfTransSM <- read.csv("C:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/AtmosphericTransmittance/TEAK_2013.csv")

setwd(sprintf("C:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/RadiationModels/DSMorDEM"))



for (day in 1:365) {
  
  TransSF <- dfTransSF[dfTransSF$yday == day,]$G.H0
  TransSM <- dfTransSM[dfTransSM$yday == day,]$G.H0
  
  DSMvar <- sprintf("Raster.Day%s.DSMSolarRadiation", day)
  DEMvar <- sprintf("Raster.Day%s.DEMSolarRadiation", day)
  
  dfSub <- dfBACKUP[,c("Point.loc_ID", "Point.Site", DSMvar, DEMvar)]
  dfSub$ID <- NA
  dfSub[dfSub$Point.Site == "SF",]$ID <- seq.int(nrow(dfSub[dfSub$Point.Site == "SF",]))
  dfSub[dfSub$Point.Site == "SM",]$ID <- seq.int(nrow(dfSub[dfSub$Point.Site == "SM",]))
  
  
  dfLong <- melt(dfSub, measure.vars = c(DSMvar, DEMvar),
                 variable.name = "group")
  
  p1 <- ggplot() +
    geom_bar(data=dfLong, aes(x=ID, y = value, fill = variable), stat="identity", position="dodge") +
    facet_wrap(~Point.Site, ncol=1) +
    scale_fill_manual(values=c("black","blue")) +
    labs(x="Sensor", fill="") +
    theme_bw() +
    theme(legend.position="top")
  
  MaxVal <- max(max(dfSub[,c(DSMvar)]),max(dfSub[,c(DEMvar)]))
  
  p2 <- ggplot() +
    geom_point(data=dfSub, aes(x=dfSub[,c(DSMvar)], y = dfSub[,c(DEMvar)])) +
    geom_abline(intercept = 0, slope = 1) +
    scale_x_continuous(limits=c(0, MaxVal)) +
    scale_y_continuous(limits=c(0, MaxVal)) +
    facet_wrap(~Point.Site, ncol=2) +
    labs(x=DSMvar, y=DEMvar) +
    theme_bw()
  
  plot <- grid.arrange(p1, p2, top=textGrob(sprintf("Radiation models - Day %s\n Measured atmospheric Transmittance: SF: %s; SM: %s",
                                                    day, TransSF, TransSM), gp=gpar(fontsize=15,font=3)))

  ggsave(file=sprintf("DSMorDEM_Day%s.png", day),
         plot, width=12,height=8, dpi=500)  
  
}

