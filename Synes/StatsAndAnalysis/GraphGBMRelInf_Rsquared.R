library(ggplot2)
library(grid)

setwd(sprintf("D:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/8_GBM_2013_ReducedVeg_IncNorthWestness"))
df <- read.csv(sprintf("MergedGbmData.csv"))

##############################
if (TRUE) {
# For models with multiple scales/types per variable category
df$IndependentVar <- factor(df$IndependentVar,
                            levels = c(
                                       "Canopy.Density.SouthRad2.5m",
                                       "Shrub.SouthRad5m",
                                       "LAI.SouthRad5m",
                                       "Canopy.Density.SouthRad10m",
                                       "DSMSolarRadiation",
                                       "Curvature.Plan.100m",
                                       "Curvature.Prof.100m",
                                       "SinSlopeNorthWestness.2m",
                                       "NorthWestness.2m",
                                       "DEM.2m",
                                       "TWI.30m"))
}
if (FALSE) {
df$IndependentVar <- factor(df$IndependentVar,
                            levels = c("Shrub.2m",
                                       "Canopy.p90.2m",
                                       "DSMSolarRadiation",
                                       "Curvature.Plan.100m",
                                       "Curvature.Prof.100m",
                                       "SinSlopeCosAspect.2m",
                                       "DEM.2m",
                                       "TWI.30m"))
}

# STACKED
pal <- c(RColorBrewer::brewer.pal(9,"Greens")[6:9],
         RColorBrewer::brewer.pal(9,"Reds")[8:8],
         RColorBrewer::brewer.pal(9,"Purples")[6:7],
         RColorBrewer::brewer.pal(9,"Greys")[4:5],
         RColorBrewer::brewer.pal(9,"Greys")[8:8],
         RColorBrewer::brewer.pal(9,"Blues")[3:3])
         #RColorBrewer::brewer.pal(9,"Blues")[4:5],
         #RColorBrewer::brewer.pal(9,"YlOrRd")[3:4],

for (interval in c("BiMonthly")) {
  for (depvar in unique(df$DependentVar)) {
      
    dfSub <- subset(df, IntervalPeriod == interval &
                      DependentVar == depvar &
                      Site == "Sierra foothills")
    
    p1a <- ggplot(data = dfSub) +
      geom_bar(aes(x=Period, y=RelInf, fill=IndependentVar), color="black", stat="identity") +
      theme_bw() +
      scale_fill_manual(values = pal) +
      labs(x = unique(dfSub$IntervalPeriod),
           y = "Relative influence (%)")
    
    p1b <- ggplot(data = dfSub) +
      geom_point(aes(x=Period, y=ModelRsquared)) +
      geom_line(aes(x=Period, y=ModelRsquared)) +
      theme_bw()
    
    dfSub <- subset(df, IntervalPeriod == interval &
                      DependentVar == depvar &
                      Site == "Sierra montane")
    
    p2a <- ggplot(data = dfSub) +
      geom_bar(aes(x=Period, y=RelInf, fill=IndependentVar), color="black", stat="identity") +
      theme_bw() +
      scale_fill_manual(values = pal) +
      labs(x = unique(dfSub$IntervalPeriod),
           y = "Relative influence (%)")
    
    p2b <- ggplot(data = dfSub) +
      geom_point(aes(x=Period, y=ModelRsquared)) +
      geom_line(aes(x=Period, y=ModelRsquared)) +
      theme_bw()
    
    plot <- grid.arrange(arrangeGrob(p1a,p1b,p2a,p2b, heights=c(3,1,3,1), ncol=1),
                         ncol=1,
                         top=textGrob(sprintf("Dependent (sensor) variable = %s", depvar),gp=gpar(fontsize=20,font=3)))
    
    ggsave(file=sprintf("Dep=%s_%s.png", depvar, interval), plot, width=12,height=8, dpi=500)
  }
}


ggplot(data=dfSub, aes(mpg, disp)) + facet_wrap(~cyl) + 
  geom_point(data = subset(mtcars, cyl == 4)) +
  geom_line(data = subset(mtcars, cyl == 6)) +
  geom_text(data = subset(mtcars, cyl == 8), aes(label = gear))


