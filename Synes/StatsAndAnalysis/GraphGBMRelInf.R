library(ggplot2)

setwd(sprintf("D:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/8_GBM_2013_ReducedVeg_IncNorthWestness"))
df <- read.csv(sprintf("MergedGbmData.csv"))

######################
# Graphs for each dependent variable
if (FALSE) {
for (interval in unique(df$IntervalPeriod)) {
  for (indvar in unique(df$IndependentVar)) {
    for (depvar in unique(df$DependentVar)) {
      
      dfSub <- subset(df, IntervalPeriod == interval &
                          IndependentVar == indvar &
                          DependentVar == depvar)
      
      plot <- ggplot(data = dfSub) +
                geom_bar(aes(x=factor(Period), y=RelInf, fill=Rank), stat="identity") +
                geom_text(aes(x=factor(Period), y=RelInf, label=sprintf("Rank: %s",Rank)),
                          vjust=-0.25, size = 3, parse=T) +
                geom_text(aes(x=factor(Period), y=RelInf, label=sprintf("R^2==%s", round(ModelRsquared,4))),
                          vjust=1.2, size = 2.5, parse=T) +
                theme_bw() +
                facet_grid(Site~.) +
                scale_fill_gradient("Relative influence ranking\nof independent variable\n(1 = most important)",limits=c(1,33),low="red", high="white") +
                labs(x = unique(dfSub$IntervalPeriod),
                     y = "Relative influence (%)",
                     title = sprintf("Dependent (sensor) variable = %s\nSelected independent variable = %s", depvar, indvar))
      
      ggsave(file=sprintf("IndividualVariableGraphs/Dep=%s_Ind=%s__%sly.png", depvar, indvar, interval), plot, width=12,height=8, dpi=500)
      

    }
  }
}
}
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
                      DependentVar == depvar)
    
    plot <- ggplot(data = dfSub) +
      geom_bar(aes(x=factor(Period), y=RelInf, fill=IndependentVar), color="black", stat="identity") +
      #geom_text(aes(x=factor(Period), y=RelInf, label=sprintf("Rank: %s",Rank)),
      #          vjust=-0.25, size = 3, parse=T) +
      geom_text(aes(x=factor(Period), y=0, label=sprintf("R^2==%s", round(ModelRsquared,2))),
                vjust=1.2, size = 2, parse=T) +
      theme_bw() +
      facet_grid(Site~.) +
      scale_fill_manual(values = pal) +
      labs(x = unique(dfSub$IntervalPeriod),
           y = "Relative influence (%)",
           title = sprintf("Dependent (sensor) variable = %s", depvar))
    
    ggsave(file=sprintf("Dep=%s_%s.png", depvar, interval), plot, width=12,height=8, dpi=500)
  }
}


