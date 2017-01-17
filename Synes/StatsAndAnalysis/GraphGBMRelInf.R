library(ggplot2)

setwd(sprintf("D:/Dropbox (ASU)/M2NEON/SensorData/GBM_June2013"))
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

df$IndependentVar <- factor(df$IndependentVar,
                            levels = c("Shrub.2m",
                                       "Shrub.5m",
                                       "Canopy.p75.2m",
                                       "Canopy.p75.5m",
                                       "Canopy.p90.2m",
                                       "Canopy.p90.5m",
                                       "SolarRadiation",
                                       "Curvature.Plan.100m",
                                       "Curvature.Plan.30m",
                                       "Curvature.Prof.100m",
                                       "Curvature.Prof.30m",
                                       "SinSlopeCosAspect.2m",
                                       "DEM.2m",
                                       "TWI.2m",
                                       "TWI.30m",
                                       "WBI.2m"))

# STACKED
pal <- c(RColorBrewer::brewer.pal(9,"Greens")[4:9],
         RColorBrewer::brewer.pal(9,"Reds")[8:8],
         RColorBrewer::brewer.pal(9,"Purples")[6:9],
         RColorBrewer::brewer.pal(9,"Greys")[6:7],
         #RColorBrewer::brewer.pal(9,"Blues")[4:5],
         #RColorBrewer::brewer.pal(9,"YlOrRd")[3:4],
         RColorBrewer::brewer.pal(9,"Blues")[6:8])

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


