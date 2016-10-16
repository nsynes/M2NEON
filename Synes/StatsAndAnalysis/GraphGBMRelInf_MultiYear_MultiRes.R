
library(ggplot2)

dfAll <- data.frame()
for (y in c(2013,2014,2015)) {
  if (y == 2013) {
    for (res in c(1,2,3)) {
      df <- read.csv(sprintf("C:/Dropbox (ASU)/M2NEON/GBM_Results/fromSet3_mergedResults/AllGBMsSet3_%sm_%s.csv", res, y))
      dfAll <- rbind(dfAll, df)
    }
  }
  else {
    res <- 1
    df <- read.csv(sprintf("C:/Dropbox (ASU)/M2NEON/GBM_Results/fromSet3_mergedResults/AllGBMsSet3_%sm_%s.csv", res, y))
    dfAll <- rbind(dfAll, df)
  }
}
df <- NULL

setwd(sprintf("C:/Dropbox (ASU)/M2NEON/GBM_Results"))


for (interval in unique(dfAll$IntervalPeriod)) {
  for (indvar in unique(dfAll$IndependentVar)) {
    for (depvar in unique(dfAll$DependentVar)) {
      
      dfSub <- subset(dfAll, IntervalPeriod == interval &
                          IndependentVar == indvar &
                          DependentVar == depvar &
                          Resolution == 1)
      
      plot <- ggplot(data = dfSub) +
                geom_bar(aes(x=factor(Period), y=RelInf, fill=Rank), stat="identity") +
                geom_text(aes(x=factor(Period), y=8, label=sprintf("R^2==%s;", round(ModelRsquared,4))),
                          vjust=0.4, size = 3.2, parse=T, angle=90) +
                geom_text(aes(x=factor(Period), y=23, label=sprintf("Rank: %s",Rank)),
                          vjust=0.4, size = 3.2, parse=T, angle=90) +
                theme_bw() +
                facet_grid(Site~Year) +
                scale_fill_gradient("Relative influence ranking\nof independent variable\n(1 = most important)",limits=c(1,33),low="red", high="white") +
                labs(x = unique(dfSub$IntervalPeriod),
                     y = "Relative influence (%)",
                     title = sprintf("Dependent (sensor) variable = %s\nSelected independent variable = %s\nCanopy fraction resolution = %sm", depvar, indvar, res))
      
      ggsave(file=sprintf("Dep=%s_Ind=%s__%sly_res=%s.png", depvar, indvar, interval, res), plot, width=12,height=8, dpi=500)
      

    }
  }
}


# STACKED bars, facetted by site and year
pal <- c(rev(RColorBrewer::brewer.pal(9,"Greens")[4:9]),
         #RColorBrewer::brewer.pal(9,"YlOrRd")[3:4],
         RColorBrewer::brewer.pal(9,"Purples")[8],
         RColorBrewer::brewer.pal(9,"Greys")[6],
         RColorBrewer::brewer.pal(9,"Blues")[4:5],
         RColorBrewer::brewer.pal(9,"Reds")[5:8])

for (interval in c("Month")) {
  for (depvar in unique(dfAll$DependentVar)) {
      
    dfSub <- subset(dfAll, IntervalPeriod == interval &
                      DependentVar == depvar &
                      Resolution == 1)
    
    plot <- ggplot(data = dfSub) +
      geom_bar(aes(x=factor(Period), y=RelInf, fill=IndependentVar), color="black", stat="identity") +
      geom_text(aes(x=factor(Period), y=13, label=sprintf("R^2==%s", round(ModelRsquared,4))),
                vjust=0.4, size = 3.2, parse=T, angle=90) +
      theme_bw() +
      facet_grid(Site~Year) +
      scale_fill_manual(name="Independent variables", values = pal) +
      labs(x = unique(dfSub$IntervalPeriod),
           y = "Relative influence (%)",
           title = sprintf("Dependent (sensor) variable = %s\nCanopy fraction resolution = %sm", depvar, unique(dfSub$Resolution)[1]))
    
    ggsave(file=sprintf("Dep=%s_2013_ByYear.png", depvar, unique(dfSub$Year)[1]), plot, width=12,height=8, dpi=500)
      
  }
}



# STACKED bars, facetted by site and resolution (for 2013)
pal <- c(rev(RColorBrewer::brewer.pal(9,"Greens")[4:9]),
         #RColorBrewer::brewer.pal(9,"YlOrRd")[3:4],
         RColorBrewer::brewer.pal(9,"Purples")[8],
         RColorBrewer::brewer.pal(9,"Greys")[6],
         RColorBrewer::brewer.pal(9,"Blues")[4:5],
         RColorBrewer::brewer.pal(9,"Reds")[5:8])

for (interval in c("Month")) {
  for (depvar in unique(dfAll$DependentVar)) {
    
    dfSub <- subset(dfAll, IntervalPeriod == interval &
                      DependentVar == depvar &
                      Year == 2013)
    
    plot <- ggplot(data = dfSub) +
      geom_bar(aes(x=factor(Period), y=RelInf, fill=IndependentVar), color="black", stat="identity") +
      geom_text(aes(x=factor(Period), y=13, label=sprintf("R^2==%s", round(ModelRsquared,4))),
                vjust=0.4, size = 3.2, parse=T, angle=90) +
      theme_bw() +
      facet_grid(Site~Resolution) +
      scale_fill_manual(name="Independent variables", values = pal) +
      labs(x = unique(dfSub$IntervalPeriod),
           y = "Relative influence (%)",
           title = sprintf("Dependent (sensor) variable = %s\nYear = %s", depvar, unique(dfSub$Year)[1]))
    
    ggsave(file=sprintf("Dep=%s_res=%sm_ByResolution.png", depvar, res), plot, width=12,height=8, dpi=500)
    
  }
}




