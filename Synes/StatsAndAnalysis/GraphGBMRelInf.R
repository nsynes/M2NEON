library(ggplot2)


res <- "1m"
df <- read.csv(sprintf("C:/Dropbox (ASU)/M2NEON/GBM_Results/fromSet3_mergedResults/AllGBMsSet3_%s_2015.csv", res))
setwd(sprintf("C:/Dropbox (ASU)/M2NEON/GBM_Results/fromSet3_mergedResults/%s_2015",res))


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
                     title = sprintf("Dependent (sensor) variable = %s\nSelected independent variable = %s\nCanopy resolution = %s", depvar, indvar, res))
      
      ggsave(file=sprintf("IndividualVariableGraphs/Dep=%s_Ind=%s__%sly_res=%s.png", depvar, indvar, interval, res), plot, width=12,height=8, dpi=500)
      

    }
  }
}


# STACKED
pal <- c(RColorBrewer::brewer.pal(9,"Greens")[4:9],
         #RColorBrewer::brewer.pal(9,"YlOrRd")[3:4],
         #RColorBrewer::brewer.pal(9,"Greys")[8:9],
         RColorBrewer::brewer.pal(9,"Purples")[7:8],
         RColorBrewer::brewer.pal(9,"Blues")[4:5],
         RColorBrewer::brewer.pal(9,"Reds")[5:8])

for (interval in c("Month")) {
  for (depvar in unique(df$DependentVar)) {
      
    dfSub <- subset(df, IntervalPeriod == interval &
                      DependentVar == depvar)
    
    #a <- levels(dfSub$IndependentVar)
    #dfSub$IndependentVar <- factor(dfSub$IndependentVar, levels=c(a[13:14],a[3:12]))
    
    plot <- ggplot(data = dfSub) +
      geom_bar(aes(x=factor(Period), y=RelInf, fill=IndependentVar), color="black", stat="identity") +
      #geom_text(aes(x=factor(Period), y=RelInf, label=sprintf("Rank: %s",Rank)),
      #          vjust=-0.25, size = 3, parse=T) +
      geom_text(aes(x=factor(Period), y=0, label=sprintf("R^2==%s", round(ModelRsquared,4))),
                vjust=1.2, size = 2, parse=T) +
      theme_bw() +
      facet_grid(Site~.) +
      scale_fill_manual(values = pal) +
      labs(x = unique(dfSub$IntervalPeriod),
           y = "Relative influence (%)",
           title = sprintf("Dependent (sensor) variable = %s\nCanopy resolution = %s", depvar, res))
    
    ggsave(file=sprintf("Dep=%s_%sly_res=%s.png", depvar, interval, res), plot, width=12,height=8, dpi=500)
      
  }
}


