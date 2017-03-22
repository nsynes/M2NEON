library(ggplot2)
library(grid)
library(plyr)

setwd(sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/6_GBM_2013_AtmosTransDsmSolar"))
dfGbm <- read.csv(sprintf("MergedGbmData.csv"))

dfRsquared <- dfGbm
dfRsquared$RelInf <- NULL
dfRsquared$Rank <- NULL
dfRsquared$IndependentVarPeriod <- NULL
dfRsquared$FullNameDependentVar <- NULL
dfRsquared$IndependentVar <- NULL
dfRsquared <- dfRsquared[!duplicated(dfRsquared), ]
dfRsquared$value <- dfRsquared$ModelRsquared
dfRsquared$ModelRsquared <- NULL
dfRsquared$type <- as.factor(sprintf("R-squared (%s)", dfRsquared$Site))
dfRsquared$variable <- as.factor("R-squared")
dfRsquared$IndependentVar <- NA
dfRsquared$row.names <- NULL

dfGbm$IndependentVarPeriod <- NULL
dfGbm$FullNameDependentVar <- NULL
dfGbm$Rank <- NULL
dfGbm$ModelRsquared <- NULL
dfGbm$value <- dfGbm$RelInf / 100
dfGbm$type <- as.factor(sprintf("Relative Influence (%s)", dfGbm$Site))
dfGbm$variable <- as.factor("RelInf")
dfGbm$RelInf <- NULL

# blank data to force graphs to show full year (1 to 365 days)
dfBlank <- rbind(data.frame(DependentVar="Not applicable",
                            IntervalPeriod="BiMonthly",
                            Period=c(1:24),
                            Site="Sierra foothills",
                            IndependentVar=NA,
                            value=0,
                            type="Relative Influence (Sierra foothills)",
                            variable="blank"),
                 data.frame(DependentVar="Not applicable",
                            IntervalPeriod="Daily",
                            Period=c(1:24),
                            Site="Sierra montane",
                            IndependentVar=NA,
                            value=0,
                            type="Relative Influence (Sierra montane)",
                            variable="blank"))

df <- rbind(dfGbm, dfRsquared, dfBlank)

df$type <- factor(df$type, levels=c("Relative Influence (Sierra foothills)",
                                    "R-squared (Sierra foothills)",
                                    "Relative Influence (Sierra montane)",
                                    "R-squared (Sierra montane)"))
dfRsquared <- NULL
dfGbm <- NULL
dfBlank <- NULL

##############################
# For models with multiple scales/types per variable category
df$IndependentVar <- factor(df$IndependentVar,
                            levels = c("Shrub.2m",
                                       "Canopy.p90.2m",
                                       "DSMSolarRadiation",
                                       "Curvature.Plan.100m",
                                       "Curvature.Prof.100m",
                                       "SinSlopeCosAspect.2m",
                                       "DEM.2m",
                                       "TWI.30m"))

# STACKED
pal <- c(RColorBrewer::brewer.pal(9,"Greens")[6:6],
         RColorBrewer::brewer.pal(9,"Greens")[8:8],
         RColorBrewer::brewer.pal(9,"Reds")[8:8],
         RColorBrewer::brewer.pal(9,"Purples")[6:7],
         RColorBrewer::brewer.pal(9,"Greys")[7:8],
         RColorBrewer::brewer.pal(9,"Blues")[5:5])
         #RColorBrewer::brewer.pal(9,"Blues")[4:5],
         #RColorBrewer::brewer.pal(9,"YlOrRd")[3:4],


for (var in c("MeanDailyMin","MeanDailyMax","Mean","MeanDiurnalRange","CDD5","CDD10")) {
  dfSub <- subset(df, DependentVar %in% c(var, "Not applicable"))
  
  plot <- ggplot(data = dfSub) + facet_wrap(~type, scales="fixed", ncol=1) +
    scale_fill_manual(values = pal) +
    scale_color_manual(values = pal) +
    geom_bar(data = subset(dfSub, type == "Relative Influence (Sierra foothills)"), aes(x=Period, y=value, fill=IndependentVar, color=IndependentVar), stat="identity") +
    geom_line(data = subset(dfSub, type == "R-squared (Sierra foothills)"), aes(x=Period, y=value)) +
    geom_bar(data = subset(dfSub, type == "Relative Influence (Sierra montane)"), aes(x=Period, y=value, fill=IndependentVar, color=IndependentVar), stat="identity") +
    geom_line(data = subset(dfSub, type == "R-squared (Sierra montane)"), aes(x=Period, y=value)) +
    labs(title = sprintf("Dependent (sensor) variable = %s\n", var), x="BiMonthly (2013)", fill = "Independent variable",
         color = "Independent variable", linetype = "Lines") +
    theme_bw()
  
  ggsave(file=sprintf("Dep=%s_BiMonthly.png", var),
         plot, width=16,height=12, dpi=500)
        
}



if (FALSE) {

# Subset to only days with a certain level of transmittance
fooSF <- subset(df, Site == "Sierra foothills" & variable == "Atmospheric Transmittance" & value >= 0.7)
DaysSF <- unique(fooSF$Period)

fooSM <- subset(df, Site == "Sierra montane" & variable == "Atmospheric Transmittance" & value >= 0.7)
DaysSM <- unique(fooSM$Period)

dfHighTran <- subset(df, (Site == "Sierra foothills" & Period %in% DaysSF) |
                         (Site == "Sierra montane" & Period %in% DaysSM))


for (var in c("Min","Mean","Max","DiurnalRange")) {
  dfHighTranSub <- subset(dfHighTran, DependentVar %in% c(var, "Not applicable"))
  
  plot <- ggplot(data = dfHighTranSub) + facet_wrap(~type, scales="fixed", ncol=1) +
    scale_fill_manual(values = pal) +
    scale_color_manual(values = pal) +
    geom_bar(data = subset(dfHighTranSub, type == "Relative Influence (Sierra foothills)"), aes(x=Period, y=value, fill=IndependentVar, color=IndependentVar), stat="identity") +
    geom_line(data = subset(dfHighTranSub, type == "R-squared and Atmospheric Transmittance (Sierra foothills)"), aes(x=Period, y=value, linetype=variable)) +
    #geom_point(data = subset(dfHighTranSub, type == "R-squared and Atmospheric Transmittance (Sierra foothills)"), aes(x=Period, y=value, shape=variable)) +
    geom_bar(data = subset(dfHighTranSub, type == "Relative Influence (Sierra montane)"), aes(x=Period, y=value, fill=IndependentVar, color=IndependentVar), stat="identity") +
    geom_line(data = subset(dfHighTranSub, type == "R-squared and Atmospheric Transmittance (Sierra montane)"), aes(x=Period, y=value, linetype=variable)) +
    #geom_point(data = subset(dfHighTranSub, type == "R-squared and Atmospheric Transmittance (Sierra montane)"), aes(x=Period, y=value, shape=variable)) +
    labs(title = sprintf("Dependent (sensor) variable = %s\n(Days with Atmospheric Transmittance < 0.7 have been removed)", var),
         x="Day of Year (2013)", fill = "Independent variable", color = "Independent variable", linetype = "Lines") +
    theme_bw()
  
  ggsave(file=sprintf("Dep=%s_BiMonthly_AtmosTransAbove0.7.png", var),
         plot, width=16,height=12, dpi=500)
  
}


}


