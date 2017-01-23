library(lattice)
library(rgl)
library(ggplot2)
library(gridExtra)
library(grid)

MainDir <- "C:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/6_GBM_2013_AtmosTransDsmSolar/ModelDirs"
OutDir <- "C:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/6_GBM_2013_AtmosTransDsmSolar/PartialDependence"
setwd(MainDir)

dfOut <- data.frame()

ModelDirs <- list.dirs(full.names=FALSE, recursive=FALSE)

for (ModelDir in ModelDirs){
  
  FullDir <- sprintf("%s/%s/PartialDependence", MainDir, ModelDir)
  listFiles <- list.files(FullDir, full.names=FALSE, recursive=FALSE)
  for (file in listFiles) {
    if (substr(file, nchar(file)-3, nchar(file)) == ".csv") {
      a <- read.csv(sprintf("%s/%s", FullDir, file))
      a$X <- NULL
      a$HM <- as.numeric(strsplit(strsplit(ModelDir, "Sensor.HM")[[1]][[2]],
                       "[.]")[[1]][[1]])
      IndVar <- colnames(a)[1]
      a$x = a[,IndVar]
      a[,IndVar] <- NULL
      if (strsplit(IndVar, "[.]")[[1]][[3]] %in% c("SolarRadiation","DSMSolarRadiation","DEMSolarRadiation")) {
        IndVar <- sprintf("Raster.%s", strsplit(IndVar, "[.]")[[1]][[3]])
      }
      a$IndependentVar <- IndVar
      DepVar <- as.factor(strsplit(strsplit(ModelDir, "Sensor.HM")[[1]][[2]],
                                   "[.]")[[1]][[2]])
      a$DependentVar <- DepVar
      Site <- strsplit(strsplit(ModelDir, "=")[[1]][[2]], "_y")[[1]][[1]]
      a$Site <- Site
      dfOut <- rbind(dfOut, a)
      a <- NULL
    }
  }
}
dfOut$Quarter <- NA
dfOut$Quarter <- ifelse(dfOut$HM > 0 & dfOut$HM <= 6, "Jan - Mar", dfOut$Quarter)
dfOut$Quarter <- ifelse(dfOut$HM > 6 & dfOut$HM <= 12, "Apr - Jun", dfOut$Quarter)
dfOut$Quarter <- ifelse(dfOut$HM > 12 & dfOut$HM <= 18, "Jul - Sep", dfOut$Quarter)
dfOut$Quarter <- ifelse(dfOut$HM > 18 & dfOut$HM <= 24, "Oct - Dec", dfOut$Quarter)
dfOut$QuarterSplit <- NA
dfOut$QuarterSplit <- ifelse(dfOut$Quarter == "Jan - Mar", dfOut$HM, dfOut$QuarterSplit)
dfOut$QuarterSplit <- ifelse(dfOut$Quarter == "Apr - Jun", dfOut$HM-6, dfOut$QuarterSplit)
dfOut$QuarterSplit <- ifelse(dfOut$Quarter == "Jul - Sep", dfOut$HM-12, dfOut$QuarterSplit)
dfOut$QuarterSplit <- ifelse(dfOut$Quarter == "Oct - Dec", dfOut$HM-18, dfOut$QuarterSplit)

dfOut$Quarter <- factor(dfOut$Quarter, levels = c("Jan - Mar",
                                                  "Apr - Jun",
                                                  "Jul - Sep",
                                                  "Oct - Dec"))

setwd(OutDir)
for (dep in unique(dfOut$DependentVar)) {
  for (ind in unique(dfOut$IndependentVar)) {

    dfSub <- subset(dfOut, DependentVar == dep &
                           IndependentVar == ind)
    
    s <- "Sierra foothills"
    p1 <- ggplot(data = subset(dfSub, Site == s)) +
      geom_line(aes(x=x, y=y, group=HM, color=QuarterSplit), size=1.5) +
      facet_wrap(~Quarter, scale="free") +
      labs(title=sprintf("Site: %s", s), color="Half-month\nperiod", x=ind, y=dep) +
      scale_color_continuous(trans = "reverse") +
      theme_bw()
    
    s <- "Sierra montane"
    p2 <- ggplot(data = subset(dfSub, Site == s)) +
      geom_line(aes(x=x, y=y, group=HM, color=QuarterSplit), size=1.5) +
      facet_wrap(~Quarter, scale="free") +
      labs(title=sprintf("Site: %s", s), color="Half-month\nperiod", x=ind, y=dep) +
      scale_color_continuous(trans = "reverse") +
      theme_bw()
    
    plot <- grid.arrange(p1,p2, top=textGrob("Partial dependence plots",gp=gpar(fontsize=20,font=3)))
    ggsave(file=sprintf("PartialDep_Dep=%s_Ind=%s.png", dep, ind), plot, width=12,height=8, dpi=500)

  }
}





####################################
## 3D plotting
####################################
if (FALSE) {
dfSub <- subset(dfOut, Site == "Sierra foothills" & 
                       DependentVar == "MeanDailyMax" &
                       IndependentVar == "Raster.SolarRadiation")
dfSub <- dfSub[order(dfSub$x, dfSub$HM),]


plot3d(dfSub$x, dfSub$HM, dfSub$y,
       xlab=unique(dfSub$IndependentVar)[1],
       ylab="Bi-weekly period",
       zlab=unique(dfSub$DependentVar)[1],
       main=sprintf("Site: %s", unique(dfSub$Site)[1]))

#plot3d(dfSub[,slopasp], dfSub[,cov], dfSub[,TempVar], col=cols, size=5, lwd=15, xlab=slopasp, ylab=cov, zlab=TempVar,
#       main=sprintf("Site: %s", SubSite),sub = sprintf("%s = (%0.4f * %s) + (%0.4f * %s)", TempVar, coef(fit)[2], cov, coef(fit)[3], slopasp))

}




