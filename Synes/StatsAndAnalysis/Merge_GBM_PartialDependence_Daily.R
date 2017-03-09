library(lattice)
library(rgl)
library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)
library(dplyr)
library(Hmisc)

MainDir <- "C:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/13_GBM_2013_Daily_DEMDSM/ModelDirs"
OutDir <- "C:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/13_GBM_2013_Daily_DEMDSM/PartialDependence"
setwd(MainDir)

ModelDirs <- list.dirs(full.names=FALSE, recursive=FALSE)

setwd(OutDir)
NoIndependentVars <- 7
my.list <- vector('list', NoIndependentVars * length(ModelDirs))

i <- 1
for (ModelDir in ModelDirs){
  cat(paste(ModelDir, "\n"))
  FullDir <- sprintf("%s/%s/PartialDependence", MainDir, ModelDir)
  listFiles <- list.files(FullDir, full.names=FALSE, recursive=FALSE)
  for (file in listFiles) {
    if (substr(file, nchar(file)-3, nchar(file)) == ".csv") {
      #cat(paste("--", file, "\n"))
      dfSingle <- read.csv(sprintf("%s/%s", FullDir, file))
      dfSingle$X <- NULL
      dfSingle$Day <- as.numeric(strsplit(strsplit(ModelDir, "Sensor.Day")[[1]][[2]],
                       "[.]")[[1]][[1]])
      IndVar <- colnames(dfSingle)[1]
      dfSingle$x = dfSingle[,IndVar]
      dfSingle[,IndVar] <- NULL
      if (strsplit(IndVar, "[.]")[[1]][[3]] %in% c("SolarRadiation",
                                                   "DSMSolarRadiation",
                                                   "DEMSolarRadiation",
                                                   "DEMDSMSolarRadiation")) {
        IndVar <- sprintf("Raster.%s", strsplit(IndVar, "[.]")[[1]][[3]])
      }
      dfSingle$IndependentVar <- as.factor(IndVar)
      DepVar <- as.factor(strsplit(strsplit(ModelDir, "Sensor.Day")[[1]][[2]],
                                   "[.]")[[1]][[2]])
      dfSingle$DependentVar <- as.factor(DepVar)
      Site <- strsplit(strsplit(ModelDir, "=")[[1]][[2]], "_y")[[1]][[1]]
      dfSingle$Site <- as.factor(Site)
      my.list[[i]] <- dfSingle
      i <- i + 1
      dfSingle <- NULL
    }
  }
}
dfOut <- do.call('rbind', my.list)

dfOut$Quarter <- NA
dfOut$Quarter <- ifelse(dfOut$Day > 0 & dfOut$Day <= 90, "Jan - Mar", dfOut$Quarter)
dfOut$Quarter <- ifelse(dfOut$Day > 90 & dfOut$Day <= 181, "Apr - Jun", dfOut$Quarter)
dfOut$Quarter <- ifelse(dfOut$Day > 181 & dfOut$Day <= 273, "Jul - Sep", dfOut$Quarter)
dfOut$Quarter <- ifelse(dfOut$Day > 273 & dfOut$Day <= 365, "Oct - Dec", dfOut$Quarter)
dfOut$QuarterSplit <- NA
dfOut$QuarterSplit <- ifelse(dfOut$Quarter == "Jan - Mar", dfOut$Day, dfOut$QuarterSplit)
dfOut$QuarterSplit <- ifelse(dfOut$Quarter == "Apr - Jun", dfOut$Day-90, dfOut$QuarterSplit)
dfOut$QuarterSplit <- ifelse(dfOut$Quarter == "Jul - Sep", dfOut$Day-181, dfOut$QuarterSplit)
dfOut$QuarterSplit <- ifelse(dfOut$Quarter == "Oct - Dec", dfOut$Day-273, dfOut$QuarterSplit)

dfOut$Quarter <- factor(dfOut$Quarter, levels = c("Jan - Mar",
                                                  "Apr - Jun",
                                                  "Jul - Sep",
                                                  "Oct - Dec"))

DaysInMonth <- data.frame(Year=2013, Month=1:12, days=monthDays(as.Date(paste0(2013,"-",1:12,"-01"))))
DaysInMonth <- mutate(DaysInMonth, CumulativeDays=cumsum(days))

dfOut$Month <- NA
for (m in 12:1) {
  dfOut$Month <- ifelse((dfOut$Day <= DaysInMonth[DaysInMonth$Month == m,]$CumulativeDays), m, dfOut$Month)
}
dfOut$MonthSplit <- dfOut$Day
for (m in 2:12) {
  dfOut$MonthSplit <- ifelse(dfOut$Month == m, dfOut$Day - DaysInMonth[DaysInMonth$Month == m-1,]$CumulativeDays, dfOut$MonthSplit)
}

dfOut$Month <- as.factor(dfOut$Month)
dfOut$Month <- revalue(dfOut$Month, c("1"="Jan",
                                      "2"="Feb",
                                      "3"="Mar",
                                      "4"="Apr",
                                      "5"="May",
                                      "6"="Jun",
                                      "7"="Jul",
                                      "8"="Aug",
                                      "9"="Sep",
                                      "10"="Oct",
                                      "11"="Nov",
                                      "12"="Dec"))


write.csv(dfOut, "MergedPartialDependence.csv")

for (dep in unique(dfOut$DependentVar)) {
  for (ind in unique(dfOut$IndependentVar)) {

    dfSub <- subset(dfOut, DependentVar == dep &
                           IndependentVar == ind)
    
    s <- "Sierra foothills"
    p1 <- ggplot(data = subset(dfSub, Site == s & Month %in% c("Apr","May","Jun","Jul","Aug","Sep","Oct"))) +
      geom_line(aes(x=x, y=y, group=Day, color=MonthSplit), size=0.3) +
      facet_wrap(~Month, scale="fixed", ncol=3) +
      labs(title=sprintf("Site: %s", s), color="Day of Month", x=ind, y=dep) +
      scale_color_continuous(trans = "reverse") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust=1))
    
    s <- "Sierra montane"
    p2 <- ggplot(data = subset(dfSub, Site == s & Month %in% c("Apr","May","Jun","Jul","Aug","Sep","Oct"))) +
      geom_line(aes(x=x, y=y, group=Day, color=MonthSplit), size=0.3) +
      facet_wrap(~Month, scale="fixed", ncol=3) +
      labs(title=sprintf("Site: %s", s), color="Day of Month", x=ind, y=dep) +
      scale_color_continuous(trans = "reverse") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust=1))
    
    plot <- grid.arrange(p1,p2, ncol=2, top=textGrob("Partial dependence plots",gp=gpar(fontsize=20,font=3)))
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
dfSub <- dfSub[order(dfSub$x, dfSub$Day),]


plot3d(dfSub$x, dfSub$Day, dfSub$y,
       xlab=unique(dfSub$IndependentVar)[1],
       ylab="Bi-weekly period",
       zlab=unique(dfSub$DependentVar)[1],
       main=sprintf("Site: %s", unique(dfSub$Site)[1]))

#plot3d(dfSub[,slopasp], dfSub[,cov], dfSub[,TempVar], col=cols, size=5, lwd=15, xlab=slopasp, ylab=cov, zlab=TempVar,
#       main=sprintf("Site: %s", SubSite),sub = sprintf("%s = (%0.4f * %s) + (%0.4f * %s)", TempVar, coef(fit)[2], cov, coef(fit)[3], slopasp))

}




