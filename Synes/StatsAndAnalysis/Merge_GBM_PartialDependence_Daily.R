library(lattice)
library(rgl)
library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)
library(dplyr)
library(Hmisc)

Dir <- "C:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Results/5_DistToStreamOverFlowAccum/MicrositeLevel"
MainDir <- sprintf("%s/ModelDirs", Dir)
OutDir <- sprintf("%s/PartialDependence", Dir)
setwd(MainDir)

ModelDirs <- list.dirs(full.names=FALSE, recursive=FALSE)

setwd(OutDir)
NoIndependentVars <- 7
NoCategoricalIndVars <- 1
my.list <- vector('list', (NoIndependentVars-NoCategoricalIndVars) * length(ModelDirs))
my.list.categorical <- vector('list', NoCategoricalIndVars * length(ModelDirs))
my.list.dif <- vector('list', (NoIndependentVars-NoCategoricalIndVars) * length(ModelDirs))

i <- 1
j <- 1
for (ModelDir in ModelDirs){
  cat(paste(ModelDir, "\n"))
  FullDir <- sprintf("%s/%s/PartialDependence", MainDir, ModelDir)
  listFiles <- list.files(FullDir, full.names=FALSE, recursive=FALSE)
  for (file in listFiles) {
    if (substr(file, nchar(file)-3, nchar(file)) == ".csv") {
      DepVar <- as.factor(strsplit(strsplit(ModelDir, "Sensor.Day")[[1]][[2]],
                                   "[.]")[[1]][[2]])
      if (DepVar %in% c("Max","Min","DiurnalRange")) {
        #cat(paste("--", file, "\n"))
        dfSingle <- read.csv(sprintf("%s/%s", FullDir, file))
        dfSingle$X <- NULL
        day <- as.numeric(strsplit(strsplit(ModelDir, "Sensor.Day")[[1]][[2]],"[.]")[[1]][[1]])
        dfSingle$Day <- day
        IndVar <- colnames(dfSingle)[1]
        dfSingle$x = dfSingle[,IndVar]
        dfSingle[,IndVar] <- NULL
        if (substr(strsplit(IndVar, "[.]")[[1]][[2]],1,3) == "Day") {
          IndVar <- sprintf("Indep.%s", strsplit(IndVar, "[.]")[[1]][[3]])
        }
        dfSingle$IndependentVar <- as.factor(IndVar)
        dfSingle$DependentVar <- as.factor(DepVar)
        Site <- strsplit(strsplit(ModelDir, "=")[[1]][[2]], "_y")[[1]][[1]]
        dfSingle$Site <- as.factor(Site)
        
        if ((IndVar %in% c("Indep.GroundCode"))) {
          my.list.categorical[[j]] <- dfSingle
          j <- j + 1
        }
        else {
          my.list[[i]] <- dfSingle
          i <- i + 1
        }
        
        # Get difference between values at minx and maxx
        if (!(IndVar %in% c("Indep.GroundCode"))) { # skip categorical variables
          ValAtMaxX <- dfSingle$y[dfSingle$x == max(dfSingle$x)]
          ValAtMinX <- dfSingle$y[dfSingle$x == min(dfSingle$x)]
          dif <-  ValAtMaxX - ValAtMinX
          dfDif <- data.frame(day=day, IndVar=as.factor(IndVar), DepVar=as.factor(DepVar), Site=as.factor(Site), ValAtMaxX=ValAtMaxX, ValAtMinX=ValAtMinX, dif=dif)
          my.list.dif[[i]] <- dfDif
        }
        
        dfDif <- NULL
        dif <- NULL
        ValAtMaxX <- NULL
        ValAtMinX <- NULL
        dfSingle <- NULL
      }
    }
  }
}
dfOut <- do.call('rbind', my.list)
dfOutCat <- do.call('rbind', my.list.categorical)
dfOutDif <- do.call('rbind', my.list.dif)

write.csv(dfOutCat, "MergedCategoricalPartialDependence.csv", row.names=FALSE)

#########################################
# Plot dif between value at max x and value at max y
#########################################

for (dep in c("Max","Min")) {
  for (ind in unique(dfOutDif$IndVar)) {
    dfSub <- subset(dfOutDif, DepVar == dep & IndVar == ind)
    if (nrow(dfSub) > 0) {
      p <- ggplot() +
        geom_bar(data=dfSub, aes(x=day, y=dif), stat="identity", width=1, color="black", fill="black") +
        geom_hline(yintercept=0, color="grey") +
        facet_wrap(~Site, ncol=1) +
        labs(title=sprintf("Dependent variable = %s\nIndependent variable = %s", dep, ind), y=sprintf("Change in partial dependence\nf(max(x)) - f(min(x))")) +
        theme_bw()
      
      ggsave(file=sprintf("PartialDepDifference_Dep=%s_Ind=%s.png", dep, ind), p, width=6,height=10, dpi=300)
    }
  }
}


#########################################
# Plot effect of each categorical variable category
#########################################

#CategoryList <- c("c0 = Sensor on bare soil or duff layer, no shrub or tall herb vegetation within 1-2 m.",
#                  "c1 = Sensor in grass or forb layer that could shade the sensor at least some of the time, , no shrub within 1-2 m",
#                  "c2 = Sensor in a small gap or near edge of shrubs that are within 1-2m",
#                  "c3 = Sensor underneath an open shrub or small tree canopy",
#                  "c4 = Sensor under a dense shrub or small tree canopy",
#                  "c5 = Among rocks",
#                  "c6 = Among fallen logs")
CategoryList <- c("c0 = Sensor in shrub gap",
                  "c1 = Sensor near or within shrub or logs",
                  "c2 = Sensor among rocks")
CategoryText <- paste0("",unlist(CategoryList),collapse="\n")


site <- "Sierra montane"
for (dep in unique(dfOutCat$DependentVar)) {
  for (ind in unique(dfOutCat$IndependentVar)) {
    dfSub <- subset(dfOutCat, DependentVar == dep & IndependentVar == ind & Site == site)
    if (nrow(dfSub) > 0) {
      p1 <- ggplot() +
        geom_line(data=dfSub, aes(x=Day, y=y), stat="identity", size=1.2) +
        geom_hline(yintercept=0, color="grey") +
        facet_wrap(~x, ncol=2) +
        labs(title=sprintf("Site = %s\nDependent variable = %s\nIndependent variable = %s", site, dep, ind), y=sprintf("f(%s)",ind)) +
        theme_bw()
      
      p2 <- ggplot() + 
        labs(title = CategoryText) + theme(plot.title = element_text(hjust = 0))
      
      plot <- grid.arrange(arrangeGrob(p1,p2, heights=c(3, 1), ncol=1),
                           ncol=1)
      
      ggsave(file=sprintf("CategoricalPartialDep_Dep=%s_Ind=%s.png", dep, ind), plot, width=12,height=10, dpi=300)
    }
  }
}

#######################################
#######################################


if (FALSE) {
pal <- c(RColorBrewer::brewer.pal(9,"Greens")[8:8], RColorBrewer::brewer.pal(9,"Greens")[6:6])

dep <- "Max"
dfSub <- subset(dfOutDif, DepVar == dep & IndVar %in% c("Indep.Canopy.Density.SouthRad2.5m","Indep.Canopy.Density.SouthRad30mCut"))

p <- ggplot() +
  geom_bar(data=dfSub, aes(x=day, y=dif, fill=IndVar), stat="identity", width=1) +
  #scale_color_manual(values=pal) +
  scale_fill_manual(values=pal) +
  #scale_color_brewer(palette = "Set1") +
  #scale_fill_brewer(palette = "Set1") +
  geom_hline(yintercept=0, color="grey") +
  #lims(x=c(50,350)) +
  scale_x_continuous(limits=c(0,365), breaks=c(0,DaysInMonth$CumulativeDays), minor_breaks=NULL, expand=c(0,0)) +
  scale_y_continuous(limits=c(-7.5,0), breaks=c(-7.5,-5,-2.5,0)) +
  facet_wrap(~Site, ncol=1) +
  #labs(title=sprintf("Dependent variable = %s", dep), y=sprintf("Change in partial dependence\nf(max(x)) - f(min(x))")) +
  labs(fill="", x="Day of year (2013)", y=sprintf("Change in partial dependence\nf(max(x)) - f(min(x))")) +
  theme_bw() +
  theme(legend.position="bottom", text = element_text(size=15))

ggsave(file=sprintf("PartialDepDifference_Dep=%s_Canopy_SierraFoothills.png", dep, ind), p, width=10,height=4, dpi=300)
}
#########################################

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


write.csv(dfOut, "MergedPartialDependence.csv", row.names=FALSE)


if (FALSE) {
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




