library(lattice)
library(rgl)
library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)
library(dplyr)
library(Hmisc)

Dir <- "C:/Dropbox/Work/ASU/Paper_2/ANALYSIS/NestedModel/Results/8_Without90m/SiteLevel"
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
      if (DepVar %in% c("Max","Min")) {
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

#write.csv(dfOutCat, "MergedCategoricalPartialDependence.csv", row.names=FALSE)




dfOutDif$Quarter <- NA
dfOutDif$Quarter <- ifelse(dfOutDif$day > 0 & dfOutDif$day <= 90, "Jan - Mar", dfOutDif$Quarter)
dfOutDif$Quarter <- ifelse(dfOutDif$day > 90 & dfOutDif$day <= 181, "Apr - Jun", dfOutDif$Quarter)
dfOutDif$Quarter <- ifelse(dfOutDif$day > 181 & dfOutDif$day <= 273, "Jul - Sep", dfOutDif$Quarter)
dfOutDif$Quarter <- ifelse(dfOutDif$day > 273 & dfOutDif$day <= 365, "Oct - Dec", dfOutDif$Quarter)
dfOutDif$QuarterSplit <- NA
dfOutDif$QuarterSplit <- ifelse(dfOutDif$Quarter == "Jan - Mar", dfOutDif$day, dfOutDif$QuarterSplit)
dfOutDif$QuarterSplit <- ifelse(dfOutDif$Quarter == "Apr - Jun", dfOutDif$day-90, dfOutDif$QuarterSplit)
dfOutDif$QuarterSplit <- ifelse(dfOutDif$Quarter == "Jul - Sep", dfOutDif$day-181, dfOutDif$QuarterSplit)
dfOutDif$QuarterSplit <- ifelse(dfOutDif$Quarter == "Oct - Dec", dfOutDif$day-273, dfOutDif$QuarterSplit)

dfOutDif$Quarter <- factor(dfOutDif$Quarter, levels = c("Jan - Mar",
                                                  "Apr - Jun",
                                                  "Jul - Sep",
                                                  "Oct - Dec"))

DaysInMonth <- data.frame(Year=2013, Month=1:12, days=monthDays(as.Date(paste0(2013,"-",1:12,"-01"))))
DaysInMonth <- mutate(DaysInMonth, CumulativeDays=cumsum(days))
DaysInMonth$MidOfMonth <- DaysInMonth$CumulativeDays - (0.5 * DaysInMonth$days)
DaysInMonth$MonthName <- as.factor(DaysInMonth$Month)
DaysInMonth$MonthName <- revalue(DaysInMonth$MonthName, c("1"="Jan",
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

dfOutDif$Month <- NA
for (m in 12:1) {
  dfOutDif$Month <- ifelse((dfOutDif$day <= DaysInMonth[DaysInMonth$Month == m,]$CumulativeDays), m, dfOutDif$Month)
}
dfOutDif$MonthSplit <- dfOutDif$day
for (m in 2:12) {
  dfOutDif$MonthSplit <- ifelse(dfOutDif$Month == m, dfOutDif$day - DaysInMonth[DaysInMonth$Month == m-1,]$CumulativeDays, dfOutDif$MonthSplit)
}

dfOutDif$Month <- as.factor(dfOutDif$Month)
dfOutDif$Month <- revalue(dfOutDif$Month, c("1"="Jan",
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





#########################################
# Plot dif between value at max x and value at max y
#########################################

dfOutDif$Site <- revalue(dfOutDif$Site, c("Sierra foothills" = "SJER", "Sierra montane" = "TEF"))

# function to wrap text in title
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

for (dep in c("Max","Min")) {
  if (dep == "Min") varlong <- "Minimum temperature"
  if (dep == "Max") varlong <- "Maximum temperature"
  if (dep == "DiurnalRange") varlong <- "diurnal range"
  for (ind in unique(dfOutDif$IndVar)) {
    
    col="dark grey"
    if (ind == "Indep.SolarRadiation30m") col="#cccc00"
    if (ind == "Indep.Canopy.Density.Circle_Radius90m") col = RColorBrewer::brewer.pal(9,"Greens")[8:8]
    if (ind == "Indep.DistToStreamOverFlowAccum") col = "#0072B2"
    if (ind == "Indep.SolarRadiation2m") col = "#cccc00"
    if (ind == "Indep.Canopy.Density.SouthRad30mCut") col = RColorBrewer::brewer.pal(9,"Greens")[8:8]
    if (ind == "Indep.Canopy.Density.SouthRad2.5m") col = RColorBrewer::brewer.pal(9,"Greens")[7:7]
    if (ind == "Indep.Canopy.Density.CircleRadius5m") col = RColorBrewer::brewer.pal(9,"Greens")[5:5]
    if (ind == "Indep.GroundCode") col = "#8B6742"
    if (ind == "Indep.Slope") col = RColorBrewer::brewer.pal(9,"Greys")[6:6]
    
    dfSub <- subset(dfOutDif, DepVar == dep & IndVar == ind)
    # Graph Indep.DistToStreamOverFlowAccum inverse to the other independent variables
    # since it makes sense to consider this in terms of cold air pooling and
    # a cooling effect at when close to a stream, rather than a warming effect at distance from a stream
    if (ind == "Indep.DistToStreamOverFlowAccum") {
      dfSub$dif <- -dfSub$dif
      yaxislabel <- "Δ partial dependence (°C)"
    } else {
      yaxislabel <- "Δ partial dependence (°C)"
    }
    dfMonth <- ddply(dfSub,~Month+Site,summarise,mean=mean(dif, na.rm=TRUE))
    foo <- merge(DaysInMonth, dfMonth, by.x=c("MonthName"), by.y=c("Month"))
    if (nrow(dfSub) > 0) {
      
      p1 <- ggplot() +
        geom_bar(data=dfSub, aes(x=day, y=dif), stat="identity", width=1, color="dark grey", fill="dark grey") +
        geom_line(data=foo, aes(x=MidOfMonth, y=mean), size=2, color="red") +
        geom_hline(yintercept=0, color="grey") +
        scale_x_continuous(limits=c(0,365), breaks=c(0,DaysInMonth$CumulativeDays), minor_breaks=NULL, expand=c(0,0)) +
        facet_wrap(~Site, ncol=1) +
        labs(title=wrapper(sprintf("Partial dependence of %s in models of daily %s", substr(ind,7,nchar(ind)), varlong), width=60),
             x="Julian day", y=yaxislabel) +
        theme_bw()
      
      #ggsave(file=sprintf("PartialDepDifference_Dep=%s_Ind=%s.png", dep, ind), p1, width=6,height=10, dpi=300)
      
      
      p2 <- ggplot(data=dfMonth) +
        geom_bar(aes(x=Month, y=mean), stat="identity", width=1, color="black", fill=col) +
        geom_hline(yintercept=0, color="black") +
        facet_wrap(~Site, ncol=1) +
        labs(title=sprintf("Independent variable:\n%s\nDependent variable:\n%s", substr(ind,7,nchar(ind)), varlong),
             y = yaxislabel) +
        scale_y_continuous(limits = c(-6.5, 6.5), breaks=c(-6,-4,-2,0,2,4,6)) +
        theme_bw() +
        theme(#legend.position="top",
              plot.title = element_text(size=13),
              axis.title = element_text(size=13),
              axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4),
              axis.text = element_text(size=13),
              strip.text = element_text(size=13))
      
      ggsave(file=sprintf("PartialDepDifferenceMeanBar_Dep=%s_Ind=%s.png", dep, ind), p2, width=3.5,height=6, dpi=300)

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


dfOutCat$x <- revalue(dfOutCat$x, c("c0" = "shrub gap", "c1" = "near or within shrub or logs"))

DaysInMonth <- data.frame(Year=2013, Month=1:12, days=monthDays(as.Date(paste0(2013,"-",1:12,"-01"))))
DaysInMonth <- mutate(DaysInMonth, CumulativeDays=cumsum(days))

dfOutCat$Month <- NA
for (m in 12:1) {
  dfOutCat$Month <- ifelse((dfOutCat$Day <= DaysInMonth[DaysInMonth$Month == m,]$CumulativeDays), m, dfOutCat$Month)
}
dfOutCat$MonthSplit <- dfOutCat$Day
for (m in 2:12) {
  dfOutCat$MonthSplit <- ifelse(dfOutCat$Month == m, dfOutCat$Day - DaysInMonth[DaysInMonth$Month == m-1,]$CumulativeDays, dfOutCat$MonthSplit)
}

dfOutCat$Month <- as.factor(dfOutCat$Month)
dfOutCat$Month <- revalue(dfOutCat$Month, c("1"="Jan",
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

dfMonth <- ddply(dfOutCat,~Month+x+IndependentVar+Site+DependentVar,summarise,mean=mean(y))
foo <- data.frame(Month=c("Jan","Jan","Jan","Jan",
                          "Feb","Feb","Feb","Feb"),
                  x=c("shrub gap","shrub gap","near or within shrub or logs","near or within shrub or logs",
                      "shrub gap","shrub gap","near or within shrub or logs","near or within shrub or logs"),
                  IndependentVar=c("Indep.GroundCode","Indep.GroundCode","Indep.GroundCode","Indep.GroundCode",
                                   "Indep.GroundCode","Indep.GroundCode","Indep.GroundCode","Indep.GroundCode"),
                  Site=c("Sierra montane","Sierra montane","Sierra montane","Sierra montane",
                         "Sierra montane","Sierra montane","Sierra montane","Sierra montane"),
                  DependentVar=c("Max","Min","Max","Min",
                                 "Max","Min","Max","Min"),
                  mean=c(NA,NA,NA,NA,
                         NA,NA,NA,NA))
dfMonth <- rbind(dfMonth, foo)

site <- "Sierra montane"
ind <- "Indep.GroundCode"
for (dep in c("Max","Min")) {
  if (dep =="Max") deplong = "Maximum temperature"
  if (dep =="Min") deplong = "Minimum temperature"
  dfSub <- subset(dfMonth, DependentVar == dep & Site == site)
  p1 <- ggplot() +
    geom_line(data=dfSub, aes(x=Month, y=mean, group=x, linetype=x), stat="identity", size=1.2) +
    labs(title=sprintf("Study area: Sierra montane (SM)\nIndependent variable: %s\n ", deplong),
         y=sprintf("Mean partial dependence (°C)"), linetype="Category") +
    #scale_x_discrete(limits=c(0,365), breaks=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
    scale_y_continuous(limits=c(-0.5, 2.6)) +
    theme_bw() +
    theme(plot.title = element_text(size=13),
          axis.title = element_text(size=13),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.4),
          axis.text = element_text(size=13),
          strip.text = element_text(size=13),
          legend.title = element_text(size=13),
          legend.text = element_text(size=13),
          legend.position="top", text = element_text(size=13),
          legend.direction='vertical',
          legend.justification = c(0, 1))
    
    ggsave(file=sprintf("CategoricalPartialDep_Dep=%s_Ind=%s.png", dep, ind), p1, width=4.5,height=6, dpi=300)
}

#######################################
#######################################


