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
# function to wrap text in title
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

for (dep in c("Max","Min","DiurnalRange")) {
  if (dep == "Min") varlong <- "minimum temperature"
  if (dep == "Max") varlong <- "maximum temperature"
  if (dep == "DiurnalRange") varlong <- "diurnal range"
  for (ind in unique(dfOutDif$IndVar)) {
    dfSub <- subset(dfOutDif, DepVar == dep & IndVar == ind)
    # Graph Indep.DistToStreamOverFlowAccum inverse to the other independent variables
    # since it makes sense to consider this in terms of cold air pooling and
    # a cooling effect at when close to a stream, rather than a warming effect at distance from a stream
    if (ind == "Indep.DistToStreamOverFlowAccum") {
      dfSub$dif <- -dfSub$dif
      yaxislabel <- "Change in partial dependence\nf(min(x)) - f(max(x))"
    }
    else {
      yaxislabel <- "Change in partial dependence\nf(max(x)) - f(min(x))"
    }
    dfMonth <- ddply(dfSub,~Month+Site,summarise,mean=mean(dif))
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
      
      ggsave(file=sprintf("PartialDepDifference_Dep=%s_Ind=%s.png", dep, ind), p1, width=6,height=10, dpi=300)
      
      
      p2 <- ggplot() +
        geom_bar(data=dfMonth, aes(x=Month, y=mean), stat="identity", width=1, color="black", fill="dark grey") +
        geom_hline(yintercept=0, color="black") +
        facet_wrap(~Site, ncol=1) +
        labs(title=wrapper(sprintf("Partial dependence of %s in\nmodels of daily %s", substr(ind,7,nchar(ind)), varlong), width=60),
             y = yaxislabel) +
        theme_bw()
      
      ggsave(file=sprintf("PartialDepDifferenceMeanBar_Dep=%s_Ind=%s.png", dep, ind), p2, width=6,height=10, dpi=300)

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


