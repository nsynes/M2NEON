library(ggplot2)
library(grid)
library(plyr)
library(scales)


OutDir <- "D:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/25&26_Merged"
SmallCanDir <- "D:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/26_DEMSolar_2.5mCanopy_NoShrub_Foothills"
LargeCanDir <- "D:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/25_DEMSolar_10mCutCanopy_NoShrub_Foothills"
SmallCanName <- "Canopy.Density.SouthRad2.5m"
LargeCanName <- "Canopy.Density.SouthRad10mCut"

#################
# Small Canopy
df <- read.csv(sprintf("%s/MergedGbmData.csv", SmallCanDir))

N <- length(unique(df$DependentVar)) * length(unique(df$Period)) * length(unique(df$Site))
my.list <- vector("list", N)
i <- 1
for (dep in unique(df$DependentVar)) {
  for (per in unique(df$Period)) {
    for (site in unique(df$Site)) {
      my.list[[i]] <- data.frame(FullNameDependentVar = sprintf("Sensor.Day%s.%s", per, dep),
                     DependentVar = dep,
                     Period = per,
                     Site = site,
                     ModelRsquared = unique(subset(df, DependentVar == dep & Period == per & Site == site)$ModelRsquared)[1])
      i <- i + 1
    }
  }
}
foo <- do.call('rbind', my.list)
foo$IntervalPeriod <- "Daily"
foo$IndependentVar <- LargeCanName
foo$IndependentVarPeriod <- NA
foo$RelInf <- 0
foo$Rank <- NA

dfSmallCanGbm <- rbind(df, foo)
df <- NULL
foo <- NULL
##########################

###################
# Large canopy
df <- read.csv(sprintf("%s/MergedGbmData.csv", LargeCanDir))

N <- length(unique(df$DependentVar)) * length(unique(df$Period)) * length(unique(df$Site))
my.list <- vector("list", N)
i <- 1
for (dep in unique(df$DependentVar)) {
  for (per in unique(df$Period)) {
    for (site in unique(df$Site)) {
      my.list[[i]] <- data.frame(FullNameDependentVar = sprintf("Sensor.Day%s.%s", per, dep),
                                 DependentVar = dep,
                                 Period = per,
                                 Site = site,
                                 ModelRsquared = unique(subset(df, DependentVar == dep & Period == per & Site == site)$ModelRsquared)[1])
      i <- i + 1
    }
  }
}
foo <- do.call('rbind', my.list)
foo$IntervalPeriod <- "Daily"
foo$IndependentVar <- SmallCanName
foo$IndependentVarPeriod <- NA
foo$RelInf <- 0
foo$Rank <- NA

dfLargeCanGbm <- rbind(df, foo)
df <- NULL
foo <- NULL
############################


Cols <- colnames(dfSmallCanGbm)

colnames(dfLargeCanGbm) <- sprintf("%s.LargeCanopy",colnames(dfLargeCanGbm))
colnames(dfSmallCanGbm) <- sprintf("%s.SmallCanopy",colnames(dfSmallCanGbm))

df <- cbind(dfSmallCanGbm, dfLargeCanGbm)
dfSmallCanGbm <- NULL
dfLargeCanGbm <- NULL


df$WhichModel <- ifelse(df$ModelRsquared.LargeCan > df$ModelRsquared.SmallCan, "LargeCanopy", "SmallCanopy")

for (col in Cols) {
  df[,col] <- ifelse(df$WhichModel == "LargeCanopy", as.character(df[,sprintf("%s.LargeCanopy", col)]), as.character(df[,sprintf("%s.SmallCanopy", col)]))
  df[,sprintf("%s.LargeCanopy", col)] <- NULL
  df[,sprintf("%s.SmallCanopy", col)] <- NULL
}
df$WhichModel <- NULL
df$FullNameDependentVar <- as.factor(df$FullNameDependentVar)
df$DependentVar <- as.factor(df$DependentVar)
df$IntervalPeriod <- as.factor(df$IntervalPeriod)
df$Period <- as.numeric(df$Period)
df$Site <- as.factor(df$Site)
df$IndependentVar <- as.factor(df$IndependentVar)
df$RelInf <- as.numeric(df$RelInf)
df$Rank <- as.numeric(df$Rank)
df$ModelRsquared <- as.numeric(df$ModelRsquared)

write.csv(df, sprintf("%s/MergedGbmData.csv", OutDir), row.names=FALSE)



# Print out which model is best (could add in here automatic copying of the correct simulation folders)
listDirs <- list()
for (site in unique(df$Site)) {
  for (depvar in unique(df$DependentVar)) {
    for (day in unique(df$Period)) {
      foo <- subset(df, Site == site & DependentVar == depvar & Period == day)
      if (foo[foo$IndependentVar == LargeCanName,]$RelInf > 0) {
        cat(paste(site, depvar, day, LargeCanName, "\n"))
        SimDir <- sprintf("%s/ModelDirs/Site(s)=%s_y=Sensor.Day%s.%s", LargeCanDir,site,day,depvar)
      }
      else {
        cat(paste(site, depvar, day, SmallCanName, "\n"))
        SimDir <- sprintf("%s/ModelDirs/Site(s)=%s_y=Sensor.Day%s.%s", SmallCanDir,site,day,depvar)
      }
      listDirs <- c(listDirs, SimDir)
    }
  }
}


write.csv(listDirs, "C:/Users/nsynes/Desktop/SimList.csv")



