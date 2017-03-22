library(ggplot2)
library(grid)
library(plyr)
library(scales)


#################
# Small Canopy
df <- read.csv(sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/11_GBM_2013_Daily_DEMDSM_SmallCanopy/MergedGbmData.csv"))

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
foo$IndependentVar <- "Canopy.Density.SouthRad10m"
foo$IndependentVarPeriod <- NA
foo$RelInf <- 0
foo$Rank <- NA

dfSmallCanGbm <- rbind(df, foo)
df <- NULL
foo <- NULL
##########################

###################
# Large canopy
df <- read.csv(sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/12_GBM_2013_Daily_DEMDSM_LargeCanopy/MergedGbmData.csv"))

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
foo$IndependentVar <- "Canopy.Density.SouthRad2.5m"
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


