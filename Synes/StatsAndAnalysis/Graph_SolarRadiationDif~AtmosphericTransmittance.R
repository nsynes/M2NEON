library(ggplot2)
library(grid)
library(plyr)
library(scales)
library(Hmisc)

dfAtmosTransSF <- read.csv("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/AtmosphericTransmittance/SJER_2013.csv")
dfAtmosTransSF$Site <- as.factor("Sierra foothills")
dfAtmosTransSM <- read.csv("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/AtmosphericTransmittance/TEAK_2013.csv")
dfAtmosTransSM$Site <- as.factor("Sierra montane")
dfAtmosTrans <- rbind(dfAtmosTransSF, dfAtmosTransSM)
dfAtmosTransSF <- NULL
dfAtmosTransSM <- NULL
dfAtmosTrans$H0 <- NULL
dfAtmosTrans$G <- NULL
dfAtmosTrans$Date <- NULL
dfAtmosTrans <- plyr::rename(dfAtmosTrans, c("G.H0" = "AtmosTrans",
                                       "yday" = "Day"))




setwd(sprintf("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Results/3_DistStreams_VariableAtmosTrans/SiteLevel"))
dfVariableAtmosGbm <- read.csv(sprintf("MergedGbmData.csv"))
dfVariableAtmosGbm$IndependentVar <- NULL
dfVariableAtmosGbm$IndependentVarPeriod <- NULL
dfVariableAtmosGbm$RelInf <- NULL
dfVariableAtmosGbm$Rank <- NULL
dfVariableAtmosGbm <- dfVariableAtmosGbm[!duplicated(dfVariableAtmosGbm),]
names(dfVariableAtmosGbm)[names(dfVariableAtmosGbm) == "ModelRsquared"] = "RsquaredVarAT"
names(dfVariableAtmosGbm)[names(dfVariableAtmosGbm) == "Period"] = "Day"

setwd(sprintf("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Results/4_DistStreams_ConstAtmosTrans/SiteLevel"))
dfConstAtmosGbm <- read.csv(sprintf("MergedGbmData.csv"))
dfConstAtmosGbm$IndependentVar <- NULL
dfConstAtmosGbm$IndependentVarPeriod <- NULL
dfConstAtmosGbm$RelInf <- NULL
dfConstAtmosGbm$Rank <- NULL
dfConstAtmosGbm <- dfConstAtmosGbm[!duplicated(dfConstAtmosGbm),]
names(dfConstAtmosGbm)[names(dfConstAtmosGbm) == "ModelRsquared"] = "RsquaredConstAT"
names(dfConstAtmosGbm)[names(dfConstAtmosGbm) == "Period"] = "Day"


dfRsquared <- merge(dfConstAtmosGbm, dfVariableAtmosGbm, by=c("FullNameDependentVar","DependentVar","IntervalPeriod","Day","Site"))
dfConstAtmosGbm <- NULL
dfVariableAtmosGbm <- NULL
dfRsquared$RsquaredDif <- dfRsquared$RsquaredVarAT - dfRsquared$RsquaredConstAT


df <- merge(dfRsquared, dfAtmosTrans, by=c("Site","Day"))




ggplot(data=df) + facet_wrap(~DependentVar) +
  geom_point(aes(x=Day, y=RsquaredDif, color=AtmosTrans), size=2) +
  geom_hline(yintercept=0) +
  labs(title = "RsquaredDif = VariableAT_Rsquared - ConstAT_Rsquared") +
  theme_bw()











