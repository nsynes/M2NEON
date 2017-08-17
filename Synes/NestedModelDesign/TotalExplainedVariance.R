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


setwd(sprintf("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Results/5_DistToStreamOverFlowAccum/SiteLevel"))
dfSite <- read.csv(sprintf("MergedGbmData.csv"))
dfSite$IndependentVar <- NULL
dfSite$IndependentVarPeriod <- NULL
dfSite$RelInf <- NULL
dfSite$Rank <- NULL
dfSite <- dfSite[!duplicated(dfSite),]
#names(dfSite)[names(dfSite) == "ModelRsquared"] = "SiteRsquared"
names(dfSite)[names(dfSite) == "ModelRsquared"] = "DistanceOverFlowRsquared"
names(dfSite)[names(dfSite) == "Period"] = "Day"

#setwd(sprintf("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Results/5_DistToStreamOverFlowAccum/MicrositeLevel"))
setwd(sprintf("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Results/3_DistStreams_VariableAtmosTrans/SiteLevel"))
dfMicrosite <- read.csv(sprintf("MergedGbmData.csv"))
dfMicrosite$IndependentVar <- NULL
dfMicrosite$IndependentVarPeriod <- NULL
dfMicrosite$RelInf <- NULL
dfMicrosite$Rank <- NULL
dfMicrosite <- dfMicrosite[!duplicated(dfMicrosite),]
#names(dfMicrosite)[names(dfMicrosite) == "ModelRsquared"] = "MicrositeRsquared"
names(dfMicrosite)[names(dfMicrosite) == "ModelRsquared"] = "DistanceRsquared"
names(dfMicrosite)[names(dfMicrosite) == "Period"] = "Day"

dfRsquared <- merge(dfSite, dfMicrosite, by=c("FullNameDependentVar","DependentVar","IntervalPeriod","Day","Site"))
dfSite <- NULL
dfMicrosite <- NULL
#dfRsquared$TotalExplainedVariance <- dfRsquared$SiteRsquared + ((1 - dfRsquared$SiteRsquared) * dfRsquared$MicrositeRsquared)
dfRsquared$TotalExplainedVariance <- dfRsquared$DistanceOverFlowRsquared - dfRsquared$DistanceRsquared



var <- "Max"

p1 <- ggplot() + facet_wrap(~Site) +
  geom_point(data=subset(dfRsquared, DependentVar==var), aes(x=DistanceRsquared, y=DistanceOverFlowRsquared), color="red", size=1) +
  geom_abline(slope=1) +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(title=sprintf("Models of %s temperature", var)) +
  theme_bw()

ggsave(file=sprintf("ColdAirPoolingModelComparison_Dep=%s.png", var), p1, width=8,height=6, dpi=300)


#########################
# Boxplots of Rsquared for two versions of model
#########################
df1 <- dfRsquared[c("Site","Day","DependentVar","DistanceOverFlowRsquared")]
names(df1)[names(df1) == "DistanceOverFlowRsquared"] = "Rsquared"
df1$Model <- as.factor("DistanceOverFlow")


df2 <- dfRsquared[c("Site","Day","DependentVar","DistanceRsquared")]
names(df2)[names(df2) == "DistanceRsquared"] = "Rsquared"
df2$Model <- as.factor("Distance")

df <- rbind(df1,df2)

p2 <- ggplot() + facet_wrap(~Site) +
  geom_boxplot(data=subset(df, DependentVar==var), aes(x=Model, y=Rsquared, fill=Model)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(title=sprintf("Models of %s temperature", var)) +
  theme_bw()

ggsave(file=sprintf("ColdAirPoolingModelComparison_boxplot_Dep=%s.png", var), p2, width=8,height=4, dpi=300)



#####################################
#############################################

df <- merge(dfRsquared, dfAtmosTrans, by=c("Site","Day"))


DaysInMonth <- data.frame(Year=2013, Month=1:12, days=monthDays(as.Date(paste0(2013,"-",1:12,"-01"))))
DaysInMonth <- mutate(DaysInMonth, CumulativeDays=cumsum(days))

df$Month <- NA
for (m in 12:1) {
  df$Month <- ifelse((df$Day <= DaysInMonth[DaysInMonth$Month == m,]$CumulativeDays), m, df$Month)
}
df$MonthSplit <- df$Day
for (m in 2:12) {
  df$MonthSplit <- ifelse(df$Month == m, df$Day - DaysInMonth[DaysInMonth$Month == m-1,]$CumulativeDays, df$MonthSplit)
}

df$Month <- as.factor(df$Month)
df$Month <- revalue(df$Month, c("1"="Jan",
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

setwd("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Results/5_DistToStreamOverFlowAccum")

for (var in c("Max","Min","DiurnalRange")) {
  
  p1<- ggplot(data=subset(df, DependentVar == var)) + facet_wrap(~Site, ncol=1) +
    geom_line(aes(x=Day, y=TotalExplainedVariance), linetype=1, size=0.7) +
    geom_line(aes(x=Day, y=AtmosTrans), linetype=2, size=0.5) +
    scale_x_continuous(limits=c(0,365), breaks=c(0,DaysInMonth$CumulativeDays), minor_breaks=NULL, expand=c(0,0)) +
    scale_y_continuous(limits=c(0,1)) +
    labs(title = sprintf("Dependent variable = %s\n", var), y="") +
    theme_bw()
  
  ggsave(file=sprintf("TotalExplainedVariance_Dep=%s.png", var), p1, width=12,height=4, dpi=300)
  
}


  






















