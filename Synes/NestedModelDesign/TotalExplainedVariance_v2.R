library(ggplot2)
library(grid)
library(plyr)
library(scales)
library(Hmisc)

SimDir <- "8_Without90m"
dfAtmosTransSF <- read.csv("C:/Dropbox/Work/ASU/Paper_2/ANALYSIS/AtmosphericTransmittance/SJER_2013.csv")
dfAtmosTransSF$Site <- as.factor("SJER")
dfAtmosTransSM <- read.csv("C:/Dropbox/Work/ASU/Paper_2/ANALYSIS/AtmosphericTransmittance/TEAK_2013.csv")
dfAtmosTransSM$Site <- as.factor("TEF")
dfAtmosTrans <- rbind(dfAtmosTransSF, dfAtmosTransSM)
dfAtmosTransSF <- NULL
dfAtmosTransSM <- NULL
dfAtmosTrans$H0 <- NULL
dfAtmosTrans$G <- NULL
dfAtmosTrans$Date <- NULL
dfAtmosTrans <- plyr::rename(dfAtmosTrans, c("G.H0" = "AtmosTrans",
                                       "yday" = "Day"))

# Remove atmos trans where models were not run:
dfAtmosTrans <- subset(dfAtmosTrans, Site == "SJER" | (Site == "TEF" & Day >= 72))

dfBlanks <- data.frame(Period=c(1:365,1:365,
                                1:365,1:365),
                       DependentVar=c(replicate(365,"Max"), replicate(365,"Min"),
                                      replicate(365,"Max"), replicate(365,"Min")),
                       Site=c(replicate(730,"SJER"),replicate(730,"TEF")))

setwd(file.path("C:/Dropbox/Work/ASU/Paper_2/ANALYSIS/NestedModel/Results", SimDir, "SiteLevel"))
dfSite <- read.csv(sprintf("MergedGbmData.csv"))
dfSite$Site <- revalue(dfSite$Site, c("Sierra foothills"="SJER", "Sierra montane"="TEF"))
dfSite <- merge(dfSite, dfBlanks, by=c("Period","DependentVar","Site"), all=TRUE)
dfSite$IndependentVar <- NULL
dfSite$IndependentVarPeriod <- NULL
dfSite$RelInf <- NULL
dfSite$Rank <- NULL
dfSite <- dfSite[!duplicated(dfSite),]
names(dfSite)[names(dfSite) == "ModelRsquared"] = "SiteRsquared"
#names(dfSite)[names(dfSite) == "ModelRsquared"] = "DistanceOverFlowRsquared"
names(dfSite)[names(dfSite) == "Period"] = "Day"

setwd(file.path("C:/Dropbox/Work/ASU/Paper_2/ANALYSIS/NestedModel/Results", SimDir, "MicrositeLevel"))
dfMicrosite <- read.csv(sprintf("MergedGbmData.csv"))
dfMicrosite$Site <- revalue(dfMicrosite$Site, c("Sierra foothills"="SJER", "Sierra montane"="TEF"))
dfMicrosite <- merge(dfMicrosite, dfBlanks, by=c("Period","DependentVar","Site"), all=TRUE)
dfMicrosite$IndependentVar <- NULL
dfMicrosite$IndependentVarPeriod <- NULL
dfMicrosite$RelInf <- NULL
dfMicrosite$Rank <- NULL
dfMicrosite <- dfMicrosite[!duplicated(dfMicrosite),]
names(dfMicrosite)[names(dfMicrosite) == "ModelRsquared"] = "MicrositeRsquared"
#names(dfMicrosite)[names(dfMicrosite) == "ModelRsquared"] = "DistanceRsquared"
names(dfMicrosite)[names(dfMicrosite) == "Period"] = "Day"

dfRsquared <- merge(dfSite, dfMicrosite, by=c("FullNameDependentVar","DependentVar","IntervalPeriod","Day","Site"), all=TRUE)
dfSite <- NULL
dfMicrosite <- NULL
dfRsquared$TotalExplainedVariance <- dfRsquared$SiteRsquared + ((1 - dfRsquared$SiteRsquared) * dfRsquared$MicrositeRsquared)
#dfRsquared$TotalExplainedVariance <- dfRsquared$DistanceOverFlowRsquared - dfRsquared$DistanceRsquared


if (FALSE) {
var <- "Max"

p1 <- ggplot() + facet_wrap(~Site) +
  geom_point(data=subset(dfRsquared, DependentVar==var), aes(x=DistanceRsquared, y=DistanceOverFlowRsquared), color="red", size=1) +
  geom_abline(slope=1) +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0,1)) +
  labs(title=sprintf("Models of %s temperature", var)) +
  theme_bw()

ggsave(file=sprintf("ColdAirPoolingModelComparison_Dep=%s.png", var), p1, width=8,height=6, dpi=500)
}

#########################
# Boxplots of Rsquared for two versions of model
#########################
if (FALSE) {
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

ggsave(file=sprintf("ColdAirPoolingModelComparison_boxplot_Dep=%s.png", var), p2, width=8,height=4, dpi=500)
}


#############################################
# Boxplots of Total Variance for two versions of model
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

setwd(file.path("C:/Dropbox/Work/ASU/Paper_2/ANALYSIS/NestedModel/Results", SimDir))

foo <- rbind(data.frame(Day = df$Day, DependentVar = df$DependentVar, Site = df$Site, Value = df$TotalExplainedVariance, Type = "r2"),
             data.frame(Day = df$Day, DependentVar = df$DependentVar, Site = df$Site, Value = df$AtmosTrans, Type = "AtmosTrans"))

#foo$Site <- revalue(foo$Site, c("Sierra foothills"="SJER", "Sierra montane"="TEF"))

for (var in c("Max","Min")) {
  if (var == "Max") varlong = "Maximum temperature"
  if (var == "Min") varlong = "Minimum temperature"
  
  p1<- ggplot(data=subset(foo, DependentVar == var)) + facet_wrap(~Site, ncol=1) +
    geom_line(aes(x=Day, y=Value, linetype=Type), size=0.5) +
    #geom_line(aes(x=Day, y=TotalExplainedVariance), linetype=1, size=0.7) +
    #geom_line(aes(x=Day, y=AtmosTrans), linetype=2, size=0.5) +
    scale_x_continuous(limits=c(0,365), breaks=c(0,DaysInMonth$CumulativeDays), minor_breaks=NULL, expand=c(0.01,0)) +
    scale_y_continuous(limits=c(0,1)) +
    scale_linetype_discrete(labels=c(expression(paste(r^2)), "Atmospheric transmittance")) +
    labs(x="Day of the year", y="",linetype="") +
    theme_bw() +
    theme(plot.title = element_text(size=15),
     axis.title = element_text(size=13),
     axis.text = element_text(size=13),
     strip.text = element_text(size=13),
     legend.title = element_text(size=13),
     legend.text = element_text(size=13),
     legend.position="bottom", text = element_text(size=13),
     #legend.direction='vertical',
     legend.justification = c(0, 1),
     legend.text.align = 0)
  
  ggsave(file=sprintf("TotalExplainedVariance_Dep=%s.png", var), p1, width=8,height=5, dpi=500)
}


  

#############################################
# Boxplots of Rsquared for micro and topo (site) scales for two versions (max, min) of model
#############################################

df <- merge(dfRsquared, dfAtmosTrans, by=c("Site","Day"), all=TRUE)


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


#df$Site <- revalue(df$Site, c("Sierra foothills"="SJER", "Sierra montane"="TEF"))

for (r2 in c("SiteRsquared","MicrositeRsquared")) {
  foo <- rbind(data.frame(Day = df$Day, DependentVar = df$DependentVar, Site = df$Site, Value = df[,r2], Type = "r2"),
               data.frame(Day = df$Day, DependentVar = df$DependentVar, Site = df$Site, Value = df$AtmosTrans, Type = "AtmosTrans"))

  
  for (var in c("Max","Min")) {
    if (var == "Max") varlong = "Maximum temperature"
    if (var == "Min") varlong = "Minimum temperature"
    if (r2 == "SiteRsquared") scalelong = "Site"
    if (r2 == "MicrositeRsquared") scalelong = "Microsite"
    
    p1<- ggplot(data=subset(foo[!is.na(foo$Value),], DependentVar == var)) + facet_wrap(~Site, ncol=1) +
      geom_line(aes(x=Day, y=Value, linetype=Type), size=0.5) +
      scale_x_continuous(limits=c(0,365), breaks=c(0,DaysInMonth$CumulativeDays), minor_breaks=NULL, expand=c(0.01,0)) +
      scale_y_continuous(limits=c(0,1)) +
      scale_linetype_discrete(labels=c(expression(paste(r^2)), "Atmospheric transmittance")) +
      labs(x="Day of the year", y="",linetype="") +
      theme_bw() +
      theme(plot.title = element_text(size=15),
            axis.title = element_text(size=13),
            axis.text = element_text(size=13),
            strip.text = element_text(size=13),
            legend.title = element_text(size=13),
            legend.text = element_text(size=13),
            legend.position="bottom", text = element_text(size=13),
            #legend.direction='vertical',
            legend.justification = c(0, 1),
            legend.text.align = 0)
    
    ggsave(file=sprintf("%sRsquared_Dep=%s.png", scalelong, var), p1, width=8,height=5, dpi=500)
  }
}
  
  





