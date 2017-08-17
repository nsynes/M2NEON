library(ggplot2)
library(grid)
library(plyr)
library(scales)
library(Hmisc)

scale <- "SiteLevel"
var <- "DiurnalRange"


if (var == "Min") varlong <- "minimum temperature"
if (var == "Max") varlong <- "maximum temperature"
if (var == "DiurnalRange") varlong <- "diurnal range"

dfAtmosTransSF <- read.csv("C:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/AtmosphericTransmittance/SJER_2013.csv")
dfAtmosTransSF$Site <- "Sierra foothills"
dfAtmosTransSM <- read.csv("C:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/AtmosphericTransmittance/TEAK_2013.csv")
dfAtmosTransSM$Site <- "Sierra montane"
dfAtmosTrans <- rbind(dfAtmosTransSF, dfAtmosTransSM)
dfAtmosTransSF <- NULL
dfAtmosTransSM <- NULL
dfAtmosTrans$H0 <- NULL
dfAtmosTrans$G <- NULL
dfAtmosTrans$Date <- NULL
dfAtmosTrans$DependentVar <- as.factor("Not applicable")
dfAtmosTrans$IntervalPeriod <- as.factor("Daily")
dfAtmosTrans$IndependentVar <- NA
dfAtmosTrans$type <-as.factor(sprintf("R-squared and Atmospheric Transmittance (%s)", dfAtmosTrans$Site))
dfAtmosTrans$variable <- "Atmospheric Transmittance"
dfAtmosTrans <- plyr::rename(dfAtmosTrans, c("G.H0" = "value",
                                       "yday" = "Period"))

setwd(sprintf("C:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Results/5_DistToStreamOverFlowAccum/%s", scale))
dfGbm <- read.csv(sprintf("MergedGbmData.csv"))

dfRsquared <- dfGbm
dfRsquared$RelInf <- NULL
dfRsquared$Rank <- NULL
dfRsquared$IndependentVarPeriod <- NULL
dfRsquared$FullNameDependentVar <- NULL
dfRsquared$IndependentVar <- NULL
dfRsquared <- dfRsquared[!duplicated(dfRsquared), ]
dfRsquared$value <- dfRsquared$ModelRsquared
dfRsquared$ModelRsquared <- NULL
dfRsquared$type <- as.factor(sprintf("R-squared and Atmospheric Transmittance (%s)", dfRsquared$Site))
dfRsquared$variable <- as.factor("R-squared")
dfRsquared$IndependentVar <- NA
dfRsquared$row.names <- NULL

dfGbm$IndependentVarPeriod <- NULL
dfGbm$FullNameDependentVar <- NULL
dfGbm$Rank <- NULL
dfGbm$ModelRsquared <- NULL
dfGbm$value <- dfGbm$RelInf / 100
dfGbm$type <- as.factor(sprintf("%s", dfGbm$Site))
dfGbm$variable <- as.factor("RelInf")
dfGbm$RelInf <- NULL

# blank data to force graphs to show full year (1 to 365 days)
dfBlank <- rbind(data.frame(DependentVar="Not applicable",
                            IntervalPeriod="Daily",
                            Period=c(1:365),
                            Site="Sierra foothills",
                            IndependentVar=NA,
                            value=0,
                            type="Sierra foothills",
                            variable="blank"),
                 data.frame(DependentVar="Not applicable",
                            IntervalPeriod="Daily",
                            Period=c(1:365),
                            Site="Sierra montane",
                            IndependentVar=NA,
                            value=0,
                            type="Sierra montane",
                            variable="blank"))

df <- rbind(dfGbm, dfRsquared, dfAtmosTrans, dfBlank)

df$type <- factor(df$type, levels=c("Sierra foothills",
                                    "R-squared and Atmospheric Transmittance (Sierra foothills)",
                                    "Sierra montane",
                                    "R-squared and Atmospheric Transmittance (Sierra montane)"))
dfRsquared <- NULL
dfGbm <- NULL
dfAtmosTrans <- NULL
dfBlank <- NULL

##############################
# For models with multiple scales/types per variable category


if (scale == "SiteLevel") {
  listvars <- c("SolarRadiation30m",
                "CanopyDensity.Circle_Radius90m",
                "DistToStreamOverFlowAccum")
  pal <- c("#cccc00",
        RColorBrewer::brewer.pal(9,"Greens")[8:8],
        "#0072B2")
}
if (scale == "MicrositeLevel" & var == "Max") {
  listvars <- c("SolarRadiation2m",
    "Canopy.Density.SouthRad30mCut",
    "Canopy.Density.SouthRad2.5m",
    "GroundCode")
  pal <- c("#cccc00",
        RColorBrewer::brewer.pal(9,"Greens")[8:8],
        RColorBrewer::brewer.pal(9,"Greens")[7:7],
        "#8B6742")
}
if (scale == "MicrositeLevel" & var == "Min") {
  listvars <- c("Canopy.Density.CircleRadius5m",
                "GroundCode",
                "Slope")
  pal <- c(RColorBrewer::brewer.pal(9,"Greens")[5:5],
           "#8B6742",
           RColorBrewer::brewer.pal(9,"Greys")[6:6])
}
if (scale == "MicrositeLevel" & var == "DiurnalRange") {
  listvars <- c("SolarRadiation2m",
                  "Canopy.Density.CircleRadius5m",
                  "GroundCode",
                  "Slope")
  pal <- c("#cccc00",
           RColorBrewer::brewer.pal(9,"Greens")[5:5],
           "#8B6742",
           RColorBrewer::brewer.pal(9,"Greys")[6:6])
}


df$IndependentVar <- factor(df$IndependentVar,
                            levels = listvars)


df$Day <- df$Period
df$Quarter <- NA
df$Quarter <- ifelse(df$Day > 0 & df$Day <= 90, "Jan - Mar", df$Quarter)
df$Quarter <- ifelse(df$Day > 90 & df$Day <= 181, "Apr - Jun", df$Quarter)
df$Quarter <- ifelse(df$Day > 181 & df$Day <= 273, "Jul - Sep", df$Quarter)
df$Quarter <- ifelse(df$Day > 273 & df$Day <= 365, "Oct - Dec", df$Quarter)
df$QuarterSplit <- NA
df$QuarterSplit <- ifelse(df$Quarter == "Jan - Mar", df$Day, df$QuarterSplit)
df$QuarterSplit <- ifelse(df$Quarter == "Apr - Jun", df$Day-90, df$QuarterSplit)
df$QuarterSplit <- ifelse(df$Quarter == "Jul - Sep", df$Day-181, df$QuarterSplit)
df$QuarterSplit <- ifelse(df$Quarter == "Oct - Dec", df$Day-273, df$QuarterSplit)

df$Quarter <- factor(df$Quarter, levels = c("Jan - Mar",
                                                  "Apr - Jun",
                                                  "Jul - Sep",
                                                  "Oct - Dec"))

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

############################
# GRAPHS
############################

############################
# DAILY
############################
dfSub <- subset(df, DependentVar %in% c(var, "Not applicable") & type %in% c("Sierra foothills", "Sierra montane"))

plot <- ggplot(data = dfSub) + facet_wrap(~type, scales="fixed", ncol=1) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  geom_bar(data = subset(dfSub, type == "Sierra foothills"), aes(x=Period, y=value, fill=IndependentVar, color=IndependentVar), stat="identity") +
  #geom_line(data = subset(dfSub, type == "R-squared and Atmospheric Transmittance (Sierra foothills)"), aes(x=Period, y=value, linetype = variable)) +
  geom_bar(data = subset(dfSub, type == "Sierra montane"), aes(x=Period, y=value, fill=IndependentVar, color=IndependentVar), stat="identity") +
  #geom_line(data = subset(dfSub, type == "R-squared and Atmospheric Transmittance (Sierra montane)"), aes(x=Period, y=value, linetype = variable)) +
  scale_x_continuous(limits=c(0,365), breaks=c(0,DaysInMonth$CumulativeDays), minor_breaks=NULL, expand=c(0,0)) +
  labs(title = sprintf("Relative influence of independent variables in models of daily %s temperature", varlong),
       x="Julian day", y="Relative influence", fill = "Independent variable", linetype = "") +
  theme_bw() +
  theme(legend.position="top", text = element_text(size=15)) + guides(color=FALSE)

ggsave(file=sprintf("%sRelInf_Dep=%s_Daily.png", scale, var),
       plot, width=16,height=12, dpi=500)
        



############################
# Monthly bar
############################
dfMonth <- ddply(dfSub,~Month+type+IndependentVar,summarise,mean=mean(value))

plot2 <- ggplot(data = dfMonth, aes(x=Month, y=mean, fill=IndependentVar, color=IndependentVar)) + facet_wrap(~type, scales="fixed", ncol=1) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = sprintf("Relative influence of independent variables in models of daily %s", varlong),
       x="Month", y="Mean relative influence by month", fill = "Independent variable", linetype = "") +
  theme_bw() +
  theme(legend.position="top", text = element_text(size=15)) + guides(color=FALSE)


ggsave(file=sprintf("%sRelInf_Dep=%s_MonthlyBar.png", scale, var),
       plot2, width=16,height=12, dpi=500)


############################
# Monthly area
############################
plot3 <- ggplot(data = dfMonth, aes(x=as.numeric(Month), y=mean, fill=IndependentVar)) + facet_wrap(~type, scales="fixed", ncol=1) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  geom_area() +
  scale_x_continuous(breaks = 1:12, labels = levels(dfMonth$Month)) +
  labs(title = sprintf("Relative influence of independent variables in models of daily %s", varlong),
       x="Month", y="Mean relative influence by month", fill = "Independent variable", linetype = "") +
  theme_bw() +
  theme(legend.position="top", text = element_text(size=15)) + guides(color=FALSE)


ggsave(file=sprintf("%sRelInf_Dep=%s_Monthly.png", scale, var),
       plot3, width=16,height=12, dpi=500)

##############################################
# Scatter plot AtmosTrans~R-squared
#################################################
if (FALSE) {
  for (var in c("Min","Max")) {
    dfR <- subset(df, variable == "R-squared")
    dfR$Rsquared <- dfR$value
    dfR$value <- NULL
    dfR$variable <- NULL
    dfA <- subset(df, variable == "Atmospheric Transmittance")
    dfA$AtmosTrans <- dfA$value
    dfA$value <- NULL
    dfA$variable <- NULL
    dfScatter <- merge(dfR, dfA, by=c("IntervalPeriod","Period","Day","Quarter","QuarterSplit","Month","MonthSplit","Site"))
    dfScatter <- subset(dfScatter, DependentVar.x == var)
    
    plot <- ggplot() +
      geom_point(data=dfScatter, aes(x=AtmosTrans, y=Rsquared, color=QuarterSplit), size=2) +
      facet_grid(Site~Quarter) +
      labs(title=sprintf("Sensor variable = %s", var), x="Atmospheric Transmittance", y="R-squared", color="Day") +
      theme_bw()
    
    ggsave(file=sprintf("Scatter_AtmosTrans~Rsquared_%s_Daily.png", var),
           plot, width=10,height=6, dpi=500)
  }
}
#################################################
#################################################




if (FALSE) {
dfSub <- subset(df, DependentVar %in% c("Max", "Not applicable") & type == "R-squared and Atmospheric Transmittance (Sierra montane)" & variable=="Atmospheric Transmittance")

plotAT <- ggplot() +
  geom_line(data = dfSub, aes(x=Period, y=value, linetype = variable)) +
  labs(title = sprintf("Dependent (sensor) variable = %s\n", var), x="Day of Year (2013)", y="Atmospheric\nTransmittance", fill = "Independent variable", linetype = "") +
  lims(x=c(75,350)) +
  theme_bw() +
  theme(legend.position="top", text = element_text(size=15)) + guides(color=FALSE)

ggsave(file=sprintf("AtmosTrans_SM.png", var),
       plotAT, width=10,height=6, dpi=300)
}


if (FALSE) {
# Subset to only days with a certain level of transmittance
fooSF <- subset(df, Site == "Sierra foothills" & variable == "Atmospheric Transmittance" & value >= 0.7)
DaysSF <- unique(fooSF$Period)

fooSM <- subset(df, Site == "Sierra montane" & variable == "Atmospheric Transmittance" & value >= 0.7)
DaysSM <- unique(fooSM$Period)

dfHighTran <- subset(df, (Site == "Sierra foothills" & Period %in% DaysSF) |
                         (Site == "Sierra montane" & Period %in% DaysSM))


for (var in c("Min","Mean","Max","DiurnalRange")) {
  dfHighTranSub <- subset(dfHighTran, DependentVar %in% c(var, "Not applicable"))
  
  plot <- ggplot(data = dfHighTranSub) + facet_wrap(~type, scales="fixed", ncol=1) +
    scale_fill_manual(values = pal) +
    scale_color_manual(values = pal) +
    geom_bar(data = subset(dfHighTranSub, type == "Relative Influence (Sierra foothills)"), aes(x=Period, y=value, fill=IndependentVar, color=IndependentVar), stat="identity") +
    geom_line(data = subset(dfHighTranSub, type == "R-squared and Atmospheric Transmittance (Sierra foothills)"), aes(x=Period, y=value, linetype=variable)) +
    #geom_point(data = subset(dfHighTranSub, type == "R-squared and Atmospheric Transmittance (Sierra foothills)"), aes(x=Period, y=value, shape=variable)) +
    geom_bar(data = subset(dfHighTranSub, type == "Relative Influence (Sierra montane)"), aes(x=Period, y=value, fill=IndependentVar, color=IndependentVar), stat="identity") +
    geom_line(data = subset(dfHighTranSub, type == "R-squared and Atmospheric Transmittance (Sierra montane)"), aes(x=Period, y=value, linetype=variable)) +
    #geom_point(data = subset(dfHighTranSub, type == "R-squared and Atmospheric Transmittance (Sierra montane)"), aes(x=Period, y=value, shape=variable)) +
    labs(title = sprintf("Dependent (sensor) variable = %s\n(Days with Atmospheric Transmittance < 0.7 have been removed)", var),
         x="Day of Year (2013)", y="", fill = "Independent variable", linetype = "") +
    theme_bw() +
    theme(legend.position="top", text = element_text(size=15)) + guides(color=FALSE)
  
  ggsave(file=sprintf("Dep=%s_Daily_AtmosTransAbove0.7.png", var),
         plot, width=16,height=12, dpi=500)
  
}
}





