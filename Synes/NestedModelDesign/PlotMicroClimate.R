###############################
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # No, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(No    = length2(xx[[col]], na.rm=na.rm),
                     max = max(xx[[col]], na.rm=na.rm),
                     min = min(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  #datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$No)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=No-1
  ciMult <- qt(conf.interval/2 + .5, datac$No-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
###############################


library(ggplot2)
library(reshape2)
library(tidyr)
library(plyr)
library(Hmisc)


df <- read.csv("C:/Dropbox/Work/ASU/SensorData/Merged_RasterAndSensorData_2013.csv")

df$Point.Site <- revalue(df$Point.Site, c("SF"="SJER", "SM"="TEF"))

# Get rid of independent variables and diurnal range
df <- df[substr(colnames(df),nchar(colnames(df)) - 3,nchar(colnames(df))) %in% c(".Max",".Min") &
           substr(colnames(df),1,nchar("Sensor")) == "Sensor" |
           substr(colnames(df),1,nchar("Point")) == "Point"]
df$Garden <- as.factor(substr(df$Point.loc_ID,5,7))
df$GardenNo <- as.factor(substr(df$Point.loc_ID,7,7))
df$Point.coords.x1 <- NULL
df$Point.coords.x2 <- NULL
df$Point.Point_Type <- NULL
df$Point.GNSS_Heigh <- NULL
df$Point.Vert_Prec <- NULL
df$Point.Horz_Prec <- NULL

# Remove landscape sensors
df <- subset(df, Garden != "sf7" & Garden != "sm7")

df$Garden <- as.factor(paste(df$Point.Site, df$GardenNo))

df$Garden <- revalue(df$Garden,
                     c("SJER 1" = "SJER 1 (North)",
                       "SJER 2" = "SJER 2 (South)",
                       "SJER 3" = "SJER 3 (Valley)",
                       "SJER 4" = "SJER 4 (North)",
                       "SJER 5" = "SJER 5 (South)",
                       "SJER 6" = "SJER 6 (Valley)",
                       "TEF 1" = "TEF 1 (North)",
                       "TEF 2" = "TEF 2 (South)",
                       "TEF 3" = "TEF 3 (Valley)",
                       "TEF 4" = "TEF 4 (North)",
                       "TEF 5" = "TEF 5 (South)",
                       "TEF 6" = "TEF 6 (Valley)"))
df$Garden <- factor(df$Garden, 
                    levels=c("SJER 1 (North)",
                             "TEF 1 (North)",
                             "SJER 2 (South)",
                             "TEF 2 (South)",
                             "SJER 3 (Valley)",
                             "TEF 3 (Valley)",
                             "SJER 4 (North)",
                             "TEF 4 (North)",
                             "SJER 5 (South)",
                             "TEF 5 (South)",
                             "SJER 6 (Valley)",
                             "TEF 6 (Valley)"))


foo <- melt(df, measure.vars = colnames(df[substr(colnames(df),1,nchar("Sensor")) == "Sensor"]), variable.name = "FullVar")
foo$Day <- as.numeric(substr(foo$FullVar, 11, nchar(as.character(foo$FullVar)) - 4))
foo$Variable <- as.factor(substr(foo$FullVar, nchar(as.character(foo$FullVar)) - 2, nchar(as.character(foo$FullVar))))
foo <- foo[!is.na(foo$value),]
foo$FullVar <- NULL



########################################
# 2 SD version
########################################

bar <- summarySE(foo, measurevar="value", groupvars=c("Point.Site","Garden","Day","Variable"))
bar <- subset(bar, No > 1)
bar$No<-NULL
bar$max<-NULL
bar$min<-NULL
bar$se<-NULL
bar$ci<-NULL

bar$Variable <- revalue(bar$Variable, c("Max"="Mean daily max (± 2xSD)", "Min"="Mean daily min (± 2xSD)"))

DaysInMonth <- data.frame(Year=2013, Month=1:12, days=monthDays(as.Date(paste0(2013,"-",1:12,"-01"))))
DaysInMonth <- mutate(DaysInMonth, CumulativeDays=cumsum(days))

p1 <- ggplot(data=bar) + facet_wrap(~Garden, ncol=2) +
  geom_line(aes(x=Day, y=mean, colour=Variable)) +
  geom_ribbon(aes(x=Day, ymin=(mean-(2*sd)), ymax=(mean+(2*sd)), fill=Variable), alpha = 0.4) +
  scale_colour_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue")) +
  scale_x_continuous(limits=c(0,365), breaks=c(0,DaysInMonth$CumulativeDays), minor_breaks=NULL, expand=c(0.01,0)) +
  scale_y_continuous(breaks=c(0,20,40,60)) +
  labs(x="Day of the year", y="Temperature °C", color="", fill="") +
  theme_bw() +
  theme(legend.position="bottom",text = element_text(size=15))

ggsave(file=sprintf("C:/Dropbox/Work/PaperWriting/ASU_Microclimate/drafts/Full MS/Figures/MicroScaleTemperature_2SD.png"), p1, width=10,height=12, dpi=500)

########################################
########################################



########################################
# 1 SD version
########################################

bar <- summarySE(foo, measurevar="value", groupvars=c("Point.Site","Garden","Day","Variable"))
bar <- subset(bar, No > 1)
bar$No<-NULL
bar$max<-NULL
bar$min<-NULL
bar$se<-NULL
bar$ci<-NULL

bar$Variable <- revalue(bar$Variable, c("Max"="Mean daily max (±SD)", "Min"="Mean daily min (±SD)"))

DaysInMonth <- data.frame(Year=2013, Month=1:12, days=monthDays(as.Date(paste0(2013,"-",1:12,"-01"))))
DaysInMonth <- mutate(DaysInMonth, CumulativeDays=cumsum(days))

p2 <- ggplot(data=bar) + facet_wrap(~Garden, ncol=2) +
  geom_line(aes(x=Day, y=mean, colour=Variable)) +
  geom_ribbon(aes(x=Day, ymin=(mean-(sd)), ymax=(mean+(sd)), fill=Variable), alpha = 0.4) +
  scale_colour_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue")) +
  scale_x_continuous(limits=c(0,365), breaks=c(0,DaysInMonth$CumulativeDays), minor_breaks=NULL, expand=c(0.01,0)) +
  scale_y_continuous(breaks=c(0,20,40,60)) +
  labs(x="Day of the year", y="Temperature °C", color="", fill="") +
  theme_bw() +
  theme(legend.position="bottom",text = element_text(size=15))

ggsave(file=sprintf("C:/Dropbox/Work/PaperWriting/ASU_Microclimate/drafts/Full MS/Figures/MicroScaleTemperature_1SD.png"), p2, width=10,height=12, dpi=500)

########################################
########################################











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

foo$Garden <- revalue(foo$Garden,
                     c("SJER 1 (North)"="1\n(North)",
                       "SJER 2 (South)"="2\n(South)",
                       "SJER 3 (Valley)"="3\n(Valley)",
                       "SJER 4 (North)"="4\n(North)",
                       "SJER 5 (South)"="5\n(South)",
                       "SJER 6 (Valley)"="6\n(Valley)",
                       "TEF 1 (North)"="1\n(North)",
                       "TEF 2 (South)"="2\n(South)",
                       "TEF 3 (Valley)"="3\n(Valley)",
                       "TEF 4 (North)"="4\n(North)",
                       "TEF 5 (South)"="5\n(South)",
                       "TEF 6 (Valley)"="6\n(Valley)"))
foo$VarGard <- interaction(foo$Garden, foo$Variable)



for (site in c("SJER","TEF")) {
  for (day in 1:365) {
    
    atmostrans <- NULL
    fooSub <- NULL
    p3 <- NULL
    
    fooSub <- subset(foo, Day==day & Point.Site == site)
    
    if (nrow(fooSub) > 0) {
    
      atmostrans <- subset(dfAtmosTrans, Day == day & Site == site)$AtmosTrans
  
      p3 <- ggplot(data=fooSub) +
        stat_boxplot(aes(x=Garden, y=value, ymin=min(value), ymax=max(value), group=VarGard, color=Variable),
                     width=1.5, coef=10, geom ='errorbar', position=position_dodge(0)) +
        geom_boxplot(aes(x=Garden, y=value, ymin=min(value), ymax=max(value), group=VarGard, color=Variable, fill=Variable),
                     width=1.5, coef=10, alpha=0.3, position=position_dodge(0)) +
        scale_fill_manual(values=c("red", "blue")) +
        scale_colour_manual(values=c("red", "blue")) +
        scale_x_discrete(expand=c(0.01,0)) +
        scale_y_continuous(limits=c(-14,71), breaks=c(-10,0,10,20,30,40,50,60)) +
        geom_jitter(aes(x=Garden, y=value), width = 0.1, size=0.5) +
        labs(title=sprintf("Study site: %s\nDay: %s\nAtmospheric transmittance: %s",
                           site,day,format(round(atmostrans, 2), nsmall = 2)),
             y="Temperature °C") +
        theme_bw() +
        theme(legend.position="bottom",text = element_text(size=8))
      
      ggsave(file=sprintf("C:/Dropbox/Work/PaperWriting/ASU_Microclimate/drafts/Full MS/Figures/MicroScaleBoxplots/MicroScaleTemp_%s_Day%s.png",
                                  site,day), p3, width=4,height=4, dpi=500)
      
    }
    
  }
}






