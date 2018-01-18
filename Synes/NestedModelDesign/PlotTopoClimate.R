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

df <- read.csv("C:/Dropbox/Work/ASU/Paper_2/ANALYSIS/NestedModel/Input/SiteLevel_AllVars.csv")

df$Point.Site <- revalue(df$Point.Site, c("Sierra foothills"="SJER", "Sierra montane"="TEF"))

# Get rid of independent variables and diurnal range
df <- df[substr(colnames(df),nchar(colnames(df)) - nchar("DiurnalRange")+1,nchar(colnames(df))) != "DiurnalRange" &
         substr(colnames(df),1,nchar("Sensor")) == "Sensor" |
         substr(colnames(df),1,nchar("Point")) == "Point"]
df$Point.Garden <- NULL
df$Point.loc_ID <- NULL

foo <- melt(df, measure.vars = colnames(df[3:length(colnames(df))]), variable.name = "FullVar")
foo$Day <- as.numeric(substr(foo$FullVar, 11, nchar(as.character(foo$FullVar)) - 4))
foo$Variable <- as.factor(substr(foo$FullVar, nchar(as.character(foo$FullVar)) - 2, nchar(as.character(foo$FullVar))))
foo <- foo[!is.na(foo$value),]
foo$FullVar <- NULL

bar <- summarySE(foo, measurevar="value", groupvars=c("Point.Site","Day","Variable"))
bar <- subset(bar, No > 1)
bar$No<-NULL
bar$max<-NULL
bar$min<-NULL
bar$se<-NULL
bar$ci<-NULL

bar$Variable <- revalue(bar$Variable, c("Max"="Mean daily max (± 2xSD)", "Min"="Mean daily min (± 2xSD)"))

DaysInMonth <- data.frame(Year=2013, Month=1:12, days=monthDays(as.Date(paste0(2013,"-",1:12,"-01"))))
DaysInMonth <- mutate(DaysInMonth, CumulativeDays=cumsum(days))

p1 <- ggplot(data=bar) + facet_wrap(~Point.Site, ncol=1) +
  geom_line(aes(x=Day, y=mean, colour=Variable)) +
  geom_ribbon(aes(x=Day, ymin=(mean-(2*sd)), ymax=(mean+(2*sd)), fill=Variable), alpha = 0.4) +
  scale_colour_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue")) +
  scale_x_continuous(limits=c(0,365), breaks=c(0,DaysInMonth$CumulativeDays), minor_breaks=NULL, expand=c(0.01,0)) +
  scale_y_continuous(breaks=c(0,20,40,60)) +
  labs(x="Day of the year", y="Temperature °C", color="", fill="") +
  theme_bw() +
  theme(legend.position="bottom",text = element_text(size=15))

ggsave(file=sprintf("C:/Dropbox/Work/PaperWriting/ASU_Microclimate/drafts/Full MS/Figures/TopoScaleTemperature_2SD.png"), p1, width=8,height=6, dpi=500)





bar <- summarySE(foo, measurevar="value", groupvars=c("Point.Site","Day","Variable"))
bar <- subset(bar, No > 1)
bar$No<-NULL
bar$max<-NULL
bar$min<-NULL
bar$se<-NULL
bar$ci<-NULL

bar$Variable <- revalue(bar$Variable, c("Max"="Mean daily max (±SD)", "Min"="Mean daily min (±SD)"))

DaysInMonth <- data.frame(Year=2013, Month=1:12, days=monthDays(as.Date(paste0(2013,"-",1:12,"-01"))))
DaysInMonth <- mutate(DaysInMonth, CumulativeDays=cumsum(days))

p1 <- ggplot(data=bar) + facet_wrap(~Point.Site, ncol=1) +
  geom_line(aes(x=Day, y=mean, colour=Variable)) +
  geom_ribbon(aes(x=Day, ymin=(mean-(sd)), ymax=(mean+(sd)), fill=Variable), alpha = 0.4) +
  scale_colour_manual(values=c("red","blue")) +
  scale_fill_manual(values=c("red","blue")) +
  scale_x_continuous(limits=c(0,365), breaks=c(0,DaysInMonth$CumulativeDays), minor_breaks=NULL, expand=c(0.01,0)) +
  scale_y_continuous(breaks=c(0,20,40,60)) +
  labs(x="Day of the year", y="Temperature °C", color="", fill="") +
  theme_bw() +
  theme(legend.position="bottom",text = element_text(size=15))

ggsave(file=sprintf("C:/Dropbox/Work/PaperWriting/ASU_Microclimate/drafts/Full MS/Figures/TopoScaleTemperature_1SD.png"), p1, width=8,height=6, dpi=500)









