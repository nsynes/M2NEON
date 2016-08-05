
###############################
# Extract different elements of date and time
###############################
# as.POSIXlt(df[,1], tz = "MST")$min
# as.POSIXlt(df[,1], tz = "MST")$hour
# as.POSIXlt(df[,1], tz = "MST")$mday # 1-31: day of the month
# as.POSIXlt(df[,1], tz = "MST")$mon # 0-11: months afer the first of the year
# as.POSIXlt(df[,1], tz = "MST")$year # years since 1900
# as.POSIXlt(df[,1], tz = "MST")$wday # 0-6 day of the week, starting on Sunday
# as.POSIXlt(df[,1], tz = "MST")$yday # 0-365: day of the year
###############################




#######################################
RemoveExtremeValuesByWindow <- function(df, halfwindow, threshold) {
  listdataframes <- list()
  clust <- makeCluster(6, outfile = "C:/Users/nsynes/Desktop/TrackProgress.txt")
  clusterEvalQ(clust, library(plyr))  
  for (garden in c("sf1")) {
    df_site <- subset(df, GardenID == sprintf("%s", garden))
    clusterApplyLB(clust, 0:10, function(i) {
      cat(paste("garden:",garden,"; ","sensor:",i,"\n"))
      df_garden <- subset(df_site, WithinGardenID != i)
      df_single <- subset(df_site, WithinGardenID == i)
      df_summary <- adply(df_garden, 1, function(x) {
        df_window <- subset(df_garden, df_garden$DateAndTime <= x$DateAndTime + halfwindow & df_garden$DateAndTime >= x$DateAndTime - halfwindow)
        x$Max <- max(df_window$value)
        x$Min <- min(df_window$value)
        return(x)
        })
      df_merge <- merge(df_summary, df_single, by = c("DateAndTime", "GardenID"))
      listdataframes[[i]] <- df_merge
      df_garden <- NULL
      df_single <- NULL
      df_summary <- NULL
      df_merge <- NULL
    }) 
    df_site <- NULL
  }
  
  #dfOut$DifFromMaxMin <- ifelse(dfOut$value > dfOut$Max,
  #                              abs(dfOut$value - dfOut$Max),
  #                              ifelse(dfOut$value < dfOut$Min,
  #                                     abs(dfOut$Min - dfOut$value),
  #                                     0))
  #dfOut$OutsideThreshold <- ifelse(dfOut$DifFromMaxMin > threshold, TRUE, FALSE)
  
  #dfOut$value <- ifelse(dfOut$OutsideThreshold, NA, dfOut$value)
  
  #dfOut <- na.omit(dfOut)
  #return(dfOut)
  return(listdataframes)
}
#######################################

#######################################
# calculates the daily max and min for each sensor, if these values are between +- threshold then
# that sensor's day's data is removed, as it is assumed to be underneath snow
RemoveSnowCoverValues <- function(df, threshold) {
  # Remove previous calculations of max and min, which may now be incorrect because of cleaning
  df$Max <- NULL
  df$Min <- NULL
  df$OutsideThreshold <- NULL
  df$DifFromMaxMin <- NULL
  df_maxmin <- GetMaxMin(df, measurevar = "value", groupvars = c("Date","loc_ID"), na.rm = TRUE)
  df_merge <- merge(df, df_maxmin, by = c("Date","loc_ID"))
  df_merge$remove <- ifelse(df_merge$Min >= -threshold & df_merge$Max <= threshold, TRUE, FALSE)
  df_merge$value <- ifelse(df_merge$remove, NA, df_merge$value)
  ValuesRemoved <- sum(df_merge$remove, na.rm=TRUE)
  df_merge$remove <- NULL
  
  df_merge<- na.omit(df_merge)
  
  cat(paste("Values removed =", ValuesRemoved,"\n"))
  
  return(df_merge[colnames(df)])
}


#######################################



#######################################
# For each sensor, this excludes it from the rest of the garden data then calculates max and min of the garden
# To compare to daily max min use "Date", to compare to ten minute max min use "DateTime"
RemoveExtremeValues <- function(df, grouping, threshold) {
  CountValuesRemoved <- 0
  listGroup = append(grouping, "GardenID")
  dfOut <- data.frame()
  for (garden in unique(df$GardenID)) {
    df_site <- subset(df, GardenID == sprintf("%s", garden))
    for (sensor in unique(df_site$WithinGardenID)) {
      df_garden <- subset(df_site, WithinGardenID != sensor)
      df_single <- subset(df_site, WithinGardenID == sensor)
      df_summary <- GetMaxMin(df_garden, measurevar = "value", groupvars = listGroup, na.rm=TRUE)
      df_summary$Min <- ifelse(!is.finite(df_summary$Min), NA, df_summary$Min)
      df_summary$Max <- ifelse(!is.finite(df_summary$Max), NA, df_summary$Max)
      df_merge <- merge(df_summary, df_single, by = listGroup)
      df_merge$DifFromMaxMin <- ifelse(df_merge$value > df_merge$Max,
                                    abs(df_merge$value - df_merge$Max),
                                    ifelse(df_merge$value < df_merge$Min,
                                           abs(df_merge$Min - df_merge$value),
                                           0))
      df_merge$OutsideThreshold <- ifelse(df_merge$DifFromMaxMin > threshold, TRUE, FALSE)
      ValuesRemoved <- sum(df_merge$OutsideThreshold, na.rm=TRUE)
      CountValuesRemoved <- CountValuesRemoved + ValuesRemoved
      
      df_merge$value <- ifelse(df_merge$OutsideThreshold, NA, df_merge$value)
      
      #df_merge<- na.omit(df_merge)
      dfOut <- rbind(dfOut, df_merge)
      cat(paste(unique(df_single$loc_ID),"; values removed =",ValuesRemoved,"\n"))
      df_garden <- NULL
      df_single <- NULL
      df_summary <- NULL
      df_merge <- NULL
      
    }
    df_site <- NULL
  }
  
  return(list(CountValuesRemoved, dfOut))
  
  # Set to descending order from
  #dfOut<-dfOut[order(dfOut$Dif, decreasing=TRUE),]
}
#######################################


###############################
# Run this when legends are switched on to grab just the legend from a plot
Getlegend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}
###############################



###############################
GetPlot <- function(loc, loc_specific_issues, legend = FALSE) {
  garden <- substr(loc,5,7)
  if (legend == FALSE) {
    p <- ggplot() +
      geom_ribbon(data = subset(dfGoodSummary, GardenID==garden), aes(x = Date, ymax = Sensor.max, ymin = Sensor.min, fill="Garden Max Min"), alpha = 0.8) +
      scale_fill_manual(values=c("grey"), guide=FALSE) +
      #scale_fill_manual(values=c("grey")) +
      geom_line(data = subset(dfBadSummary, loc_ID==loc), aes(x = Date, y = Sensor.mean, color="Sensor Mean")) +
      geom_line(data = subset(dfBadSummary, loc_ID==loc), aes(x = Date, y = Sensor.max, colour="Sensor Max")) +
      geom_line(data = subset(dfBadSummary, loc_ID==loc), aes(x = Date, y = Sensor.min, colour="Sensor Min")) +
      geom_line(data = subset(dfGoodSummary, GardenID==garden), aes(x = Date, y = Sensor.mean, colour="Garden Mean"), linetype=2) +
      scale_colour_manual(values=c("black","darkgreen", "red","blue"), guide = FALSE) +
      #scale_colour_manual(values=c("black","green", "red","blue")) +
      geom_vline(data=loc_specific_issues, aes(xintercept = as.numeric(Date)), size=0.8) +
      geom_text(data=loc_specific_issues, aes(x=Date, y=45, label=value), angle = 90, nudge_x=12, size = 8) +
      labs(fill="Fill legend",color="Line legend",y="Temperature",title=sprintf("%s", loc)) +
      theme_bw() +
      theme(legend.position="bottom")

  }
  else {
    p <- ggplot() +
      geom_ribbon(data = subset(dfGoodSummary, GardenID==garden), aes(x = Date, ymax = Sensor.max, ymin = Sensor.min, fill="Max Min of garden (or landscapes)"), alpha = 0.8) +
      #scale_fill_manual(values=c("grey"), guide=FALSE) +
      scale_fill_manual(values=c("grey")) +
      geom_line(data = subset(dfBadSummary, loc_ID==loc), aes(x = Date, y = Sensor.mean, color="Sensor Mean")) +
      geom_line(data = subset(dfBadSummary, loc_ID==loc), aes(x = Date, y = Sensor.max, colour="Sensor Max")) +
      geom_line(data = subset(dfBadSummary, loc_ID==loc), aes(x = Date, y = Sensor.min, colour="Sensor Min")) +
      geom_line(data = subset(dfGoodSummary, GardenID==garden), aes(x = Date, y = Sensor.mean, colour="Mean of garden (or landscapes)"), linetype=2) +
      #scale_colour_manual(values=c("black","green", "red","blue"), guide = FALSE) +
      scale_colour_manual(values=c("black","darkgreen", "red","blue")) +
      scale_x_date(breaks = date_breaks("3 months"), minor_breaks = date_breaks("1 month")) +
      geom_vline(data=loc_specific_issues, aes(xintercept = as.numeric(Date)), size=0.8) +
      geom_text(data=loc_specific_issues, aes(x=Date, y=45, label=value), angle = 90, nudge_x=12, size = 8) +
      labs(fill="Fill legend",color="Line legend",y="Temperature",title=sprintf("%s", loc)) +
      theme_bw() +
      theme(legend.position="bottom")

  }
  return(p)
}
###############################



###############################
SubsetSensorsWithIssues <- function(dfAllIssues) {
  
  df <- subset(dfAllIssues, Checked == "SensorsChecked")
  df$Date <- as.Date(df$Date, format="%d/%m/%Y", tz = "MST")
  # Select only columns (loc_IDs) that have had an issue in the given time period)
  df <- df[colSums(!is.na(df)) > 0]
  # All sensors that had no issues during checks are marked with 0
  df[is.na(df)] <- " "
  df <- melt(df, measure.vars = colnames(df[3:length(colnames(df))]),
                         variable.name = "loc_ID")
  df$GardenID <- substr(df$loc_ID,5,7)
  return(subset(df, !is.na(value) & value != ""))
}
###############################



###############################
GetSensorData <- function(FilePath) {
  
  # Open sensor file and convert date column to a date format
  df1 <- read.csv(FilePath, na.strings="")
  colnames(df1)[1] <- "DateAndTime"
  df1$DateAndTime <- as.POSIXct(df1[,1], tz = "MST")
  #df1$DateAndTime <- as.POSIXct(strptime(df1[,1], "%m/%d/%Y %H:%M"), tz = "MST")
  #df1$Date <- as.Date(df1[,1], tz = "MST")
  #df1$Month <- as.POSIXlt(df1[,1], tz = "MST")$mon + 1 # to get actual month, as by default this get "months after first of year"
  #df1$Year <- as.POSIXlt(df1[,1], tz = "MST")$year + 1900 # to get actual year, as by default this get "years since 1900"
  
  # Convert from wide to long data format (better for ggplot)
  SensorsOnly <- colnames(df1)
  SensorsOnly <- SensorsOnly[SensorsOnly != "DateAndTime"]
  df2 <- melt(df1, id.vars = c("DateAndTime"),
              measure.vars = SensorsOnly,
              variable.name = "loc_ID", na.rm = FALSE)
  df2$GardenID <- substr(df2$loc_ID,5,7)
  df2$WithinGardenID <- as.integer(substr(df2$loc_ID,8,9))
  df2$Date <- as.Date(df2[,1], tz = "MST")
  
  return(df2)
}
###############################




###############################
GetSensorSummary <- function(FilePath, SummaryGroups) {
  df <- GetSensorData(FilePath)
  
  AllSummaryGroups = append(SummaryGroups, c("loc_ID", "GardenID", "WithinGardenID"))
  df2 <- summarySE(df, measurevar = "value", groupvars = AllSummaryGroups)
  
  return(df2)
}





###############################
MergeBySite <- function(df) {
  # for each unique name in the data frame. This line gets the dataframe column names without the .1 and .2
  for (x in unique(sapply(colnames(df)[4:length(colnames(df))], function(name) {strsplit(name,"\\.")[[1]][1]}))) {
    a <- df[,sprintf("%s.1", x)]
    # This assumes that if the first column in NA, the second won't be
    df[,sprintf("%s", x)] <- ifelse(!is.na(df[,sprintf("%s.1", x)]),
                                    df[,sprintf("%s.1", x)],
                                    df[,sprintf("%s.2", x)])
    # Removes old .1 and .2 columns
    df[,sprintf("%s.1", x)] <- NULL
    df[,sprintf("%s.2", x)] <- NULL
  }
  return(df)
}
###############################



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
                   c(Sensor.No    = length2(xx[[col]], na.rm=na.rm),
                     Sensor.max = max(xx[[col]], na.rm=na.rm),
                     Sensor.min = min(xx[[col]], na.rm=na.rm),
                     Sensor.mean = mean   (xx[[col]], na.rm=na.rm),
                     Sensor.sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  #datac <- rename(datac, c("mean" = measurevar))
  
  datac$Sensor.se <- datac$Sensor.sd / sqrt(datac$Sensor.No)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=No-1
  ciMult <- qt(conf.interval/2 + .5, datac$Sensor.No-1)
  datac$Sensor.ci <- datac$Sensor.se * ciMult
  
  return(datac)
}
###############################


###############################
GetMaxMin <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, .drop=TRUE) {
  require(plyr)
  
  # This does the summary. For each group's data frame, return a vector with
  # No, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(Max = max(xx[[col]], na.rm=na.rm),
                     Min = min(xx[[col]], na.rm=na.rm))
                 },
                 measurevar
  )  
  return(datac)
}
###############################
