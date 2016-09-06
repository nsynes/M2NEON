
library(ggplot2)
library(gtools)
#library(dismo)
library(gbm)
library(tidyr)




# Get the functions which I have stored in a separate file
source("C:/Dropbox (ASU)/M2NEON/GitHub/M2NEON/Synes/SensorDataCleaning/M2NEON_Rfunctions.R")

setwd("C:/Dropbox (ASU)/M2NEON/SensorData")

df <- read.csv("Merged_RasterAndSensorData.csv")
df$Point.Site <- substr(df$Point.loc_ID,5,6)
df$Point.Site <- ifelse(df$Point.Site == "sf", "Sierra foothills", df$Point.Site)
df$Point.Site <- ifelse(df$Point.Site == "sm", "Sierra montane", df$Point.Site)
df$Point.Site <- as.factor(df$Point.Site)

# Generate temperature range values by month and quarter from max and min sensor values
for (period in c("Month","Quarter")) {
  if (period == "Month") Quantity <- 1:12
  if (period == "Quarter") Quantity <- 1:4
  listSensorPeriod <- lapply(paste0(period, Quantity), function(p) sprintf("Sensor.%s", p))
  for (SensorPeriod in listSensorPeriod) {
    df[sprintf("%s.TempRange", SensorPeriod)] <- df[[sprintf("%s.max", SensorPeriod)]] - df[[sprintf("%s.min", SensorPeriod)]]
  }
}
dfBACKUP <- df

####################
# Reformat to 'long format' data
dfLong <- melt(df, measure.vars = colnames(df[ , !(names(df) %in% c("Point.loc_ID","Point.coords.x1","Point.coords.x2"))]),
                   variable.name = "variable")
dfLong$VarName <- dfLong$variable
dfLong<-separate(data = dfLong, col = variable, into = c("Group", "Type","Subtype","RadiationOrCoverType", "LAStype"), sep = "\\.")
####################


####################
# Generate plots of raster data against sensor data by month and quarter
for (period in c("Month","Quarter")) {
  for (measure in c(paste0("CDD", c(5,10,15,20,25,30)),c("max","min","mean","TempRange"))) {
    for (radiation in c("DirectDuration","DirectRadiation","SolarRadiation",
                        "CanopyFracDirectDuration.LASdns","CanopyFracDirectRadiation.LASdns","CanopyFracSolarRadiation.LASdns",
                        "CanopyFracDirectDuration.LAScov","CanopyFracDirectRadiation.LAScov","CanopyFracSolarRadiation.LAScov")) {
      if (period == "Month") Quantity <- 1:12
      if (period == "Quarter") Quantity <- 1:4
      
      listSensorVars <- lapply(paste0(period, Quantity), function(p) sprintf("Sensor.%s.%s", p, measure))
      
      dfSensor <- melt(df, id.vars = c("Point.loc_ID", "Point.Site"), measure.vars = listSensorVars, variable.name = "variable")
      dfSensor <- separate(data = dfSensor, col = variable, into = c("Sensor", "Period", "Measure"), sep = "\\.")
      dfSensor$SensorValue <- dfSensor$value
      dfSensor$value <- NULL
      
      listRasterVars <- lapply(paste0(period, Quantity), function(p) sprintf("Raster.%s.Solar.%s", p, radiation))
  
      dfRaster <- melt(df, id.vars = c("Point.loc_ID", "Point.Site"), measure.vars = listRasterVars, variable.name = "variable")
      dfRaster <- separate(data = dfRaster, col = variable, into = c("Sensor", "Period", "Solar", "RadiationType"), sep = "\\.")
      dfRaster$RasterValue <- dfRaster$value
      dfRaster$RadiationType <- as.factor(dfRaster$RadiationType)
      dfRaster$value <- NULL
      
      foo <- merge(dfSensor, dfRaster, by=c("Point.loc_ID", "Period", "Point.Site"))
      foo$Period <- as.factor(foo$Period)
      foo$Period <- factor(foo$Period, levels = mixedsort(levels(foo$Period)))
      dfSensor <- NULL
      dfRaster <- NULL
      
      plot <- ggplot(data = foo) +
        geom_point(aes(x=RasterValue, y=SensorValue, color=Point.Site), size=1.5) +
        theme_bw() +
        scale_colour_manual(values = c("red","blue")) +
        labs(title=sprintf("%s ~ %s (each point represents one sensor location)", unique(foo$RadiationType), unique(foo$Measure)),
             x=sprintf("Raster value: %s", unique(foo$RadiationType)),
             y=sprintf("Sensor value: %s", unique(foo$Measure))) +
        facet_wrap(~Period)
      
      ggsave(file=sprintf("OUT/%s~%s (by %s).png", unique(foo$RadiationType), unique(foo$Measure), period), plot, width=12,height=8, dpi=500)

    }
  }
}
####################



####################
# Boosted regression tree work
# Note could use either gbm or gbm.fit, gbm.fit seems easier for trying many variables
if (FALSE) {
GbmModel1 <- gbm(formula = formula(df$Sensor.Quarter3.mean ~
                      df$Raster.Quarter3.Solar.DirectDuration +
                      df$Raster.DEM),
                      distribution = "gaussian",
                      var.monotone = NULL,
                      n.trees = 10000,
                      interaction.depth = 1,
                      n.minobsinnode = 10,
                      shrinkage = 0.001,
                      bag.fraction = 0.5,
                      train.fraction = 1.0,
                      cv.folds=0,
                      keep.data = TRUE,
                      verbose = "CV",
                      class.stratify.cv=NULL,
                      n.cores = NULL)
}

# Parameter recommendations from: 
# http://www.listendata.com/2015/07/gbm-boosted-models-tuning-parameters.html
##########
# interaction.depth = 6,
# n.trees; unclear: "Increasing N reduces the error on training set, but setting it too high may lead to over-fitting."
# shrinkage = 0.001; 0.01 for datasets with <10,000 records, 0.1 for >10,000 records, but we only have <200, so
#... the extra shrinkage will not make the models too slow to run...
#... One typically chooses the shrinkage parameter beforehand and varies the number of
#... iterations (trees) N with respect to the chosen shrinkage. Small shrinkage generally gives a better result,
#... but at the expense of more iterations (number of trees) required.
# n.minobsinnode = 10; "the minimum number of observations in trees' terminal nodes. Set n.minobsinnode = 10.
#... When working with small training samples it may be vital to lower this setting to five or even three."
# bag.fraction = 0.5 "the fraction of the training set observations randomly selected to propose the next
#... tree in the expansion. By default, it is 0.5"
# train.fraction = 1.0


Allxnames <- colnames(dfBACKUP)[substr(colnames(dfBACKUP),1,nchar("Raster")) == "Raster" &
                              colnames(dfBACKUP) != "Raster.DEM" &
                              colnames(dfBACKUP) != "Raster.TWI" &
                              colnames(dfBACKUP) != "Raster.CWD"]
Allynames <- colnames(dfBACKUP)[substr(colnames(dfBACKUP),1,nchar("Sensor")) == "Sensor"]


for (df in list(dfBACKUP,
                subset(dfBACKUP, Point.Site == "Sierra foothills"),
                subset(dfBACKUP, Point.Site == "Sierra montane"))) {
  for (period in c("Month","Quarter")) {
    if (period == "Month") listQuantity <- 1:12
    else if (period == "Quarter") listQuantity <- 1:4
    for (quantity in listQuantity) {
      
      foo <- list()
      foo <- SubsetVarNames(Allxnames, Allynames, period, quantity)
      xnames <- unlist(foo[1])
      ynames <- unlist(foo[2])
      
      GbmModel1 <- gbm.fit(x=df[xnames],
                           y=b,
                           distribution="gaussian",
                           n.trees = 10000)
      
      
    }
  }
}
  
  GbmModel1 <- gbm.fit(x=df[xnames],
                       y=b,
                       distribution="gaussian",
                       n.trees = 10000)
  
  Gbm1Summary <- summary.gbm(GbmModel1)
  
  p1 <- ggplot(data=Gbm1Summary) +
    geom_bar(aes(x=var, y=rel.inf, fill=var), color="black", stat="identity") +
    scale_fill_discrete(guide=FALSE) +
    theme_bw() +
    labs(title = sprintf("y = Sensor.Quarter2.max\nSite(s): %s", paste0(unique(df$Point.Site), collapse=", ")),
         x="Relative Influence") +
    coord_flip()
  
  cat(paste0(unique(dfBACKUP$Point.Site), collapse=", "))
  ggsave(file=sprintf("OUT/Sensor.Quarter2.max_AllVars_%s.png", paste0(unique(df$Point.Site), collapse=", ")), p1, width=12,height=8, dpi=500)

  
  
  GbmModel2 <- gbm.fit(x=df[colnames(df)[colnames(df) == "Raster.Quarter2.Solar.DirectDuration" |
                                                 colnames(df) %in% c("Raster.DEM", "Raster.CanopyFraction")]],
                       y=df$Sensor.Quarter2.max,
                       distribution="gaussian",
                       n.trees = 10000)
  
  Gbm2Summary <- summary.gbm(GbmModel2)
  
  p2 <- ggplot(data=Gbm2Summary) +
    geom_bar(aes(x=var, y=rel.inf, fill=var), color="black", stat="identity") +
    scale_fill_discrete(guide=FALSE) +
    theme_bw() +
    labs(title = sprintf("y = Sensor.Quarter2.max\nSite(s): %s", paste0(unique(df$Point.Site), collapse=", ")),
         x="Relative Influence") +
  coord_flip()
  
  ggsave(file=sprintf("OUT/Sensor.Quarter2.max_SubsetVars_%s.png", paste0(unique(df$Point.Site), collapse=", ")), p2, width=12,height=8, dpi=500)
  
}



####################







