library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)
library(dplyr)
library(Hmisc)
library(tidyr)
library(rgdal)

MainDir <- "C:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/14_TEST/ModelDirs"
OutDir <- "C:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/14_TEST/Residuals"
setwd(MainDir)

ModelDirs <- list.dirs(full.names=FALSE, recursive=FALSE)

setwd(OutDir)
my.list <- vector('list',length(ModelDirs))

i <- 1
for (ModelDir in ModelDirs){
  cat(paste(ModelDir, "\n"))
  
  dfSingle <- read.csv(sprintf("%s/%s/Predictions.csv", MainDir, ModelDir))
  dfSingle$X <- NULL
  dfSingle$model <- NULL
  dfSingle$dataType <- NULL
  dfSingle$object <- NULL
  dfSingle$Day <- as.numeric(strsplit(strsplit(ModelDir, "Sensor.Day")[[1]][[2]],
                                      "[.]")[[1]][[1]])
  dfSingle$Residual <- dfSingle$obs - dfSingle$pred
  dfSingle$DepVar <- as.factor(strsplit(ModelDir,"[.]")[[1]][[3]])
  
  my.list[[i]] <- dfSingle
  i <- i + 1
  dfSingle <- NULL
}
dfOut <- do.call('rbind', my.list)

dfOut$Garden <- as.factor(substr(as.character(dfOut$loc_ID),7,7))
dfOut$Site <- as.factor(substr(as.character(dfOut$loc_ID),5,6))
dfOut$Sensor <- as.factor(substr(as.character(dfOut$loc_ID),7,9))
dfOut$Sensor <- as.numeric(dfOut$Sensor)
dfOut$ObsValue <- NULL
write.csv(dfOut, file = sprintf("Residuals.csv"), row.names=FALSE)
dfOut <- read.csv("C:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/14_TEST/Residuals/CSVs/Residuals.csv")

###############
# Plot obs ~ modelled
setwd("C:/Dropbox (ASU)/M2NEON/SensorData/GBM_Results/14_TEST/Residuals")
for (depvar in c("Max","Min","Mean","DiurnalRange")) {
  p <- ggplot() +
    geom_point(data=subset(dfOut, DepVar==depvar), aes(x=obs, y=Residual), size=0.1, shape=3) +
    geom_hline(yintercept=0, color="dark grey") +
    lims(y=c(-25,25)) +
    facet_wrap(~Site, ncol=2) +
    labs(title=sprintf("All days. Dependent variable = %s", depvar)) +
    theme_bw()
  
  ggsave(file=sprintf("Obs~Residual_DepVar=%s_AllDays.png", depvar),
         p, width=10,height=6, dpi=500)
}
################



##############
if (FALSE) {
# Export residual data in format ready to join to sensors locations on map
dfExport <- dfOut[c("loc_ID","Residual","Day","DepVar")]
dfExport$Day <- sprintf("Day%sResidual", dfExport$Day)
dfExportWide <- spread(dfExport, Day, Residual)
for (depvar in c("Max","Min","Mean","DiurnalRange")) {
  write.csv(subset(dfExportWide, DepVar==depvar), file = sprintf("Residuals_DepVar=%s.csv", depvar), row.names=FALSE)
}
}
##############


#############
# Plot for all days
for (depvar in c("Max","Min","Mean","DiurnalRange")) {
  plot <- ggplot() +
    geom_point(data=subset(dfOut, DepVar==depvar), aes(x=Sensor, y=Residual, color=Garden), size=0.5) +
    geom_hline(yintercept = 0) +
    facet_wrap(~Site, ncol=1) +
    labs(title=sprintf("All days. Dependent variable = %s", depvar), x="Sensors") +
    theme_bw()
    #theme(axis.text.x=element_blank())
  
  ggsave(file=sprintf("ResidualPerSensor_DepVar=%s_AllDays.png", depvar),
         plot, width=10,height=6, dpi=500)
}
#############


#############
# Plot per day
for (day in 1:365) {
  for (depvar in c("Max","Min","Mean","DiurnalRange")) {
    foo <- subset(dfOut, DepVar==depvar & Day==day)
    if (nrow(foo) > 0) {
      p <- ggplot() +
        geom_point(data=foo, aes(x=Sensor, y=Residual, color=Garden)) +
        geom_hline(yintercept = 0) +
        labs(title=sprintf("Day %s. Dependent variable = %s", day, depvar), x="Sensors") +
        facet_wrap(~Site, ncol=1) +
        theme_bw() +
        theme(axis.text.x=element_blank())
  
      ggsave(file=sprintf("ResidualPerSensor_DepVar=%s_Day%s.png", depvar, day),
             p, width=10,height=6, dpi=500)
    }
  }
}
#################











