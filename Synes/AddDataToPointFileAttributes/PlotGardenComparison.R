
library(ggplot2)
library(reshape2)
library(gridExtra)
library(GGally)
# Note that tidyr and raster both have an extract tool, so loading tidyr first means that
# the tidyr extract is masked by the raster extract
library(tidyr)
library(raster)
library(rgdal)
library(stringr)



# Get the functions which I have stored in a separate file
source("C:/Dropbox (ASU)/M2NEON/GitHub/M2NEON/Synes/SensorDataCleaning/M2NEON_Rfunctions.R")

setwd("C:/Dropbox (ASU)/M2NEON/SensorData")

###############################
# Sensor data file information
###############################
y <- 2013
SensorType <- "temp5cm" # temp1m, temp2m, temp4m, temp5cm, tempmax5cm, tempmin5cm, temps

Site <- "sf"
sfFilePath <- sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/CleanPass2_FINAL/%s_%s_%s0101-%s1231.csv",
                      SensorType, Site, y, y)

Site <- "sm"
smFilePath <- sprintf("C:/Dropbox (ASU)/M2NEON/SensorData/CleanPass2_FINAL/%s_%s_%s0101-%s1231.csv",
                      SensorType, Site, y, y)
###############################


###############################
# Collect sensor summary data
###############################
ColumnsToRename <- c("Sensor.No","Sensor.max","Sensor.min","Sensor.mean","Sensor.sd","Sensor.se","Sensor.ci")

dfSensorAnnual <- rbind(GetSensorSummary(sfFilePath, SummaryVar="Year"), GetSensorSummary(smFilePath, SummaryVar="Year"))
dfSensorAnnual <- RenameColumns(dfSensorAnnual, "Annual", ColumnsToRename)

dfSensorMonth <- rbind(GetSensorSummary(sfFilePath, SummaryVar="Month"), GetSensorSummary(smFilePath, SummaryVar="Month"))
dfSensorMonth <- OneRowPerSensor(dfSensorMonth, "Month", ColumnsToRename)

dfSensorQuarter <- rbind(GetSensorSummary(sfFilePath, SummaryVar="Quarter", Quarters=c(1,92,184,276)), GetSensorSummary(smFilePath, SummaryVar="Quarter", Quarters=c(1,92,184,276)))
dfSensorQuarter <- OneRowPerSensor(dfSensorQuarter, "Quarter", ColumnsToRename)

# Cumulative degree days
df_DailySummary <- rbind(read.csv("sf2013_DailySummary.csv"), read.csv("sm2013_DailySummary.csv"))

listCDDy <- list()
listCDDm <- list()
listCDDq <- list()
i<-1
for (CDD_base in c(5,10,15,20,25,30)) {
  dfCDDy <- GetCDD(df_DailySummary, dailymean = "Sensor.mean", base = CDD_base, interval = "Year")
  dfCDDy <- RenameColumns(dfCDDy, "Annual", sprintf("CDD%s", CDD_base))
  dfCDDm <- GetCDD(df_DailySummary, dailymean = "Sensor.mean", base = CDD_base, interval = "Month")
  dfCDDm <- OneRowPerSensor(dfCDDm, "Month", sprintf("CDD%s", CDD_base))
  dfCDDq <- GetCDD(df_DailySummary, dailymean = "Sensor.mean", base = CDD_base, interval = "Quarter")
  dfCDDq <- OneRowPerSensor(dfCDDq, "Quarter", sprintf("CDD%s", CDD_base))
  listCDDy[[i]] <- dfCDDy
  listCDDm[[i]] <- dfCDDm
  listCDDq[[i]] <- dfCDDq
  dfCDDy <- NULL
  dfCDDm <- NULL
  dfCDDq <- NULL
  i<-i+1
}
df_DailySummary <- NULL
dfCDDAnnual <- Reduce(function(x, y) merge(x, y, all=TRUE, by="loc_ID"), listCDDy)
dfCDDMonth <- Reduce(function(x, y) merge(x, y, all=TRUE, by="loc_ID"), listCDDm)
dfCDDQuarter <- Reduce(function(x, y) merge(x, y, all=TRUE, by="loc_ID"), listCDDq)
###############################

dfCDDAnnual$GardenID <- substr(dfCDDAnnual$loc_ID,5,7)
dfCDDAnnual$SensorNo <- substr(dfCDDAnnual$loc_ID,8,9)
dfCDDAnnual$Site <- substr(dfCDDAnnual$loc_ID,5,6)
dfCDDMonth$GardenID <- substr(dfCDDMonth$loc_ID,5,7)
dfCDDMonth$SensorNo <- substr(dfCDDMonth$loc_ID,8,9)
dfCDDMonth$Site <- substr(dfCDDMonth$loc_ID,5,6)
dfCDDQuarter$GardenID <- substr(dfCDDQuarter$loc_ID,5,7)
dfCDDQuarter$SensorNo <- substr(dfCDDQuarter$loc_ID,8,9)
dfCDDQuarter$Site <- substr(dfCDDQuarter$loc_ID,5,6)

listVars <- append(paste0("Quarter",1:4),paste0("Month",1:12))
listVars <- append("Annual",listVars)
for (CDD in c(5,10,15,20,25,30)) {
  for (period in listVars) {
    nameVar <- sprintf("%s.CDD%s", period, CDD)
    if (substr(period,1,3) == "Ann") nameDF <- "dfCDDAnnual"
    if (substr(period,1,3) == "Qua") nameDF <- "dfCDDQuarter"
    if (substr(period,1,3) == "Mon") nameDF <- "dfCDDMonth"
    cat(paste("\n",nameVar,"\n",nameDF))
    
    sf_plot <- ggplot(data = subset(get(nameDF), Site == "sf")) +
      geom_bar(aes(x=SensorNo, y=get(nameVar), fill=factor(SensorNo)), color="black", stat="identity") +
      facet_grid(~GardenID) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, size = 6)) +
      labs(title="Sierra Foothills (2013)", x="", y=nameVar) +
      guides(fill=FALSE)
    
    sm_plot <- ggplot(data = subset(get(nameDF), Site == "sm")) +
      geom_bar(aes(x=SensorNo, y=get(nameVar), fill=factor(SensorNo)), color="black", stat="identity") +
      facet_grid(~GardenID) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, size = 6)) +
      labs(title="Sierra Montane (2013)", x="", y=nameVar) +
      guides(fill=FALSE)
    
    plot <- grid.arrange(sf_plot, sm_plot)
    
    ggsave(file=sprintf("GardenCDD/%s.png",nameVar), plot, width=15,height=5, dpi=500)
  }
}











