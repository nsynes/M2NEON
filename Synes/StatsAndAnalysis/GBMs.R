
library(ggplot2)
library(gtools)
#library(dismo)
library(gbm)
library(tidyr)
library(caret)
library(gridExtra)


year <- "2013"

# Get the functions which I have stored in a separate file
source("D:/Dropbox (ASU)/M2NEON/GitHub/M2NEON/Synes/SensorDataCleaning/M2NEON_Rfunctions.R")

setwd("D:/Dropbox (ASU)/M2NEON/SensorData")

dfBACKUP <- read.csv(sprintf("Merged_RasterAndSensorData_%s.csv", year))
dfBACKUP$Point.Site <- substr(dfBACKUP$Point.loc_ID,5,6)
dfBACKUP$Point.Site <- ifelse(dfBACKUP$Point.Site == "sf", "Sierra foothills", dfBACKUP$Point.Site)
dfBACKUP$Point.Site <- ifelse(dfBACKUP$Point.Site == "sm", "Sierra montane", dfBACKUP$Point.Site)
dfBACKUP$Point.Site <- as.factor(dfBACKUP$Point.Site)

# Generate temperature range values by month and quarter from max and min sensor values
for (period in c("Month","Quarter")) {
  if (period == "Month") Quantity <- 1:12
  if (period == "Quarter") Quantity <- 1:4
  listSensorPeriod <- lapply(paste0(period, Quantity), function(p) sprintf("Sensor.%s", p))
  for (SensorPeriod in listSensorPeriod) {
    dfBACKUP[sprintf("%s.TempRange", SensorPeriod)] <- dfBACKUP[[sprintf("%s.max", SensorPeriod)]] - dfBACKUP[[sprintf("%s.min", SensorPeriod)]]
  }
}

####################
# Reformat to 'long format' data
#dfLong <- melt(dfBACKUP, measure.vars = colnames(dfBACKUP[ , !(names(dfBACKUP) %in% c("Point.loc_ID","Point.coords.x1","Point.coords.x2"))]),
#               variable.name = "variable")
#dfLong$VarName <- dfLong$variable
#dfLong<-separate(data = dfLong, col = variable, into = c("Group", "Type","Subtype","RadiationOrCoverType", "LAStype"), sep = "\\.")
###################

####################
# Boosted regression tree work

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

set.seed(1)

res <- "1m"
Allxnames <- colnames(dfBACKUP)[#substr(colnames(dfBACKUP),1,nchar("Raster")) == "Raster" &
                                  colnames(dfBACKUP) == "Raster.NDNI" |
                                  colnames(dfBACKUP) == "Raster.NDLI" |
                                  colnames(dfBACKUP) == sprintf("Raster.CanopyFrac.%sLAScov", res) |
                                  colnames(dfBACKUP) == sprintf("Raster.CanopyFrac.%sLASdns", res) |
                                  colnames(dfBACKUP) == sprintf("Raster.CanopyFrac.%sLASp25", res) |
                                  colnames(dfBACKUP) == sprintf("Raster.CanopyFrac.%sLASp75", res) |
                                  colnames(dfBACKUP) == sprintf("Raster.CanopyFrac.%sLASp90", res) |
                                  colnames(dfBACKUP) == sprintf("Raster.CanopyFrac.%sLASstd", res) |
                                  colnames(dfBACKUP) == "Raster.CHM" |
                                  colnames(dfBACKUP) == "Raster.CoAspect" |
                                  substr(colnames(dfBACKUP),nchar(colnames(dfBACKUP))-nchar("DirectDuration")+1,
                                         nchar(colnames(dfBACKUP))) == "DirectDuration" |
                                  substr(colnames(dfBACKUP),nchar(colnames(dfBACKUP))-nchar("DirectRadiation")+1,
                                         nchar(colnames(dfBACKUP))) == "DirectRadiation" |
                                  substr(colnames(dfBACKUP),nchar(colnames(dfBACKUP))-nchar("SolarRadiation")+1,
                                         nchar(colnames(dfBACKUP))) == "SolarRadiation" |
                                  substr(colnames(dfBACKUP),nchar(colnames(dfBACKUP))-nchar("DiffuseRadiation")+1,
                                         nchar(colnames(dfBACKUP))) == "DiffuseRadiation"]
                                  
Allynames <- colnames(dfBACKUP)[substr(colnames(dfBACKUP),1,nchar("Sensor")) == "Sensor"]

for (dfSub in list(subset(dfBACKUP, Point.Site == "Sierra foothills"),
                   subset(dfBACKUP, Point.Site == "Sierra montane"))) {
  for (period in c("Month")) {
    if (period == "Month") {
      if (dfSub$Point.Site == "Sierra foothills") {
        listQuantity <- 11:12
      }
      else if (dfSub$Point.Site == "Sierra montane") {
        listQuantity <- 6:12
      }
    }
    else if (period == "Quarter") listQuantity <- 1:4
    for (quantity in listQuantity) {
      
      foo <- list()
      foo <- SubsetVarNames(Allxnames, Allynames, period, quantity)
      xnames <- unlist(foo[1])
      ynames <- unlist(foo[2])
      
      for (yname in ynames) {
        
        if (!(yname %in% paste0(sprintf("Sensor.%s%s.",period,quantity), c("ci","No","se","CDD20","CDD25","CDD30")))) {
          # Remove any NAs in the dependent variable
          df<-dfSub[!(is.na(dfSub[,yname])),]
          
          
          # Check how many unique y values are in dataset
          if (length(unique(df[,yname])) > 60) {
      
            # Create a grid of parameter space to run gbm for:
            gbmGrid <-  expand.grid(interaction.depth = 1:5,
                                    n.trees = seq(1000,10000,1000), 
                                    shrinkage = 0.001,
                                    n.minobsinnode = 10)
            
            # Set up training control
            fitControl <- trainControl(method = "repeatedcv",
                                       number=10, # <^ 10fold cross validation
                                       repeats = 5, # do 5 repititions of cv
                                       preProcOptions = list(thresh = 0.95),
                                       #classProbs = TRUE, # Estimate class probabilities
                                       #summaryFunction = twoClassSummary, # Use AUC to pick the best model
                                       allowParallel = TRUE)
            
            inTraining <- createDataPartition(df[,yname], p=.75, list=FALSE)
            trainingdata <- df[ inTraining,]
            testingdata  <- df[-inTraining,]
            
            gbm.tune <- caret::train(x = trainingdata[, xnames],
                              y = trainingdata[, yname],
                              #get(yname)~.,
                              #data = trainingdata[c(xnames,yname)],
                              distribution = "gaussian",
                              method = "gbm", bag.fraction = 0.5,
                              #nTrain = round(nrow(df) *.75), #not sure how this works...already split train and test manually
                              trControl = fitControl,
                              verbose = TRUE,
                              tuneGrid = gbmGrid,
                              ## Specify which metric to optimize
                              metric = "RMSE") # "Rsquared")
            
            #if (length(dev.list()) > 0) dev.off()
            #par(mfrow=c(1,1))
            dir.create(sprintf("Site(s)=%s_y=%s_res=%s", paste0(unique(df$Point.Site), collapse=", "), yname, res))
            #png(sprintf("Site(s)=%s_y=%s/ModelTuning.png", paste0(unique(df$Point.Site), collapse=", "), yname))
            #plot(gbm.tune)
            #dev.off()
            
            write.csv(gbm.tune$bestTune, sprintf("Site(s)=%s_y=%s_res=%s/BestModel.csv", paste0(unique(df$Point.Site), collapse=", "), yname, res))
            write.csv(gbm.tune$results, sprintf("Site(s)=%s_y=%s_res=%s/TuningResults.csv", paste0(unique(df$Point.Site), collapse=", "), yname, res))
              
            
            # This seems to give the same result as rel.inf below, but with less control over the plot:
            #plot(varImp(gbm.tune))
            plot_RelInf <- ggplot(data=summary(gbm.tune$finalModel)) +
              geom_bar(aes(x=reorder(var, rel.inf), y=rel.inf, fill=var), color="black", stat="identity") +
              scale_fill_discrete(guide=FALSE) +
              theme_bw() +
              labs(title = sprintf("Site(s): %s\nDependent variable = %s\nCanopy resolution = %s", paste0(unique(df$Point.Site), collapse=", "), yname, res),
                   x="Independent variables",
                   y="Relative Influence") +
              coord_flip()
            
            ggsave(file=sprintf("Site(s)=%s_y=%s_res=%s/RelativeInfluence.png", paste0(unique(df$Point.Site), collapse=", "), yname, res),
                   plot_RelInf, width=12,height=8, dpi=500)
            dev.off()
    
            # Make predictions using the test data set
            #gbm.pred <- predict.train(gbm.tune,df[,xnames])
            gbm.pred <- extractPrediction(list(gbm.tune), testX=testingdata[,xnames], testY=testingdata[,yname])
            write.csv(gbm.pred,
                      sprintf("Site(s)=%s_y=%s_res=%s/TestDataPrediction.csv ", paste0(unique(df$Point.Site), collapse=", "), yname, res))
            
            # Relative influence values
            write.csv(summary(gbm.tune$finalModel),
                      sprintf("Site(s)=%s_y=%s_res=%s/RelativeInfluence.csv", paste0(unique(df$Point.Site), collapse=", "), yname, res))
            
            
            nplots <- 0
            group <- 1
            for (varname in summary(gbm.tune$finalModel)$var) {
              nplots <- nplots + 1
              if (nplots == 1) {
                png(sprintf("Site(s)=%s_y=%s_res=%s/RelativeInfluence_%s.png", paste0(unique(df$Point.Site), collapse=", "), yname, res, group),
                    width=800, height=800, res=100)
                par(mfrow=c(2,2))
                par(mar = c(4, 4, 2, 2), oma = c(1, 1, 1, 1))
                group <- group + 1
              }
              plot.gbm(gbm.tune$finalModel, i.var = varname, lwd=2, col = "blue")
              if (nplots == 4) {
                nplots <- 0
                dev.off()
              }
            }
            if (length(dev.list()) > 0) dev.off()
          }
        }
        
      }
    }
  }
}        
        



