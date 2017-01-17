
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

Allxnames <- colnames(dfBACKUP)[substr(colnames(dfBACKUP),1,nchar("Raster")) == "Raster" &
                               (colnames(dfBACKUP) == "Raster.Shrub.2m" |
                                colnames(dfBACKUP) == "Raster.Canopy.p90.2m" |
                                colnames(dfBACKUP) == "Raster.Curvature.Plan.100m" |
                                colnames(dfBACKUP) == "Raster.Curvature.Prof.100m" |
                                colnames(dfBACKUP) == "Raster.SinSlopeCosAspect.2m" |  
                                colnames(dfBACKUP) == "Raster.DEM.2m" |
                                colnames(dfBACKUP) == "Raster.TWI.30m" |
                                substr(colnames(dfBACKUP),1,nchar("Raster.HM")) == "Raster.HM")]
                                  
Allynames <- colnames(dfBACKUP)[substr(colnames(dfBACKUP),1,nchar("Sensor")) == "Sensor"]

for (site in c("Sierra foothills", "Sierra montane")) {  
    
  dfSub <- subset(dfBACKUP, Point.Site == site)
  if (site == "Sierra foothills") {
    listQuantity <- 1:24
  }
  else if (site == "Sierra montane") {
    listQuantity <- 6:24
    }
  
  for (quantity in listQuantity) {
    
    foo <- list()
    foo <- SubsetVarNamesHM(Allxnames, Allynames, quantity)
    xnames <- unlist(foo[1])
    ynames <- unlist(foo[2])
    
    for (yname in ynames) {
      
      if (!(yname %in% paste0(sprintf("Sensor.HM%s.",quantity), c("VarsToIgnore")))) {
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
          dir.create(sprintf("Site(s)=%s_y=%s", paste0(unique(df$Point.Site), collapse=", "), yname))
          dir.create(sprintf("Site(s)=%s_y=%s/PartialDependence", paste0(unique(df$Point.Site), collapse=", "), yname))
          #png(sprintf("Site(s)=%s_y=%s/ModelTuning.png", paste0(unique(df$Point.Site), collapse=", "), yname))
          #plot(gbm.tune)
          #dev.off()
          
          write.csv(gbm.tune$bestTune, sprintf("Site(s)=%s_y=%s/BestModel.csv", paste0(unique(df$Point.Site), collapse=", "), yname))
          write.csv(gbm.tune$results, sprintf("Site(s)=%s_y=%s/TuningResults.csv", paste0(unique(df$Point.Site), collapse=", "), yname))
          
          # This seems to give the same result as rel.inf below, but with less control over the plot:
          #plot(varImp(gbm.tune))
          plot_RelInf <- ggplot(data=summary(gbm.tune$finalModel)) +
            geom_bar(aes(x=reorder(var, rel.inf), y=rel.inf, fill=var), color="black", stat="identity") +
            scale_fill_discrete(guide=FALSE) +
            theme_bw() +
            labs(title = sprintf("Site(s): %s\nDependent variable = %s", paste0(unique(df$Point.Site), collapse=", "), yname),
                 x="Independent variables",
                 y="Relative Influence") +
            coord_flip()
          
          ggsave(file=sprintf("Site(s)=%s_y=%s/RelativeInfluence.png", paste0(unique(df$Point.Site), collapse=", "), yname),
                 plot_RelInf, width=12,height=8, dpi=500)
          dev.off()
  
          # Make predictions using the test data set
          #gbm.pred <- predict.train(gbm.tune,df[,xnames])
          gbm.pred <- extractPrediction(list(gbm.tune), testX=testingdata[,xnames], testY=testingdata[,yname])
          write.csv(gbm.pred,
                    sprintf("Site(s)=%s_y=%s/TestDataPrediction.csv ", paste0(unique(df$Point.Site), collapse=", "), yname))
          
          # Relative influence values
          write.csv(summary(gbm.tune$finalModel),
                    sprintf("Site(s)=%s_y=%s/RelativeInfluence.csv", paste0(unique(df$Point.Site), collapse=", "), yname))
          
          
          nplots <- 0
          group <- 1
          for (varname in summary(gbm.tune$finalModel)$var) {
            nplots <- nplots + 1
            if (nplots == 1) {
              png(sprintf("Site(s)=%s_y=%s/PartialDependence/Plot_%s.png", paste0(unique(df$Point.Site), collapse=", "), yname, group),
                  width=800, height=800, res=100)
              par(mfrow=c(2,2))
              par(mar = c(4, 4, 2, 2), oma = c(1, 1, 1, 1))
              group <- group + 1
            }
            plot.gbm(gbm.tune$finalModel, i.var = varname, lwd=2, col = "blue")
            write.csv(plot.gbm(gbm.tune$finalModel, i.var = varname, lwd=2, col = "blue", return.grid=TRUE),
                      sprintf("Site(s)=%s_y=%s/PartialDependence/%s.csv", paste0(unique(df$Point.Site), collapse=", "), yname, varname))
            if (nplots == 4) {
              nplots <- 0
              dev.off()
            }
          }
          if (length(dev.list()) > 0) dev.off()
          graphics.off()
        }
      }
    }
  }
}
     
        



