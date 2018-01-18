library(ggplot2)


Dir <- "C:/Dropbox/Work/ASU/Paper_1/parameter_set3"
setwd(Dir)







canopyVars <- c("MEAN","MAXIMUM")

# Just read one csv file to find out how many independent variables were used
df <- read.csv(sprintf("%s/50/50m_chm_MAXIMUM/RelativeInfluence.csv", Dir))
NoIndependentVars <- nrow(df)
df <- NULL

ModelDirs <- list.dirs(full.names=FALSE, recursive=FALSE)

my.list <- vector('list', NoIndependentVars * length(ModelDirs) * length(canopyVars))

i <- 1
for (scale in ModelDirs) {
  
  for (var in c("MEAN","MAXIMUM")) {
    DepVar <- sprintf("%sm_chm_%s", scale, var)
    FullDir <- sprintf("%s/%s/%s/PartialDependence", Dir, scale, DepVar)
    listFiles <- list.files(FullDir, full.names=FALSE, recursive=FALSE, pattern="*.csv")
    
    for (file in listFiles) {
      cat(scale, DepVar, file, "\n")
      dfSingle <- read.csv(sprintf("%s/%s", FullDir, file))
      dfSingle$X <- NULL
      IndVar <- colnames(dfSingle)[1]
      dfSingle$x = dfSingle[,IndVar]
      dfSingle$type <- as.factor(class(dfSingle$x))
      dfSingle$scale <- as.numeric(scale)
      dfSingle$var <- as.factor(var)
      if (class(dfSingle$x) %in% c("integer","numeric")) {
        dfSingle$x.numeric <- dfSingle$x
        dfSingle$x.factor <- as.factor(NA)
        dfSingle$x <- NULL
      }
      if (class(dfSingle$x) == "factor") {
        dfSingle$x.numeric <- NA
        dfSingle$x.factor <- as.factor(dfSingle$x)
        dfSingle$x <- NULL
      }
      
      dfSingle[,IndVar] <- NULL
      dfSingle$IndependentVar <- as.factor(IndVar)
      dfSingle$DependentVar <- as.factor(DepVar)
      
      my.list[[i]] <- dfSingle
      i <- i + 1
      
      dfSingle <- NULL
    }
  }
}
df <- do.call('rbind', my.list)
    
  
  
  
for (chmVar in canopyVars) {
  for (IndVar in unique(df$IndependentVar)) {
    
    dfSubset <- subset(df, var == chmVar & IndependentVar == IndVar)
    
    if (unique(dfSubset$type)[1] == "numeric") {
      plot <- ggplot(data = dfSubset) + facet_wrap(~scale) + #, scale="free_x") +
        geom_line(aes(x=x.numeric, y=y)) +
        theme_bw()
    }
    if (unique(dfSubset$type)[1] == "factor") {
      plot <- ggplot(data = dfSubset) + facet_wrap(~scale) + #, scale="free_x") +
        geom_bar(aes(x=x.factor, y=y), stat="identity") +
        theme_bw()
    }
    cat(paste(chmVar,IndVar,unique(dfSubset$type)[1],max(dfSubset$y),min(dfSubset$y),"\n"))
    
    ggsave(file=sprintf("%s~CHM_%s.png", IndVar, chmVar),
           plot, width=12,height=8, dpi=300)
    
    dfSubset <- NULL
    
  }
}








