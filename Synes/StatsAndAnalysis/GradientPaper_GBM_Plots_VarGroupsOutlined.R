library(ggplot2)
library(grid)
library(plyr)
library(scales)
library(stringr)
library(RColorBrewer)


############################
# PARAMETERS
# NEED TO RUN THE PYTHON SCRIPT TO GENERATE "MergedGbmData_**.csv" BEFORE RUNNING THIS
###########################
# location of the simulation results folders (25,50,100,250,500,1000)
setwd("C:/Dropbox/Work/ASU/Paper_1/21-11-17/p3")
# name of the dependent variable (You need to run this script for each dependent variable)
depvar <- "chm_MEAN"
# This is a specific file I created where you can put the descriptions for each variable
# that will appear in the legend. If a variable is not in this list, then there might be problems
# and it might not show in the graphs.
IndependentVariableList <- "C:/Dropbox/Work/ASU/Paper_1/VariableList_v9.csv"
###########################
###########################



###############################
# Colour Brewer palette
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#n <- 40
#pie(rep(1,n), col=sample(col_vector, n))
################################



###############################
MergedGbmFile <- sprintf("MergedGbmData_%s.csv", depvar)
# Load data
df <- read.csv(MergedGbmFile)
dfVars <- read.csv(IndependentVariableList)
df <- merge(df, dfVars, by.x="IndependentVar", by.y="StagingName")
df$IndVarFull <- as.factor(sprintf("%s = %s", df$IndependentVar, df$Description))

dfRelInf <- df
dfRelInf$value <- dfRelInf$RelInf
dfRelInf$RelInf <- NULL
dfRelInf$ModelRsquared <- NULL
dfRelInf$ValueType <- as.factor("Relative Influence (%)")
dfRsquared <- df
dfRsquared$value <- dfRsquared$ModelRsquared
dfRsquared$ModelRsquared <- NULL
dfRsquared$RelInf <- NULL
dfRsquared$ValueType <- as.factor("R-squared")
df <- rbind(dfRelInf, dfRsquared)
dfRelInf <- NULL
dfRsquared <- NULL
###############################




df$IndVarFull <- factor(df$IndVarFull, levels=unique(df[order(df$VarType),]$IndVarFull), ordered=TRUE)

dfSub <- subset(df, ValueType=="Relative Influence (%)")
dfSummary <- aggregate(dfSub$value, by=list(VarType=dfSub$VarType, ValueType=dfSub$ValueType, Scale=dfSub$Scale), FUN=sum)
dfSub <- NULL



###############################
###############################
# PLOTTING
###############################
###############################


###############################
# NEW version with colour lines representing var group and fill colour representing specific variable
###############################
nVars <- length(unique(df$IndependentVar))

pal <- sample(col_vector, nVars)

pz <- ggplot() +
  #facet_wrap(~ValueType, scales="fixed", ncol=1) +
  geom_bar(data = subset(df, ValueType=="Relative Influence (%)"), aes(x=as.factor(Scale), y=value/100, fill=IndVarFull),
           color="black", stat="identity") +
  geom_bar(data = dfSummary, aes(x=as.factor(Scale), y=x/100, colour=VarType),
           stat="identity", width=0.9, fill=NA, size=2) +
  #geom_point(data = subset(df, ValueType=="R-squared"), aes(x=as.factor(Scale), y=value), color="black", size=5) +
  scale_colour_manual(values=c("blue","black","green")) +
  scale_fill_manual(values=pal) +
  lims(y = c(0,1.01)) +
  labs(title=sprintf("Dependent variable = %s", unique(df$DependentVar)), x="Scale (m)", y="", fill="Independent variable") +
  theme_bw() +
  guides(fill=guide_legend(ncol=1))

ggsave(file=sprintf("GradientAcrossScales_AllVars_%s.png", depvar),
       pz, width=12,height=8, dpi=300)
###############################

pr <- ggplot() +
  geom_point(data = subset(df, ValueType=="R-squared"), aes(x=as.factor(Scale), y=value), color="black", size=5) +
  lims(y = c(0,1.01)) +
  labs(title=sprintf("Dependent variable = %s", unique(df$DependentVar)), x="Scale (m)", y="", fill="Independent variable") +
  theme_bw() +
  guides(fill=guide_legend(ncol=1))

ggsave(file=sprintf("GradientAcrossScales_rsquared_%s.png", depvar),
       pr, width=12,height=8, dpi=300)


################################
# Graphs with top N contributing variables
################################
for (nSubVars in c(5,10,15)) {
  ###############################
  # SUBSET
  dfSub <- subset(df, Rank <= nSubVars)
  dfSub$IndVarFull <- droplevels(dfSub$IndVarFull)
  ###############################
  
  dfRel <- subset(dfSub, ValueType=="Relative Influence (%)")
  dfSummary <- aggregate(dfRel$value, by=list(VarType=dfRel$VarType, ValueType=dfRel$ValueType, Scale=dfRel$Scale), FUN=sum)
  dfRel <- NULL
  
  
  ###############################
  # Create palette based on total number of vars in plot
  nVars <- length(unique(dfSub$IndependentVar))
  
  pal <- sample(col_vector, nVars)
  
  ###############################
  
  dfSub$IndVarFull <- factor(dfSub$IndVarFull, levels=unique(dfSub[order(dfSub$VarType),]$IndVarFull), ordered=TRUE)
  
  ###############################
  # Plotting
  p2 <- ggplot() + 
    #facet_wrap(~ValueType, scales="fixed", ncol=1) +
    geom_bar(data = subset(dfSub, ValueType=="Relative Influence (%)"), aes(x=as.factor(Scale), y=value/100, fill=IndVarFull), color="black", stat="identity") +
    geom_bar(data = dfSummary, aes(x=as.factor(Scale), y=x/100, colour=VarType),
             stat="identity", width=0.9, fill=NA, size=2) +
    #geom_point(data = subset(dfSub, ValueType=="R-squared"), aes(x=as.factor(Scale), y=value), color="black", size=5) +
    scale_fill_manual(values=pal) +
    scale_colour_manual(values=c("blue","black","green")) +
    lims(y = c(0,1.01)) +
    labs(title=sprintf("Dependent variable = %s\nTop %s contributing variables", unique(df$DependentVar)[1], nSubVars), x="Scale (m)", y="", fill="Independent variable") +
    theme_bw() +
    guides(fill=guide_legend(ncol=1))
  
  ggsave(file=sprintf("GradientAcrossScales_Top%sVars_%s.png", nSubVars, depvar),
         p2, width=12,height=8, dpi=300)

###############################
}






