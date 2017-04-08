library(ggplot2)
library(grid)
library(plyr)
library(scales)
library(stringr)

############################
# PARAMETERS
# NEED TO RUN THE PYTHON SCRIPT TO GENERATE "MergedGbmData_**.csv" BEFORE RUNNING THIS
###########################
# location of the simulation results folders (25,50,100,250,500,1000)
setwd("C:/Dropbox (ASU)/M2NEON/Paper_1/ANALYSIS/GBM_US_IALE/parameter_set3")
# name of the dependent variable (You need to run this script for each dependent variable)
depvar <- "chm_MAXIMUM"
# This is a specific file I created where you can put the descriptions for each variable
# that will appear in the legend. If a variable is not in this list, then there might be problems
# and it might not show in the graphs.
IndependentVariableList <- "C:/Dropbox (ASU)/M2NEON/Paper_1/ANALYSIS/VariableList.csv"
###########################
###########################





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



###############################
# Create a palette that maintains the same color group for each variable group
#nBioClim <- length(unique(subset(df, VarType == "bioclim")$IndependentVar))
nFlints <- length(unique(subset(df, VarType == "flints")$IndependentVar))
nSoil <- length(unique(subset(df, VarType == "soil")$IndependentVar))
nTopo <- length(unique(subset(df, VarType == "topo")$IndependentVar))
pal <- c(#colorRampPalette(c("blue", "white"))(nBioClim+2)[1:nBioClim],
         colorRampPalette(c("red", "white"))(nFlints+2)[1:nFlints],
         colorRampPalette(c("purple", "white"))(nSoil+2)[1:nSoil],
         colorRampPalette(c("green", "white"))(nTopo+2)[1:nTopo])

df$IndVarFull <- factor(df$IndVarFull, levels=unique(df[order(df$VarType),]$IndVarFull), ordered=TRUE)
###############################



###############################
# Plotting
p1 <- ggplot() + facet_wrap(~ValueType, scales="fixed", ncol=1) +
  geom_bar(data = subset(df, ValueType=="Relative Influence (%)"), aes(x=as.factor(Scale), y=value/100, fill=IndVarFull), color="black", stat="identity") +
  geom_point(data = subset(df, ValueType=="R-squared"), aes(x=as.factor(Scale), y=value), color="black", size=5) +
  scale_fill_manual(values=pal) +
  lims(y = c(0,1.01)) +
  labs(title=sprintf("Dependent variable = %s", unique(df$DependentVar)), x="Scale (m)", y="", fill="Independent variable") +
  theme_bw() +
  guides(fill=guide_legend(ncol=2))

ggsave(file=sprintf("GradientAcrossScales_AllIndVars_%s.png", depvar),
       p1, width=20,height=15, dpi=300)

######
# Sum data by scale and variable type for trend lines across scales
bar <- subset(df, ValueType == "Relative Influence (%)")
dfTrends <- aggregate(bar$value, by=list(VarType=bar$VarType, Scale=bar$Scale), FUN=sum)
dfTrends$CatSum <- dfTrends$x
dfTrends$x <- NULL
df <- merge(df, dfTrends, by=c("VarType", "Scale"))
bar <- NULL
dfTrends <- NULL
######

# lines
p1a <- ggplot() + facet_wrap(~ValueType, scales="fixed", ncol=1) +
  geom_line(data = subset(df, ValueType=="Relative Influence (%)"), aes(x=as.factor(Scale), y=CatSum/100, group=VarType, color=VarType), size=2) +
  geom_point(data = subset(df, ValueType=="R-squared"), aes(x=as.factor(Scale), y=value), color="black", size=2) +
  scale_color_manual(values=c("red","purple","green")) +
  lims(y = c(0,1)) +
  labs(title=sprintf("Dependent variable = %s", unique(df$DependentVar)), x="Scale (m)", y="Relative influence (%)", color="Variable type") +
  #guides(size=FALSE, color=guide_legend(override.aes=list(size=c(2)))) +
  theme_bw()

ggsave(file=sprintf("GradientAcrossScales_lines_AllIndVars_%s.png", depvar),
       p1a, width=6,height=6, dpi=300)
###############################



for (nSubVars in c(5,10,15)) {
###############################
# SUBSET VERSION
  dfSub <- subset(df, Rank <= nSubVars)
  dfSub$IndVarFull <- droplevels(dfSub$IndVarFull)
  ###############################
  
  
  ###############################
  # Create a palette that maintains the same color group for each variable group
  #nBioClim <- length(unique(subset(dfSub, VarType == "bioclim")$IndependentVar))
  nFlints <- length(unique(subset(dfSub, VarType == "flints")$IndependentVar))
  nSoil <- length(unique(subset(dfSub, VarType == "soil")$IndependentVar))
  nTopo <- length(unique(subset(dfSub, VarType == "topo")$IndependentVar))
  pal <- c(#colorRampPalette(c("blue", "white"))(nBioClim+2)[1:nBioClim],
           colorRampPalette(c("red", "white"))(nFlints+2)[1:nFlints],
           colorRampPalette(c("purple", "white"))(nSoil+2)[1:nSoil],
           colorRampPalette(c("green", "white"))(nTopo+2)[1:nTopo])
  
  dfSub$IndVarFull <- factor(dfSub$IndVarFull, levels=unique(dfSub[order(dfSub$VarType),]$IndVarFull), ordered=TRUE)
  ###############################
  
  
  ###############################
  # Plotting
  p2 <- ggplot() + facet_wrap(~ValueType, scales="fixed", ncol=1) +
    geom_bar(data = subset(dfSub, ValueType=="Relative Influence (%)"), aes(x=as.factor(Scale), y=value/100, fill=IndVarFull), color="black", stat="identity") +
    geom_point(data = subset(dfSub, ValueType=="R-squared"), aes(x=as.factor(Scale), y=value), color="black", size=5) +
    scale_fill_manual(values=pal) +
    lims(y = c(0,1)) +
    labs(title=sprintf("Dependent variable = %s\nTop %s contributing variables", unique(df$DependentVar)[1], nSubVars), x="Scale (m)", y="", fill="Independent variable") +
    theme_bw()
  
  ggsave(file=sprintf("GradientAcrossScales_Top%sVars_%s.png", nSubVars, depvar),
         p2, width=12,height=8, dpi=300)

###############################
}


####################################
# Soil family partial dependence
#####################################

my.datalist <- vector('list', 6)
i <- 1
for (scale in c(25,50,100,250,500,1000)) {
  df <- read.csv(sprintf("C:/Dropbox (ASU)/M2NEON/Paper_1/ANALYSIS/GBM_US_IALE/parameter_set1/%s/%sm_%s/PartialDependence/soil_fam.csv",scale,scale,depvar))
  df$X <- NULL
  df$Scale <- scale
  my.datalist[[i]] <- df
  i <- i + 1
}

dfAll <- do.call('rbind', my.datalist)

bar <- subset(dfAll, Scale == 25)
dfAll$soil_fam <- factor(dfAll$soil_fam, levels=bar[order(-bar$y),]$soil_fam, ordered=TRUE)

p3 <- ggplot() +
  facet_wrap(~Scale) +
  #geom_bar(data=na.omit(dfAll), aes(x=soil_fam, y=y, fill=soil_fam), stat="identity") +
  geom_bar(data=dfAll, aes(x=soil_fam, y=y, fill=soil_fam), stat="identity") +
  scale_fill_discrete(h=c(150,0)) +
  labs(title = "Soil family partial dependence at each scale", x="Soil family", y=depvar) +
  theme_bw() +
  theme(axis.text.x  = element_blank())


ggsave(file=sprintf("SoilFamily_PartialDependence_%s_AllScales.png", depvar),
       p3, width=12,height=8, dpi=300)







