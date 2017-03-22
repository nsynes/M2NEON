library(ggplot2)
library(grid)
library(plyr)
library(scales)
library(stringr)

setwd("C:/Dropbox (ASU)/M2NEON/Paper_1/DATA/ForSynes")

###############################
# Load data
df <- read.csv("MergedGbmData.csv")
foo <- str_split_fixed(df$IndependentVar, "_",2)
foo <-as.data.frame(foo)
df <- cbind(df, foo)
dfVars <- read.csv("VariableList.csv")
df <- merge(df, dfVars, by.x="V1", by.y="StagingName")
df$VarType <- as.factor(substr(df$IndependentVar,1,1))
df$IndVarFull <- as.factor(sprintf("%s = %s", df$IndependentVar, df$Description))

dfRelInf <- df
dfRelInf$value <- dfRelInf$RelInf
dfRelInf$RelInf <- NULL
dfRelInf$ModelRsquared <- NULL
dfRelInf$ValueType <- "Relative Influence (%)"
dfRsquared <- df
dfRsquared$value <- dfRsquared$ModelRsquared
dfRsquared$ModelRsquared <- NULL
dfRsquared$RelInf <- NULL
dfRsquared$ValueType <- "R-squared"
df <- rbind(dfRelInf, dfRsquared)
dfRelInf <- NULL
dfRsquared <- NULL
###############################


###############################
# Create a palette that maintains the same color group for each variable group
UniqueNames <- levels(df$IndVarFull)
nBioClim <- sum(substr(UniqueNames,1,1) == "b")
nFlints <- sum(substr(UniqueNames,1,1) == "f")
nTopo <- sum(substr(UniqueNames,1,1) == "t")
pal <- c(colorRampPalette(c("blue", "white"))(nBioClim+2)[1:nBioClim],
         colorRampPalette(c("red", "white"))(nFlints+2)[1:nFlints],
         colorRampPalette(c("green", "white"))(nTopo+2)[1:nTopo])
###############################


###############################
# Plotting
p1 <- ggplot() + facet_wrap(~ValueType, scales="free_y", ncol=1) +
  geom_bar(data = subset(df, ValueType=="Relative Influence (%)"), aes(x=as.factor(Scale), y=value, fill=IndVarFull), color="black", stat="identity") +
  geom_point(data = subset(df, ValueType=="R-squared"), aes(x=as.factor(Scale), y=value), color="black", size=5) +
  scale_fill_manual(values=pal) +
  labs(title=sprintf("Dependent variable = %s", unique(df$DependentVar)), x="Scale (m)", y="", fill="Independent variable") +
  theme_bw()

ggsave(file=sprintf("GradientAcrossScales_AllIndVars.png"),
       p1, width=30,height=20, dpi=500)
###############################



###############################
# SUBSET VERSION
nSubVars <- 5
dfSub <- subset(df, Rank <= nSubVars)
dfSub$IndVarFull <- droplevels(dfSub$IndVarFull)
###############################


###############################
# Create a palette that maintains the same color group for each variable group
UniqueNames <- levels(dfSub$IndVarFull)
nBioClim <- sum(substr(UniqueNames,1,1) == "b")
nFlints <- sum(substr(UniqueNames,1,1) == "f")
nTopo <- sum(substr(UniqueNames,1,1) == "t")
pal <- c(colorRampPalette(c("blue", "white"))(nBioClim+2)[1:nBioClim],
         colorRampPalette(c("red", "white"))(nFlints+2)[1:nFlints],
         colorRampPalette(c("green", "white"))(nTopo+2)[1:nTopo])
###############################


###############################
# Plotting
p2 <- ggplot() + facet_wrap(~ValueType, scales="free_y", ncol=1) +
  geom_bar(data = subset(dfSub, ValueType=="Relative Influence (%)"), aes(x=as.factor(Scale), y=value, fill=IndVarFull), color="black", stat="identity") +
  geom_point(data = subset(dfSub, ValueType=="R-squared"), aes(x=as.factor(Scale), y=value), color="black", size=5) +
  scale_fill_manual(values=pal) +
  labs(title=sprintf("Dependent variable = %s\nTop %s contributing variables", unique(df$DependentVar)[1], nSubVars), x="Scale (m)", y="", fill="Independent variable") +
  theme_bw()

ggsave(file=sprintf("GradientAcrossScales_Top%sVars.png", nSubVars),
       p2, width=12,height=8, dpi=500)
###############################







