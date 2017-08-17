library(ggplot2)
library(scales)


TopDir <- "D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Results/5_DistToStreamOverFlowAccum"
SimDir <- sprintf("%s/SiteLevel", TopDir)

ResidualDir <- sprintf("%s/Residuals", SimDir)
setwd(ResidualDir)

dfResiduals <- read.csv(sprintf("%s/CSVs/Residuals.csv", ResidualDir))

dfVariables <- read.csv("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/NestedModel/Input/SiteLevel_AllVars.csv")

############################################
# Plots of atmospheric transmittance ~ residuals
###########################################
dfAtmosTransSF <- read.csv("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/AtmosphericTransmittance/SJER_2013.csv")
dfAtmosTransSF$Site <- "sf"
dfAtmosTransSM <- read.csv("D:/Dropbox (ASU)/M2NEON/Paper_2/ANALYSIS/AtmosphericTransmittance/TEAK_2013.csv")
dfAtmosTransSM$Site <- "sm"
dfAtmosTrans <- rbind(dfAtmosTransSF, dfAtmosTransSM)
dfAtmosTransSF <- NULL
dfAtmosTransSM <- NULL
dfAtmosTrans$H0 <- NULL
dfAtmosTrans$G <- NULL
dfAtmosTrans$Date <- NULL
dfAtmosTrans <- plyr::rename(dfAtmosTrans, c("G.H0" = "AtmosTrans",
                                             "yday" = "Day"))

dfAT <- merge(dfAtmosTrans, dfResiduals, by=c("Day","Site"))

for (dep in c("Max","Min","DiurnalRange")) {
  p <-ggplot() +
    geom_point(data=subset(dfAT, DepVar == dep), aes(x=AtmosTrans, y=Residual), size=0.2, shape=3) +
    geom_hline(yintercept=0, color="grey") +
    lims(y = c(-25, 25)) +
    facet_wrap(~Site, ncol=1, scales="free_x") +
    labs(x="Atmospheric Transmittance", title=sprintf("Dependent variable = %s", dep)) +
    theme_bw()
  
  ggsave(file=sprintf("AtmosTrans~Residual_DepVar=%s.png", dep),
         p, width=6,height=10, dpi=300)
}


################################################
#################################################

varlist <- colnames(dfVariables)[substr(colnames(dfVariables),1,6) == "Indep." & substr(colnames(dfVariables),7,9) != "Day"]


for (dep in c("Max","Min","DiurnalRange")) {
  for (var in c("Indep.CanopyDensity.Circle_Radius90m","Indep.DistToStreamOverFlowAccum")) {
    
    dfVar <- dfVariables[c("Point.loc_ID",var)]
  
  
  df <- merge(dfVar, dfResiduals, by.x="Point.loc_ID", by.y="loc_ID")
  df <- subset(df, DepVar == dep)
  df$Site <- substr(df$Point.loc_ID,5,6)
  df$Garden <- substr(df$Point.loc_ID,7,7)
  
  #df <- subset(df, Day >=180 & Day <= 184)
  
  
  p <- ggplot() +
    geom_point(data=df, aes(x=get(var), y=Residual, color=Day), size=1, shape=3) +
    geom_hline(yintercept=0, color="grey") +
    scale_x_continuous(limits=c(0.1,0.7)) +
    facet_wrap(~Site, ncol=1, scales="free_x") +
    labs(title=sprintf("Dependent variable: %s", dep), x=var) +
    theme_bw()
  
  ggsave(file=sprintf("%s~Residual_DepVar=%s_FOCUSSED.png", var, dep),
         p, width=6,height=6, dpi=300)
  }
}










