
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
library(plyr)
library(scales)
library(lattice)
library(grid)
library(rgl)
library(akima)




# Get the functions which I have stored in a separate file
source("C:/Dropbox (ASU)/M2NEON/GitHub/M2NEON/Synes/SensorDataCleaning/M2NEON_Rfunctions.R")

# Load Data
setwd("C:/Dropbox (ASU)/M2NEON/SensorData")
dfAll <- read.csv("Merged_RasterAndSensorData_2013.csv")

# Set site, and variables to include
SubSite = "SM"
# Dependent variable
TempVar = "Sensor.HM14.MeanDailyMin"

# Independent variables
slopasp = "Raster.SinSlopeCosAspect.2m"
dem = "Raster.DEM.2m"
canopy = "Raster.Canopy.p75.2m"
shrub = "Raster.Shrub.2m"
cov = "Raster.Cover"

# Subset to a single site
dfSite <- subset(dfAll, Point.Site == SubSite)

# Select only the chosen variables and omit NAs
dfSub <- na.omit(dfSite[c("Point.loc_ID","Point.coords.x1","Point.coords.x2", dem, TempVar, slopasp, canopy, shrub)])

# Normalise (or previously just scale from 0 to 1)
#dfSub[,cov] = scale(dfSub[,canopy]) + scale(dfSub[,shrub])
dfSub[,cov] = (dfSub[,canopy]/ max(dfSub[,canopy])) + (dfSub[,shrub] / max(dfSub[,shrub]))

# Order the datafrane by temperature (for colouring)
dfSub <- dfSub[order(dfSub[,TempVar]),]


# 2D linear regression: just temperature in terms of cover
fit <- lm(dfSub[,TempVar] ~ dfSub[,cov])

# Plot the 2D data and the regression line
plot(dfSub[,cov],dfSub[,TempVar])
abline(lm(dfSub[,TempVar] ~ dfSub[,cov]))

# Create a dataframe and add residuals to the dataframe
# (for exporting and plotting residual maps)
if (FALSE) {
dfSub2D <- dfSub
dfSub2D$resid <- fit$resid
write.csv(dfSub2D, sprintf("%s~%s.csv", TempVar,cov))
}

# 3D linear regression:  temperature as function of slopasp and cover
fit <- lm(dfSub[,TempVar] ~ dfSub[,cov] + dfSub[,slopasp])

# Create a dataframe and add residuals to the dataframe
# (for exporting and plotting residual maps)
if (FALSE) {
dfSub3D <- dfSub
dfSub3D$resid <- fit$resid
write.csv(dfSub3D, sprintf("%s~%s + %s.csv", TempVar,slopasp,cov))
}

# Set up for creating a 3D plane
n <- 200
x1 <- 4*runif(n)
x2 <- 4*runif(n)
y <- f(x1, x2)

# Colour the points according to temperature
c = dfSub[,TempVar]
c = cut(c, breaks=length(c))
cols = rev(rainbow(length(c), start=0, end=1/6)[as.numeric(c)])


# Plot of dem, aspect and cover, with temperature by colour - but not enough points for this to work very well
#plot3d(dfSub[,slopasp], dfSub[,cov], dfSub[,dem], col=cols, size=5, lwd=15, xlab=slopasp, ylab=cov, zlab=dem,
#       main=sprintf("Site: %s", SubSite),sub = sprintf("%s = (%0.4f * %s) + (%0.4f * %s)", dem, coef(fit)[2], cov, coef(fit)[3], slopasp))


plot3d(dfSub[,slopasp], dfSub[,cov], dfSub[,TempVar], col=cols, size=5, lwd=15, xlab=slopasp, ylab=cov, zlab=TempVar,
       main=sprintf("Site: %s", SubSite),sub = sprintf("%s = (%0.4f * %s) + (%0.4f * %s)", TempVar, coef(fit)[2], cov, coef(fit)[3], slopasp))

my_surface(f)





###############################################
# 3D PLOTTING
my_surface <- function(f, n=10, ...) { 
  ranges <- rgl:::.getRanges()
  x <- seq(ranges$xlim[1], ranges$xlim[2], length=n)
  y <- seq(ranges$ylim[1], ranges$ylim[2], length=n)
  z <- outer(x,y,f)
  surface3d(x, y, z, alpha=0.2, ...)
  surface3d(x, y, z, color="black", front="lines", alpha=0.5, ...)
  surface3d(x, y, z, color="black", back="lines", alpha=0.5, ...)
}

# Function to generate 3D plane
f <- function(x1, x2) {
  coef(fit)[1] + (coef(fit)[2] * x2) + (coef(fit)[3] * x1)
}




