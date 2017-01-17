# -*- coding: utf-8 -*-
"""
Created on Tue Oct 25 11:48:13 2016

@author: Nick
"""

import arcpy, os
import math

arcpy.CheckOutExtension("Spatial")

pthDEM = r"C:\Dropbox (ASU)\M2NEON\Rasters\DEM\DEM.tif"
dirOut = r"C:\Dropbox (ASU)\M2NEON\Rasters\DEM"
dirTemp = r"C:\Dropbox (ASU)\M2NEON\Rasters\DEM\temp"
os.mkdir(dirTemp)


for measurement in ["DEGREE","PERCENT_RISE"]:
    print measurement
    outSlope = arcpy.sa.Slope(pthDEM, measurement)
    outSlope.save(os.path.join(dirOut, "slope_%s.tif" %measurement.lower()))

outAspect = arcpy.sa.Aspect(pthDEM)
pthAspect = os.path.join(dirOut, "aspect_degree.tif")
outAspect.save(pthAspect)

outSetNull = arcpy.sa.SetNull(pthAspect, pthAspect, "VALUE = -1")
pthSetNull = os.path.join(dirTemp, "SetNull.tif")
outSetNull.save(pthSetNull)

outCosAspect = arcpy.sa.Cos(outSetNull * math.pi / 180.0)
pthCosAspect = os.path.join(dirOut, "CosAspect.tif")
outCosAspect.save(pthCosAspect)

outSinAspect = arcpy.sa.Sin(outSetNull * math.pi / 180.0)
pthSinAspect = os.path.join(dirOut, "SinAspect.tif")
outSinAspect.save(pthSinAspect)

pthSlopePercentRise = os.path.join(dirOut, "slope_percent_rise.tif")
outSlopeCosAspect = arcpy.Raster(pthSlopePercentRise) * arcpy.Raster(pthCosAspect)
pthSlopeCosAspect = os.path.join(dirOut, "SlopeCosAspect.tif")
outSlopeCosAspect.save(pthSlopeCosAspect)






