# -*- coding: utf-8 -*-
"""
Created on Tue Oct 25 11:48:13 2016

@author: Nick
"""

import arcpy, os
import math

arcpy.CheckOutExtension("Spatial")

pthDEM = r"C:\Dropbox (ASU)\M2NEON\Paper_2\DATA\RASTER\DEM\DEM_2m.tif"
dirOut = r"C:\Dropbox (ASU)\M2NEON\Paper_2\DATA\RASTER\NorthWestness\Calculations"


measurement = "DEGREE"
outSlope = arcpy.sa.Slope(pthDEM, measurement)
outSlope.save(os.path.join(dirOut, "slope_%s.tif" %measurement.lower()))

outAspect = arcpy.sa.Aspect(pthDEM)
pthAspect = os.path.join(dirOut, "aspect_degree.tif")
outAspect.save(pthAspect)

outSetNull = arcpy.sa.SetNull(pthAspect, pthAspect, "VALUE = -1")
pthSetNull = os.path.join(dirOut, "SetNull.tif")
outSetNull.save(pthSetNull)

outRotate = arcpy.sa.Plus(pthSetNull, 45)
pthRotate = os.path.join(dirOut, "Rotate.tif")
outRotate.save(pthRotate)

outNorthWest = arcpy.sa.Cos(outRotate * math.pi / 180.0)
pthNorthWest = os.path.join(dirOut, "NorthWestness.tif")
outNorthWest.save(pthNorthWest)

pthSlopeDegree = os.path.join(dirOut, "slope_degree.tif")
outSinSlope = arcpy.sa.Sin(arcpy.Raster(pthSlopeDegree) * math.pi / 180.0)
pthSinSlope = os.path.join(dirOut, "SinSlope.tif")
outSinSlope.save(pthSinSlope)

outSinSlopeNorthWest = arcpy.Raster(pthSinSlope) * arcpy.Raster(pthNorthWest)
pthSinSlopeNorthWest = os.path.join(dirOut, "SinSlopeNorthWestness.tif")
outSinSlopeNorthWest.save(pthSinSlopeNorthWest)



