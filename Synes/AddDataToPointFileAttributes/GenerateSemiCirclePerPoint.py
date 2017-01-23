# -*- coding: utf-8 -*-
"""
Created on Mon Jan 23 12:27:33 2017

@author: Nick
"""

import arcpy, math

pthPoints = r"C:\Dropbox (ASU)\M2NEON\DGPS\CleanedLinked\SJER_TEAK_Sensors.shp"

buffs = []
buffRad = 2.5   
sr = arcpy.Describe(pthPoints).spatialReference
with arcpy.da.SearchCursor(pthPoints,["SHAPE@", "loc_ID"],spatial_reference=sr) as cursor:
    for row in cursor:
        print row[1]
        buff = row[0].buffer(buffRad)
        angFL = 0
        dxFL = math.cos(math.radians(angFL)) * buffRad * 1.5
        dyFL = math.sin(math.radians(angFL)) * buffRad * 1.5
        ptFL = arcpy.PointGeometry(arcpy.Point(row[0].centroid.X - dxFL, row[0].centroid.Y - dyFL))
        angFR = angFL + 180
        dxFR = math.cos(math.radians(angFR)) * buffRad * 1.5 
        dyFR = math.sin(math.radians(angFR)) * buffRad * 1.5 
        ptFR = arcpy.PointGeometry(arcpy.Point(row[0].centroid.X - dxFR, row[0].centroid.Y - dyFR))
        angB = angFL - 90
        dxB = math.cos(math.radians(angB)) * buffRad * 1.5 
        dyB = math.sin(math.radians(angB)) * buffRad * 1.5 
        ptB = arcpy.PointGeometry(arcpy.Point(row[0].centroid.X - dxB, row[0].centroid.Y - dyB))
        triangle = arcpy.Polygon(arcpy.Array([arcpy.Point(ptFL.centroid.X,ptFL.centroid.Y),arcpy.Point(ptFR.centroid.X,ptFR.centroid.Y),arcpy.Point(ptB.centroid.X,ptB.centroid.Y)]),sr)
        semicircle = buff.difference(triangle)
        buffs.append(semicircle)
arcpy.CopyFeatures_management(buffs, r"C:\Dropbox (ASU)\M2NEON\Paper_2\DATA\VECTOR\SensorPoints\SouthFacing_Radius%sm.shp" %buffRad)
arcpy.JoinField_management ("C:\Dropbox (ASU)\M2NEON\Paper_2\DATA\VECTOR\SensorPoints\SouthFacing_Radius%sm.shp" %buffRad, "FID",
                            pthPoints, "FID", ["loc_ID"])



