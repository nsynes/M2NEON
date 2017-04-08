# -*- coding: utf-8 -*-
"""
Created on Mon Jan 23 12:27:33 2017

@author: Nick
"""

import arcpy, math

BigRad = 10
SmallRad = "2_5"

pthBigCircle = r"C:\Dropbox (ASU)\M2NEON\Paper_2\DATA\VECTOR\SensorPoints\SouthFacing_Radius%sm.shp" %BigRad
pthSmallCircle = r"C:\Dropbox (ASU)\M2NEON\Paper_2\DATA\VECTOR\SensorPoints\SouthFacing_Radius%sm.shp" % SmallRad
arcpy.MakeFeatureLayer_management(pthBigCircle, "BigCircle")
arcpy.MakeFeatureLayer_management(pthSmallCircle, "SmallCircle")

listSensors = []
sr = arcpy.Describe(pthBigCircle).spatialReference

with arcpy.da.SearchCursor(pthBigCircle,["SHAPE@", "loc_ID"],spatial_reference=sr) as cursor:
    for row in cursor:
        print "_________________"
        for smallrow in arcpy.da.SearchCursor(pthSmallCircle,["SHAPE@", "loc_ID"],spatial_reference=sr):
            if smallrow[1] == row[1]:
                print smallrow[1], row[1]

                arcpy.SelectLayerByAttribute_management("BigCircle", "NEW_SELECTION", ' "loc_ID" = \'%s\' ' %row[1])

                arcpy.SelectLayerByAttribute_management("SmallCircle", "NEW_SELECTION", ' "loc_ID" = \'%s\' ' %row[1])

                SensorFile = r"C:\Dropbox (ASU)\M2NEON\Paper_2\DATA\VECTOR\SensorPoints\temp\TEST_%s.shp" %row[1]
                
                arcpy.Erase_analysis("BigCircle", "SmallCircle", SensorFile)

                listSensors.append(SensorFile)
        


arcpy.Merge_management(listSensors, "C:\Dropbox (ASU)\M2NEON\Paper_2\DATA\VECTOR\SensorPoints\SouthFacing_Radius%smCut.shp" %BigRad)

