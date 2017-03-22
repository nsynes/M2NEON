# -*- coding: utf-8 -*-
"""
Created on Mon Jan 23 12:27:33 2017

@author: Nick
"""

import arcpy, math, pandas, os

arcpy.CheckOutExtension("spatial")

buffRad = 5
pthPoints = r"D:\Dropbox (ASU)\M2NEON\DGPS\CleanedLinked\SJER_TEAK_Sensors.shp"
pthSensorShape = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\VECTOR\SensorPoints\SensorCircle_Radius%sm.shp" %buffRad

"""
buffs = []
sr = arcpy.Describe(pthPoints).spatialReference
with arcpy.da.SearchCursor(pthPoints,["SHAPE@", "loc_ID"],spatial_reference=sr) as cursor:
    for row in cursor:
        buff = row[0].buffer(buffRad)
        angFL = 0
        dxFL = math.cos(math.radians(angFL)) * buffRad * 1.5
        dyFL = math.sin(math.radians(angFL)) * buffRad * 1.5
        ptFL = arcpy.PointGeometry(arcpy.Point(row[0].centroid.X - dxFL, row[0].centroid.Y - dyFL))
        angFR = angFL + 360
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
arcpy.CopyFeatures_management(buffs, pthSensorShape)
arcpy.JoinField_management (pthSensorShape, "FID",
                            pthPoints, "FID", ["loc_ID"])
"""


pthDEMsf = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\RASTER\DEM\SJER_DEM_2m.tif"
pthDEMsm = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\RASTER\DEM\TEAK_DEM_2m.tif"
pthDSMsf = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\RASTER\DSM\SJER_DSM_2m.tif"
pthDSMsm = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\RASTER\DSM\TEAK_DSM_2m.tif"
pthSensorLyr = "SensorCircleLyr"
dirTemp = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\temp"

dirSolarDays = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\SolarDays"
dirSolarPerSensor = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\SolarPerSensor"
dirEmptySensorGdb = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\EmptySensorFC.gdb"
dirRaster = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\RASTER"
pthSensorPoints = r"D:\Dropbox (ASU)\M2NEON\DGPS\CleanedLinked\SJER_TEAK_Sensors.shp"
pthSensorPointLyr = "SensorPointLyr"

if not os.path.exists(dirSolarDays):
            os.mkdir(dirSolarDays)

dfTransSF = pandas.read_csv(r"D:\Dropbox (ASU)\M2NEON\Paper_2\ANALYSIS\AtmosphericTransmittance\SJER_2013.csv", index_col="yday")
dfTransSM = pandas.read_csv(r"D:\Dropbox (ASU)\M2NEON\Paper_2\ANALYSIS\AtmosphericTransmittance\TEAK_2013.csv", index_col="yday")
        
fields = ['loc_ID']

if arcpy.Exists(pthSensorLyr):
    arcpy.Delete_management(pthSensorLyr)
arcpy.MakeFeatureLayer_management (pthSensorShape, pthSensorLyr)

if arcpy.Exists(pthSensorPointLyr):
    arcpy.Delete_management(pthSensorPointLyr)
arcpy.MakeFeatureLayer_management (pthSensorPoints, pthSensorPointLyr)

# Use Python's sorted method to sort rows
for row in sorted(arcpy.da.SearchCursor(pthSensorLyr, fields)):
    locID = str(row[0])
    if locID[-1] == "0" and locID != "tdl_sf100_0":
        Site = locID.split("_")[1][:2]
        if Site == "sf":
            MaxDay = 365
            df = dfTransSF
            pthDEM = pthDEMsf
            pthDSM = pthDSMsf
        elif Site == "sm":
            MaxDay == 346
            df = dfTransSM
            pthDEM = pthDEMsm
            pthDSM = pthDSMsm
        else:
            print "SOMETHING WENT WRONG"
            MaxDay = None
            df = None
            pthDEM = None
            pthDSM = None
            break
        print "#####################################"
        print locID, Site
        arcpy.SelectLayerByAttribute_management(pthSensorLyr, "NEW_SELECTION", "\"loc_ID\" = '%s'" %row[0])
        pthClipDEM = os.path.join(dirTemp, "%s.tif" %row[0])
        pthNewDSM = os.path.join(dirTemp, "DSM_%s.tif" %row[0])
        if not os.path.exists(pthClipDEM):
            arcpy.Clip_management(pthDEM, "#", pthClipDEM, pthSensorLyr, "#", "ClippingGeometry", "NO_MAINTAIN_EXTENT")
        if not os.path.exists(pthNewDSM):
            arcpy.MosaicToNewRaster_management("%s;%s" %(pthClipDEM, pthDSM), os.path.split(pthNewDSM)[0], os.path.split(pthNewDSM)[1],
                                               "#","32_BIT_FLOAT", "2", "1", "FIRST")
                                   
        arcpy.SelectLayerByAttribute_management(pthSensorPointLyr, "NEW_SELECTION", "\"loc_ID\" = '%s'" %row[0])
            
        for day in range(1, MaxDay + 1):
            
            pthOutShp = os.path.join(dirSolarDays, "Solar_%s_%s_CutDSM_2013Day%s.shp" %(locID, Site, day))
            height = ""
            latitude = ""
            skySize = 1000
            if day == 365:
                nextday = 1
            else:
                nextday = day + 1
            timeConfig = arcpy.sa.TimeMultipleDays(2013, day, nextday)
            dayInterval = 1
            hourInterval = 0.5
            eachInterval = "NOINTERVAL"
            zFactor = 1
            SlopeAspect = "FROM_DEM"
            calcDirections = 32
            zenithDivisions = 8
            azimuthDivisions = 8
            diffuseType = "UNIFORM_SKY"
            diffuseProp = 0.3
            transmittivity = df["G/H0"][day]
        
            print "Day: %s, Atmospheric Transmittance: %s" %(day, transmittivity)
            
            arcpy.sa.PointsSolarRadiation(pthNewDSM, pthSensorPointLyr, pthOutShp, height,
                                 latitude, skySize, 
                                 timeConfig, dayInterval, hourInterval, eachInterval, 
                                 zFactor, SlopeAspect, calcDirections, zenithDivisions, 
                                 azimuthDivisions, diffuseType, diffuseProp, 
                                 transmittivity)
            
        pthEmptySensorFC = os.path.join(dirEmptySensorGdb, "%s" %locID)
        if not arcpy.Exists(pthEmptySensorFC):
            arcpy.CopyFeatures_management(pthOutShp, pthEmptySensorFC)
            arcpy.AddField_management(pthEmptySensorFC, "loc_ID", "TEXT")
            arcpy.DeleteField_management(pthEmptySensorFC, "T0")
            cursor = arcpy.UpdateCursor(pthEmptySensorFC)
            row = cursor.next()
            while row:
                row.setValue("loc_ID", locID)
                cursor.updateRow(row)
                row = cursor.next()
        """
        pthSolarSensor = os.path.join(dirSolarPerSensor, "%s.shp" %locID)
        if not os.path.exists(pthSolarSensor):
            arcpy.FeatureClassToShapefile_conversion(pthEmptySensorFC, dirSolarPerSensor)
                
        print "Merging shapefiles"
        MinDay = 1
        for day in range(MinDay, MaxDay + 1):
            print day
            pthSingleDaySensorSolar = os.path.join(dirSolarDays, "Solar_%s_%s_CutDSM_2013Day%s.shp" %(locID, Site, day))
            arcpy.AddField_management(pthSingleDaySensorSolar, "Day%s" %day, "DOUBLE")
            arcpy.CalculateField_management(pthSingleDaySensorSolar, "Day%s" %day, "[T0]", "VB", "")
            arcpy.DeleteField_management(pthSingleDaySensorSolar, "T0")
            
            arcpy.JoinField_management(pthSolarSensor,
                                       "FID",
                                       pthSingleDaySensorSolar,
                                       "FID")
        """
        pthOutShp = None
        
        arcpy.Delete_management(pthNewDSM)
            


