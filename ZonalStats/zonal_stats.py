# Quarterly Solar Radiation Splitter
#solar_splitter_v2.py

"""
This script is meant to take fishnets and raster and compute individual values for each fishnet cell and each raster
Then output to a table identified by 'FID' which is the feature identifier for each fishnet
"""

import arcpy, os
from arcpy import env
from arcpy.sa import *

from datetime import datetime
start=datetime.now()
start_str=str(start)
print"program start date, time = " + start_str 
from arcpy import env
arcpy.env.overwriteOutput=True
arcpy.CheckOutExtension("Spatial")

baseRoot = r"D:\ASU\PYTHON\zonal_stats"    #working directory 
rasterRoot = r"D:\ASU\PYTHON\zonal_stats\BIOCLIM_CLIP" #raster directory
fcRoot = r"D:\ASU\PYTHON\zonal_stats\FISHNETS_MACRO"

#unique_name = arcpy.CreateUniqueName("outTable")
outFolder = arcpy.CreateFolder_management(baseRoot, "outTable")

arcpy.env.workspace = rasterRoot
rasterList = arcpy.ListRasters()
arcpy.env.workspace = fcRoot
fcList = arcpy.ListFeatureClasses()
arcpy.env.workspace = baseRoot      #set working directory back to baseRoot
print "The root directory for this project is set to: " +str(baseRoot) 

for fc in fcList:
    #arcpy.env.workspace = rasterRoot
    for r in rasterList:
        inZoneData = fcRoot+os.sep+fc
        zoneField = "FID"
        tableName = r[:-4]
        outDBF = str(outFolder) + os.sep + tableName + ".dbf"
        outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, rasterRoot+os.sep+r, outDBF, "NODATA", "ALL")
        print "done: poly = %s, raster = %s..." % (fc,r)

arcpy.env.workspace = fcRoot
for fc in fcList:
    arcpy.env.workspace = str(outFolder)
    tblList = arcpy.ListTables()
    for tbl in tblList:
        arcpy.AddJoin_management(fcRoot+os.sep+fc, "FID", tbl, "OID")
    outFeature  = fc[:-4]+"_joinedTable.shp"
    arcpy.CopyFeatures_management(fc, outFeature)

print "tables joined"

print 'Zonal stats output complete'   
finish = datetime.now()
finish_str=str(finish)
print"program finish date, time = " + finish_str
totaltime= finish-start
print 'total processing time = ' + str(totaltime)
