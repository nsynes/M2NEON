# Quarterly Solar Radiation Splitter
#solar_splitter_v2.py

"""
This script is meant to use lidar canopy height models and output solar radiation rasters for quarterly periods

input: Lidar derived digital surface model (DSM)
output: 4 rasters for each quarter representing the solar areal radiation for a three month period to
coincide with the BIOCLIM variables
"""

import arcpy 
from datetime import datetime
start=datetime.now()
start_str=str(start)
print"program start date, time = " + start_str 
from arcpy import env
arcpy.env.overwriteOutput=True
arcpy.CheckOutExtension("spatial")

baseRoot = "D:\\PROJECTS\\NEON_D17\\ANALYSIS\\EXPORT\\RASTER\\SOLAR\\SOLAR_SCRIPT_TEST"    #working directory 
arcpy.env.workspace = baseRoot                                                             #set working directory
print "The root directory for this project is set to: " +str(baseRoot)                     #print root directory folder

nameGDB = "TEAK_SOLAR_QUARTERS"                                                                    #name of GDB
inRaster = r"D:\PROJECTS\NEON_D17\ANALYSIS\EXPORT\RASTER\DSM\TEAK_DSM_1m.tif"
#inRaster = r"D:\PROJECTS\NEON_D17\ANALYSIS\EXPORT\RASTER\DSM\SJER_DSM_CLIP_v1.tif"

outGDB = arcpy.CreateFileGDB_management(baseRoot, nameGDB, "CURRENT")                               #Create GDB
arcpy.env.workspace = str(outGDB)                                                             #set working directory
print "Adding"+inRaster+" to: Geodatabase = " + nameGDB + "...."
inRaster = arcpy.RasterToGeodatabase_conversion(inRaster, outGDB, "")
rasterList = arcpy.ListRasters()
inRasterName = rasterList[0]
inRaster = str(inRaster)+"\\"+str(inRasterName)
latitude = "36.5" #approx latitude
skySize = "200"
year = "2016"

quarterParameters = ["MultiDays   "+year+"    1   91","MultiDays   "+year+"    92   183",
               "MultiDays   "+year+"    184   275","MultiDays   "+year+"    276   366"]     #Define the quarters in the quarter parameters
quarterName = ["_q1","_q2","_q3","_q4"]
quarterList = [0,1,2,3]

for quarter in quarterList:
    quarter = quarterList[quarter]
    outRaster = str(outGDB)+"\\SolarRadiation"+str(quarterName[quarter])
    outDR = str(outGDB)+"\\DirectRadiation"+quarterName[quarter]
    outDiffR = str(outGDB)+"\\DiffuseRadiation"+quarterName[quarter]
    outDD = str(outGDB)+"\\DirectDuration"+quarterName[quarter]
    arcpy.gp.AreaSolarRadiation_sa(inRaster,outRaster,latitude,skySize,quarterParameters[quarter], "14", "0.5", "NOINTERVAL", "1", "FROM_DEM", "32", "8", "8", "UNIFORM_SKY", "0.3", "0.5", outDR, outDiffR, outDD)
    print "done with quarter: #"+ str(quarter+1)

print 'solar rasters output by quarter complete'   
finish = datetime.now()
finish_str=str(finish)
print"program finish date, time = " + finish_str
totaltime= finish-start
print 'total processing time = ' + str(totaltime)
