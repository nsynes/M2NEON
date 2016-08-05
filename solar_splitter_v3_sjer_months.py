# Quarterly Solar Radiation Splitter
#solar_splitter_v3_sjer_months.py

"""
This script is meant to use lidar canopy height models and output solar radiation rasters for quarterly periods

input: Lidar derived digital surface model (DSM)
output: 12 rasters for each month representing the solar areal radiation for a month period to
coincide with the HOBO temp sensors
"""
"""COMMIT CHECK"""
"""COMMIT CHECKONLINE"""

import arcpy 
from datetime import datetime
start=datetime.now()
start_str=str(start)
print"program start date, time = " + start_str 
from arcpy import env
arcpy.env.overwriteOutput=True
arcpy.CheckOutExtension("spatial")

baseRoot = "D:\\ASU\\SOLAR_SCRIPT"    #working directory 
arcpy.env.workspace = baseRoot                                                             #set working directory
print "The root directory for this project is set to: " +str(baseRoot)                     #print root directory folder

nameGDB = "SJER_SOLAR_MONTHS_2013"                                                                    #name of GDB
#inRaster = r"D:\PROJECTS\NEON_D17\ANALYSIS\EXPORT\RASTER\DSM\TEAK_DSM_1m.tif"
inRaster = r"D:\ASU\DSMs\SJER_DSM_CLIP_v1.tif"

outGDB = arcpy.CreateFileGDB_management(baseRoot, nameGDB, "CURRENT")                               #Create GDB
arcpy.env.workspace = str(outGDB)                                                             #set working directory
print "Adding... "+inRaster+" to: Geodatabase = " + nameGDB + "...."
inRaster = arcpy.RasterToGeodatabase_conversion(inRaster, outGDB, "")
rasterList = arcpy.ListRasters()
inRasterName = rasterList[0]
inRaster = str(inRaster)+"\\"+str(inRasterName)
latitude = "36.5" #approx latitude
skySize = "200"
year = "2013"

quarterParameters = ["MultiDays   "+year+"    1   31","MultiDays   "+year+"    32   59",
                     "MultiDays   "+year+"    60   90","MultiDays   "+year+"    91   120",
                     "MultiDays   "+year+"    121   151","MultiDays   "+year+"    152   181",
                     "MultiDays   "+year+"    182   212","MultiDays   "+year+"    213   242",
                     "MultiDays   "+year+"    243   272","MultiDays   "+year+"    273   303",
                     "MultiDays   "+year+"    304   333","MultiDays   "+year+"    334   365"]     #Define the quarters in the quarter parameters
quarterName = ["_m1","_m2","_m3","_m4","_m5","_m6","_m7","_m8","_m9","_m10","_m11","_m12"]
quarterList = [0,1,2,3,4,5,6,7,8,9,10,11]

for quarter in quarterList:
    quarter = quarterList[quarter]
    outRaster = str(outGDB)+"\\SolarRadiation"+str(quarterName[quarter])
    outDR = str(outGDB)+"\\DirectRadiation"+quarterName[quarter]
    outDiffR = str(outGDB)+"\\DiffuseRadiation"+quarterName[quarter]
    outDD = str(outGDB)+"\\DirectDuration"+quarterName[quarter]
    arcpy.gp.AreaSolarRadiation_sa(inRaster,outRaster,latitude,skySize,quarterParameters[quarter], "14", "0.5", "NOINTERVAL", "1", "FROM_DEM", "32", "8", "8", "UNIFORM_SKY", "0.3", "0.5", outDR, outDiffR, outDD)
    print "done with month: #"+ str(quarterName[quarter])

print 'solar rasters output by quarter complete'   
finish = datetime.now()
finish_str=str(finish)
print"program finish date, time = " + finish_str
totaltime= finish-start
print 'total processing time = ' + str(totaltime)
