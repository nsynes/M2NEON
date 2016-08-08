# Quarterly Solar Radiation Splitter
#solar_splitter_v3_sjer_months.py

"""
This script is meant to use lidar canopy height models and output solar radiation rasters for quarterly periods

input: Lidar derived digital surface model (DSM)
output: 12 rasters for each month representing the solar areal radiation for a month period to
coincide with the HOBO temp sensors
"""

import arcpy 
from datetime import datetime
start=datetime.now()
start_str=str(start)
print"program start date, time = " + start_str 
from arcpy import env
arcpy.env.overwriteOutput=True
arcpy.CheckOutExtension("spatial")

#working directory 
baseRoot = "D:\\ASU\\SOLAR_SCRIPT"    
arcpy.env.workspace = baseRoot                                                             
print "The root directory for this project is set to: " +str(baseRoot)                     

nameGDB = "SJER_SOLAR_MONTHS_2013"                                                                    
inRaster = r"D:\ASU\DSMs\SJER_DSM_CLIP_v1.tif"

outGDB = arcpy.CreateFileGDB_management(baseRoot, nameGDB, "CURRENT")                               
arcpy.env.workspace = str(outGDB)                                                            
print "Adding... "+inRaster+" to: Geodatabase = " + nameGDB + "..."
inRaster = arcpy.RasterToGeodatabase_conversion(inRaster, outGDB, "")
rasterList = arcpy.ListRasters()
inRasterName = rasterList[0]
inRaster = str(inRaster)+"\\"+str(inRasterName)
latitude = "36.5" #approx latitude
skySize = "200"
year = "2013"

#Define the moths/quarters
monthParameters = ["MultiDays   "+year+"    1   31","MultiDays   "+year+"    32   59",
                     "MultiDays   "+year+"    60   90","MultiDays   "+year+"    91   120",
                     "MultiDays   "+year+"    121   151","MultiDays   "+year+"    152   181",
                     "MultiDays   "+year+"    182   212","MultiDays   "+year+"    213   242",
                     "MultiDays   "+year+"    243   272","MultiDays   "+year+"    273   303",
                     "MultiDays   "+year+"    304   333","MultiDays   "+year+"    334   365"]      
monthName = ["_m1","_m2","_m3","_m4","_m5","_m6","_m7","_m8","_m9","_m10","_m11","_m12"]
monthList = [0,1,2,3,4,5,6,7,8,9,10,11]

for month in monthList:
    month = monthList[month]
    outRaster = str(outGDB)+"\\SolarRadiation"+str(monthName[month])
    outDR = str(outGDB)+"\\DirectRadiation"+monthName[month]
    outDiffR = str(outGDB)+"\\DiffuseRadiation"+monthName[month]
    outDD = str(outGDB)+"\\DirectDuration"+monthName[month]
    arcpy.gp.AreaSolarRadiation_sa(inRaster,outRaster,latitude,skySize,monthParameters[month],
    "14", "0.5", "NOINTERVAL", "1", "FROM_DEM", "32", "8", "8", "UNIFORM_SKY", "0.3", "0.5",
    outDR, outDiffR, outDD)
    print "done with month: #"+ str(monthName[month])

print 'solar rasters output by quarter complete'   
finish = datetime.now()
finish_str=str(finish)
print"program finish date, time = " + finish_str
totaltime= finish-start
print 'total processing time = ' + str(totaltime)
