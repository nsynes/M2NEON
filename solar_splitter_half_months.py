# Monthly Solar Radiation Splitter
#solar_splitter_half_months.py

"""
Author:Fricker

This script is meant to use lidar dervoed canopy surface models and
output solar radiation rasters for approximately 2 week periods.
The script is based on the monthly solar splitter, but the script splits
the month in half regardless of actual time period length

input: Lidar derived digital surface model (DSM)
output: 24 rasters for each half/month representing the solar areal
radiation for a month period to coincide with the HOBO temp sensors
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
baseRoot = r"D:\PROJECTS\NEON_D17\ANALYSIS\EXPORT\RASTER\SOLAR\monthly_solar_splitter"    
arcpy.env.workspace = baseRoot                                                             
print "The root directory for this project is set to: " +str(baseRoot)                     

nameGDB = "TEAK_SOLAR_HALF_MONTHS_2013_Buffered150m"                                                                    
inRaster = r"D:\PROJECTS\NEON_D17\ANALYSIS\EXPORT\RASTER\DSM\TEAK_DSM_1m.tif"

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
monthParameters = ["MultiDays   "+year+"    1   15","MultiDays   "+year+"    16   31",
                   "MultiDays   "+year+"    32   45","MultiDays   "+year+"    46   59",
                   "MultiDays   "+year+"    60   75","MultiDays   "+year+"    76   90",
                   "MultiDays   "+year+"    91   105","MultiDays   "+year+"    106   120",
                   "MultiDays   "+year+"    121   136","MultiDays   "+year+"    137   151",
                   "MultiDays   "+year+"    152   166","MultiDays   "+year+"    167   181",
                   "MultiDays   "+year+"    182   197","MultiDays   "+year+"    198   212",
                   "MultiDays   "+year+"    213   227","MultiDays   "+year+"    228   242",
                   "MultiDays   "+year+"    243   257","MultiDays   "+year+"    258   272",
                   "MultiDays   "+year+"    273   288","MultiDays   "+year+"    289   303",
                   "MultiDays   "+year+"    304   318","MultiDays   "+year+"    319   333",
                   "MultiDays   "+year+"    334   349","MultiDays   "+year+"    350   365"]

monthName = ["_hm1","_hm2","_hm3","_hm4","_hm5","_hm6","_hm7","_hm8","_hm9","_hm10","_hm11","_hm12",
             "_hm13","_hm14","_hm15","_hm16","_hm17","_hm18","_hm19","_hm20","_hm21","_hm22","_hm23","_hm24"]
monthList = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]

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

print 'solar rasters output by month complete'   
finish = datetime.now()
finish_str=str(finish)
print"program finish date, time = " + finish_str
totaltime= finish-start
print 'total processing time = ' + str(totaltime)
