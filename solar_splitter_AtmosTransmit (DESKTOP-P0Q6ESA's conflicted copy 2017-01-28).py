# Monthly Solar Radiation Splitter
#solar_splitter_half_months.py

"""
Author:Fricker and Synes

This script is meant to use lidar derived canopy surface models and
output solar radiation rasters for half-monthly periods.
This script incorporates atmospheric transmittance into the solar radiation (from Frank's calculations) on a daily basis.

input: Lidar derived digital surface model (DSM) or DEM
output: 24 rasters for each half/month representing the solar areal
radiation for a month period to coincide with the HOBO temp sensors
"""

import arcpy, pandas, os

arcpy.CheckOutExtension("spatial")

RasterModel = "DEM"
Site = "TEAK"
Year = "2013"

dirOut = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\Solar%s_%s" %(Site, RasterModel)
if not os.path.exists(dirOut):
    os.mkdir(dirOut)
dirRaster = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\RASTER"
pthSensorPoints = r"D:\Dropbox (ASU)\M2NEON\DGPS\CleanedLinked\%s_Point_Sensors.shp" %Site
df = pandas.read_csv(r"D:\Dropbox (ASU)\M2NEON\Paper_2\ANALYSIS\AtmosphericTransmittance\%s_%s.csv" %(Site, Year), index_col="yday")

# DSM version
pthRaster = os.path.join(dirRaster, "%s\%s_%s_2m.tif" %(RasterModel, Site, RasterModel))


for day in range(1,366):
    
    pthOutShp = os.path.join(dirOut, "Solar_%s_%s_%sDay%s.shp" %(Site, RasterModel, Year, day))
    height = ""
    latitude = ""
    skySize = 1000
    if day == 365:
        nextday = 1
    else:
        nextday = day + 1
    timeConfig = arcpy.sa.TimeMultipleDays(Year, day, nextday)
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
    
    
    arcpy.sa.PointsSolarRadiation(pthRaster, pthSensorPoints, pthOutShp, height,
                         latitude, skySize, 
                         timeConfig, dayInterval, hourInterval, eachInterval, 
                         zFactor, SlopeAspect, calcDirections, zenithDivisions, 
                         azimuthDivisions, diffuseType, diffuseProp, 
                         transmittivity)


print "Merging shapefiles"
for day in range(3,347):
    print day
    pthAllDaysTable = os.path.join(dirOut, "Solar_%s_%s_%sAllDays.shp" %(Site, RasterModel, Year))
    pthSingleTable = os.path.join(dirOut, "Solar_%s_%s_%sDay%s.shp" %(Site, RasterModel, Year, day))
    arcpy.JoinField_management(pthAllDaysTable,
                               "FID",
                               pthSingleTable,
                               "FID")
    arcpy.AddField_management(pthAllDaysTable, "Day%s" %day, "DOUBLE")
    arcpy.CalculateField_management(pthAllDaysTable, "Day%s" %day, "[T0]", "VB", "")
    arcpy.DeleteField_management(pthAllDaysTable, "T0")

    
dicHM = {1: [1,15],
           2:[16, 31],
           3:[32, 45],
           4:[46, 59],
           5:[60, 75],
           6:[76, 90],
           7:[91, 105],
           8:[106, 120],
           9:[121, 136],
           10:[137, 151],
           11:[152, 166],
           12:[167, 181],
           13:[182, 197],
           14:[198, 212],
           15:[213, 227],
           16:[228, 242],
           17:[243, 257],
           18:[258, 272],
           19:[273, 288],
           20:[289, 303],
           21:[304, 318],
           22:[319, 333],
           23:[334, 349],
           24:[350, 365]}

pthAllDaysTable = os.path.join(dirOut, "Solar_%s_%s_%sAllDays.shp" %(Site, RasterModel, Year))

print "Summing for half months"
for hm in dicHM.keys():
    print "hm: %s" %hm
    NewFieldName = "HM%s" %hm
    arcpy.AddField_management(pthAllDaysTable, NewFieldName, "DOUBLE")
    fieldlist = ["Day%s" %x for x in range(dicHM[hm][0], dicHM[hm][1]+1)]
    SumExpression = ",".join("!{0}!".format(x) for x in fieldlist)
    arcpy.CalculateField_management(pthAllDaysTable, NewFieldName, 
                                "sum([{0}])".format(SumExpression), "PYTHON", "")
    
