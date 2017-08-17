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

import arcpy, pandas, os, numpy

arcpy.CheckOutExtension("spatial")

RasterModel = "DEM"
Year = "2013"


for scale in ["Microsite","Site"]:
    for Site in ["SJER","TEAK"]:

        if scale == "Microsite":
            dirOut = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\VECTOR\MicrositeLevel_PointBasedSolar\Solar%s_%s" %(Site, RasterModel)
        elif scale == "Site":
            dirOut = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\VECTOR\SiteLevel_PointBasedSolar\Solar%s_%s" %(Site, RasterModel)
        if not os.path.exists(dirOut):
            os.mkdir(dirOut)
        dirRaster = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\RASTER"
        if scale == "Microsite":
            pthSensorPoints = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\VECTOR\SensorPoints\%s_Point_Sensors.shp" %Site
        elif scale == "Site":
            pthSensorPoints = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\VECTOR\SensorPoints\%s_SiteLevel.shp" %Site
            
        df = pandas.read_csv(r"D:\Dropbox (ASU)\M2NEON\Paper_2\ANALYSIS\AtmosphericTransmittance\%s_%s.csv" %(Site, Year), index_col="yday")

        if scale == "Microsite":
            pthRaster = os.path.join(dirRaster, "%s\%s_%s_2m.tif" %(RasterModel, Site, RasterModel))
        elif scale == "Site":
            pthRaster = os.path.join(dirRaster, "%s\%s_%s_30m.tif" %(RasterModel, Site, RasterModel))

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

            if not numpy.isnan(transmittivity):
                print "Day: %s, Atmospheric Transmittance: %s" %(day, transmittivity)

                arcpy.sa.PointsSolarRadiation(pthRaster, pthSensorPoints, pthOutShp, height,
                                     latitude, skySize, 
                                     timeConfig, dayInterval, hourInterval, eachInterval, 
                                     zFactor, SlopeAspect, calcDirections, zenithDivisions, 
                                     azimuthDivisions, diffuseType, diffuseProp, 
                                     transmittivity)

"""
Site = "SJER"
dirOut = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\VECTOR\MicrositeLevel_PointBasedSolar\Solar%s_%s" %(Site, RasterModel)
pthLocation = r"D:\Dropbox (ASU)\M2NEON\Paper_2\DATA\VECTOR\SensorPoints\%s_Point_Sensors.shp" %(Site)


print "Merging shapefiles"
for day in range(1,366):
    print day
    pthSingleTable = os.path.join(dirOut, "Solar_%s_%s_%sDay%s.shp" %(Site, RasterModel, Year, day))
    arcpy.JoinField_management(pthSingleTable,
                               "FID",
                               pthLocation,
                               "FID")
"""
