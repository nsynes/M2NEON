# -*- coding: utf-8 -*-
"""
Created on Mon Jan 23 13:31:09 2017
@author: gfricker
Resample rasters
"""

import arcpy, os #math
from datetime import datetime
start=datetime.now()
start_str=str(start)
print"program start date, time = " + start_str
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")
arcpy.env.overwriteOutput=True

baseRoot = r"E:\D17_GIS\PAPER_1_ANALYSIS\ZS_STAGING\inRASTER\HIDE_SORT\resample"
env.workspace = baseRoot

newRes = 10
resampling_type = "NEAREST"
clipBoundary = r"D:\PROJECTS\NEON_D17\ANALYSIS\EXPORT\VECTOR\DOMAINS\ALL_DOMAINS_TRIM.shp"

resStr = str(newRes)
outFolderName =  "RasterOut_"+resStr
arcpy.CreateFolder_management (baseRoot, outFolderName) #str(outFolder)
outFolder = baseRoot + os.sep + outFolderName
                               
print outFolder
rasters = arcpy.ListRasters()

for raster in rasters:
    print raster
    rasterName = raster[:-4]
    outRaster = outFolder + os.sep + rasterName + "_reprojected_UTM11.tif"
    print outRaster
    arcpy.ProjectRaster_management(raster, outRaster, "PROJCS['WGS_1984_UTM_zone_11N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['false_easting',500000.0],PARAMETER['false_northing',0.0],PARAMETER['central_meridian',-117.0],PARAMETER['scale_factor',0.9996],PARAMETER['latitude_of_origin',0.0],UNIT['Meter',1.0]]", "NEAREST", "90 90", "WGS_1984_(ITRF00)_To_NAD_1983", "", "PROJCS['NAD_1983_Albers',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['false_easting',0.0],PARAMETER['false_northing',0.0],PARAMETER['central_meridian',-96.0],PARAMETER['standard_parallel_1',29.5],PARAMETER['standard_parallel_2',45.5],PARAMETER['latitude_of_origin',23.0],UNIT['Meter',1.0]]")
    print raster + " = re-projected  processed"
    outRaster2 = outFolder + os.sep + rasterName + "_reprojected_UTM11_clip.tif"
    rectangle = "-2076515.93658681 1784702.10973666 -1998099.15361783 1826715.65237365"
    arcpy.Clip_management(outRaster, rectangle, outRaster2, clipBoundary, "-3.402823e+038", "ClippingGeometry", "NO_MAINTAIN_EXTENT") #{in_template_dataset}, {nodata_value}, {clipping_geometry}, {maintain_clipping_extent})
    print raster + " = clipped processed"
    outRaster3 = outFolder + os.sep + rasterName + "_reprojected_UTM11_clip_resampled_%sm.tif" % resStr
    arcpy.Resample_management(outRaster2,outRaster3, newRes, resampling_type)
    print raster + " = resampled  processed" + "outputs written to: " + outRaster3

finish = datetime.now() 
finish_str=str(finish)
print"program finish date, time = %s" % finish_str
totaltime= finish-start
print 'total processing time = %s' % totaltime