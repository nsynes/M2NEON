import arcpy, os
from datetime import datetime
start=datetime.now()
start_str=str(start)
print"program start date, time = " + start_str 

rasterRoot = r"D:\PROJECTS\NEON_D17\ANALYSIS\EXPORT\RASTER\BIO_CLIM"
outRoot = r"D:\PROJECTS\NEON_D17\ANALYSIS\EXPORT\RASTER\BIO_CLIM\20m"
arcpy.env.workspace = rasterRoot
rasterList = arcpy.ListRasters()

for r in rasterList:
    print r
    #arcpy.Resample_management(r, , "20", "NEAREST")
    # Replace a layer/table view name with a path to a dataset (which can be a layer file) or create the layer/table view within the script
    # The following inputs are layers or table views: "bio1_12.tif"
    arcpy.ProjectRaster_management(r, outRoot+os.sep+r[:-4]+"_20m.tif", "PROJCS['WGS_1984_UTM_zone_11N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['false_easting',500000.0],PARAMETER['false_northing',0.0],PARAMETER['central_meridian',-117.0],PARAMETER['scale_factor',0.9996],PARAMETER['latitude_of_origin',0.0],UNIT['Meter',1.0]]",
                                   "NEAREST", "20 20", "", "", "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]")
print 'raster resample complete'   
finish = datetime.now()
finish_str=str(finish)
print"program finish date, time = " + finish_str
totaltime= finish-start
print 'total processing time = ' + str(totaltime)
