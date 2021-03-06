"""
Mosaic Water Indices
"""
import arcpy, sys, os
from datetime import datetime
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")

start=datetime.now()
start_str=str(start)
print"program start date, time = " + start_str 

inFolder = r"D:\PROJECTS\NEON_D17\ANALYSIS\IMPORT\VEG_WATER_INDICES\PROV_Spectrometer\PROV_Spectrometer\Water_Indices"
outFolder = arcpy.CreateUniqueName("outBands", inFolder)
os.mkdir(outFolder)
arcpy.env.workspace = inFolder

for r in arcpy.ListFiles("*.dat"):
    in_raster = r  
    desc = arcpy.Describe(r)  
    for band in desc.children:  
        bandName = band.name  
        band_path = os.path.join(r, bandName)
        dest_path = os.path.join(outFolder, r[:-4]+"_"+ bandName + '.tif')  
        arcpy.CopyRaster_management(band_path, dest_path, "", "", "", "NONE", "NONE", "")
        
arcpy.env.workspace = outFolder
mosaicPath = outFolder + os.sep + "mosaicOut"
os.mkdir(mosaicPath)

for band in desc.children:
    indexPath = outFolder + os.sep + band.name
    os.mkdir(indexPath)
    
#Moisture Stress Index (no data values == 1)
arcpy.env.workspace = outFolder
MSIFolder = outFolder + os.sep + "Moisture Stress Index"
listMSI = arcpy.ListRasters("*Moisture Stress Index.tif")
for rMSI in listMSI:
    whereClause = "VALUE = 1"
    outSetNull = SetNull(rMSI, rMSI, whereClause)
    outsetNull_full = MSIFolder + os.sep + rMSI[:-4] + "_noZero.tif"
    outSetNull.save(outsetNull_full)

arcpy.env.workspace = MSIFolder
listMSI_noZero = arcpy.ListRasters("*Moisture Stress Index_noZero.tif")
outMosaic = "MSI_MOSAIC_noZero.tif"
prj = "PROJCS['WGS_1984_UTM_zone_11N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],\
PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['false_easting',500000.0],\
PARAMETER['false_northing',0.0],PARAMETER['central_meridian',-117.0],PARAMETER['scale_factor',0.9996],PARAMETER['latitude_of_origin',\
0.0],UNIT['Meter',1.0]]"
arcpy.MosaicToNewRaster_management(listMSI_noZero, mosaicPath, outMosaic, prj, "32_BIT_FLOAT", "1", "1", "MINIMUM","MATCH")
print "Mosaic for MSI done"

#Normalized Difference Infrared Index (no data values == 0)  
arcpy.env.workspace = outFolder
NDIIFolder = outFolder + os.sep + "Normalized Difference Infrared Index"
listNDII = arcpy.ListRasters("*Normalized Difference Infrared Index.tif")
for rNDII in listNDII:
    print rNDII
    whereClause = "VALUE = 0"
    outSetNull = SetNull(rNDII, rNDII, whereClause)
    outsetNull_full = NDIIFolder + os.sep + rNDII[:-4] + "_noZero.tif"
    outSetNull.save(outsetNull_full)

arcpy.env.workspace = NDIIFolder
listNDII_noZero = arcpy.ListRasters("*Normalized Difference Infrared Index_noZero.tif")
outMosaic = "NDII_MOSAIC_noZero.tif"
prj = "PROJCS['WGS_1984_UTM_zone_11N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],\
PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['false_easting',500000.0],\
PARAMETER['false_northing',0.0],PARAMETER['central_meridian',-117.0],PARAMETER['scale_factor',0.9996],PARAMETER['latitude_of_origin',\
0.0],UNIT['Meter',1.0]]"
arcpy.MosaicToNewRaster_management(listNDII_noZero, mosaicPath, outMosaic, prj, "32_BIT_FLOAT", "1", "1", "MINIMUM","MATCH")
print "Mosaic for NDII done"
    
#Normalized Difference Water Index (no data values == 1)  
arcpy.env.workspace = outFolder
NDWIFolder = outFolder + os.sep + "Normalized Difference Water Index"
listNDWI = arcpy.ListRasters("*Normalized Difference Water Index.tif")
for rNDWI in listNDWI:
    print rNDWI
    whereClause = "VALUE = 1"
    outSetNull = SetNull(rNDWI, rNDWI, whereClause)
    outsetNull_full = NDWIFolder + os.sep + rNDWI[:-4] + "_noZero.tif"
    outSetNull.save(outsetNull_full)

arcpy.env.workspace = NDWIFolder
listNDWI_noZero = arcpy.ListRasters("*Normalized Difference Water Index_noZero.tif")
outMosaic = "NDWI_MOSAIC_noZero.tif"
prj = "PROJCS['WGS_1984_UTM_zone_11N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],\
PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['false_easting',500000.0],\
PARAMETER['false_northing',0.0],PARAMETER['central_meridian',-117.0],PARAMETER['scale_factor',0.9996],PARAMETER['latitude_of_origin',\
0.0],UNIT['Meter',1.0]]"
arcpy.MosaicToNewRaster_management(listNDWI_noZero, mosaicPath, outMosaic, prj, "32_BIT_FLOAT", "1", "1", "MINIMUM","MATCH")
print "Mosaic for NDWI done"

#Normalized Multiband Drought Index (no data values == 1)  
arcpy.env.workspace = outFolder
NMDIFolder = outFolder + os.sep + "Normalized Multiband Drought Index"

listNMDI = arcpy.ListRasters("*Normalized Multiband Drought Index.tif")
for rNMDI in listNMDI:
    print rNMDI
    whereClause = "VALUE = 1"
    outSetNull = SetNull(rNMDI, rNMDI, whereClause)
    outsetNull_full = NMDIFolder + os.sep + rNMDI[:-4] + "_noZero.tif"
    outSetNull.save(outsetNull_full)

arcpy.env.workspace = NMDIFolder
listNMDI_noZero = arcpy.ListRasters("*Normalized Multiband Drought Index_noZero.tif")
outMosaic = "NMDI_MOSAIC_noZero.tif"
prj = "PROJCS['WGS_1984_UTM_zone_11N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],\
PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['false_easting',500000.0],\
PARAMETER['false_northing',0.0],PARAMETER['central_meridian',-117.0],PARAMETER['scale_factor',0.9996],PARAMETER['latitude_of_origin',\
0.0],UNIT['Meter',1.0]]"
arcpy.MosaicToNewRaster_management(listNMDI_noZero, mosaicPath, outMosaic, prj, "32_BIT_FLOAT", "1", "1", "MINIMUM","MATCH")
print "Mosaic for NMDI done"

#Water Band Index (no data values == 1)  
arcpy.env.workspace = outFolder
WBIFolder = outFolder + os.sep + "Water Band Index"

listWBI = arcpy.ListRasters("*Water Band Index.tif")
for rWBI in listWBI:
    print rWBI
    whereClause = "VALUE = 1"
    outSetNull = SetNull(rWBI, rWBI, whereClause)
    outsetNull_full = WBIFolder + os.sep + rWBI[:-4] + "_noZero.tif"
    outSetNull.save(outsetNull_full)

arcpy.env.workspace = WBIFolder
listWBI_noZero = arcpy.ListRasters("*Water Band Index_noZero.tif")
outMosaic = "WBI_MOSAIC_noZero.tif"
prj = "PROJCS['WGS_1984_UTM_zone_11N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],\
PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['false_easting',500000.0],\
PARAMETER['false_northing',0.0],PARAMETER['central_meridian',-117.0],PARAMETER['scale_factor',0.9996],PARAMETER['latitude_of_origin',\
0.0],UNIT['Meter',1.0]]"
arcpy.MosaicToNewRaster_management(listWBI_noZero, mosaicPath, outMosaic, prj, "32_BIT_FLOAT", "1", "1", "MINIMUM","MATCH")
print "Mosaic for WBI done"

print 'raster bands set null and mosaic complete!!'

finish = datetime.now()
finish_str=str(finish)
print"program finish date, time = " + finish_str
totaltime= finish-start
print 'total processing time = ' + str(totaltime)
