
"""
This script is meant to take fishnets and raster and compute individual values for each fishnet cell and each raster
Then output to a table identified by 'FID' which is the feature identifier for each fishnet
"""

import arcpy, os
import numpy as np

from datetime import datetime
start=datetime.now()
start_str=str(start)
print"program start date, time = " + start_str 
arcpy.CheckOutExtension("Spatial")

rasterRoot = r"E:\D17_GIS\PAPER_1_ANALYSIS\inRASTER" 
fcRoot = r"E:\D17_GIS\PAPER_1_ANALYSIS\inVECTOR"

baseRoot = os.path.split(rasterRoot)[0]
outFolder = arcpy.CreateUniqueName("outTable", baseRoot)
os.mkdir(outFolder)

listCSVs = []
for fc in os.listdir(fcRoot):
    if fc[-4:] == ".shp":
        suboutFolder = os.path.join(outFolder, fc[:-4])
        os.mkdir(suboutFolder)
        print suboutFolder
        for r in os.listdir(rasterRoot):
            if r[-4:] == ".tif":
                for stat in ["MEAN","MAXIMUM","MINIMUM","RANGE","STD","SUM"]:
                    inZoneData = os.path.join(fcRoot, fc)
                    inValueData = os.path.join(rasterRoot, r)
                    zoneField = "FID"
                    outZSaT = arcpy.sa.ZonalStatisticsAsTable(inZoneData, zoneField, inValueData, stat, "DATA")
                    outZSaT.save(os.path.join(suboutFolder, "%s_%s.dbf" %(r[:-4], stat)))
                    print outZSaT 
"""
# Generate data frames for each stats csv, then merge and export as csv
listDf = []
for csv in listCSVs:
    df = pd.DataFrame.from_csv(csv)
    df = df.add_prefix("%s_" %os.path.split(csv)[1][:-4])
    df["OID"] = df.index
    listDf.append(df)
df_final = pd.DataFrame()
df_final = reduce(lambda left,right: pd.merge(left,right,on='OID'), listDf) # Merge each dataframe on OID
df_final=df_final[["OID"] + [x for x in df_final.columns.tolist() if x.upper() != "OID"]] # Make OID the first column in the DF
outTableName = "%s_MergedTable.csv" %fc[:-4]
df_final.to_csv(os.path.join(str(outFolder), outTableName), index=False)
"""


print 'Zonal stats output complete'   
finish = datetime.now()
finish_str=str(finish)
print"program finish date, time = " + finish_str
totaltime= finish-start
print 'total processing time = ' + str(totaltime)


#########################################
# THE BELOW DOES NOT WORK FOR LARGE RASTERS!
#########################################
"""
import pandas as pd
import arcpy, os
from arcpy import env
from arcpy.sa import *
import numpy as np

#raster directory should only contain those rasters that you want stats on
rasterRoot = r"D:\Dropbox (ASU)\M2NEON\Paper_1\DATA\RASTER" 
fcRoot = r"D:\Dropbox (ASU)\M2NEON\Paper_1\DATA\VECTOR"
# ZoneRaster replaces the zone shape in the previous script.
# This defines the zones that you want stats on and needs to be of the same extent and resolution as the value raster
ZoneRaster = r"D:\Dropbox (ASU)\M2NEON\Paper_1\DATA\RASTER\Mask\D17_1000m_inside_FullExtent.tif"

baseRoot = os.path.split(rasterRoot)[0]
outFolder = arcpy.CreateUniqueName("outTable", baseRoot)
os.mkdir(outFolder)

for f in os.listdir(rasterRoot):
    #if f[-4:] == ".tif":
    print f
    if f == "DEM_ALL_AREAS_v2.tif":
        ValueRaster = os.path.join(rasterRoot, f)
        zones = arcpy.RasterToNumPyArray(ZoneRaster)
        value = np.ma.masked_equal(arcpy.RasterToNumPyArray(ValueRaster),
                           arcpy.Raster(ValueRaster).noDataValue)
        print f
        print("Zone\tCount\tSum\tNoData\tMean\tMax\tMin\tSTD")
        for z in np.unique(zones):
            sel = (zones == z)
            print z, sel.sum(), value[sel].sum(), value.mask[sel].sum(), value[sel].mean(), value[sel].max(), value[sel].min(), value[sel].std()
        del ValueRaster
        del zones
        del value
"""
