# Quarterly Solar Radiation Splitter
#solar_splitter_v2.py

"""
This script is meant to take fishnets and raster and compute individual values for each fishnet cell and each raster
Then output to a table identified by 'FID' which is the feature identifier for each fishnet
"""

import arcpy, os
import pandas as pd
from arcpy import env
from arcpy.sa import *

from datetime import datetime
start=datetime.now()
start_str=str(start)
print"program start date, time = " + start_str 
from arcpy import env
arcpy.env.overwriteOutput=True
arcpy.CheckOutExtension("Spatial")

baseRoot = r"C:\Dropbox (ASU)\M2NEON\Paper_1\DATA"    #working directory 
rasterRoot = r"C:\Dropbox (ASU)\M2NEON\Paper_1\DATA\RASTER" #raster directory
fcRoot = r"C:\Dropbox (ASU)\M2NEON\Paper_1\DATA\VECTOR"

#unique_name = arcpy.CreateUniqueName("outTable")
outFolder = arcpy.CreateFolder_management(baseRoot, "outTable")

arcpy.env.workspace = rasterRoot
rasterList = arcpy.ListRasters()
arcpy.env.workspace = fcRoot
fcList = arcpy.ListFeatureClasses()
arcpy.env.workspace = baseRoot      #set working directory back to baseRoot
print "The root directory for this project is set to: " +str(baseRoot) 


listCSVs = []
for fc in fcList:
    #arcpy.env.workspace = rasterRoot
    for r in rasterList:
        inZoneData = fcRoot+os.sep+fc
        zoneField = "FID"
        tableName = r[:-4]
        outDBF = str(outFolder) + os.sep + tableName + ".dbf"
        outZSaT = ZonalStatisticsAsTable(inZoneData, zoneField, rasterRoot+os.sep+r, outDBF, "NODATA", "ALL")
        print "done: poly = %s, raster = %s..." % (fc,r)
        outCSV = "%s.csv" %os.path.split(outDBF)[1][:-4]
        arcpy.TableToTable_conversion(outDBF, str(outFolder), outCSV)
        listCSVs.append(os.path.join(str(outFolder), outCSV))

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



print 'Zonal stats output complete'   
finish = datetime.now()
finish_str=str(finish)
print"program finish date, time = " + finish_str
totaltime= finish-start
print 'total processing time = ' + str(totaltime)
