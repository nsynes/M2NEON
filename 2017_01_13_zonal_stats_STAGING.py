"""
Purpose to run the zonal stats to table tool for multiple rasters in a directory
then merge into a single table for analysis and join to original s

D17 zonal statistics calculator
@author: MHP,
modified by GAF
name: 2016_12_28_zonal stats_calculator.py
date: 2016_12_28
purpose: to compute the statistics for different grid cells for multiple rasters
Input: a folder with rasters and a zone polygon 
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

#input and output parameters
##set the workspace address
base_root = r"C:\Dropbox (ASU)\M2NEON\Paper_1\ANALYSIS\ScriptTesting"
env.workspace = base_root

ZS_Out = arcpy.CreateFolder_management (base_root, "ZS_Out")                                 #output folder
outFolder = base_root + os.sep + "ZS_Out" 

##set the zone feature class
zones= r'C:\Dropbox (ASU)\M2NEON\Paper_1\ANALYSIS\ScriptTesting\inZones\D17_1000m_inside_area.shp'
##set the zone field
zoneField="TARGET_FID"

rasterDir = r"C:\Dropbox (ASU)\M2NEON\Paper_1\ANALYSIS\ScriptTesting\inRasters"
env.workspace = rasterDir
rasters = arcpy.ListRasters()

for inValueRaster in rasters:
    ## set the output table name
    print inValueRaster + "= inValueRaster"
    rName = inValueRaster[:-4]
    print rName + " = rName"
    outputTable= outFolder + os.sep + rName
    print outputTable    
    noDataOption = "NODATA" ##set statistics type: MEAN, SUM, MAX, MIN. OR ALL
    zonalSummaryType = "ALL"
    splitNumber = 1 ##Set splite number. if you have a million feature, 10-15 would be good.
    oidfield= 'FID'
    
    rows=arcpy.SearchCursor(zones)
    countFeature=0
    for row in rows:
        countFeature+=1
    print 'Number of features in the zone dataset is: ', countFeature
    
    divisionNumber = int((countFeature/splitNumber)+1)
    
    partsCount=(int((countFeature/divisionNumber)+1))
    print 'each part has ' + str(divisionNumber) + ' features'
    tableList=[]
    
    for i in range(0,partsCount):
        arcpy.MakeFeatureLayer_management (zones, "zonelyr")
        selected=arcpy.SelectLayerByAttribute_management ("zonelyr", "NEW_SELECTION", '"FID" >=' +str(divisionNumber*i) + 'AND "FID" <' + str((i+1)*divisionNumber))
        #print 'selection is done for part ' + str (i+1)
        Output= arcpy.CreateFeatureclass_management(env.workspace, "selected"+str(i)+".shp", "POLYGON", zones)
        #print 'the layer for part ' + str(i+1)+' is created'
        arcpy.CopyFeatures_management(selected,Output)
        print 'selected features of part '+str(i+1)+' are being copied...'
        try:
            outZSaT = ZonalStatisticsAsTable("selected"+str(i)+".shp", zoneField, inValueRaster,"tblPart"+str(i), noDataOption, zonalSummaryType)
            tablePart='tblPart'+str(i)
            tableList.append(env.workspace+"\\"+tablePart)
            print 'zonal analysis for part ' +str(i+1) +' is done'
        except:
            print 'I got an error; skiping this part'
        arcpy.Delete_management("selected" +str(i) +".shp")
    arcpy.Merge_management(tableList, outputTable)
    
    print 'tables are merged'
    for i in range(0,len(tableList)):
        try:    
            arcpy.Delete_management("tblPart" +str(i))
        except:
            pass
    print "Table Created for Raster: " + os.sep + inValueRaster
    
print "joining tables"
env.workspace = outFolder
tList = arcpy.ListTables()
print str(tList) + " = tables in outfolder"
masterTableGDB = arcpy.CreateFileGDB_management (outFolder, "masterTableGDB", "CURRENT")
print str(masterTableGDB) + "= masterTableGDB"
arcpy.TableToGeodatabase_conversion (tList, masterTableGDB)
env.workspace = outFolder + os.sep + "masterTableGDB.gdb"
tList = arcpy.ListTables()
tbl = tList[0]    
masterTableGDB = str(masterTableGDB) + os.sep + "masterTableGDB"
arcpy.Copy_management(tbl,masterTableGDB)

stats = ["MIN","MEAN","MAX","RANGE","STD","SUM"]
for t in tList:
    for stat in stats:
        varName = t # formerly this was: #varName = t[:-4]
        varNameStat = varName[:3]+"_%s" %stat
        print varNameStat + " = varNameStat"
        arcpy.JoinField_management(masterTableGDB, "TARGET_FID", t , "TARGET_FID")
        arcpy.AddField_management(masterTableGDB, varNameStat, "FLOAT", "20", "4", "", varNameStat, "NULLABLE", "NON_REQUIRED", "")  # Process: Add Field
        arcpy.CalculateField_management(masterTableGDB, varNameStat, "[%s]" %stat, "VB", "")  # Process: Calculate Field
        arcpy.DeleteField_management(masterTableGDB, stat)     # Process: Delete Field
    arcpy.DeleteField_management(masterTableGDB, ["TARGET_FID_1", "COUNT_1", "AREA_1"] + ["%s_1" %stat for stat in stats])
    print  " joined table to masterTableGDB"

Output_Geodatabase = outFolder + os.sep + "masterTableGDB.gdb"
arcpy.FeatureClassToGeodatabase_conversion (zones, Output_Geodatabase)

fcList = arcpy.ListFeatureClasses()
for fc in fcList:
    arcpy.JoinField_management(fc,"TARGET_FID", masterTableGDB , "TARGET_FID")
    print "Joined Master Table to Feature Class = " + fc

print 'All zonal statistics tables written to:' + outFolder   
finish = datetime.now() 
finish_str=str(finish)
print"program finish date, time = " + finish_str
totaltime= finish-start
print 'total processing time = '
print totaltime


#Scratch Notes     
  
"""
    if fieldInfo.getFieldName(t)=="MEAN":
        # Process: Add Field
        arcpy.AddField_management(layer, "varNameMean", "FLOAT", "", "", "50", "", "NULLABLE", "NON_REQUIRED", "")
        # Process: Calculate Field                            
        arcpy.CalculateField_management(layer, "varNameMean", "!MEAN!", "PYTHON_9.3", "")
        # Process: Delete Field                     
        arcpy.DeleteField_management(layer, "MEAN")
        #print tList
"""