import arcpy, os
from datetime import datetime
start=datetime.now()
start_str=str(start)
print"program start date, time = " + start_str 
arcpy.CheckOutExtension("Spatial")

rasterRoot = r"E:\D17_GIS\PAPER_1_ANALYSIS\inRASTER"
fcRoot = r"E:\D17_GIS\PAPER_1_ANALYSIS\inVECTOR\HIDE\INTERMEDIATE"

arcpy.env.workspace = rasterRoot
rasterList = arcpy.ListRasters()
arcpy.env.workspace = fcRoot
fcList = arcpy.ListFeatureClasses()

baseRoot = os.path.split(rasterRoot)[0]
outFolder = arcpy.CreateUniqueName("outTable", baseRoot)
os.mkdir(outFolder)

for fc in fcList:
    print fc
    for r in rasterList:
        print r
        zoneField = "FID"
        outTable = outFolder +os.sep+str(fc[:-4])+"_"+str(r[:-4])+".dbf"
        inRaster = rasterRoot+os.sep+r
        outZSaT = arcpy.sa.ZonalStatisticsAsTable(fc, zoneField, inRaster, outTable,"NODATA","ALL")
        
print 'Zonal stats output complete'   
finish = datetime.now()
finish_str=str(finish)
print"program finish date, time = " + finish_str
totaltime= finish-start
print 'total processing time = ' + str(totaltime)
