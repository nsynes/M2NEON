# -*- coding: utf-8 -*-
"""
Created on Thu Aug 25 09:08:49 2016

@author: Nick
"""


import arcpy, os



def tmp(extension):
    """Generates a unique filename in the scratch folder using arcpy"""
    try:
        # Only works for ArcGIS 10.1+
        scratch = arcpy.env.scratchFolder
    except:
        # Should be a fix for ArcGIS 10.0
        scratch = os.path.join(tempfile.gettempdir(), "scratch")
        if not os.path.exists(scratch):
            os.mkdir(scratch)
    return arcpy.CreateUniqueName("tmp." + extension, scratch)



arcpy.CheckOutExtension("Spatial")

OutDir = r"C:\Dropbox (ASU)\M2NEON\Rasters\CanopyFractionAdjustedSolar"

for CanopyFracData in ["cov","dns"]:

    CanopyPercent = r"C:\Dropbox (ASU)\M2NEON\Rasters\CANOPY_FRACTION\tif\LASTOOLS_5m\SJER_TEAK_%s.tif" %CanopyFracData

    CanopyFraction = tmp("tif")
    arcpy.sa.Divide(CanopyPercent, 100).save(CanopyFraction)
    
    for SolarPeriod in ["monthly","quarterly"]:
        for Site in ["sjer","teak"]:
            
            SolarDir = r"C:\Dropbox (ASU)\M2NEON\Rasters\%s_SOLAR\tif\%s" %(SolarPeriod, Site)

            arcpy.env.workspace = SolarDir
            listSolarRasters = arcpy.ListRasters()

            for raster in listSolarRasters:
                # Do not process Diffuse radiation rasters, since there is no variation between months
                if "DiffuseRadiation" and "DSM" not in raster:
                    SolarRaster = os.path.join(SolarDir, raster)

                    TempSolarFrac = tmp("tif")
                    arcpy.sa.Times(CanopyFraction, SolarRaster).save(TempSolarFrac)
    
                    CanopyFractionSolar = os.path.join(OutDir, r"%s\%s\LAS_%s_%s" %(SolarPeriod, Site, CanopyFracData, raster))
                    arcpy.sa.Minus(SolarRaster, TempSolarFrac).save(CanopyFractionSolar)
                    print "%s DONE\n" %CanopyFractionSolar


