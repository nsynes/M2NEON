import arcpy
import os
import numpy

site = "TEAK"

def main():
    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")

    dirRaster = r"C:\Dropbox (ASU)\M2NEON\RASTER"

    print "CHM reclassify"
    # reclassify CHM to change areas above 1m(?) (1 to 110) to NoData
    #fnCHM = os.path.join(dirRaster, "CHM\CHM_%s_1m.tif" %site) # SJER file name
    fnCHM = os.path.join(dirRaster, "CHM\%s_CHM_reprocess_v2.tif" %site) # TEAK file name
    fnCHMBelow1m = tmp("tif")
    arcpy.gp.Reclassify_sa(fnCHM, "VALUE", "1 110 NODATA", fnCHMBelow1m)

    print "Extract solar by CHM mask"
    # extract SOLAR by new reclassified CHM mask
    #fnSolar = os.path.join(dirRaster, "Solar\SOLAR_DSM_%s_1m.tif" %site) # SJER filename
    fnSolar = os.path.join(dirRaster, "Solar\TEAK_SOLAR_v2\DIFFUSE_RADIATION_v1.tif") # TEAK filename
    fnExtractedSolar = os.path.join(dirRaster, "Solar\SOLAR_Extracted_DSM_%s_1m.tif" %site)
    arcpy.sa.ExtractByMask(fnSolar, fnCHMBelow1m).save(fnExtractedSolar)

    print "raster to numpy to count NoData points"
    # Check how many NoData values are in original Solar raster
    # to compare with the new raster below
    OrigNoDataRaster = arcpy.sa.IsNull(fnSolar)
    npOrigNoData = arcpy.RasterToNumPyArray(OrigNoDataRaster)
    OrigNoDataCount = numpy.sum(npOrigNoData)
    print "Original NoData count: %s" %OrigNoDataCount

    # use average neighbourhood values to replaces nodata values
    old = fnExtractedSolar
    for i in range(100):
        print i
        # for each loop it uses 2x2 neighbourhood to give NoData cells
        # the mean of their neighbours... probably a much faster
        # numpy method to do this?
        new = arcpy.sa.Con(arcpy.sa.IsNull(old),
                     arcpy.sa.FocalStatistics(old,
                     arcpy.sa.NbrRectangle(3,3),'MEAN'),
                     old)

        old = arcpy.CopyRaster_management(new) # copy for next iteration
        # Check how many NoData cells are left
        NoDataRaster = arcpy.sa.IsNull(new)
        npNoData = arcpy.RasterToNumPyArray(NoDataRaster)
        NoDataCount = numpy.sum(npNoData)
        print NoDataCount
        # If it is back to the number of NoData cells in the original raster,
        # then finish
        if NoDataCount <= OrigNoDataCount:
            break

    new.save(os.path.join(dirRaster, "Solar_CanopyRemoved_%s_1m.tif" %site))

    return


def tmp(extension):
    """Generates a unique filename in the scratch folder using arcpy"""
    return arcpy.CreateUniqueName("tmp." + extension, arcpy.env.scratchFolder)





# Run the app
if __name__ == "__main__":
    main()
    
