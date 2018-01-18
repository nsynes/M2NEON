# -*- coding: utf-8 -*-
"""
Created on Wed Sep 14 10:09:54 2016

@author: Nick
"""
import os


############################
# PARAMETERS
###########################
# location of the simulation results folders (25,50,100,250,500,1000)
GBMDir = r"C:\Dropbox\Work\ASU\Paper_1\p3"
# name of the dependent variable (You need to run this script for each dependent variable)
DependentVar = "chm_MAXIMUM"
###########################
###########################






##########################################
OutCsv = os.path.join(GBMDir, "MergedGbmData_%s.csv" %DependentVar)
fout = open(OutCsv, "w")
fout.write("FullNameDependentVar,DependentVar,Scale,IndependentVar,RelInf,Rank,ModelRsquared")

for Scale in os.listdir(GBMDir):
    FullNameDependentVar = "%sm_%s" %(Scale, DependentVar)
    SimDir = os.path.join(GBMDir, Scale, FullNameDependentVar)
    nScale = Scale.split("m")[0]
    print FullNameDependentVar
    print SimDir
    print nScale
    if os.path.isdir(SimDir):
        
        # Relative influence data
        f = open(os.path.join(SimDir, "RelativeInfluence.csv"), "r")
        RelInf_lines = f.readlines()
        f.close()
        
        # Get the ID for the best model
        f = open(os.path.join(SimDir, "BestModel.csv"), "r")
        BestMod_lines = f.readlines()
        f.close()
        BestMod = BestMod_lines[1].split(",")[0]
        
        # Find the best model (by ID from above)
        # and get its R-squared value
        f = open(os.path.join(SimDir, "TuningResults.csv"), "r")
        TuneRes_lines = f.readlines()
        f.close()
        headers = TuneRes_lines[0].split(",")
        Rsquared = "NA"
        RsquaredLoc = None
        for i in range(len(headers)):
            if headers[i] == "\"Rsquared\"":
                RsquaredLoc = i
        for line in TuneRes_lines[1:]:
            if line.split(",")[0] == BestMod:
                Rsquared = line.split(",")[RsquaredLoc].strip()
        
        New = []
        n = 1
        for line in RelInf_lines[1:]:
            IndependentVar = line.split(",")[0]
            RelInf = line.split(",")[-1].split()[0]
            Rank = n
            fout.write("\n%s,%s,%s,%s,%s,%s,%s" %(FullNameDependentVar, DependentVar, nScale, IndependentVar, RelInf, Rank, Rsquared))
            n += 1
        
        
        
fout.close()
