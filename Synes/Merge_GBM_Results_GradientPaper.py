# -*- coding: utf-8 -*-
"""
Created on Wed Sep 14 10:09:54 2016

@author: Nick
"""

import os
year = 2013
GBMDir = r"C:\Dropbox (ASU)\M2NEON\Paper_1\DATA\ForSynes"
OutCsv = os.path.join(GBMDir, "MergedGbmData.csv")
DependentVar = "chm_MEAN"
fout = open(OutCsv, "w")
fout.write("FullNameDependentVar,DependentVar,Scale,IndependentVar,RelInf,Rank,ModelRsquared")

for Scale in os.listdir(GBMDir):
    FullNameDependentVar = "%s_%s" %(Scale, DependentVar)
    SimDir = os.path.join(GBMDir, Scale, FullNameDependentVar)
    nScale = Scale.split("m")[0]
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
