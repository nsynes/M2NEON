# -*- coding: utf-8 -*-
"""
Created on Wed Sep 14 10:09:54 2016

@author: Nick
"""

import os
year = 2013
BaseDir = r"C:\Dropbox (ASU)\M2NEON\SensorData\GBM_Results\6_GBM_2013_AtmosTransDsmSolar\ModelDirs"
OutCsv = r"C:\Dropbox (ASU)\M2NEON\SensorData\GBM_Results\6_GBM_2013_AtmosTransDsmSolar\MergedGbmData.csv"
fout = open(OutCsv, "w")
fout.write("FullNameDependentVar,DependentVar,IntervalPeriod,Period,Site,IndependentVar,IndependentVarPeriod,RelInf,Rank,ModelRsquared")

for SimDir in os.listdir(BaseDir):
    if os.path.isdir(os.path.join(BaseDir,SimDir)):
        
        # Relative influence data
        f = open(os.path.join(BaseDir, r"%s\RelativeInfluence.csv" %SimDir), "r")
        RelInf_lines = f.readlines()
        f.close()
        
        # Get the ID for the best model
        f = open(os.path.join(BaseDir, r"%s\BestModel.csv" %SimDir), "r")
        BestMod_lines = f.readlines()
        f.close()
        BestMod = BestMod_lines[1].split(",")[0]
        
        # Find the best model (by ID from above)
        # and get its R-squared value
        f = open(os.path.join(BaseDir, r"%s\TuningResults.csv" %SimDir), "r")
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
        
        FullNameDependentVar = SimDir.split("_y=")[-1].split("_")[0]
        print FullNameDependentVar
        DependentVar = FullNameDependentVar.split(".")[2]
        if "Month" in FullNameDependentVar.split(".")[1]:
            IntervalPeriod = "Month"
            Period = FullNameDependentVar.split(".")[1].split("Month")[1]
        elif "Quarter" in FullNameDependentVar.split(".")[1]:
            IntervalPeriod = "Quarter"
            Period = FullNameDependentVar.split(".")[1].split("Quarter")[1]
        elif "HM" in FullNameDependentVar.split(".")[1]:
            IntervalPeriod = "BiMonthly"
            Period = FullNameDependentVar.split(".")[1].split("HM")[1]
        Site = SimDir.split("=")[1].split("_y")[0]
        New = []
        n = 1
        for line in RelInf_lines[1:]:
            Var = ".".join(line.split(",")[0].split(".")[1:]).split("\"")[0]
            if "Month" in Var or "Quarter" in Var or "HM" in Var:
                IndependentVar = ".".join(Var.split(".")[1:])
                IndependentVarPeriod = Var.split(".")[0]
            else:
                IndependentVar = Var
                IndependentVarPeriod = "NA"
            RelInf = line.split(",")[2].split()[0]
            Rank = n
            fout.write("\n%s,%s,%s,%s,%s,%s,%s,%s,%s,%s" %(FullNameDependentVar, DependentVar, IntervalPeriod, Period, Site, IndependentVar, IndependentVarPeriod, RelInf, Rank, Rsquared))
            n += 1
        
        
        
fout.close()