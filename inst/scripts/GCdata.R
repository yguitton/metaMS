## scripts to generate the intermediate GC data from the CDF files of
## metaMSdata. Can be used to re-generate results in case of changes
## to either package.

library(metaMS)
library(metaMSdata)

## ########################################################################
## Creation of the database for annotation
data(threeStdsInfo)
all.files <- list.files(system.file("extdata", package = "metaMSdata"),
                        pattern = "_GC_", full.names = TRUE)
stdInfo[,"stdFile"] <- rep(all.files[3], 3)
data(FEMsettings)    ## provides a.o. TSQXLS.GC, the GC settings file
data(threeStdsNIST)  ## provides object smallDB, excerpt from NIST DB

## Go!
DB <- createSTDdbGC(stdInfo, TSQXLS.GC, extDB = smallDB, nSlaves = 2)
## saved in "threeStdsDB.RData" in the data directory of the metaMS
## package


## ########################################################################
## Creation of GCresults object
data(threeStdsDB) 
data(FEMsettings)

cdfdir <- system.file("extdata", package = "metaMSdata")
cdffiles <- list.files(cdfdir, pattern = "_GC_",
                       full.names = TRUE, ignore.case = TRUE)
GCresults <- runGC(files = cdffiles, settings = TSQXLS.GC, DB = DB,
                   returnXset = TRUE, nSlaves = 2)

