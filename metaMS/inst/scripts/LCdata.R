## scripts to generate the intermediate GC data from the CDF files of
## metaMSdata. Can be used to re-generate results in case of changes
## to either package.

library(metaMS)
library(metaMSdata)

## ########################################################################
## Creation of the database for annotation
data(FEMsettings)    ## provides a.o. Synapt.RP, the LC settings file
data(exptable)
cdfpath <- system.file("CDF_LC", package = "metaMSdata")

## files 
files <- list.files(cdfpath, "_RP_", full.names=TRUE)
exptable$stdFile <-
    sapply(exptable$stdFile,
           function(x)
           files[grep(x,files)])
  
## Go!
metaSetting(Synapt.RP, "DBconstruction")$minfeat  <- 2
LCDBtest <- createSTDdbLC(stdInfo=exptable, 
                          settings = Synapt.RP,
                          polarity = "positive",
                          Ithr = 20, nSlaves = 2)
## saved in "LCDBtest.RData" in the data directory of the metaMS
## package


## ########################################################################
## Creation of LCresults object
data(FEMsettings)
data(LCDBtest)

cdfpath <- system.file("extdata", package = "metaMSdata")
files <- list.files(cdfpath, "_RP_", full.names = TRUE)

LCresults <- runLC(files, settings = Synapt.RP, 
                   DB = LCDBtest$DB, returnXset = TRUE, nSlaves = 2)
## saved in "LCresults.RData") in the data directory of the metaMS
## package
