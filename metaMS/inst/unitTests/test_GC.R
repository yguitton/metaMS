library(metaMS)
data(FEMsettings)
data(threeStdsNIST)
data(threeStdsInfo)
data(threeStdsDB)
data(GCresults)

test_GCDBcreation <- function() {
  ## Script to test the main metaMS functions for GC
  if (require("metaMSdata", quietly = TRUE)){
    ## <----------------   createSTDdbGC -------------------------- >
    
    ## create the DB
    stdInfo[,"stdFile"] <-
        rep(list.files(system.file("CDF_GC", package = "metaMSdata"),
                       full.names = TRUE)[3], 3)
    
    DB.live <- createSTDdbGC(stdInfo, TSQXLS.GC, extDB = smallDB, nSlaves = 2)
    ## load the pre-compiled version
    
    ## Run some tests on the output
    checkEquals(length(DB), length(DB.live))
    checkEquals(sapply(DB, length), sapply(DB.live, length))
  } else {
    message("Not testing building the data base of standards:\n\tpackage metaMSdata is necessary for that...")
  }
}


## <----------------   runGC -------------------------- >
test_GC <- function() {
  ## Peak picking in xcms returns different numbers of features in
  ## different versions. We are not interested in
  ## testing this behaviour, but want to test our own code. So we
  ## present the xcmsSet object rather than the file names.
  result.live <- runGC(xset = GCset, settings = TSQXLS.GC,
                       DB = DB, nSlaves = 2)
  
  checkEquals(nrow(result.annot$PeakTable), nrow(result.live$PeakTable))
  checkEquals(ncol(result.annot$PeakTable), ncol(result.live$PeakTable))
  checkEquals(result.annot$PeakTable[,"Name"], result.live$PeakTable[,"Name"])
}
          
