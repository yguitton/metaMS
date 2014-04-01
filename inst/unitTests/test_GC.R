test_GC <- function() {
  data(FEMsettings)   ## settings
  data(threeStdsDB)   ## annotation database
  data(GCresults)     ## pre-computed results
  
  ## Peak picking in xcms returns different numbers of features in
  ## different versions. We are not interested in
  ## testing this behaviour, but want to test our own code. So we
  ## present the xcmsSet object rather than the file names.
  result.live <- runGC(xset = GCresults$xset, settings = TSQXLS.GC,
                       DB = DB, nSlaves = 2)
  
  checkEquals(nrow(GCresults$PeakTable), nrow(result.live$PeakTable))
  checkEquals(ncol(GCresults$PeakTable), ncol(result.live$PeakTable))
  checkEquals(GCresults$PeakTable[,"Name"], result.live$PeakTable[,"Name"])
}
          
