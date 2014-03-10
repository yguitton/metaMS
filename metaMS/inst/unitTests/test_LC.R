test_LC <- function() {
  data(FEMsettings)  ## The settings
  data(LCDBtest)     ## The database
  data(LCresults)    ## The reference output
  
  ## Peak picking in xcms returns different numbers of features in
  ## different versions. We are not interested in
  ## testing this behaviour, but want to test our own code. So we
  ## present the xcmsSet object rather than the file names.
  LC <- runLC(xset = LCresults$xset, settings = Synapt.RP,
              DB = LCDBtest$DB, nSlaves = 2)

  checkEquals(nrow(LC$PeakTable), nrow(LCresults$PeakTable))
  checkEquals(ncol(LC$PeakTable), ncol(LCresults$PeakTable))
  checkEquals(LC$Annotation$ann.features, LCresults$Annotation$ann.features)
}
  
