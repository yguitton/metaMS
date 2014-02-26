library(metaMS)
data(FEMsettings)  ## The settings
data(LCDBtest)     ## The database
data(LCresults)    ## The reference output


## <----------------   runLC -------------------------- >

## Peak picking in xcms returns different numbers of features in
## different versions. We are not interested in
## testing this behaviour, but want to test our own code. So we
## present the xcmsSet object rather than the file names.
LC <- runLC(xset = LCxset, settings = Synapt.RP, DB = LCDBtest$DB, nSlaves = 2)


## Run some tests on the output
test_that("xcms and CAMERA are gicing consistent results", {
  expect_equal(nrow(LC$PeakTable), nrow(LCresults$PeakTable))
  expect_equal(ncol(LC$PeakTable), ncol(LCresults$PeakTable))
})

## Annotation gives the same results...
test_that("Peak table annotation gives consistent results", {
  expect_equal(LC$Annotation$ann.features,LCresults$Annotation$ann.features)
})
  
