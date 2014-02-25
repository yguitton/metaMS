library(metaMS)
data(FEMsettings)  ## The settings
data(LCDBtest)     ## The database
data(LCresults)    ## The reference output

## Script to test the main metaMS functions for LC
if (require("metaMSdata", quietly = TRUE)) {
  ## <----------------   createSTDdbLC -------------------------- >

  data(exptable)       ## the manually curated reference table
  exptable$stdFile <- sapply(exptable$stdFile,
                             function(x)
                             files[grep(x,files)])
  metaSetting(Synapt.RP, "DBconstruction.minfeat")  <- 2
  
  LCDB <- createSTDdbLC(stdInfo=exptable, 
                        settings = Synapt.RP,
                        polarity = "positive",
                        Ithr = 20, nSlaves = 2)
  
  ## Run some tests on the output
  test_that("DB creation works fine",{
    expect_equal(nrow(LCDB$DB), nrow(LCDBtest$DB))
  })  
} else{
  message("Not testing building the data base of standards:\n\tpackage metaMSdata is necessary for that...")
}

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
  
