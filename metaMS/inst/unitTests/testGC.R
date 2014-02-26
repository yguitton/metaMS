library(metaMS)
data(FEMsettings)
data(threeStdsNIST)
data(threeStdsInfo)
data(threeStdsDB)
data(GCresults)

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
  test_that("DB creation: equal number of DB elements",{
    expect_equal(length(DB), length(DB.live))
  })
  test_that("DB creation: equal number of peaks in all DB elements",{
    expect_equal(sapply(DB, length), sapply(DB.live, length))
  })
} else {
  message("Not testing building the data base of standards:\n\tpackage metaMSdata is necessary for that...")
}

## <----------------   runGC -------------------------- >

## Peak picking in xcms returns different numbers of features in
## different versions. We are not interested in
## testing this behaviour, but want to test our own code. So we
## present the xcmsSet object rather than the file names.
result.live <- runGC(xset = GCset, settings = TSQXLS.GC,
                     DB = DB, nSlaves = 2)


## Run some tests on the output
test_that("runGC: dimensions of peak table consistent with precompiled results.", {
  expect_equal(nrow(result.annot$PeakTable), nrow(result.live$PeakTable))
  expect_equal(ncol(result.annot$PeakTable), ncol(result.live$PeakTable))
})

## Annotation gives the same results...
test_that("Annotation consistent with precompiled results.", {
  expect_equal(result.annot$PeakTable[,"Name"],
               result.live$PeakTable[,"Name"])
})
