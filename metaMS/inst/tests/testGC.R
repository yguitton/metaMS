## Script to test the main metaMS functions for GC
if (require("metaMSdata", quietly = TRUE)){
  library(metaMS)
  data(FEMsettings)
  
  ## <----------------   createSTDdbGC -------------------------- >
  
  ## create the DB
  data(threeStdsNIST)
  data(threeStdsInfo)
  stdInfo[,"stdFile"] <-
      rep(list.files(system.file("CDF_GC", package = "metaMSdata"),
                     full.names = TRUE)[3], 3)
  
  DB.live <- createSTDdbGC(stdInfo, TSQXLS.GC, extDB = smallDB, nSlaves = 2)
  ## load the pre-compiled version
  data(threeStdsDB)
  
  ## Run some tests on the output
  test_that("DB creation: equal number of DB elements",{
    expect_equal(length(DB), length(DB.live))
  })
  test_that("DB creation: equal number of peaks in all DB elements",{
    expect_equal(sapply(DB, length), sapply(DB.live, length))
  })
  
  ## <----------------   runGC -------------------------- >

  ## do peak picking and annotation
  cdfdir <- system.file("CDF_GC", package = "metaMSdata")
  cdffiles <- list.files(cdfdir, pattern = "cdf",
                         full.names = TRUE, ignore.case = TRUE)
  
  result.live <- runGC(files = cdffiles, settings = TSQXLS.GC,
                       DB = DB, nSlaves = 2)

  ## load the precompiled version, result.annot
  data(GCresults)
  
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
} else{
  warning("cannot run tests --  package metaMSdata not available")
}
