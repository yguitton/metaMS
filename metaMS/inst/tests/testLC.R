## Script to test the main metaMS functions for LC
if (require("metaMSdata", quietly = TRUE)) {
  ## We plan to test runLC and createSTDdbLC
  ## in both cases the function are run on the metaMSdata samples
  ## and the outputs are compared with the ones present in the data folder
  
  ## <----------------   runLC -------------------------- >
  ## run the peak picking
  library(metaMS)
  data(FEMsettings)  ## The settings
  data(LCDBtest)     ## The database
  library(metaMSdata)
  cdfpath <- system.file("CDF_LC", package = "metaMSdata")
  files <- list.files(cdfpath, "CDF", full.names=TRUE)
  LC <- runLC(files, settings = Synapt.RP, DB = LCDBtest$DB, nSlaves = 2)
  
  ## load the reference output
  data(LCresults)
  
  ## Run some tests on the output
  test_that("xcms and CAMERA are gicing consistent results", {
    expect_equal(nrow(LC$PeakTable), nrow(LCresults$PeakTable))
    expect_equal(ncol(LC$PeakTable), ncol(LCresults$PeakTable))
  })
  
  ## Annotation gives the same results...
  test_that("Peak table annotation gives consistent results", {
    expect_equal(LC$Annotation$ann.features,LCresults$Annotation$ann.features)
  })
  
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
  warning("package metaMSdata not available")
}
