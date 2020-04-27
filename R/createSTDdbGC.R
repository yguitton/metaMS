createSTDdbGC <- function(stdInfo, settings, extDB = NULL, manualDB = NULL, RIstandards = NULL, nSlaves = 0) {
    if (is.null(stdInfo)) {
        if (is.null(manualDB)) {
            stop("At least one of stdInfo and manualDB must be non-null!")
        } else {
            ## only manualDB will be returned
            totalXset <- NULL
        }
    } else {
        if (is.null(extDB) & !is.null(manualDB)) 
            printWarning("Watch out:\n", "combining unvalidated experimental data with a ", "manually curated database!\n", "If this is not what you intended, cancel now.")
        
        printString("Processing", length(unique(stdInfo[, "stdFile"])), "input files for", nrow(stdInfo), "standards")
        printString("Running XCMS and CAMERA")
        
        totalXset <- processStandards(stdInfo, settings, nSlaves = nSlaves)
    }
    
    generateStdDBGC(totalXset, settings = metaSetting(settings, "DBconstruction"), extDB = extDB, manualDB = manualDB, RIstandards = RIstandards)
}
