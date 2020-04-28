## Function constructExpPseudoSpectra creates an msp object containing all the patterns referenced to in the annotation. The
## first argument is the output of function matchSamples2Samples and contains the full annotation matrix and the pseudospectra
## of the known unknowns; the second is the msp object containing the standards that are actually found

constructExpPseudoSpectra <- function(allMatches, standardsDB) {
    allAnnotations <- sort(unique(unlist(sapply(allMatches$annotations, function(x) x[, "annotation"]))))
    
    stdDB <- lapply(allAnnotations[allAnnotations > 0], function(x) c(standardsDB[[x]], list(DB.idx = x)))
    
    if (min(allAnnotations) < 0) {
        ## write known unknowns
        unknDB <- allMatches$unknowns
        
        if ("RI" %in% colnames(unknDB[[1]])) {
            extra.info <- data.frame(Name = paste("Unknown", 1:length(unknDB)), DB.idx = sort(allAnnotations[allAnnotations < 
                0], decreasing = TRUE), rt = sapply(unknDB, function(x) round(mean(x[, "rt"]), 2)), RI = sapply(unknDB, function(x) round(mean(x[, 
                "RI"]), 0)), Class = "Unknown", stringsAsFactors = FALSE)
        } else {
            extra.info <- data.frame(Name = paste("Unknown", 1:length(unknDB)), DB.idx = sort(allAnnotations[allAnnotations < 
                0], decreasing = TRUE), rt = sapply(unknDB, function(x) round(mean(x[, "rt"]), 2)), Class = "Unknown", stringsAsFactors = FALSE)
        }
        unkn.msp <- construct.msp(spectra = unknDB, extra.info)
        
        c(stdDB, unkn.msp)
    } else {
        stdDB
    }
}
