## Function getAnnotationMat obtains relative intensities for features in individual samples. A robust linear regression is
## performed when the number of common features is five or larger; for 2-4 peaks a weighted linear regression is used, and if
## only one peak is in common the intensity ratio for this peak is used. As a reference either the stdDB patterns are used, or
## the first file in which a particular unknown is found.

## New version, Aug 13, now working with the new annotation object

getAnnotationMat <- function(exp.msp, pspectra, allMatches) {
    allAnnotations <- sort(unique(unlist(sapply(allMatches$annotations, function(x) x[, "annotation"]))))
    allAnnotations <- c(allAnnotations[allAnnotations > 0], rev(allAnnotations[allAnnotations < 0]))
    result <- matrix(0, length(allAnnotations), length(exp.msp))
    colnames(result) <- names(exp.msp)
    
    pspec.names <- sapply(pspectra, function(x) x$Name)
    
    ## for warnings from relInt...
    oldWarn <- options("warn")$warn
    options(warn = 2)
    
    for (j in seq(along = allAnnotations)) {
        ## a list, containing for every injection the spectra that match this particular annotation
        hits <- lapply(allMatches$annotations, function(x) x[x[, "annotation"] == allAnnotations[j], "pattern"])
        hit.idx <- which(sapply(hits, length) > 0)
        refpat <- pspectra[[j]]$pspectrum
        
        for (k in hit.idx) {
            ## if two different patterns are assigned to the same DB entry, only the larger intensity is kept
            intensities <- sapply(hits[[k]], function(idx) {
                pat <- exp.msp[[k]][[idx]]
                relInt(pat, refpat)
            })
            ## if result[j, k] already contains a number from another pattern, the largest of the intensities is kept
            result[j, k] <- max(result[j, k], intensities)
        }
    }
    
    options(warn = oldWarn)
    
    result
}

