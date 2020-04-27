## Matching grouped data with the database of standards Input: msp data object containing the samples, a msp data object
## containing the preprocessed DB, and some settings in the form of a list.  Output: a list of two elements, both nested
## lists: the first indicates which pseudospectrum in which sample is associated to which standard, the second contains the
## spectral match factors.

## New implementation, only doing spectral matching for those cases where the rt differences are below the threshold. Much
## faster!

matchSamples2DB <- function(xset.msp, DB, settings, quick) {
    if (settings$timeComparison == "RI") {
        standard.rts <- sapply(DB, function(x) x$std.RI)
        ## rt.matches is a list of two-column matrices, one for each xset.msp element. The first column gives the DB entry that
        ## matches with the sample entry in the second column. This is very fast to calculate.
        rt.matches <- lapply(1:length(xset.msp), function(ii) {
            group.rts <- sapply(xset.msp[[ii]], function(x) mean(x[, "RI"]))
            which(abs(outer(standard.rts, group.rts, FUN = "-")) < settings$RIdiff, arr.ind = TRUE)
        })
    } else {
        standard.rts <- sapply(DB, function(x) x$std.rt)
        
        rt.matches <- lapply(1:length(xset.msp), function(ii) {
            group.rts <- sapply(xset.msp[[ii]], function(x) mean(x[, "rt"]))
            which(abs(outer(standard.rts, group.rts, FUN = "-")) < settings$rtdiff, arr.ind = TRUE)
        })
    }
    
    if (quick) {
        ## xset.msp has already been scaled
        match.results <- lapply(1:length(xset.msp), function(ii) {
            if (nrow(rt.matches[[ii]] > 0)) {
                result <- matrix(0, length(DB), length(xset.msp[[ii]]))
                for (i in 1:nrow(rt.matches[[ii]])) {
                  DB.idx <- rt.matches[[ii]][i, 1]
                  sample.idx <- rt.matches[[ii]][i, 2]
                  result[DB.idx, sample.idx] <- mzmatch(DB[[DB.idx]]$pspectrum, xset.msp[[ii]][[sample.idx]])
                }
            }
            result
        })
    } else {
        ## scaling is done for each comparison separately, since high mz values may be removed depending on MonoMW of the standard
        ## compound. Slower, obviously.
        match.results <- lapply(1:length(xset.msp), function(ii) {
            result <- matrix(0, length(DB), length(xset.msp[[ii]]))
            if (nrow(rt.matches[[ii]] > 0)) {
                for (i in 1:nrow(rt.matches[[ii]])) {
                  DB.idx <- rt.matches[[ii]][i, 1]
                  sample.idx <- rt.matches[[ii]][i, 2]
                  exp.pat <- xset.msp[[ii]][[sample.idx]]
                  
                  MWlimit <- DB[[DB.idx]]$monoMW + 4
                  ## sometimes, with manually added spectra, monoMW is not present... then we use everything up until the highest mass present.
                  if (length(MWlimit) == 0) 
                    MWlimit <- max(DB[[DB.idx]]$pspectrum[, 1])
                  
                  ok.mz <- which(exp.pat[, "mz"] <= MWlimit)
                  if (length(ok.mz) > settings$minfeat) {
                    exp.pat <- treat.DB(list(exp.pat[ok.mz, ]), isMSP = FALSE)
                    result[DB.idx, sample.idx] <- mzmatch(DB[[DB.idx]]$pspectrum, exp.pat[[1]])
                  }
                }
            }
            result
        })
    }
    names(match.results) <- names(xset.msp)
    
    annotations <- lapply(match.results, function(xx) {
        sapply(1:ncol(xx), function(ii) which(xx[, ii] > settings$simthresh))
    })
    
    list(annotations = mapply(annotations2tab, annotations, match.results, SIMPLIFY = FALSE))
}
