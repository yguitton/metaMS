## New version, working for both LC and GC, and reading cdf as well as mzxml files

## 1) compare filenames from input file with files in standards dir 2) process all input files in parallel 3) split xset
## object, and loop over all elements separately to keep only retention times close to the manually defined times 4) return
## result

processStandards <- function(stdInfo, settings, polarity = NULL, nSlaves) {
    
    chrom <- metaSetting(settings, "chrom")
    if (chrom == "LC") {
        if (!polarity %in% c("positive", "negative")) {
            stop("For LC data, 'polarity' should either be 'positive' or 'negative'")
        }
    }
    
    stdNames <- sort(unique(stdInfo[, "stdFile"]))
    xset.l <- peakDetection(stdNames, metaSetting(settings, "PeakPicking"), convert2list = TRUE, nSlaves = nSlaves)
    fn <- names(xset.l)
    
    xset.l.small <- lapply(1:length(fn), function(i) {
        standard.hits <- which(stdInfo$stdFile == stdNames[i])
        if (length(standard.hits) == 0) {
            warning("No standard compound from table in file ", fn[i], "...skipping\n")
            
            NULL
        } else {
            standard.info <- stdInfo[standard.hits, ]
            
            rts <- stdInfo[standard.hits, "RTman"]
            rttol <- metaSetting(settings, "DBconstruction")$rttol
            rt.ranges <- cbind(rts - rttol, rts + rttol) * 60
            
            idx <- which(apply(sapply(1:nrow(rt.ranges), function(ii) xset.l[[i]]@peaks[, "rt"] > rt.ranges[ii, 1] & xset.l[[i]]@peaks[, 
                "rt"] < rt.ranges[ii, 2]), 1, any))
            if (length(idx) <= metaSetting(settings, "DBconstruction")$minfeat) {
                warning("No peaks with the right retention time found in file ", fn[i], "...skipping\n")
                
                NULL
            } else {
                xset.l[[i]]@peaks <- xset.l[[i]]@peaks[idx, ]
                
                list(info = standard.info, xset = xset.l[[i]])
            }
        }
    })
    
    ## remove empty elements from xset.l.small
    empty.idx <- which(sapply(xset.l.small, function(x) length(x[[1]])) == 0)
    if (length(empty.idx) > 0) 
        xset.l.small <- xset.l.small[-empty.idx]
    
    lapply(xset.l.small, function(xx) list(info = xx$info, xset = runCAMERA(xx$xset, chrom = chrom, settings = metaSetting(settings, 
        "CAMERA"), polarity = polarity, quick = FALSE)))
}
