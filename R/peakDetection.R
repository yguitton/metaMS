peakDetection <- function(files, settings, rtrange = NULL, mzrange = NULL, convert2list = FALSE, nSlaves = 0) {
    ## Rmpi tends to give many warnings that are not relevant to end users: this is an attempt to suppress this output
    owarn <- options("warn")
    on.exit(options(warn = owarn$warn))
    options(warn = -1)
    
    ## if an rtrange is given, we first find out which scans correspond to this and then use the scanRange argument of xcmsSet
    if (!is.null(rtrange)) {
        if (length(rtrange) != 2) 
            stop("Improper rtrange given!")
        
        rtrange <- rtrange * 60  ## convert from minutes to seconds
        
        xr <- xcmsRaw(files[1])
        scanRange <- c(max(1, which(xr@scantime > rtrange[1])[1], na.rm = TRUE), min(length(xr@scantime), which(xr@scantime > 
            rtrange[2])[1] - 1, na.rm = TRUE))
        
        allSettings <- c(list(files = files, scanrange = scanRange, nSlaves = nSlaves), settings)
    } else {
        allSettings <- c(list(files = files, nSlaves = nSlaves), settings)
    }
    
    xset <- do.call(xcmsSet, allSettings)
    
    if (!is.null(mzrange)) {
        idx <- (xset@peaks[, "mz"] > mzrange[1]) & (xset@peaks[, "mz"] < mzrange[2])
        xset@peaks <- xset@peaks[idx, ]
    }
    
    
    if (convert2list) {
        ## for GC, for example explicit definition of levels keeps the original order of the samples
        split(xset, factor(sampnames(xset), levels = sampnames(xset)))
    } else {
        xset
    }
}

