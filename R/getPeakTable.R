getPeakTable <- function(xs, intval = "into") {
    ## get peak table from XCMS object -----------------------------------------
    if (class(xs)[1] == "xcmsSet") {
        if (length(sampnames(xs)) == 1) {
            sortorder <- order(xs@peaks[, "rt"])  ## order by retention time  
            peakTable <- data.frame(mz = xs@peaks[sortorder, "mz"], rt = xs@peaks[sortorder, "rt"]/60, intensity = xs@peaks[sortorder, 
                intval], row.names = NULL, stringsAsFactors = FALSE)
            ## put the right title to the intensity column
            colnames(peakTable)[colnames(peakTable) == "intensity"] <- intval
            return(peakTable)
        }
        ## if Xset object and more than one sample
        sortorder <- order(xs@groups[, "rtmed"])  ## rt ordering
        dataMatrix <- groupval(xs, value = intval)  ## extract the intensities
        peakTable <- data.frame(mz = xs@groups[sortorder, "mzmed"], rt = xs@groups[sortorder, "rtmed"]/60, dataMatrix[sortorder, 
            ], row.names = NULL, stringsAsFactors = FALSE)
        return(peakTable)
        
    } else {
        ## get peak table from CAMERA object ----------------------------
        pt <- getPeaklist(xs, intval = intval)  ## function of CAMERA package
        sortorder <- order(pt[, "rt"])  ## rt ordering
        pt <- pt[sortorder, ]
        ## also here one sample ....
        if (length(sampnames(xs@xcmsSet)) == 1) 
            {
                peakTable <- data.frame(pcgroup = as.numeric(pt$pcgroup), adduct = pt$adduct, isotopes = pt$isotopes, mz = pt$mz, 
                  rt = pt$rt/60, intensity = pt[, intval], row.names = NULL, stringsAsFactors = FALSE)
                ## put the right title to the intensity column
                colnames(peakTable)[colnames(peakTable) == "intensity"] <- intval
                return(peakTable)
            }  ## more samples
        dataMatrix <- pt[, make.names(sampnames(xs@xcmsSet))]  ## get only the columns with the intensities works also for banes with special characters
        peakTable <- data.frame(pcgroup = as.numeric(pt$pcgroup), adduct = pt$adduct, isotopes = pt$isotopes, mz = pt$mz, rt = pt$rt/60, 
            dataMatrix, row.names = NULL, stringsAsFactors = FALSE)
        return(peakTable)
    }
}


