## RIstandards is a two-column matrix, column 1 containing the RI, column 2 the corresponding retention time. Only if RI is
## not already there.
addRI <- function(mspobj, RIstandards, isMSP = TRUE) {
    if (isMSP) {
        lapply(mspobj, function(obj) {
            if (is.null(obj$RI)) 
                obj$RI <- round(approx(RIstandards[, "rt"], RIstandards[, "RI"], obj$rt)$y)
            obj
        })
    } else {
        lapply(mspobj, function(obj) {
            cbind(obj, RI = round(approx(RIstandards[, "rt"], RIstandards[, "RI"], obj[, "rt"])$y))
        })
    }
}
