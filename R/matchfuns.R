## Since in our GC data m/z values are rounded to whole numbers, in some cases an m/z value may occur multiple times - in that
## case the mean of the intensities is kept (and the retention times are averaged).  removeDoubleMasses takes a list of
## spectra in the form of a three-column matrix, (mz, I, rt), summing the intensities and averaging the retention times for
## multiple identical masses

## Aug 1, 2013: adapt to handle both 2- and 3-column spectra (retention time is optional)
removeDoubleMasses <- function(spclist) {
    lapply(spclist, function(pspc) {
        if (max(table(pspc[, 1])) > 1) {
            if (ncol(pspc) == 3) {
                huhn <- cbind(aggregate(pspc[, 2], list(pspc[, 1]), FUN = sum), aggregate(pspc[, 3], list(pspc[, 1]), FUN = mean)$x)
            } else {
                huhn <- cbind(aggregate(pspc[, 2], list(pspc[, 1]), FUN = sum))
            }
            colnames(huhn) <- colnames(pspc)
            
            huhn
        } else {
            pspc
        }
    })
}



## match of preprocessed pseudospectra
mzmatch <- function(spec1, spec2) {
    common.masses1 <- which(spec1[, 1] %in% spec2[, 1])
    common.masses2 <- which(spec2[, 1] %in% spec1[, 1])
    
    if (length(common.masses1) > 0) {
        sum(spec1[common.masses1, 2, drop = FALSE] * spec2[common.masses2, 2, drop = FALSE])
    } else {
        0
    }
}


## This function transforms the 'raw' data in an external DB object into preprocessed data. Even if no scale and no
## mass.weight is applied, the intensities are still changed: scaled to unit length. If isMSP is FALSE, the DB contains a list
## of matrices, the first column being mz and the second being the intensity measure.
treat.DB <- function(DB, scale.p = c("sqrt", "none"), mass.weight = TRUE, isMSP = TRUE) {
    scale.p = match.arg(scale.p)
    if (scale.p == "sqrt") {
        if (isMSP) {
            for (i in 1:length(DB)) {
                DB[[i]]$pspectrum[, 2] <- sqrt(DB[[i]]$pspectrum[, 2])
            }
        } else {
            for (i in 1:length(DB)) {
                DB[[i]][, 2] <- sqrt(DB[[i]][, 2])
            }
        }
    }
    
    if (mass.weight) {
        if (isMSP) {
            for (i in 1:length(DB)) {
                huhn <- sqrt(DB[[i]]$pspectrum[, 1]) * DB[[i]]$pspectrum[, 2]
                DB[[i]]$pspectrum[, 2] <- huhn/sqrt(sum(huhn^2))
            }
        } else {
            for (i in 1:length(DB)) {
                huhn <- sqrt(DB[[i]][, 1]) * DB[[i]][, 2]
                DB[[i]][, 2] <- huhn/sqrt(sum(huhn^2))
            }
        }
    } else {
        if (isMSP) {
            for (i in 1:length(DB)) {
                DB[[i]]$pspectrum[, 2] <- DB[[i]]$pspectrum[, 2]/sqrt(sum(DB[[i]]$pspectrum[, 2]^2))
            }
        } else {
            for (i in 1:length(DB)) {
                DB[[i]][, 2] <- DB[[i]][, 2]/sqrt(sum(DB[[i]][, 2]^2))
            }
        }
    }
    
    DB
}

## convenience function for comparing different match functions March 14: added an argument DB.treated. Much of the matching
## time needed is to preprocess the DB entries, which could better be done once and saved under a different name. To maintain
## compatibility the default is to assume no treatment. Note that treatment is not idempotent, so that doing it twice will
## lead to different results!!  Also implemented is a convenience function treat.DB

## Jan 21, 2013: now no double masses are allowed anymore in pspec

matchExpSpec <- function(pspec, DB, DB.treated, plotIt = FALSE, scale.p = c("sqrt", "none"), mass.weight = TRUE, ...) {
    if (DB.treated) {
        mass.weight <- TRUE
        scale.p <- "sqrt"
    }
    
    scale.p = match.arg(scale.p)
    if (scale.p == "sqrt") {
        pspec[, 2] <- sqrt(pspec[, 2])
        
        if (!DB.treated) {
            for (i in 1:length(DB)) {
                DB[[i]]$pspectrum[, 2] <- sqrt(DB[[i]]$pspectrum[, 2])
            }
        }
    }
    
    if (mass.weight) {
        if (!DB.treated) {
            for (i in 1:length(DB)) {
                huhn <- sqrt(DB[[i]]$pspectrum[, 1]) * DB[[i]]$pspectrum[, 2]
                DB[[i]]$pspectrum[, 2] <- huhn/sqrt(sum(huhn^2))
            }
        }
        
        huhn <- sqrt(pspec[, 1]) * pspec[, 2]
    } else {
        huhn <- pspec[, 2]  ## no weight
    }
    pspec[, 2] <- huhn/sqrt(sum(huhn^2))  ## normalize
    
    if (!DB.treated) {
        for (i in 1:length(DB)) {
            DB[[i]]$pspectrum[, 2] <- DB[[i]]$pspectrum[, 2]/sqrt(sum(DB[[i]]$pspectrum[, 2]^2))
            
        }
    }
    
    huhn <- sapply(DB, function(x) mzmatch(x$pspectrum, pspec))
    
    if (plotIt) {
        best.match <- which.max(huhn)
        plot(pspec[, 1], 1000 * pspec[, 2]/max(pspec[, 2]), ylim = c(0, 1000), type = "h", col = 2, lwd = 3, xlab = "mz", ylab = "I", 
            sub = paste("Match factor:", round(max(huhn), 3)), ...)
        lines(DB[[best.match]]$pspectrum[, 1], 1000 * DB[[best.match]]$pspectrum[, 2]/max(DB[[best.match]]$pspectrum[, 2]), col = 4, 
            lwd = 1, type = "h")
        legend("topright", legend = c("Compound", "DB best match"), col = c(2, 4), lwd = c(4, 2), lty = 1)
    }
    
    huhn
}

