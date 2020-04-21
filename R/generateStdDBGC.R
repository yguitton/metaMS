## Function changes a list of xset objects from injections of (combinations of) standards into spectra for individual
## standards.
generateStdDBGC <- function(totalXset, settings, extDB = NULL, manualDB = NULL, RIstandards = NULL) {
    if (!is.null(manualDB)) {
        for (i in 1:length(manualDB)) if (is.null(manualDB[[i]]$Class)) 
            manualDB[[i]]$Class <- "Manual"
    }
    
    if (!is.null(totalXset)) {
        if (!is.null(extDB)) {
            DBobj <- match2ExtDB(totalXset, extDB, settings)
        } else {
            if (!is.null(totalXset)) {
                DBobj <- xset2msp(totalXset, settings)
            }
        }
        for (i in 1:length(DBobj)) DBobj[[i]]$Class <- "Standard"
        
        ## also works when manualDB == NULL :-D
        DBobj <- c(DBobj, manualDB)
        
        ## Here we set the name of the intensity value for all entries in the DB to the appropriate value
        for (i in 1:length(DBobj)) colnames(DBobj[[i]]$pspectrum)[2] <- settings$intensityMeasure
        
        ## for those entries in the DB that do not have a date field: add it
        nodate <- which(sapply(DBobj, function(x) is.null(x$date)))
        for (i in nodate) DBobj[[i]]$date <- format(Sys.time(), "%b %d %Y")
    } else {
        DBobj <- manualDB
    }
    
    if (!is.null(RIstandards)) {
        DBobj <- addRI(DBobj, RIstandards)
        ## rename all 'RI' fields to 'std.RI'
        DBobj <- lapply(DBobj, function(x) {
            x$std.RI <- x$RI
            x$RI <- NULL
            
            x
        })
    }
    
    ## rename all 'rt' and 'rt.std' fields to 'std.rt' and 'std.rt.sd'
    lapply(DBobj, function(x) {
        x$std.rt <- x$rt
        x$std.rt.sd <- x$rt.sd
        x$rt <- x$rt.sd <- NULL
        
        x
    })
}


match2ExtDB <- function(xsetList, extDB, settings) {
    DBobj <- vector(0, mode = "list")
    
    for (f.idx in 1:length(xsetList)) {
        ## loop over injections
        injection <- xsetList[[f.idx]]
        
        cnames <- injection$info[, 1]
        
        for (c.idx in 1:length(cnames)) {
            ## loop over standards if an extDB entry is found we check whether MW agrees with the monoMW given by Georg, and obtain the
            ## molecular formula.
            cas.idx <- which(sapply(extDB, function(x, y) x$CASNO == y, injection$info[c.idx, "CAS"]))
            
            if (length(cas.idx) == 0) {
                ## This should not happen!!
                printWarning("No matching CAS number found for", injection$info[c.idx, "Name"], "\nPlease check and perhaps update table.")
            } else {
                MWs <- sapply(extDB[cas.idx], function(x) x$MW)
                MW <- unique(MWs)
                if (length(MW) > 1) {
                  printWarning("More than one MW value for ", injection$info[c.idx, "Name"], ", CAS ", injection$info[c.idx, 
                    "CAS"], "\n\tcomparing value in table with the most common one.")
                  MW <- which.max(tabulate(MWs))
                }
                if (length(MW) == 0) {
                  printWarning("No MW value in external DB for CAS", injection$info[c.idx, "CAS"])
                }
                
                if (MW != round(injection$info[c.idx, "monoMW"])) {
                  printWarning("Ext DB MW (", MW, ") does not agree with MW from table (", round(injection$info[c.idx, "monoMW"]), 
                    ") for ", injection$info[c.idx, "Name"], ", CAS ", injection$info[c.idx, "CAS"])
                }
                
            }
            
            ## In the following, we use the table value from Georg
            MW <- injection$info[c.idx, "monoMW"]
            
            psp2 <- to.msp(injection$xset, file = NULL, settings = settings)
            psp2 <- lapply(psp2, function(x) x[order(x[, "mz"]), , drop = FALSE])
            
            ## remove features higher than the molecular mass plus 3 (to allow for isotopes)
            psp2 <- lapply(psp2, function(x) x[x[, "mz"] < MW + 4, , drop = FALSE])
            psp2 <- psp2[sapply(psp2, nrow) >= settings$minfeat]
            psp2 <- lapply(psp2, function(x) cbind(x[, 1:2], rt = round(x[, 3], 2)))
            
            rtrange <- injection$info$RT[c.idx] + c(-settings$rttol, settings$rttol)
            rts <- sapply(psp2, function(x) mean(x[, 3]))
            idx <- which(rts < max(rtrange) & rts > min(rtrange))
            psp2 <- psp2[idx]
            
            ## now calculate matches with extDB - only retain those spectra having a match higher than the threshold
            if (length(idx) > 0) {
                if (length(cas.idx) > 0) {
                  psp2 <- lapply(psp2, function(x) cbind(round(x[, 1]), x[, -1]))
                  match.results <- sapply(psp2, matchExpSpec, extDB[cas.idx], plotIt = FALSE, DB.treated = TRUE)
                  
                  if (is.matrix(match.results)) {
                    best.match <- round(apply(match.results, 2, max), 4)
                  } else {
                    if (length(idx) > 1) {
                      ## only one entry in ext DB!
                      best.match <- round(match.results, 4)
                    } else {
                      ## only one pattern found in the experimental data
                      best.match <- round(max(match.results), 4)
                    }
                  }
                }
            }
            
            if (length(psp2) > 0) {
                ## rt given is the one from Georg (in minutes); individual retention times are available in the third column of pspectrum,
                ## again in minutes
                output.info <- injection$info[c.idx, ]
                output.df <- as.data.frame(output.info, stringsAsFactors = FALSE)
                output.df <- cbind(output.df[rep(1, length(psp2)), ], bestDBmatch = best.match)
                msp.obj <- construct.msp(psp2, output.df)
                ## Filtering this object is not necessary since it has aready been done at the psp2 level
                
                DBobj <- c(DBobj, msp.obj[best.match > settings$DBthreshold])
            } else {
                printWarning("No pattern found matching all criteria for", injection$info[c.idx, "Name"], "- CAS number", injection$info[c.idx, 
                  "CAS"])
            }
        }
    }
    
    DBobj
}



## conversion function, to get from a list of xset objects and corresponding meta-information to an msp object containing all
## relevant bits.

xset2msp <- function(xsetList, settings) {
    DBobj <- NULL
    
    for (f.idx in 1:length(xsetList)) {
        ## loop over injections
        injection <- xsetList[[f.idx]]
        
        cnames <- injection$info[, 1]
        
        for (c.idx in 1:length(cnames)) {
            ## loop over standards
            MW <- injection$info[c.idx, "monoMW"]
            psp2 <- to.msp(xsetList[[f.idx]]$xset, file = NULL, settings = settings)
            psp2 <- lapply(psp2, function(x) x[order(x[, "mz"]), , drop = FALSE])
            ## remove features higher than the molecular mass plus 3 (to allow for isotopes)
            psp2 <- lapply(psp2, function(x) x[x[, "mz"] < MW + 4, , drop = FALSE])
            psp2 <- psp2[sapply(psp2, nrow) >= settings$minfeat]
            psp2 <- lapply(psp2, function(x) cbind(x[, 1:2], rt = round(x[, 3], 2)))
            
            rtrange <- injection$info$RT[c.idx] + c(-settings$rttol, settings$rttol)
            rts <- sapply(psp2, function(x) mean(x[, 3]))
            idx <- which(rts < max(rtrange) & rts > min(rtrange))
            psp2 <- psp2[idx]
            if (length(psp2) > 0) {
                ## rt given is the one from Georg (in minutes); individual retention times are available in the third column of pspectrum,
                ## again in minutes
                output.info <- injection$info[c.idx, ]
                output.df <- as.data.frame(output.info, stringsAsFactors = FALSE)
                msp.obj <- construct.msp(psp2, output.df)
                ## Filtering this object is not necessary since it has aready been done at the psp2 level
                
                DBobj <- c(DBobj, msp.obj)
            }
        }
    }
    
    DBobj
}
