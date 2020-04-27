generateStdDBLC <- function(stdxsets, settings, Ithr) {
    ## Get out the Peak lists
    pklists <- lapply(stdxsets, function(xx) {
        list(info = xx$info, pkt = getPeakTable(xx$xset, intval = "maxo"))
    })
    
    
    ## Matching with the tables to get the DB
    pseudospectra <- lapply(pklists, function(x) {
        feat.match <- lapply(1:nrow(x$info), function(rw) {
            id <- (abs(x$pkt$mz - x$info[rw, "mz.observed"]) < settings$mztol) & (abs(x$pkt$rt - x$info[rw, "RTman"]) < settings$rttol)
            pcgrpid <- x$pkt$pcgroup[id]
            if (sum(id) == 0) {
                printInfo("No match for", x$info[rw, "compound"])
                return(data.frame())
            } else {
                printInfo("Processing", x$info[rw, "compound"])
                mytable <- x$pkt[x$pkt$pcgroup == pcgrpid, ]
                sortid <- order(mytable$mz)  ## sort as a function of the mass
                mytable <- mytable[sortid, ]
                mytable <- mytable[mytable$maxo > Ithr, ]  ## Threshold the ion intensity
                if (nrow(mytable) < settings$minfeat) {
                  return(data.frame())
                }
                out <- data.frame(ChemSpiderID = rep(x$info[rw, "ChemSpiderID"], times = nrow(mytable)), compound = rep(x$info[rw, 
                  "compound"], times = nrow(mytable)), mytable[, -(colnames(mytable) == "pcgroup")], validated = rep("automatic", 
                  times = nrow(mytable)))
            }
        })
        
        feat.match <- do.call(rbind, feat.match)
    })
    
    ## join the tables to have a DB
    db <- do.call(rbind, pseudospectra)
    rownames(db) <- NULL
    db
}
