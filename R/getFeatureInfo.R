## Function getFeatureInfo yields a data.frame with meta-information on all features detected in the samples. Features are
## rows; information is in the columns. The last column of the meta-info is always 'rt'.

getFeatureInfo <- function(stdDB, allMatches, sampleList) {
    signif.rt <- 3
    signif.rt.sd <- 4
    signif.RI <- 0
    
    allAnnotations <- sort(unique(unlist(sapply(allMatches$annotations, function(x) x[, "annotation"]))))
    allAnnotations <- c(allAnnotations[allAnnotations > 0], rev(allAnnotations[allAnnotations < 0]))
    
    ## add rt and rt.sd information, and possibly RI info
    patRTs <- lapply(sampleList, function(x) sapply(x, function(xx) mean(xx[, "rt"])))
    if ("RI" %in% colnames(sampleList[[1]][[1]])) {
        RIpresent <- TRUE
        patRIs <- lapply(sampleList, function(x) sapply(x, function(xx) mean(xx[, "RI"])))
    } else {
        RIpresent <- FALSE
    }
    
    pSpectra <- lapply(allAnnotations[allAnnotations > 0], function(x) {
        hits <- lapply(allMatches$annotations, function(xx) xx[xx[, "annotation"] == x, "pattern"])
        rts <- mapply(function(x, y) ifelse(length(y) > 0, x[y], NA), patRTs, hits)
        
        if (RIpresent) {
            RIs <- mapply(function(x, y) ifelse(length(y) > 0, x[y], NA), patRIs, hits)
            c(stdDB[[x]], list(rt = round(mean(rts, na.rm = TRUE), signif.rt), rt.sd = round(sd(rts, na.rm = TRUE), signif.rt.sd), 
                RI = round(mean(RIs, na.rm = TRUE), signif.RI)))
        } else {
            c(stdDB[[x]], list(rt = round(mean(rts, na.rm = TRUE), signif.rt), rt.sd = round(sd(rts, na.rm = TRUE), signif.rt.sd)))
        }
    })
    
    if (length(pSpectra) > 0) {
        ## in the meta information we include all fields present in the stdDB, except for the pspectrum, std.rt.sd, and bestDBmatch.
        ## 'Name' is always the first entry, 'rt' always the last, and the other rt-related fields directly in front of 'rt'.
        fields <- unique(unlist(lapply(pSpectra, names)))
        if (RIpresent) {
            rtfields <- c("std.RI", "std.rt", "RI", "rt.sd", "rt")
        } else {
            rtfields <- c("std.rt", "rt.sd", "rt")
        }
        fields <- c("Name", fields[!(fields %in% c("Name", "pspectrum", "bestDBmatch", "std.rt.sd", rtfields))], rtfields)
        
        meta.info <- lapply(pSpectra, function(x) {
            notPresent <- which(is.na(match(fields, names(x))))
            if (length(notPresent) > 0) {
                newlst <- lapply(notPresent, function(nm) NA)
                names(newlst) <- fields[notPresent]
                c(newlst, x[fields[-notPresent]])
            } else {
                x[fields]
            }
        })
        ## Next two instructions as suggested by Martin Morgan at
        ## http://stackoverflow.com/questions/4512465/what-is-the-most-efficient-way-to-cast-a-list-as-a-data-frame?rq=1
        f <- function(x) function(i) unlist(lapply(x, `[[`, i), use.names = FALSE)
        meta.info.df <- as.data.frame(Map(f(meta.info), fields), stringsAsFactors = FALSE)
    } else {
        ## no DB, or no hits from DB
        meta.info.df <- data.frame(Name = 1, Class = 1, rt.sd = 1, rt = 1)[FALSE, ]
    }
    
    ## Since the labels are negative, label -1 corresponds to the last unknown spectrum. This convention is also used in
    ## allAnnotations and in function constructExpPseudoSpectra
    nstd <- nrow(meta.info.df)
    nunkn <- sum(allAnnotations < 0)
    
    if (nunkn > 0) {
        meta.info.df <- rbind(meta.info.df, meta.info.df[rep(1, nunkn), ])
        row.names(meta.info.df) <- 1:nrow(meta.info.df)
        meta.info.df[(nstd + 1):nrow(meta.info.df), ] <- NA
        
        meta.info.df[(nstd + 1):(nstd + nunkn), "Name"] <- paste("Unknown", 1:nunkn)
        meta.info.df[(nstd + 1):(nstd + nunkn), "Class"] <- "Unknown"
        meta.info.df[(nstd + 1):(nstd + nunkn), "rt"] <- round(sapply(allMatches$unknowns, function(x) mean(x[, "rt"])), signif.rt)
        meta.info.df[(nstd + 1):(nstd + nunkn), "rt.sd"] <- round(sapply(allMatches$unknowns, function(x) sd(x[, "rt"])), signif.rt.sd)
        
        if (RIpresent) 
            meta.info.df[(nstd + 1):(nstd + nunkn), "RI"] <- round(sapply(allMatches$unknowns, function(x) mean(x[, "RI"])), 
                signif.RI)
    }
    
    meta.info.df
}


