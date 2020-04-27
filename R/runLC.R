runLC <- function(files, xset, settings, rtrange = NULL, mzrange = NULL, DB = NULL, polarity = "positive", errf = NULL, returnXset = FALSE, 
    intensity = "into", nSlaves = 0) {
    ## initial check on the inputs.
    if (!missing(files)) {
        nexp <- length(files)
    } else {
        if (missing(xset)) 
            stop("Either 'files' or 'xset' should be given")
        if (is(xset)[1] != "xsAnnotate") 
            stop("xset should be of class 'xsAnnotate'")
        nexp <- length(sampnames(xset@xcmsSet))
    }
    ## Ok let's start the analysis
    printString(paste("Experiment of", nexp, "samples"))
    printString(paste("Instrument:", metaSetting(settings, "protocolName"), " - polarity:", polarity))
    
    ## limit the retention time
    if (length(rtrange) == 2) {
        printString(paste("Retention time range:", rtrange[1], "to", rtrange[2], "minutes"))
    }
    ## a short echo on the size of the DB
    if (!is.null(DB)) {
        printString("Database of", length(unique(DB$ChemSpiderID)), "compounds")
    }
    ## if there are samples ...
    if (!missing(files)) {
        
        #------------------------- Peak picking  ------------------------------ >
        printString("Performing peak picking")
        xset <- peakDetection(files, metaSetting(settings, "PeakPicking"), rtrange = rtrange, mzrange = mzrange, nSlaves = nSlaves)
        
        #------------------------- Grouping and alignment  -------------------- >
        printString("Grouping and retention time alignment")
        xset <- alignmentLC(xset, metaSetting(settings, "Alignment"))
        
        #------------------------- CAMERA ---------------- -------------------- >
        printString("Performing CAMERA grouping")
        xset <- runCAMERA(xset, chrom = "LC", metaSetting(settings, "CAMERA"), polarity)
    } else {
        ## If I provide already the CAMERA
        printString("Using xcmsSet object - only doing annotation")
    }
    
    #------------------------- Get The Peak Table---------------------------- >
    peakTable <- getPeakTable(xset, intval = intensity)
    
    #------------------------- Annotation ----------------------------------- >
    
    if (!is.null(DB)) {
        ## if an annotation DB is provided
        printString("Performing annotation")  # --------------------------- >
        annotation <- getAnnotationLC(xset, metaSetting(settings, "match2DB"), DB, errf)
        
        ## To run the annotation we need: table mz,rt,I, errf and DB.  With the additional functions AnnotateFeaturesAnnotateTable on
        ## the configurations I need the rt tol, the rt4validation, mass tolerance if no surface.  else return an empty annotation
        ## object
    } else {
        annotation <- list(raw = data.frame(), for_table = data.frame())
    }
    
    ## If the annotation is required add the output ---------------
    if (!is.null(DB)) {
        peakTable <- cbind(annotation$for_table, peakTable)
    }
    
    
    ## ----------------- Prepare the outputs ------------------------------ >
    printString("Done !")
    if (returnXset) {
        list(PeakTable = peakTable, xset = xset, Annotation = annotation$raw, Settings = settings, SessionInfo = sessionInfo())
    } else {
        list(PeakTable = peakTable, Annotation = annotation$raw, Settings = settings, SessionInfo = sessionInfo())
    }
}
