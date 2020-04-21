runGC <- function(files, xset, settings, rtrange = NULL, DB = NULL, removeArtefacts = TRUE, findUnknowns = nexp >= mcs, returnXset = FALSE, 
    RIstandards = NULL, nSlaves = 0) {
    ## ################################################################### some preliminary sanity checks
    if (!missing(files)) {
        nexp <- length(files)
    } else {
        if (missing(xset)) 
            stop("Either 'files' or 'xset' should be given")
        
        if (is(xset)[1] == "xcmsSet") 
            stop("xset should be a list of CAMERA-grouped xcmsSet objects, see man page")
        
        xset.l <- xset
        nexp <- length(xset.l)
    }
    
    ## same criterion as in matchSamples2Samples
    mcs <- max(2, min(metaSetting(settings, "betweenSamples.min.class.size"), metaSetting(settings, "betweenSamples.min.class.fraction") * 
        nexp))
    ## check for unrealistic expectations: findUnknowns can only be relevant if enough samples are present.
    if (findUnknowns & nexp < mcs) {
        stop("Number of samples too small to define unknowns - either provide more samples or change the settings.")
    }
    
    if (is.null(DB) & !findUnknowns) 
        stop("Nothing to do. Provide a DB or set 'findUnknowns' to TRUE...")
    
    if (findUnknowns & !is.null(DB) & metaSetting(settings, "betweenSamples.timeComparison") != metaSetting(settings, "match2DB.timeComparison")) 
        stop("Settings error: choose one value for timeComparison only...")
    
    if (is.null(RIstandards) & ((metaSetting(settings, "betweenSamples.timeComparison") == "RI" & findUnknowns) | (metaSetting(settings, 
        "match2DB.timeComparison") == "RI" & !is.null(DB)))) 
        stop("Argument RIstandards is mandatory when using RI for matching")
    
    if (!is.null(RIstandards) & metaSetting(settings, "betweenSamples.timeComparison") == "rt" & metaSetting(settings, "match2DB.timeComparison") == 
        "rt") 
        printWarning("Warning: argument RIstandards provided, but using retention times for matching")
    
    printString(paste("Experiment of", nexp, "samples"))
    printString(paste("Instrument:", metaSetting(settings, "protocolName")))
    if (length(rtrange) == 2) 
        printString(paste("Retention time range:", rtrange[1], "to", rtrange[2], "minutes"))
    
    if (!is.null(DB)) {
        DB.orig <- DB
        DB <- treat.DB(DB.orig)
        
        printString(paste("Annotation using database of", length(DB), "spectra"))
    } else {
        printString("No annotation performed")
        DB.orig <- NULL
    }
    
    ## ################################################################### Peak picking and CAMERA
    if (!missing(files)) {
        printString("Performing peak picking and CAMERA")
        xset.l <- peakDetection(files, metaSetting(settings, "PeakPicking"), rtrange = rtrange, convert2list = TRUE, nSlaves = nSlaves)
        
        allSamples <- lapply(xset.l, runCAMERA, chrom = metaSetting(settings, "chrom"), settings = metaSetting(settings, "CAMERA"))
    } else {
        printString("Using xcmsSet object - only doing annotation")
        allSamples <- xset.l
    }
    
    
    ## ################################################################### convert into msp format (a nested list)
    allSamples.msp <- lapply(allSamples, to.msp, file = NULL, settings = metaSetting(settings, "DBconstruction"))
    names(allSamples.msp) <- sapply(allSamples, function(x) sampnames(x@xcmsSet))
    if (!is.null(RIstandards)) 
        allSamples.msp <- lapply(allSamples.msp, addRI, RIstandards, isMSP = FALSE)
    
    ## check: files without any features - should not happen very often...
    nofeats <- which(sapply(allSamples.msp, length) == 0)
    if ((nnof <- length(nofeats)) > 0) {
        printWarning("Removing", nnof, "injections without any features:\n\t", paste(names(allSamples)[nofeats], collapse = "\n\t "))
        allSamples.msp <- allSamples.msp[-nofeats]
    }
    ## now scale the rest...
    allSamples.msp.scaled <- lapply(allSamples.msp, treat.DB, isMSP = FALSE)
    
    ## remove Artefacts
    if (!is.null(DB)) {
        if (removeArtefacts) {
            printString(paste("Removing artefacts (", paste(metaSetting(settings, "matchIrrelevants.irrelevantClasses"), collapse = ", "), 
                ")", sep = ""))
            irrel.idx <- which(sapply(DB, function(x) x$Class) %in% metaSetting(settings, "matchIrrelevants.irrelevantClasses"))
            if (length(irrel.idx) > 0) {
                subDB <- DB[irrel.idx]
                junkPatterns <- lapply(matchSamples2DB(allSamples.msp.scaled, subDB, metaSetting(settings, "matchIrrelevants"), 
                  quick = TRUE)$annotations, function(x) x[, "pattern"])
                
                allSamples.msp <- mapply(function(x, y) if (length(y) > 0) {
                  x[-y]
                } else {
                  x
                }, allSamples.msp, junkPatterns)
                allSamples.msp.scaled <- mapply(function(x, y) if (length(y) > 0) {
                  x[-y]
                } else {
                  x
                }, allSamples.msp.scaled, junkPatterns)
                
                ## check: files without any features
                nofeats <- which(sapply(allSamples.msp, length) == 0)
                if ((nnof <- length(nofeats)) > 0) {
                  printWarning("Removing", nnof, "injections containing only artefacts:\n\t", paste(names(allSamples)[nofeats], 
                    collapse = "\n\t "))
                  allSamples <- allSamples[-nofeats]
                }
                
                DB <- DB[-irrel.idx]
                DB.orig <- DB.orig[-irrel.idx]
            }
        }
        
        printString("Matching with database of standards")
        allSam.matches <- matchSamples2DB(allSamples.msp, DB = DB, settings = metaSetting(settings, "match2DB"), quick = FALSE)
    } else {
        allSam.matches <- NULL
    }
    
    if (findUnknowns) {
        printString("Matching unknowns across samples")
        allSam.matches <- matchSamples2Samples(allSamples.msp.scaled, allSamples.msp, annotations = allSam.matches$annotations, 
            settings = metaSetting(settings, "betweenSamples"))
    }
    
    # Add this if because when you obtain no result, there was an error during 'sweep' function for ann.df2
    # if(sum(sapply(allSam.matches$annotations,nrow)) > 0){
    
    printString("Formatting results")
    
    ## First obtain the pseudospectra
    PseudoSpectra <- constructExpPseudoSpectra(allMatches = allSam.matches, standardsDB = DB.orig)
    ## and replace the DB indices of the identified standards with the indices in the pseudospec DB - they are simply ordered. The
    ## alternatives should also be there!  Then export the quantitations. The first part is a data.frame describing the features
    features.df <- getFeatureInfo(stdDB = DB.orig, allMatches = allSam.matches, sampleList = allSamples.msp)
    
    ## Second data.frame contains the actual annotations, for the moment as relative intensities
    ann.df <- getAnnotationMat(exp.msp = allSamples.msp, pspectra = PseudoSpectra, allMatches = allSam.matches)
    
    ## To get to intensities comparable to the ones identified by xcms, use largest peak in PseudoSpectra as the common intensity
    ## measure
    ann.df2 <- sweep(ann.df, 1, sapply(PseudoSpectra, function(x) max(x$pspectrum[, 2])), FUN = "*")
    
    printString("Done!")
    
    if (returnXset) {
        list(PeakTable = cbind(data.frame(features.df), data.frame(round(ann.df2))), PseudoSpectra = PseudoSpectra, settings = settings, 
            xset = allSamples, annotation = allSam.matches$annotation, samples.msp = allSamples.msp, SessionInfo = sessionInfo())
    } else {
        list(PeakTable = cbind(data.frame(features.df), data.frame(round(ann.df2))), PseudoSpectra = PseudoSpectra, settings = settings, 
            SessionInfo = sessionInfo())
    }
    # }else{ peaktable <- data.frame(Name = 1, Class = 1, rt.sd = 1, rt = 1)[FALSE,] result <- matrix(0, 0,
    # length(allSamples.msp)) colnames(result) <- names(allSamples.msp) if (returnXset) { list(PeakTable =
    # cbind(data.frame(peaktable), data.frame(result)), PseudoSpectra = NULL, settings = settings, xset = allSamples, annotation
    # = allSam.matches$annotation, samples.msp = allSamples.msp, SessionInfo = sessionInfo()) } else { list(PeakTable =
    # cbind(data.frame(peaktable), data.frame(result)), PseudoSpectra = NULL, settings = settings, SessionInfo = sessionInfo()) }
    # }
}
