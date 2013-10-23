runGC <- function(files,
                  settings,
                  rtrange = NULL,
                  DB = NULL,
                  removeArtefacts = TRUE,
                  findUnknowns = length(files) > 1,
                  returnXset = FALSE)
{
  printString(paste("Experiment of", length(files), "samples"))
  printString(paste("Instrument:", settings$instName))
  if (length(rtrange) == 2)
      printString(paste("Retention time range:", rtrange[1], "to",
                        rtrange[2], "minutes"))
  
  if (is.null(DB) & !findUnknowns)
      stop("Nothing to do. Provide a DB or set findUnknowns to TRUE...")

  if (!is.null(DB)) {
    DB.orig <- DB
    DB <- treat.DB(DB.orig)

    printString(paste("Annotation using database of", length(DB), "spectra"))
  } else {
    printString("No annotation performed")
    DB.orig <- NULL
  }

  ## Peak picking and CAMERA
  printString("Performing peak picking")
  xset.l <- peakDetection(files, settings = settings$PeakPicking,
                          rtrange = rtrange, convert2list = TRUE)
  allSamples <- lapply(xset.l, runCAMERA,
                         chrom = settings$chrom,
                         settings = settings$CAMERA)

  
  ## convert into msp format (a nested list)
  allSamples.msp <- lapply(allSamples,
                           to.msp,
                           file = NULL,
                           settings = settings$DBconstruction)
  names(allSamples.msp) <- sapply(allSamples,
                                  function(x) sampnames(x@xcmsSet))

  ## check: files without any features - should not happen very often...
  nofeats <- which(sapply(allSamples.msp, length) == 0)
  if ((nnof <- length(nofeats)) > 0) {
    printWarning("Removing", nnof, "injections without any features:\n\t",
                 paste(names(allSamples)[nofeats], collapse = "\n\t "))
    allSamples.msp <- allSamples.msp[-nofeats]
  }
  ## now scale the rest...
  allSamples.msp.scaled <- lapply(allSamples.msp, treat.DB,
                                     isMSP = FALSE)

  ## remove Artefacts
  if (!is.null(DB)) {
    if (removeArtefacts) {
      printString(paste("Removing artefacts (",
                        paste(settings$matchIrrelevants$irrelevantClasses,
                              collapse = ", "), ")", sep = ""))
      irrel.idx <- which(sapply(DB, function(x) x$Class) %in%
                         settings$matchIrrelevants$irrelevantClasses)
      if (length(irrel.idx) > 0) {
        subDB <- DB[irrel.idx]
        junkPatterns <-
            lapply(matchSamples2DB(allSamples.msp.scaled,
                                   subDB,
                                   settings$matchIrrelevants,
                                   quick = TRUE)$annotations,
                   function(x) x[,"pattern"])
        
        allSamples.msp <- mapply(function(x, y)
                                 if (length(y) > 0) {
                                   x[-y]
                                 } else {
                                   x
                                 },
                                 allSamples.msp, junkPatterns)
        allSamples.msp.scaled <- mapply(function(x, y)
                                        if (length(y) > 0) {
                                          x[-y]
                                        } else {
                                          x
                                        },
                                        allSamples.msp.scaled, junkPatterns)
        
        ## check: files without any features
        nofeats <- which(sapply(allSamples.msp, length) == 0)
        if ((nnof <- length(nofeats)) > 0) {
          printWarning("Removing", nnof,
                       "injections containing only artefacts:\n\t",
                       paste(names(allSamples)[nofeats], collapse = "\n\t "))
          allSamples <- allSamples[-nofeats]
        }
        
        DB <- DB[-irrel.idx]
        DB.orig <- DB.orig[-irrel.idx]
      }
    }
    
    printString("Matching with database of standards")
    allSam.matches <-
        matchSamples2DB(allSamples.msp,
                        DB = DB,
                        settings = settings$match2DB,
                        quick = FALSE)
  } else {
    allSam.matches <- NULL
  }

  if (findUnknowns) {
    printString("Matching unknowns across samples")
    allSam.matches <-
        matchSamples2Samples(allSamples.msp.scaled,
                             allSamples.msp,
                             annotations = allSam.matches$annotations,
                             settings = settings$betweenSamples)
  }
  
  printString("Formatting results")
  
  ## First obtain the pseudospectra 
  PseudoSpectra <-
      constructExpPseudoSpectra(allMatches = allSam.matches,
                                standardsDB = DB.orig)
  ## and replace the DB indices of the identified standards with the
  ## indices in the pseudospec DB - they are simply ordered. The
  ## alternatives should also be there!
  
  ## Then export the quantitations. The first part is a data.frame
  ## describing the features
  features.df <- getFeatureInfo(stdDB = DB.orig,
                                allMatches = allSam.matches,
                                sampleList = allSamples.msp)
  
  ## Second data.frame contains the actual annotations, for the moment
  ## as relative intensities
  ann.df <- getAnnotationMat(exp.msp = allSamples.msp,
                             pspectra = PseudoSpectra,
                             allMatches = allSam.matches)

  ## To get to intensities comparable to the ones identified by xcms,
  ## use largest peak in PseudoSpectra as the common intensity measure
  ann.df2 <-
      sweep(ann.df, 1,
            sapply(PseudoSpectra, function(x) max(x$pspectrum[,2])),
            FUN = "*")

  printString("Done!")

  if (returnXset) {
    list(PeakTable = cbind(data.frame(features.df),
             data.frame(round(ann.df2))),
         PseudoSpectra = PseudoSpectra,
         settings = settings,
         xset = xset.l,
         annotation = allSam.matches$annotation,
         samples.msp = allSamples.msp)
  } else {
    list(PeakTable = cbind(data.frame(features.df),
             data.frame(round(ann.df2))),
         PseudoSpectra = PseudoSpectra,
         settings = settings)
  }
}
