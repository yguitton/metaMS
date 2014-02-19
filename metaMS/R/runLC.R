runLC <- function(files,
                  xset,
                  settings,
                  rtrange = NULL,
                  mzrange = NULL,
                  DB = NULL,
                  polarity = "positive",                  
                  errf = NULL,
                  runCAMERA = TRUE,
                  returnXset = FALSE,
                  intensity = "into",
                  nSlaves = 0)
{
  if (!missing(files)) {
    nexp <- length(files)
  } else {
    if (missing(xset))
        stop("Either 'files' or 'xset' should be given")

    nexp <- length(sampnames(xset))
  }
    
  printString(paste("Experiment of", nexp, "samples"))
  printString(paste("Instrument:", metaSetting(settings, "instName"),
                    " - polarity:", polarity))
  if (length(rtrange) == 2)
      printString(paste("Retention time range:", rtrange[1], "to",
                        rtrange[2], "minutes"))
  
  if (!is.null(DB)){
    printString("Database of", length(unique(DB$ChemSpiderID)), "compounds")
  }

  if (!missing(files)) {
    printString("Performing peak picking")
    xset  <-  peakDetection(files,
                            metaSetting(settings, "PeakPicking"),
                            rtrange = rtrange, mzrange = mzrange,
                            nSlaves = nSlaves)
  } else {
    printString("Using xcmsSet object, no peak picking performed.")
  }
  
  printString("Grouping and retention time alignment")
  xset <- alignmentLC(xset, metaSetting(settings, "Alignment"))
 
  ## ------ CAMERA ------------------------------------------------
  if (runCAMERA){
    printString("Performing CAMERA annotation")
    xset <- runCAMERA(xset, chrom = "LC",
                      metaSetting(settings, "CAMERA"), polarity)
  }
  
  if (!is.null(DB)){
    printString("Performing annotation")
   
   
    annotation <- getAnnotationLC(xset,
                                  metaSetting(settings, "match2DB"),
                                  DB, errf)

    ## To run the annotation we need: table mz,rt,I, errf and DB.
    ## With the additional functions AnnotateFeaturesAnnotateTable 
    ## on the configurations I need the rt tol, the rt4validation,
    ## mass tolerance if no surface. 
  } else {
    
    annotation  <- list("raw" = data.frame(),
                        "for_table" = data.frame())
  }
  
 
  ## ------ Create the Peak Table  ------------------------------------
  peakTable <- getPeakTable(xset, intval = intensity) 
  
  ## ----- If the annotation is required add the output ---------------
  
  if (!is.null(DB)){ 
    peakTable <- cbind(annotation$for_table, peakTable)
  }
  
    

  # ----------------- Prepare the outputs ------------------------
  printString("Done !")
  if (returnXset) {
    list("PeakTable" = peakTable,
         "xset" = xset,
         "Annotation" = annotation$raw,
         "Settings" = settings)
  } else {
    list("PeakTable" = peakTable,
         "Annotation" = annotation$raw,
         "Settings" = settings)
  }

}
