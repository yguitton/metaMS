runLC <- function (files, 
                   settings,
                   rtrange = NULL,
                   mzrange = NULL,
                   DB = NULL,
                   polarity = "positive",                  
                   errf = NULL,
                   runCAMERA = TRUE,
                   returnXset = FALSE,
                   intensity = "into")
{
  printString(paste("Experiment of", length(files), "samples"))
  printString(paste("Instrument:", settings$instName,
                    " - polarity:", polarity))
  if (length(rtrange) == 2)
      printString(paste("Retention time range:", rtrange[1], "to",
                        rtrange[2], "minutes"))
  
  if (!is.null(DB)){
    printString("Database of", length(unique(DB$ChemSpiderID)), "compounds")
  }
  
  printString("Performing peak picking")
  xset  <-  peakDetection(files, settings$PeakPicking, rtrange = rtrange, mzrange = mzrange)   
  
  printString("Grouping and retention time alignment")
  xset <- alignmentLC(xset, settings$Alignment) 
 
  ## ------ CAMERA ------------------------------------------------
  if (runCAMERA){
    printString("Performing CAMERA annotation")
    xset <- runCAMERA(xset, chrom = "LC", settings$CAMERA, polarity)
  }
  
  if (!is.null(DB)){
    printString("Performing annotation")
   
   
    annotation <- getAnnotationLC(xset, settings$match2DB, DB, errf)

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
