alignmentLC <- function (xset, settings){
  ## set the extra and missing on the bases of the 
  ## number of samples
  missing <- ceiling((settings$missingratio) * length(xset@filepaths))
  extra <- ceiling((settings$extraratio) * length(xset@filepaths))
  
  if (length(sampnames(xset)) == 1){
    return(xset)
  } else{
    myminsamp  <- min(c(settings$min.class.size, 
                        ceiling(length(sampnames(xset))*settings$min.class.fraction)))
    printString("minsamp:",myminsamp)
    
    ## --------  xcms grouping ----------------------------
    xset <- do.call(group,
                    c(list(object = xset, 
                           bw = settings$bws[1],
                           minsamp = myminsamp,
                           minfrac = 0),
                      settings["mzwid"]))
    
    ## ------- Retention time correction  ------------------
    printString("missing:", missing)
    printString("extra:", extra)
    xset <- retcor(xset,  
                   method  = settings$retcormethod,
                   family = settings$retcorfamily,
                   missing = missing,
                   extra = extra) 
    
    
    
    xset <- do.call(group,
                    c(list(object = xset, 
                           bw = settings$bws[2],
                           minsamp = myminsamp,
                           minfrac = 0),
                      settings["mzwid"]))
    
    
    if (settings$fillPeaks) {
      printString("Filling missing peaks")
      fillPeaks(xset)
    } else {
      xset
    }
  }
  
}

