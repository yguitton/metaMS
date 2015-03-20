alignmentLC <- function (xset, settings){
## if only one sample do not retcor ....
  if (length(sampnames(xset)) == 1){
    return(xset)
  }
  ## calculate min samp / min frac 
  myminsamp  <- min(c(settings$min.class.size, 
    ceiling(length(sampnames(xset))*settings$min.class.fraction)))
  printString("minsamp:",myminsamp)
  
  ## The workflow is 1) grouping, 2) retcor, 3) grouping, optionally fillpeaks 
  ## for density based two grouping runs are required (before and after retcor)
  ## for obiwarp only one is necessary (the first one is useless)
      
  ## identify the two bandwidths for grouping ------------------------------- >
  if (length(settings$bws) == 2) {
    bw1 = settings$bws[1]
    bw2 = settings$bws[2]
  } else {
    bw1 = settings$bws[1]
    bw2 = settings$bws[1]
  }
  ## perform the first grouping --------------------------------------------- >
  xset <- do.call(group,
                  c(list(object = xset, bw = bw1,
                         minsamp = myminsamp, minfrac = 0),
                    settings["mzwid"]))
  
  ## perform the retention time correction ---------------------------------- >
  ## create a list of parameters to use do.call 
  if ("obiwarp" %in% settings$Retcor) {   
    ## if obiwarp use only the provided parameters
    printString("Obiwarp retcor")
    retcorlist <- c(list(object = xset), settings$Retcor)
  } else {
    ## use the "standard" density based idea
    printString("Density-based retcor")
    ## calculate missing and extra ....
    missing <- ceiling((settings$missingratio) * length(xset@filepaths))
    extra <- ceiling((settings$extraratio) * length(xset@filepaths))
    printString("missing:", missing)
    printString("extra:", extra)
    retcorlist <- c(list(object = xset, missing = missing, extra = extra),
                    settings$Retcor)
  }
  # do the real retention time correction
  xset <- do.call(retcor, retcorlist)

  # perform the second round of grouping ------------------------------------- >
  xset <- do.call(group,c(list(object = xset,  bw = bw2,
                               minsamp = myminsamp, minfrac = 0),
                          settings["mzwid"]))

  # optional fill missing peaks ---------------------------------------------- >
  if (settings$fillPeaks) {
    printString("Filling missing peaks")
    fillPeaks(xset)
    }
  return(xset)
}


