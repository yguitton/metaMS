alignmentLC <- function (xset, settings){
  ## if only one sample do not retcor ....
  if (length(sampnames(xset)) == 1){
      return(xset)
    } else {
      ## calculate min frac 
      myminsamp  <- min(c(settings$min.class.size, 
        ceiling(length(sampnames(xset))*settings$min.class.fraction)))
      printString("minsamp:",myminsamp)

      ## check if obiwarp retcor is required
       if (settings$retcormethod == "obiwarp"){
       ## Do it with obiwarp ....
        printString("obiwarp retcor")
        xset <- retcor(xset,
          method  = settings$method,
          profStep = settings$profStep)
         ## Grouping 
        xset <- do.call(group,
          c(list(object = xset, 
            bw = settings$bw,
            minsamp = myminsamp,
            minfrac = 0),
          settings["mzwid"]))
      } else {
      ## use the default density based approach
      ## set the extra and missing on the bases of the 
      ## number of samples
        missing <- ceiling((settings$missingratio) * length(xset@filepaths))
        extra <- ceiling((settings$extraratio) * length(xset@filepaths))
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
        ## --------- Second Grouping  
        xset <- do.call(group,
          c(list(object = xset, 
            bw = settings$bws[2],
            minsamp = myminsamp,
            minfrac = 0),
          settings["mzwid"]))
   }
    if (settings$fillPeaks) {
      printString("Filling missing peaks")
      fillPeaks(xset)
    } else {
      xset
    }
  }  
}


