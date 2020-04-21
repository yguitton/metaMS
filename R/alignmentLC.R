alignmentLC <- function(xset, settings) {
    ## if only one sample do not retcor ....
    if (length(sampnames(xset)) == 1) {
        return(xset)
    }
    ## The workflow is different depending on the type of retcor approach: for types linear and loess one needs the missing and
    ## extra and two runs of grouping are performed (before and after retcor). for obiwarp only one grouping is needed after the
    ## retcor
    
    ## calculate minsamp. The parameter used for grouping ...
    myminsamp <- min(c(settings$min.class.size, ceiling(length(sampnames(xset)) * settings$min.class.fraction)))
    printString("minsamp:", myminsamp)
    ## Test the type of retcor ... -------------------------------------------- >
    if (settings$Retcor$method %in% c("loess", "linear")) {
        printString("Density-based retcor")
        ## Perform the first Grouping
        xset <- do.call(group, c(list(object = xset, bw = settings$bws[1], minsamp = myminsamp, minfrac = 0), settings["mzwid"]))
        ## calculate missing and extra ....
        missing <- ceiling((settings$missingratio) * length(xset@filepaths))
        extra <- ceiling((settings$extraratio) * length(xset@filepaths))
        printString("missing:", missing)
        printString("extra:", extra)
        ## Perform the retention time correction
        retcorlist <- c(list(object = xset, missing = missing, extra = extra), settings$Retcor)
        xset <- do.call(retcor, retcorlist)
        ## Perform the second run of grouping
        xset <- do.call(group, c(list(object = xset, bw = settings$bws[2], minsamp = myminsamp, minfrac = 0), settings["mzwid"]))
        ## If Obiwarp ...... ---------------------------------------------------- >
    } else if (settings$Retcor$method == "obiwarp") {
        printString("Obiwarp retcor")
        ## Perform the retention time correction
        retcorlist <- c(list(object = xset), settings$Retcor)
        xset <- do.call(retcor, retcorlist)
        ## Perform the second run of grouping
        xset <- do.call(group, c(list(object = xset, bw = settings$bws[1], minsamp = myminsamp, minfrac = 0), settings["mzwid"]))
        ## If the method is not specified --------------------------------------- >
    } else {
        printString("Default Density-based retcor")
        myminsamp <- min(c(settings$min.class.size, ceiling(length(sampnames(xset)) * settings$min.class.fraction)))
        printString("minsamp:", myminsamp)
        ## First Grouping
        xset <- do.call(group, c(list(object = xset, bw = settings$bws[1], minsamp = myminsamp, minfrac = 0), settings["mzwid"]))
        ## calculate missing and extra ....
        missing <- ceiling((settings$missingratio) * length(xset@filepaths))
        extra <- ceiling((settings$extraratio) * length(xset@filepaths))
        printString("missing:", missing)
        printString("extra:", extra)
        retcorlist <- c(list(object = xset, missing = missing, extra = extra), settings$Retcor)
        ## Retcor
        xset <- do.call(retcor, retcorlist)
        ## Second Grouping
        xset <- do.call(group, c(list(object = xset, bw = settings$bws[2], minsamp = myminsamp, minfrac = 0), settings["mzwid"]))
    }
    ## optional fill missing peaks -------------------------------------------- >
    if (settings$fillPeaks) {
        printString("Filling missing peaks")
        xset <- fillPeaks(xset)
    }
    return(xset)
}
