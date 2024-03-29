## Functions for manipulating, creating, reading and writing msp
## objects, i.e. lists of compounds containing both pseudospectra and
## meta-information.

## Creates an msp object from a list of two-column matrices, supplemented by
## meta-information such as Name, rt, etc. In case three-column
## matrices are given, this will be used for rt and will override any
## rt or rt.sd present in extra.info.

## For consistency: only two-column spectra will be written in the msp
## file, so any retention time information is summarized in the rt and
## rt.sd slots. (Aug 1, 2013)

## bug in construct.msp: current function works only for data.frames
## and not for nested lists, as is promised in the documentation.
construct.msp <- function(spectra, extra.info) {
  if (!(all(sapply(extra.info, length) == length(spectra))))
    stop("Unequal lengths in arguments")

  if (all(sapply(spectra, ncol) == 3)) {
    extra.info$rt <- round(sapply(spectra, function(x) mean(x[,3])), 3)
    extra.info$rt.sd <- round(sapply(spectra, function(x) sd(x[,3])), 4)
  }

  lapply(seq_len(length(spectra)),
         function(ii) 
           c(lapply(extra.info, "[", ii),
             list(pspectrum = spectra[[ii]][,1:2])))
}

## write.msp writes an existing and complete msp object to a
## file. Empty elements are not written. If Name is present, it will
## be used as the first field.
write.msp <- function(msp, file, newFile = TRUE) {
  if (newFile) {
    system(paste("rm -f", file))
    cat("", file = file)
  }
  
  for (i in seq(along = msp)) {
    fields <- names(msp[[i]])
    fields <- fields[fields != "pspectrum"]

    if ("Name" %in% fields)
      fields <- c("Name", fields[fields != "Name"])
    
    for (j in fields) 
      if (length(msp[[i]][[j]]) > 0)
        cat(j, ": ", msp[[i]][[j]], "\n",
            sep = "", file = file, append = TRUE) 

    pspec <- msp[[i]]$pspectrum
    nI <- nrow(pspec)
    cat("Num Peaks:", nI, file = file, append = TRUE)
    for (j in seq_len(nI)) {
      if ((j-1) %% 5 == 0) cat("\n", file = file, append = TRUE)
      cat(" ", pspec[j,1], " ", pspec[j,2], ";",
          sep = "", file = file, append = TRUE)
    }

    cat("\n\n", file = file, append = TRUE)
  }
}

## function to select pseudospectra from an msp object: filtering can
## be done on the basis of rt, mz, relative intensity and number of
## features. Defaults amount to no filtering.
filter.msp <- function(msp, rtrange = NULL, mzrange = NULL,
                       minintens = 0, minPeaks = 0) {
  if (!is.null(rtrange)) {
    rts <- sapply(msp, function(x) x$rt)
    msp <- msp[which(rts < max(rtrange) & rts > min(rtrange))]
  }
  if (length(msp) == 0) return(msp)

  if (!is.null(mzrange)) {
    msp <- lapply(msp,
                      function(x) {
                        mz.idx <- x$pspectrum[,1] > min(mzrange) &
                          x$pspectrum[,1] < max(mzrange)
                        x$pspectrum <- x$pspectrum[mz.idx,]

                        x
                      })
  }

  if (minintens > 0) {
    msp <- lapply(msp,
                      function(x) {
                        I.idx <- (x$pspectrum[,2] / max(x$pspectrum)) > minintens
                        x$pspectrum <- x$pspectrum[I.idx,]
                        
                        x
                      })
  }

  if (minPeaks > 0) {
    npks <- sapply(msp, function(x) nrow(x$pspectrum))
    msp <- msp[npks > minPeaks]
  }

  msp
}

## Function reads an msp file, and returns one list of
## compounds. Argument only.org allow to exclude all molecular
## formulas with non-organic elements. What exactly are organic
## elements can be defined with the optional argument org.set. A
## second optional argument indicates which fields should never be
## converted into numbers, even if possible.
read.msp <- function(file, only.org = FALSE,
                     org.set = c("C", "H", "D", "N", "O", "P", "S"),
                     noNumbers = NULL) {
  ## first define three auxiliary functions, not used outside this function

  ## return the value of the given field in string x. If the field is
  ## not present, either stop with an error or return NULL (e.g. if it
  ## is not a required field)
  get.text.value <- function(x, field, do.err = TRUE) {
    woppa <- strsplit(x, field)
    woppa.lengths <- sapply(woppa, length)
    if (all(woppa.lengths == 2)) {  ## remove leading white space
      sapply(woppa, function(y) gsub("^ +", "", y[2]))
    } else {
      if (do.err) {
        stop(paste("Invalid field", field, "in", x[woppa.lengths != 2]))
      } else {
        NULL
      }
    }
  }

  ## see if only organic elements are present. Strategy: replace these
  ## by "" and convert to numbers. If not possible, then return FALSE.
  ## Jan 26, 2012: add deuterium as an organic element - sometimes
  ## deuterated standards are used
  is.org <- function(strs, org.set)
      { 
        formulas <- get.text.value(strs, "Formula:")
        org.string <- paste("[", paste(org.set, collapse=""), "]",
                            collapse = "")
        suppressWarnings(which(!is.na(as.numeric(gsub(org.string, "",
                                                      formulas)))))
      }
  
  ## read ALL fields in a set of strings from an msp file and return as
  ## an msp object. All elements that can be converted into numbers are
  ## converted, unless explicitly stated in the exception list. Element
  ## "Num Peaks" is treated separately.
  read.compound <- function(strs, noNumbers) {
    if (is.null(noNumbers))
        noNumbers <-  c("Name", "CAS", "stdFile",
                        "date", "validated", "ChemspiderID",
                        "SMILES", "InChI", "Class", "comment",
                        "csLinks")
    
    fields.idx <- grep(":", strs)
    fields <- sapply(strsplit(strs[fields.idx], ":"), "[[", 1)
    
    pk.idx <- which(fields == "Num Peaks")
    if (length(pk.idx) == 0) ## should not happen
        stop("No spectrum found")
    
    ## read all normal fields
    cmpnd <- lapply(fields.idx[-pk.idx],
                    function(x)
                    get.text.value(strs[x],
                                   paste(fields[x], ":", sep = "")))
    names(cmpnd) <- fields[-pk.idx]
    ## convert numeric ones
    cnvrt.idx <- which(!(names(cmpnd) %in% noNumbers))
    cmpnd[cnvrt.idx] <- lapply(cmpnd[cnvrt.idx],
                               function(x) {
                                 if (is.na((y <- as.numeric(x)))) {
                                   x
                                 } else {
                                   y
                                 }
                               })
    
    ## now add pseudospectrum
    nlines <- length(strs)
    npeaks <- as.numeric(get.text.value(strs[pk.idx], "Num Peaks:"))
    peaks.idx <- (pk.idx+1):nlines
    pks <- gsub("^ +", "", unlist(strsplit(strs[peaks.idx], ";")))
    pks <- pks[pks != ""]
    if (length(pks) != npeaks)
        stop("Not the right number of peaks in compound", cmpnd$Name)
    pklst <- strsplit(pks, " ")
    pklst <- lapply(pklst, function(x) x[x != ""])
    cmz <- as.numeric(sapply(pklst, "[[", 1))
    cintens <- as.numeric(sapply(pklst, "[[", 2))
    ##  intens.OK <- cintens >= minintens
    
    finaltab <- matrix(c(cmz, cintens), ncol = 2)
    if (any(table(cmz) > 1)) {
      warning("Duplicate mass in compound ", cmpnd$Name,
              " (CAS ", cmpnd$CAS, ")... summing up intensities")
      finaltab <- aggregate(finaltab[,2],
                            by = list(finaltab[,1]),
                            FUN = sum)
    } 
    colnames(finaltab) <- c("mz", "intensity")
    
    c(cmpnd, list(pspectrum = finaltab))
  }
  

  ## Go!

  huhn <- scan(file, what = "", sep = "\n", quiet = TRUE)
  
  starts <- which(regexpr("Name: ", huhn) == 1)
  ends <- c(starts[-1] - 1, length(huhn))
  
  if (only.org) { ## filter out those compounds with non-organic elements
    formulas <- which(regexpr("Formula:", huhn) == 1)

    if (length(formulas) > 0) {
      orgs <- is.org(huhn[formulas], org.set)
      starts <- starts[orgs]
      ends <- ends[orgs]
    }
  }
  
  
  lapply(seq_len(length(starts)),
         function(i)
         read.compound(huhn[starts[i]:ends[i]], noNumbers = noNumbers))
}


## to.msp is a function to export features (mz / I combinations) to a
## text file so that it can be used for searching the extDB (in our case NIST)
## library. In general, several groups will be written. If the first
## argument is an annotation object, CAMERA groups will be used; if it
## is a peak table, the retention time will be used to create groups
## (retention times should be exactly equal!). Nothing will be written
## if file equals NULL, but an object will be returned, now also
## containing retention time information.
## Addition Jan 31: the xsAnnotate object can contain data from more
## than one file.
## Bug, corrected May 15: ndigit was only used for rounding when
## writing to a file...
## bug: minintens not used in the proper way (addressed March 25,
## 2013) - should be a fraction of the most intense signal
## March 25: removed default values for minfeat and minintens. These
## should always come from the pipeline settings
## July 23: added an additional settings argument which summarizes
## individual arguments (and has precedence over these, too).

to.msp <- function(object, file = NULL,
                   settings = NULL, minfeat, minintens,
                   intensity = c("maxo", "into"), secs2mins = TRUE) {
  if (!is.null(settings)) {
    intensity <- settings$intensityMeasure
    minfeat <- settings$minfeat
    minintens <- settings$minintens
    ndigit <- settings$ndigits
  } else {
    intensity <- match.arg(intensity)
  }
  
  if (is(object)[1] == "xsAnnotate") { ## CAMERA annotation object
    allpks <- object@groupInfo
    minI <- minintens * max(allpks[, intensity])
    tooSmall <- which(allpks[, intensity] < minI)
    
    pspectra <- lapply(object@pspectra,
                       function(x) x[!x %in% tooSmall])
  } else { ## a peak table
    minI <- minintens * max(object[, intensity])
    allpks <- object[object[, intensity] >= minI,]
    pspectra <- split(seq_len(nrow(allpks)), allpks[,"rt"])
  }
  
  ## remove all spectra with less than minfeat peaks
  npeaks <- sapply(pspectra, length)
  pspectra <- pspectra[npeaks >= minfeat]

  if (!is.null(file)) {
    if (length(pspectra) > 0) {
      for (i in seq_len(length(pspectra))) {
        ## maximum of 1000 features per file
        ofile <- paste(file, "_", ceiling(i / 1000), ".txt", sep = "")
        newfile <- (i %% 1000) == 1
        
        idx <- pspectra[[i]]
        pks <- allpks[idx,,drop = FALSE]
        pks <- pks[order(pks[,"mz"]),,drop = FALSE]
        pks[,intensity] <- 1000*pks[,intensity]/max(pks[,intensity])
        
        cat("Name: grp ", i, " (rt: ", mean(pks[,"rt"]), ")", sep = "",
            file = ofile, append = !newfile)
        cat("\nNum Peaks:", nrow(pks), file = ofile, append = TRUE)
        for (ii in seq_len(nrow(pks))) 
          cat("\n(",
              round(pks[ii, "mz"], ndigit), "\t",
              round(pks[ii, intensity], ndigit), ")",
              file = ofile, append = TRUE, sep = "")
        
        cat("\n\n", file = ofile, append = TRUE)
      }
    }
  }
  
  result <-
    removeDoubleMasses(lapply(pspectra,
                              function(x)
                              cbind(mz = round(allpks[x,"mz"], digits = ndigit),
                                    allpks[x, c(intensity, "rt")])))

  if (secs2mins) {
    invisible(lapply(result,
                     function(x) cbind(x[,c("mz", intensity)],
                                       rt = x[,"rt"] / 60)))
  } else {
    invisible(result)
  }
}

## SearchNIST is a function added to assist user accessing their own copy of NIST database
## starting from a MSP file created by to.msp function
## The fucntion will help creating the connection between R and NIST mass spectral library search tool on Windows OS
## Require NIST MS (Mass Spectral) Search Program Version 2.0 or higher AND Windows OS
## Please remove AUTOIMP.MSD file from C:\NISTMS\MSSEARCH before first launch
# GUITTON Yann - 2015 Laberca

SearchNIST<-function(mspfile=NULL, savepath=NULL){
  
  
  #create a local output directory
  if (is.null(savepath)){
    savepath<-choose.dir(default=getwd(), caption="Please, select your Saving directory")
  }
  
  setwd(savepath)
  st<-strsplit(date(), " ")[[1]]
  stBis<-strsplit(st[4], ":")[[1]]
  Hour<-paste(stBis[1], stBis[2], stBis[3], sep="-")
  Date<-paste(st[1], st[2], st[3], Hour, sep="_")
  Mypath<-paste("output_SearchNIST", "_", "result", Date, sep="")
  dir.create(Mypath)
  
  if (is.null(mspfile)==TRUE) {
    rm(mspfile)
  }
  
  #System Check 
  if ((Sys.info()["sysname"])!="Windows"){
    cat("Sorry, this function only works on Windows. Your OS is not Windows.\n")
    
  }
  else
  {
    #Search for MSSEARCH location should be c:/nist/mssearch
    if (file.exists(file.path("C:", "Windows","win.ini"))==TRUE){
      nistpath<-readLines(file.path("C:", "Windows","win.ini"))[pmatch("Path32", readLines(file.path("C:", "Windows","win.ini")))]
      if(is.na(nistpath)!=TRUE){
        nistpath<-file.path(unlist(strsplit(nistpath, split="="))[2])
        
      }
      if(is.na(nistpath)){
        nistpath<-readLines(file.path("C:", "Windows","win.ini"))[pmatch("Path16", readLines(file.path("C:", "Windows","win.ini")))]
        print("Your NIST MS program version is <2.0 or impossible to detect NIST")
        if(is.na(nistpath)){
          nistpath<-choose.dir(default=getwd(), caption="Please, select the MS SEARCH directory, should be c:/nist/mssearch/")
          print(nistpath)
        }
      }
      
      
    }
    if (file.exists(file.path("C:", "Windows","win.ini"))==FALSE){
      nistpath<-choose.dir(default=getwd(), caption="Please, select the MS SEARCH directory, should be c:/nist/mssearch/")
    }
    if(file.exists(file.path(nistpath,"AUTOIMP.MSD"))==TRUE){
      firstlocatorpath<-file.path(nistpath,"AUTOIMP.MSD")
      secondlocatorpath<-file.path(readLines(file.path(firstlocatorpath)))
    }
    if(file.exists(file.path(nistpath,"AUTOIMP.MSD"))==FALSE){
      #create AUTIMP.MSD file
      secondlocatorpath<-"FILESPEC.FIL"
      zz<-file(file.path(nistpath,"AUTOIMP.MSD"))
      cat(secondlocatorpath, file = zz, sep = "\n")
      close(zz)
      firstlocatorpath<-file.path(nistpath,"AUTOIMP.MSD")
    }
    if(file.exists(file.path(nistpath,"SRCREADY.TXT"))==TRUE){
      unlink(file.path(nistpath,"SRCREADY.TXT"))
    }
    
    #create FILSPEC.FIL
    if(exists("mspfile")==FALSE){
      mspfile<-choose.files(default=paste(getwd(),"/*.msp",sep=""),caption="Please, select the MSP file", multi=FALSE, filters=matrix(c("your msp file","*.msp"),ncol=2))
      zz<-file("FILESPEC.FIL", "w")
      cat(paste(file.path(mspfile),"Overwrite",sep=" "), file = zz, sep = "\n")
      cat(paste(23,62789), file = zz, sep = "\n")
      close(zz)
    }
    if(exists("mspfile")==TRUE){
      
      #zz<-file(paste(nistpath,"FILESPEC.FIL", sep=""), "w")
      zz<-file("FILESPEC.FIL", "w")
      cat(paste(file.path(mspfile),"Overwrite",sep=" "), file = zz, sep = "\n")
      cat(paste(23,62789), file = zz, sep = "\n")
      close(zz)
    }
    code<-paste(file.path(nistpath,"nistms$.exe"), " /Instrument /par=2", sep="")
    #system("c:/nist05/mssearch/nistms$.exe /Instrument /par=2")
    system(code)
    stopflag=FALSE
    print("Please wait...")
    Sys.sleep(3)
    
    while(stopflag==FALSE){
      
      if(file.exists(file.path(nistpath,"SRCREADY.TXT"))==TRUE){
        stopflag=TRUE
        print("Done")
        
      }
      
      
      
    }
    file.copy(file.path(nistpath,"SRCRESLT.TXT"),file.path(Mypath,"ResultsFromNIST.txt"))
    print(paste("A file called ResultsFromNIST.txt has been generated in ", Mypath, sep=" "))
    #a parser for that file is needed
    #clean unused files from directories needed
  }#end else system check
  
}#end function
