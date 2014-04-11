metaMSsettings <-
    setClass("metaMSsettings", 
             slots = c(protocolName = "character",
                 chrom = "character",
                 PeakPicking = "list",
                 Alignment = "list", 
                 CAMERA = "list",
                 ## match2DB is used both for GC and for LC -
                 ## which elements must be present depends on the
                 ## chromatography
                 match2DB = representation(
                     rtdiff = "numeric",                      # both
                     minfeat = "numeric",                     # both
                     rtval = "numeric",                       # LC
                     mzdiff = "numeric",                      # LC
                     ppm = "numeric",                         # LC
                     simthresh = "numeric",                   # GC
                     timeComparison = "character",            # GC
                     RIdiff = "numeric"                       # GC
                     ),
                 ## DBconstruction is used both for GC and for LC -
                 ## which elements must be present depends on the
                 ## chromatography
                 DBconstruction = representation(
                     minfeat = "numeric",                     # both
                     rttol = "numeric",                       # LC
                     mztol = "numeric",                       # LC
                     minintens = "numeric",                   # LC
                     intensityMeasure = "character",          # GC
                     DBthreshold = "numeric"),                # GC
                 matchIrrelevants = representation(
                     irrelevantClasses = "character",
                     simthresh = "numeric",
                     timeComparison = "character",
                     RIdiff = "numeric",    
                     rtdiff = "numeric"),
                 betweenSamples = representation(
                     min.class.fraction = "numeric",
                     min.class.size = "numeric",
                     timeComparison = "character",
                     rtdiff = "numeric",
                     RIdiff = "numeric",    
                     simthresh = "numeric")
                 ))

setMethod("show", "metaMSsettings",
          function(object) {
            cat("Object of class '", class(object), "'\n", sep = "")
            cat("Instrument:", object@protocolName, "\n")
            cat("Chromatography:", object@chrom, "\n")
          })

setGeneric("metaSetting",
           function(object, field){standardGeneric("metaSetting")})

setGeneric("metaSetting<-",
           function(object, field, value){standardGeneric("metaSetting<-")})

setMethod("metaSetting", "metaMSsettings",
          function(object, field) {
            if (field %in% slotNames(object)) {
              return(slot(object, field))
            } else {
              chromatography <- object@chrom
              if (chromatography == "GC") {
                multiFields <- c('match2DB', 'DBconstruction',
                                 'matchIrrelevants', 'betweenSamples')
              } else {
                multiFields <- c('match2DB', 'DBconstruction')
              }
              if (field %in% multiFields) {
                allnames <- slotNames(object)
                allnames <- allnames[grep(field, allnames)]
                lnames.lst <- strsplit(allnames, field)
                lnames <- sapply(lnames.lst, function(x) substr(x[2],
                                                                2, 100))
                result <- lapply(allnames, function(x) slot(object, x))
                names(result) <- lnames
                
                result[sapply(result, length) > 0]
              } else {
                stop(paste("For ", chromatography,
                           ", 'field' should be a valid slot name, or one of ",
                           paste(multiFields, collapse = ", "),
                           sep = "")) 
              }
            }})

setReplaceMethod(
    f = "metaSetting",
    signature = "metaMSsettings",
    definition = function(object, field , value) {
      if (field %in% slotNames(object)) {
        slot(object, field) <- value
        return(object)
      } else {
        chromatography <- object@chrom
        if (chromatography == "GC") {
          multiFields <- c('match2DB', 'DBconstruction',
                           'matchIrrelevants', 'betweenSamples')
        } else {
          multiFields <- c('match2DB', 'DBconstruction')
        }
        if (field %in% multiFields) {
          for (i in seq(along = value)) {
            slname <- names(value)[i]
            slot(object, paste(field, slname, sep = "."),
                 check = TRUE) <- value[[i]]
          }
          
          return(object)
        } else {
          stop(paste("For ", chromatography,
                     ", 'field' should be a valid slot name, or one of ",
                     paste(multiFields, collapse = ", "),
                     sep = "")) 
        }
      }})

