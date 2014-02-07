metaMSsettings <- setClass("metaMSsettings", 
                           slots = c(instName = "character",
                               chrom = "character",
                               PeakPicking = "list",
                               Alignment = "list", 
                               CAMERA = "list",
                               match2DB = "list",
                               DBconstruction = "list"))

setMethod("show", "metaMSsettings",
          function(object) {
            cat("Object of class '", class(object), "'\n", sep = "")
            cat(" Instrument:", object@instName, "\n")
          })
