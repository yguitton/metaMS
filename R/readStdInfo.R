readStdInfo <- function(stdInfo, InputDir, sep = "", dec = ".", ...) {
    ## ################################################################## Read stdInfo file
    ## ##################################################################
    info <- read.table(stdInfo, header = TRUE, as.is = TRUE, sep = sep, dec = dec, ...)
    info[, "RTman"] <- as.numeric(info[, "RTman"])
    info[, "monoMW"] <- as.numeric(info[, "monoMW"])
    
    ## remove columns that only contain NA values
    NAcols <- which(apply(info, 2, function(x) all(is.na(x))))
    if (length(NAcols) > 0) 
        info <- info[, -NAcols]
    
    ## ################################################################## compare and check input files. Case sensitive!
    ## ##################################################################
    extensions <- file_ext(info[, "stdFile"])
    
    fnames <- list_files_with_exts(InputDir, exts = extensions, full.names = FALSE)
    fpaths <- list_files_with_exts(InputDir, exts = extensions, full.names = TRUE)
    fn <- file_path_sans_ext(fnames)
    
    ## check 1: are all input files present that are needed? (if not: error)
    presenceCheck <- info[, "stdFile"] %in% fnames
    if (!all(presenceCheck)) 
        stop("Not all required data files present in ", InputDir, ":\n", paste(info[!presenceCheck, "stdFile"], collapse = "\n\t"))
    
    ## check 2: are all data files that are present actually used?  (if not: warning)
    presenceCheck <- fnames %in% info[, "stdFile"]
    if (!all(presenceCheck)) 
        printWarning(paste(paste("The following data files in", InputDir, "are not used:\n\t"), paste(fn[!presenceCheck], collapse = "\n\t"), 
            sep = ""))
    
    ## ################################################################## replace file names in table with complete filename
    ## ##################################################################
    info[, "stdFile"] <- fpaths[match(info[, "stdFile"], fn)]
    
    info
}
