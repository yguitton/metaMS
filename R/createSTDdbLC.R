## Main function to prepare the LC db

createSTDdbLC <- function(stdInfo, settings, polarity, Ithr = 10, nSlaves = 0) {
    ## Read the reference table produced by the experimentalists
    
    printString("Processing the manually curated reference table")
    printString("Polarity:", polarity)
    
    
    ## Perform the peakpicking and the CAMERA analysis
    stdxsets <- processStandards(stdInfo, settings, polarity, nSlaves = nSlaves)
    
    ## match with the reference table
    db <- generateStdDBLC(stdxsets, metaSetting(settings, "DBconstruction"), Ithr)
    
    ## prepare an output object with some infos
    printString("Done !")
    list(Reftable = stdInfo, Info = list(Modified = date(), settings = settings), DB = db)
}
