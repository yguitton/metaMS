getSamples <- function (X, 
                        selection="names"){
# get selected sample names
#  names = getSamples(path,"names") -- all sample names, same as: getSamples(path,"names")
#  id = getSamples(path,"QC") -- id vector of QC-samples
#  id = getSamples(path,"STD") 
#  id = getSamples(path,"blank") 
#
#  snames = getSamples(path) -- to use with path, or
#  snames = getSamples(xset) -- to use with xset object, or
#  snames = getSamples(peakTable)-- to use with an xcms data-frame
#  
#

# test:
#   library(metaMS)
#   source('../Func/getSamples.R')
#   load('xcmsPeakTable.RData')
#   


##################################################################

# library(xcms)


# PATH
   if (is.character(X)) {
    path = X
    # if xset object: xset=X

    # get all filenames of directory 
    
    samplenames <- list.files(path = path, pattern = "*.CDF", ignore.case = TRUE, full.names = FALSE)
    # remove ".CDF" or ".cdf"
    for (i in 1:length(samplenames)) samplenames[i]=gsub(".CDF","",samplenames[i],ignore.case = TRUE)
   }

# XSET object
   if (isS4(X)) {
    xset = X
    if (class(xset) == "xsAnnotate") xset <- xset@xcmsSet # in case of CAMERA object
    samplenames = sampnames(xset)
   }

# peak table
   if (is.data.frame(X)) {
    peakTable = X
    samplenames = colnames(peakTable)
   }

# peak table
   if (is.matrix(X)) {
    peakTable = X
    samplenames = colnames(peakTable)
   }

#################################################################
# select 

   if (selection=="names")  Z = samplenames
   if (selection=="QC")     Z = grep("QC",    samplenames) # return IDs 
   if (selection=="STD")    Z = grep("STD",   samplenames) # return IDs
   if (selection=="blank")  Z = grep("blank", samplenames) # return IDs




return(Z)
}
