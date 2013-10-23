getPolarity <- function (xset,XCMSsettings=data.frame(CAMpol="automatic")){
#
# pol <- getPolarity(xset)
# pol <- getPolarity(xset,XCMSsettings)
#
# get polarity from xset internal path-name
#
# search for "pos" and "neg" somewhere in xset-path (directory and filename)
# If the exact last 3 positions of a filename are "pos" or "neg",
# this will dominate even if the directory is named opposite. 
#
# XCMSsettings dominates filename 
# 
#
# source("getPolarity.R")
# 
#


pol=as.character(XCMSsettings$CAMpol) # predefined settings

# set polarity automatically
# using pathname from xset (only first sample filename)
if (pol == "automatic") {
  if (class(xset) == "xcmsSet")    path=xset@filepaths[1]
  if (class(xset) == "xsAnnotate") path=xset@xcmsSet@filepaths[1]
  pol="positive"; wrn=TRUE # default
  if (grepl("neg",path,ignore.case = TRUE)) pol="negative"; wrn=FALSE # somewhere neg in directory+filename
  if (grepl("pos",path,ignore.case = TRUE)) pol="positive"; wrn=FALSE # somewhere pos in directory+filename
   file3end = substr(path,nchar(path)-6,nchar(path)-4) # exact last 3 positions 
   if (grepl("neg",file3end,ignore.case = TRUE)) pol="negative" # "...neg.cdf"
   if (grepl("pos",file3end,ignore.case = TRUE)) pol="positive" # "...pos.cdf"
}


  if (wrn) { 
    txt="WARNING: please specify polarity in the end of the filename ('..._pos.cdf' or '..._neg.cdf')"
    write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
  }
  txt=as.character(paste(c("     Set polarity to: ",pol),collapse=''))
  write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")




return(pol)
}

