## Pietro 9/7

getInstrument <- function (X, 
                           filename="settingsXCMS",
                           processInfo=TRUE){

# --------------------------------------------------------------
# get instrument name from path or xset object
# e.g., "Synapt-QTOF-NP", "Synapt-QTOF-RP", "GC-Triple-Quad"
# 
# read all instrument-codes from file settingsXCMS
# and tries to indentify the right instrument by searching 
# for the code in the CDF file & directory name, and xset object
#
# 
#  intrumentCode <- getInstrument(path)
#  intrumentCode <- getInstrument(xset)
#
#
# 
#

#
# test: 
#   source('../Func/getInstrument.R')
#   CDFpath = '/mnt/data/DemoGCtripleQ/'
#   CDFpath = '/mnt/data/DemoCDFdata/'
#   intrumentCode <- getInstrument(CDFpath)


# get filePath  (directory + first filename), e.g. "/mnt/data/grape//x034_QC_RP_pos02.CDF"

  # PATH
     if (is.character(X)) {
          CDFpath = X
          files <- list.files(path = CDFpath, pattern = "*.CDF", ignore.case = TRUE, full.names = TRUE)
          filePath = files[1]  # directory + first filename
     }

  # XSET object
     if (isS4(X)) {
      xset = X
      if (class(xset) == "xcmsSet")    filePath=xset@filepaths[1]
      if (class(xset) == "xsAnnotate") filePath=xset@xcmsSet@filepaths[1]
     }

# read defined instrument codes from settingXCMS file
   dataDir = system.file("data", package = "metaMS") # <<<
   settingPath = paste(c(dataDir, .Platform$file.sep, filename),  collapse='')
   s = read.delim(settingPath, row.names=1, as.is=TRUE) 
   fullInstrumentNames = rownames(s)
   shortInstrumentNames = s[,1]
   

# get instrument
   intrumentCode='NaN'; wrn=TRUE # default
  for (i in 1:length(shortInstrumentNames)){
    if (grepl(shortInstrumentNames[i], filePath,ignore.case = TRUE)) intrumentCode=fullInstrumentNames[i]; wrn=FALSE 
    if (grepl( fullInstrumentNames[i], filePath,ignore.case = TRUE)) intrumentCode=fullInstrumentNames[i]; wrn=FALSE 
  }
  # if (grepl("GCtripleQ",      filePath,ignore.case = TRUE)) intrumentCode="GC-Triple-Quad"; wrn=FALSE 
  # if (grepl("tripleQ",        filePath,ignore.case = TRUE)) intrumentCode="GC-Triple-Quad"; wrn=FALSE 
  # if (grepl("_QQQ",           filePath,ignore.case = TRUE)) intrumentCode="GC-Triple-Quad"; wrn=FALSE 
  # if (grepl("GC-Triple-Quad", filePath,ignore.case = TRUE)) intrumentCode="GC-Triple-Quad"; wrn=FALSE 
  # if (grepl("_NP",            filePath,ignore.case = TRUE)) intrumentCode="Synapt-QTOF-NP"; wrn=FALSE  
  # if (grepl("_RP",            filePath,ignore.case = TRUE)) intrumentCode="Synapt-QTOF-RP"; wrn=FALSE 
  # if (grepl("Synapt-QTOF-RP", filePath,ignore.case = TRUE)) intrumentCode="Synapt-QTOF-RP"; wrn=FALSE 


# error message
   if (intrumentCode == 'NaN') {  
      txt="\n\nError: To detect the instrument, please include one of the\n       following instrument codes in file- or directory-name\n"
      #    ('..._GCtripleQ.cdf', '..._NP.cdf' or '..._RP.cdf')"
      # \n\n         for more information see\n         https://sites.google.com/a/fmach.it/xcms/cdf-filenames"
      write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
      txt = shortInstrumentNames
      write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
      Sys.sleep(12) # give time to show it in the refreshing of process_info.txt
      stop('Invalide MS instrument code')
  } else {
      txt=as.character(paste(c("\n-- Detected instrument --\n      ",intrumentCode),collapse=''))
      if (processInfo) { write(txt, file = "process_info.txt", append = TRUE) } 
      write(txt, file = "")
  }


return(intrumentCode)
}


