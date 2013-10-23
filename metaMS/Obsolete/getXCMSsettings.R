getXCMSsettings <- function(settingID = "QTOFRPpos"){
  load("XCMSsettings.RData")
  ## to be changed for packaging
  out <- XCMSsettings[[settingID]]
  
  if (length(out) == 0) { 
    stop(settingID," is not a valid instrument name!")
  } 
  out
}
  
  
# getXCMSsettings <- function (settingID="default", filename="settingsXCMS"){ # <<<
# # read settings from file "settingsXCMS.txt"
# #  
# # XCMSsettings <- getXCMSsettings(settingID="GC-Triple-Quad", filename="settingsXCMS")
# # XCMSsettings <- getXCMSsettings(settingID="GC-Triple-Quad")
# # XCMSsettings <- getXCMSsettings(settingID="Synapt-QTOF-NP")
# # XCMSsettings <- getXCMSsettings(settingID="Synapt-QTOF-RP")
# #
# # settings: default, test, qtof...
# #
# # test: 
# #   source('../Func/getXCMSsettings.R')
# 
# #############################################################################
# 
# # read settingXCMS file
# dataDir = system.file("data", package = "metaMS") # <<<
# settingPath  = paste(c(dataDir, .Platform$file.sep, filename),  collapse='')
# s = read.delim(settingPath, row.names=1, as.is=TRUE)   # <<<
# if (is.na(match(settingID,rownames(s)))) { 
#   textstr = paste( c('\'',settingID,'\' is not a valid instrument name!', 
# 		'\nValid setting names are:\n',paste(rownames(s),'\n')),collapse='')
#   stop(textstr)} 
# 
# 
# XCMSsettings=s[settingID,]
# 
# XCMSsettings$tomcat = FALSE # running xcms-package directly on command line, without tomcat-server
#                             # i.e. no output in file "process_info.txt"
# 
# txt = paste(c('\nTo adapt the default XCMS settings, edit or replace file:\n ', settingPath,'\n'),  collapse='')
# write(txt, file = "")
# 
# 
# return(XCMSsettings)
# 
# }
