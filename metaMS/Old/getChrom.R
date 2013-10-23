getChrom <- function (settingID="default", filename="settingsXCMS"){
#
# get chromatography "LC" or "GC" 
# 
# chrom <- getChrom("GC-Triple-Quad")
#
#
#
# 
#

#
# test: 
#   source('getChrom.R')
#   getChrom('GC-Triple-Quad')



# read settingXCMS file
   dataDir = system.file("data", package = "metaMS") # <<<
   settingPath  = paste(c(dataDir, .Platform$file.sep, filename),  collapse='')
   s = read.delim(settingPath, row.names=1, as.is=TRUE)   # <<<
   if (is.na(match(settingID,rownames(s)))) { 
    textstr = paste( c('\'',settingID,'\' is not a valid instrument name!', 
		'\nValid setting names are:\n',paste(rownames(s),'\n')),collapse='')
    stop(textstr)} 

   chrom=s[settingID,"chrom"]


return(chrom)
}
