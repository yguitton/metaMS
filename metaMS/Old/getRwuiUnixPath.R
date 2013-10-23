getRwuiUnixPath <- function (path,processInfo=TRUE){
# Converts a Windows path into a system specific Unix path. 
# to use in the xcms pipeline
#
# path <- getRwuiUnixPath(path)
#

# test:
#   library(metaMS)
#   source('../Func/getRwuiUnixPath.R')
#
#


# path='EMC-SNAS:T6.0.40.4 (Iasma103)\experiments\trials\Battilana_Yuri_chardonnay_2011.PRO\CDF'
#  have to be converted by hand into linux path "\" -> "/" (R problem)
# path='EMC-SNAS:T6.0.40.4 (Iasma103)/experiments/trials/Battilana_Yuri_chardonnay_2011.PRO/CDF'
# path='/mnt/waters02/experiment/DemoCDFdata/'
# path='/mnt/waters/play_ground/DemoCDFdata/'
# path='/mnt/data/DemoCDFdata/'



# write process.info 
   txt=as.character(paste(c("\ncdf-directory: ",path),collapse=''))
   if (processInfo) {write(txt, file = "process_info.txt", append = TRUE)}; write(txt, file = "")


# convert windows into unix path   
     s='/experiments/' # search phrase

     if (grepl(s,path,ignore.case = TRUE)) { 
      s2=strsplit(path,s,fixed = TRUE); 
      # if (s2[[1]][1] != "/mnt/watersnew") 
       # path <- paste(c('/mnt/watersnew/experiments/',s2[[1]][2]), collapse='');
      if (s2[[1]][1] != "/remote/watersnew") 
       path <- paste(c('/remote/watersnew/experiments/',s2[[1]][2]), collapse='');
       txt=as.character(paste(c("convert path into: ",path),collapse=''))
       if (processInfo) {write(txt, file = "process_info.txt", append = TRUE)}; write(txt, file = "")
     }
  
return(path)
}



