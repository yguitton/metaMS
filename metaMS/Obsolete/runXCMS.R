## Pietro 13/07
## Changes add choice GC or LC in view of a configuration file in form of a RData object



runXCMS <- function (path='.', 
                     settings = getXCMSsettings(),
                     runCAMERA = TRUE,
                     runAnnotation = FALSE){
  
  # run XCMS pipeline for MS data
  # main pipeline function
  
  # test:
  #   library(metaMS)
  #   source('../Func/runXCMS.R')
  #
  #
  #    path = '/mnt/data/DemoGCtripleQ/'
  #    runXCMS(path,runAnnotation=TRUE)
  #
  #    path <- '/mnt/data/testing/demo01/'# 3 samples
  #    path <- '/mnt/data/DemoCDFdata/'  # 8 samples, inluding QC STD blank
  #    runLC(path,runAnnotation=TRUE)
  
  
  #################################
   # split into LC or GC run
  
  # LC-MS
  if (settings$chrom == 'LC'){
    output <- runLC(path,settings,runCAMERA,runAnnotation)
  }
  # GC-MS
  if (settings$chrom == 'GC'){
    runGC(path,settings) # (always compound annotation in GC)
  }
  return(output)
}
