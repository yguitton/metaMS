processStandardsLC <- function(reftable, settings, polarity) {
  
  # process raw LC-MS files using the 'xcms' and 'Camera' packages. 
  
  printString("Performing peak picking", RWui)
  
  
  
  ## We want to analyze the separate files ...
  xsets <- lapply(seq(1:nrow(reftable)), function(x){
    xs <- peakDetection(reftable$file.name[x], settings$PeakPicking)
    rtmin <- max(0,reftable$rt[x]*60-150)
    rtmax <- min(reftable$rt[x]*60+150, max(xs@peaks[,"rt"]))
    idx <- which((xs@peaks[,"rt"] > rtmin) & (xs@peaks[,"rt"] < rtmax))
    xs@peaks <- xs@peaks[idx,,drop = FALSE]
    xs  
  })
  
  
  
  ## Camera Annotation
  xsetsA  <- mclapply(xsets, function(x) runCAMERA(x,settings$CAMERA,polarity))
   
  
  ## List of CAMERA peak lists
  pktables <- lapply(xsetsA,function(x) getPeakTable(x, intval = "maxo"))
  
 
}
