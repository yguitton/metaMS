getPeakTable <- function (xs, intval = "into")
{
  ## get peak table from XCMS object  ------------------------------------------
  if (class(xs)[1] == "xcmsSet") { 
    if (length(xs@filepaths) == 1){
      
      sortorder <- order(xs@peaks[,"rt"])
      
      peakTable <- data.frame("mz" = xs@peaks[sortorder, "mz"],
                              "rt" = xs@peaks[sortorder, "rt"]/60,
                              "intensity" = xs@peaks[sortorder,intval],
                              row.names = NULL,
                              stringsAsFactors = FALSE)
      colnames(peakTable)[colnames(peakTable) == "intensity"] <- intval
      
      peakTable
    } else {
      
      sortorder <- order(xs@groups[,"rtmed"])
      dataMatrix <- groupval(xs, value=intval)
      
      peakTable <- data.frame(dataMatrix[sortorder,], row.names=NULL)
      data.frame("mz" = xs@groups[sortorder, "mzmed"],
                 "rt" = xs@groups[sortorder, "rtmed"]/60,
                 dataMatrix[sortorder,],
                 row.names = NULL,
                 stringsAsFactors = FALSE) 
    }    
  } else {
    ## get peak table from CAMERA object  ------------------------------
    ##  if (class(xs)[1] == "xsAnnotate") {     
    pt  <-  getPeaklist(xs, intval=intval)  # function of CAMERA package
    ## sort the table by rt
    sortid <- order(pt$rt)
    pt <- pt[sortid,]
    if (length(xs@xcmsSet@filepaths) == 1){
      
      I  <- pt[, intval]
      
      peakTable <- data.frame("pcgroup" = as.numeric(pt$pcgroup),
                              "adduct" = pt$adduct,
                              "isotopes" = pt$isotopes,
                              "mz" = pt$mz,
                              "rt" = pt$rt/60,
                              "intensity" = I,
                              row.names=NULL,
                              stringsAsFactors = FALSE)
      colnames(peakTable)[colnames(peakTable) == "intensity"] <- intval
      
      peakTable
    } else {
      
      ## organize the output
      DM <- pt[,9:(dim(pt)[2]-3)]
      peakTable <- data.frame(DM, row.names=NULL)
      data.frame("pcgroup" = as.numeric(pt$pcgroup),
                 "adduct" = pt$adduct,
                 "isotopes" = pt$isotopes,
                 "mz" = pt$mz,
                 "rt" = pt$rt/60,
                 DM,
                 row.names = NULL,
                 stringsAsFactors = FALSE) 
      
    } 
  }
} 


