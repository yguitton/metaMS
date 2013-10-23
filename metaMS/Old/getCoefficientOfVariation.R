getCoefficientOfVariation <- function (peakTable,th=25){
# Coefficient of variation (QC sample only)
#
# CV = getCoefficientOfVariation(peakTable)
# 

# test:
#   source("../Func/getCoefficientOfVariation.R")
#

###############################################################


# select QC-samples only
   id = getSamples(peakTable,"QC")

if (length(id) > 1) {
  data = peakTable[,id]

  # CV coefficient of variation = std/mean * 100  
  # CVall = ( sd(t(data), na.rm = TRUE) / mean(t(data), na.rm = TRUE) ) * 100  # wrong!!
  stdev = apply( data, 1 , sd, na.rm=TRUE)
  avg   = apply(data, 1, mean, na.rm=TRUE)
  CVall = (stdev / avg) * 100 

 

# good/bad peaks

  idxGood = CVall <  th
  idxBad  = CVall >= th

  

  numGood = sum(idxGood, na.rm = TRUE)
  numBad = sum(idxBad, na.rm = TRUE)

  percent = round( numGood/length(CVall) * 1000) / 10

  total = length(CVall)

} else { numGood = NA; numBad = NA;  percent= NA;  total= NA}

  Z <- data.frame(numGood,numBad,percent,total)



return(Z)
}
