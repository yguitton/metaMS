plotPCA <- function (X,outputfile="screen",filename="figure_PCA",onlyQC=FALSE,rejectBlanks=FALSE){
# PCA plot of xcms data
#
# plotPCA(xset)
# plotPCA(peakTable)
# plotPCA(peakTable, outputfile="pdf")
# plotPCA(peakTable, outputfile="pdf", filename="figure1_PCA")
#
# outputfile can be set as "pdf"  (default: "screen") 
#
# function is based on the scribt 'PCAQC.R' by Ron Wehrens
# adapted for xcmsFEM library by Matthias Scholz
#
# test:
#   source("../Func/plotPCA.R")
#   source("../Func/plotPCAnew.R")
#   source("../Func/getSamples.R")
#   source("../Func/getShortSampleNames.R")
#   load('xcmsXsetObject.RData')
#   plotPCA(xset)
#
##################################################

# library(PCA)
# library(xcms)

# if input is XSET object
   if (isS4(X)) {
    xset = X
    if (class(xset) == "xsAnnotate") xset <- xset@xcmsSet # in case of CAMERA object
    peakTable <- getPeakTable(xset)
   }

# if input is peak table
   if (is.data.frame(X)) {
    peakTable = X
   }



#################################################
## get sample-data (all columns behind mz rt) 

    # peakTable -> peakTableData

    posRT = which(colnames(peakTable) == "rt") 
    posMZ = which(colnames(peakTable) == "mz")
    b = max(posRT, posMZ) + 1 # "begin" first sample column
    e = ncol(peakTable) # length(peakTable) # "end" last sample column (last column of peak table)
    peakTableData = peakTable[,c(b:e)]

#################################################
## check if PCA is possible, otherwise --> error message

    if (onlyQC) {
     txt="\n-- generate PCA (only QC) --"
    } else {
     txt="\n-- generate PCA --"
    }
     write(txt, file = "process_info.txt", append = TRUE)

    if (sum(is.na(peakTableData)) > 0) {
       txt="     WARNING: cannot generate the PCA plot because of missing values"
       write(txt, file = "process_info.txt", append = TRUE)
       warning(txt)
       return(-1)
    } else if (ncol(peakTableData) < 3) { 
       txt="     WARNING: cannot generate the PCA plot because 3 or more samples are required"
       write(txt, file = "process_info.txt", append = TRUE)
       warning(txt)
       return(-1)
    }


#################################################

## get column ID of sample categories (QC, STD, etc)
    QCs    <- getSamples(peakTableData,"QC")
    STDs   <- getSamples(peakTableData,"STD")
    blanks <- getSamples(peakTableData,"blank")
    others <- (1:ncol(peakTableData))[-c(QCs, STDs, blanks)]

    snames <- getSamples(peakTableData,"names")

if (rejectBlanks) {
      blanks = as.integer(c())
    }

if (onlyQC) {
     if (length(QCs) < 3) { # return if less than 3 QCs
      # txt="     WARNING: cannot generate a PCA plot only of QCs,\n       since 3 or more QCs are required"
      # write(txt, file = "process_info.txt", append = TRUE)
      # warning(txt)
      return(-1)
     }
   peakTableData=peakTableData[,QCs]
   snames = colnames(peakTableData)  
}

## scaling / normalizing 
 DM = t(peakTableData) # data matrix
 DMn <- DM/rowSums(DM) ## scaled version (vector norm)
 DMs <- scale(DMn)
 # remove peaks that are NaN after unit-variance (scale) normalization
     rmPeakID = which(is.na(colSums(DMs)))
       if (length(rmPeakID) >= 1) {
        txt=as.character(paste(c("     WARNING (PCA plot): remove ",as.character(length(rmPeakID))," peaks that are NaN after normalization"),collapse=''))
        write(txt, file = "process_info.txt", append = TRUE)
        DMs <- DMs[,-rmPeakID]
       }

## PCA
  DM.PCA <- PCA(DMs)
  pc=DM.PCA$scores[,1:2]
  x=as.numeric(pc[,"PC 1"])
  y=as.numeric(pc[,"PC 2"])

## define colors
  cols <- rep("blue", nrow(DMn)) 
  cols[QCs]    <-  "red" 
  cols[STDs]   <-  "green" 
  cols[blanks] <-  "orange" 

## define symbols
  pchL <- rep(20, nrow(DMn))
  pchL[QCs]    <- 1
  pchL[STDs]   <- 4
  pchL[blanks] <- 2


##################
## plot PCA scores
 if  (outputfile == "pdf") {  
  s = paste(c(filename,".pdf"), collapse='')
  pdf(file = s)
 }

  if (onlyQC) {
      plot(x,y, main="PCA (QC samples)", xlab="PC 1", ylab="PC 2", pch=1, col="red") 
      lines(x[1],y[1], type="p", pch=16, lty=1, col="red")
      lines(x,y, type="l", pch=22, lty=1, col="grey")
  } else {
      scoreplot(DM.PCA, col = cols, pch = pchL)
      if ((length(STDs) != 0) || (length(blanks) != 0)) { 
         legend("topright", # including STD and blanks
         legend = c("sample", "STD", "QC", "blank"), 
         col = c("blue", "green", "red", "orange"),  
         pch = c(20, 4, 1, 2) )
      } else { 
         legend("topright", # without STD and blanks
         legend = c("sample", "QC"), 
         col = c("blue", "red"),  
         pch = c(20, 1) )
      }
   }

 if  (outputfile == "pdf") {  
  dev.off() # close pdf file 1
 }
 
###############################################
## print again, but including samplenames
 if  (outputfile == "pdf") {  
  s = paste(c(filename,"_samplenames.pdf"), collapse='')
  pdf(file = s)

  if (onlyQC) {
      plot(x,y, main="PCA (QC samples)", xlab="PC 1", ylab="PC 2", pch=1, col="red") 
      lines(x[1],y[1], type="p", pch=16, lty=1, col="red")
      lines(x,y, type="l", pch=22, lty=1, col="grey")
  } else {
      scoreplot(DM.PCA, col = cols, pch = pchL)
  }
  # add sample names
  snames = getShortSampleNames(snames) 
  for (i in 1:length(snames)) text(x[i], y[i], as.character(snames[i]),cex=.5 , pos = 4)

  dev.off() # close pdf file 2 (sample names)
 }



}
