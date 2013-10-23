writePeakTable <- function (peakTable, fileformat="default", filename="dataSet.csv"){
# print peak table 
#
# writePeakTable (peakTable, fileformat='default')


if (fileformat == 'default') { # for use in Tomcat/Rwui webserver, and for Excel
  naID=""; 
  quoteID = TRUE;
  decPoint = ",";  # Italian system
  delimiter = "\t";} # or "," ?
if (fileformat == 'R') {
  naID="NA"; 
  quoteID = TRUE;
  decPoint = ".";
  delimiter = "\t";
  if (filename == "xcmsPeakTable.csv") filename="xcmsPeakTable_R.csv"
  }
if (fileformat == 'Matlab') {
  naID="NaN"; 
  quoteID = FALSE;
  decPoint = ".";
  delimiter = "\t";
  if (filename == "xcmsPeakTable.csv") filename="xcmsPeakTable_Matlab.csv"}
  # old: filename=paste(filename,"_matlab.csv",sep="")
  # in Matlab: open file with: dataStruct=importdata('xcms_peakTable_matlab.txt')

# set decimal place precision: rt, mz, feature intensity
    # set decimal places to 1 ("%.1f") 
    dataPrec = "%.1f" # feature value precision
    rtPrec   = "%.2f" 
    mzPrec   = "%.4f"   
    # find feature-data columns (starting behind mz rt) 
    posRT = which(colnames(peakTable) == "rt") 
    posMZ = which(colnames(peakTable) == "mz")
    # peakTable[,posRT] = as.numeric(sprintf(rtPrec, peakTable[,posRT]))
    # new RT conversion: works for both numerical (LC) and factor (GC)
    peakTable[,posRT] = as.numeric(sprintf(rtPrec, as.numeric(as.character(peakTable[,posRT])) ))
    peakTable[,posMZ] = as.numeric(sprintf(mzPrec, peakTable[,posMZ]))
    # change decimal places of peak/feature intensity
       b = max(posRT, posMZ) + 1 # "begin" first sample column
       e = length(peakTable)     # "end" last sample column (last column of peak table)
       x = peakTable[,c(b:e)]
       if (length(c(b:e)) == 1) {
              xx = as.numeric(sprintf(dataPrec, x))
       } else xx = as.numeric(apply(x, 2, function(x) sprintf(dataPrec, x))) 
       peakTable[,c(b:e)] = xx

# write to file
write.table(peakTable, file = filename, append = FALSE, quote = quoteID, sep = delimiter, eol = "\n", na = naID, dec = decPoint, row.names = FALSE, col.names = TRUE, qmethod = "double")


# write.csv(...)
# write.csv2(...)
# write.csv(peakTable, file=filename, quote = TRUE, na = missingDataId)

}

