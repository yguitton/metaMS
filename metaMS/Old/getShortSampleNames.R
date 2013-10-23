getShortSampleNames <- function (snames){
# shorten sample names
#  removes "_pos" "_neg" "_NP" "_RP"
#  removes last "01" "02" "03" "04"
#
#  snames <- getShortSampleNames(snames)
#
# used in "getPeakTable.R" to shorten the column (sample) names in the peak table
# used in "plotPCA.R"      to have short sample labels in a PCA plot
#
# source("/home/scholz/Projects_IASMA/V07_R_library_pipeline/Functions/getShortSampleNames.R")
# snames = colnames(peakTable)
#


for (i in 1:length(snames)) {
  # remove ending "01" "02" "03" "04"
  snames[i]=gsub("_pos01","_pos",snames[i],fixed = TRUE,ignore.case = FALSE) 
  snames[i]=gsub("_pos02","_pos",snames[i],fixed = TRUE,ignore.case = FALSE) 
  snames[i]=gsub("_pos03","_pos",snames[i],fixed = TRUE,ignore.case = FALSE)
  snames[i]=gsub("_pos04","_pos",snames[i],fixed = TRUE,ignore.case = FALSE)
  snames[i]=gsub("_neg01","_neg",snames[i],fixed = TRUE,ignore.case = FALSE) 
  snames[i]=gsub("_neg02","_neg",snames[i],fixed = TRUE,ignore.case = FALSE) 
  snames[i]=gsub("_neg03","_neg",snames[i],fixed = TRUE,ignore.case = FALSE) 
  snames[i]=gsub("_neg04","_neg",snames[i],fixed = TRUE,ignore.case = FALSE)
  # remove "_NP"
  snames[i]=gsub("_NP_","_",snames[i],fixed = TRUE,ignore.case = FALSE) 
  snames[i]=gsub("_RP_","_",snames[i],fixed = TRUE,ignore.case = FALSE) 
  snames[i]=gsub("_NP","_",snames[i],fixed = TRUE,ignore.case = FALSE) 
  snames[i]=gsub("_RP","_",snames[i],fixed = TRUE,ignore.case = FALSE) 
  # remove "_pos"
  snames[i]=gsub("_pos","",snames[i],fixed = TRUE,ignore.case = FALSE) 
  snames[i]=gsub("_neg","",snames[i],fixed = TRUE,ignore.case = FALSE) 
  # replace points "." to underscore "_"
  snames[i]=gsub(".","_",snames[i],fixed = TRUE,ignore.case = FALSE) 
  snames[i]=gsub("__","_",snames[i],fixed = TRUE,ignore.case = FALSE) 
}


return(snames)
}
######################################################àà
