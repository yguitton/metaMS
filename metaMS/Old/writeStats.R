writeStats <- function (xset,peakTable,intrumentCode,XCMSsettings,runAnnotation=TRUE){
# 
# source("writeStats.R")
# writeStats(xset,peakTable,intrumentCode,XCMSsettings,runAnnotation)
#
# number of annotated peaks, etc
#
#
#

# test:
#  source('../Func/writeStats.R')


###############################################################
# statistics (number of features, etc..) -> process_info.txt

    txt="\n\n\n-- Feature data set --"
    write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")

# total and annotated peaks
    totalPeaks = nrow(peakTable)
    if (runAnnotation) {
      annotatedPeaks = length(which(peakTable$compounds != ""))
      txt=as.character(paste(c("     total features: ",totalPeaks, "   annotated features: ",annotatedPeaks),collapse=''))
      write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
      } else {
      txt=as.character(paste(c("     total features (xcmsPeakTable.txt): ",totalPeaks),collapse=''))
      write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
    }

# write sample names
    writeSampleNames(xset)

# Coefficient of variation (QC-sample only)
   if (length(getSamples(peakTable,"QC")) > 2) {
    CV = getCoefficientOfVariation(peakTable)
     txt = "\n-- Coefficient of variation (QC-samples only) --"
     write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
     txt=as.character(paste(c("     good features (%CV<25): ",as.character(CV$numGood)," (", as.character(CV$percent),"%)" ),collapse=''))
     write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
     txt=as.character(paste(c("     problematic features (%CV > 25): ",as.character(CV$numBad)," (", as.character(100-CV$percent),"%)" ),collapse=''))
     write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
     txt=as.character(paste(c("     total features: ",as.character(CV$total) ),collapse=''))
     write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
    }

  # write xcms settings
   txt = "\n-- XCMS settings --"
     write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
   txt=as.character(paste(c("     Instrument: ",as.character(intrumentCode)),collapse=''))
     write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
   txt=as.character(paste(c("     signal-to-noise threshold (sthresh): ",as.character(XCMSsettings[1,"sthresh"])),collapse=''))
     write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
   txt=as.character(paste(c("     minimum number of samples in which a feature should be present (minsamp): ",as.character(XCMSsettings[1,"minsamp"])),collapse=''))
     write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
   txt=as.character(paste(c("     maximum number of peaks per extracted ion chromatogram (max): ",as.character(XCMSsettings[1,"max"])),collapse=''))
     write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
   txt=as.character(paste(c("     step size to use for profile generation (step): ",as.character(XCMSsettings[1,"step"])),collapse=''))
     write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")
   txt=as.character(paste(c("     minimum difference in m/z for peaks (mzdiff): ",as.character(XCMSsettings[1,"mzdiff"])),collapse=''))
     write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "")

   txt = "\n\n"
   write(txt, file = "process_info.txt", append = TRUE); write(txt, file = "") # some space


}
