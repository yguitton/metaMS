## Function reads all CDF files in the input directory, does
## one-sample xcms processing using the settings in the list, and
## returns the result.

CDF2RData <- function(files, settingslist) {
  ## xset is a list of xcmsSet objects
  xset <- peakDetection(samples, settings)
  ## capture.output(xset <- lapply(samples,
  ##                               function(x)
  ##                               xcmsSet(x,
  ##                                       snthresh = settingslist$snthresh,
  ##                                       max = settingslist$max,
  ##                                       fwhm = settingslist$fwhm,
  ##                                       step = settingslist$step,
  ##                                       steps = settingslist$steps,
  ##                                       mzdiff = settingslist$mzdiff)),
  ##                file = NULL)

  lapply(xset,
         function(x) {
           y <- xsAnnotate(x, sample = 1)
           capture.output(z <- groupFWHM(y, perfwhm = settingslist$perfwhm),
                          file = NULL)
           z})
}

