peakDetection <- function (files, settings, rtcut = NULL)
{
  ## RW
  ## It would be better if we could find all argument
  ## names programmatically but i do not know how to do that...
  if (settings.temp$method == "matchedFilter") {
    param <- c("step", "steps", "fwhm", "snthresh", "max", "nSlaves")
  }
  else{
    param <- c("step", "peakwidth", "prefilter", "ppm")
  }

  xset <- do.call(xcmsSet, c(list(files = files, settings.temp[param])))

  ## Optionally: cut the first rtcut seconds of the chromatography to
  ## avoid alignment problems 
  if (!is.null(rtcut)) {
    idx <-  xset@peaks[,"rt"] > rtcut
    xset@peaks <- xset@peaks[idx,]
  }

  xset
}

