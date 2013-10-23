##obsolete!
cameraGC <- function(xsetObj, settings) {
  y <- xsAnnotate(xsetObj, sample = 1)
  capture.output(
      z <-  do.call(groupFWHM,
                    c(list(object = y), settings)),
      file = NULL)

  z
}
