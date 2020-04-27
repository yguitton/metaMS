\name{peakDetection}
\alias{peakDetection}
\title{Wrapper for XCMS peak detection, to be used for both GC-MS and
  LC-MS data.}
\description{XCMS peak detection using settings, defined for individual
  laboratories and depending on the chromatographic and
  mass-spectrometric characteristics of the instruments at hand.}
\usage{
peakDetection(files, settings, rtrange = NULL, mzrange= NULL,
              convert2list = FALSE, nSlaves = 0) 
}
\arguments{
  \item{files}{input files (including path names) that will be processed
    by xcms.}
  \item{settings}{a list of settings that will be passed on to the
    xcmsSet function. See the help of \code{FEMsettings} for a detailed
    description of the fields in this list.} 
  \item{rtrange}{If non-NULL, a vector to subset the region of the
    chromatography retained for further analysis (given in minutes).}
  \item{mzrange}{If non-NULL, a vector indicating the subset of the
    mass spectrum retained for the analysis.}
  \item{convert2list}{logical. If \code{TRUE}, the xcmsSet object will
    be split into a list of one-sample objects which will be treated
    separately. This is useful not only in GC-MS data, where peak
    alignment is avoided, but also in setting up databases of standards,
    where no common peaks between injections are expected to occur.}
  \item{nSlaves}{Number of cores to be used in the peak picking.}
}
\value{Either an xcmsSet object, or a list of one-sample xcmsSet objects.}
\author{Ron Wehrens and Pietro Franceschi}
\examples{
## Example of settings
  data(FEMsettings)
  metaSetting(object = TSQXLS.GC, field= "PeakPicking")
}
\keyword{manip}
