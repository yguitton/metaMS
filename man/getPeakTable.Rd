\name{getPeakTable}
\alias{getPeakTable}
\title{Extract a peak table from an xcms or CAMERA object}
\description{
  Extracts the peak table (a \code{data.frame}) from an xcms (or CAMERA)
  object (without compound annotation). The peak table contains the mass
  and retention time for all the features and their intensities across
  the samples.  This function is not meant to be called directly, but it
  is internally used by \code{runLC}, \code{getAnnotationLC},
  \code{createSTDdbLC}.
}
\usage{
getPeakTable(xs, intval = "into")
}

\arguments{
  \item{xs}{
    The xcmsSet (or CAMERA) object}
  \item{intval}{
    The intensity measure extracted form \code{xs}.}
}
\details{
  The function process an \code{xs} object and returns for it a
  PeakTable which associates intensities to features and samples. The
  default measure for the intensity is \code{into} (the chromatographic
  peak area for a feature), but in the case of annotation, \code{maxo}
  (value for the intensity of the ion over the chromatographic peak) is
  used to measure the intensity. For a more detailed description of the
  possible intensity measures refer to the documentation of \code{xcms]}.
}
\value{
  A data frame with the intensity for each feature (rows) in all the
  samples (columns). Each feature is identified by its m/z value and
  retention time (in minutes). If the \code{xs} object is of class
  CAMERA, the results of the camera annotation (isotope, adduct,
  pcgroup) are included in the table.  
}
\author{Pietro Franceschi}
\examples{
## Example of results
  data(LCresults) ## pre-compiled results
  LCresults$PeakTable
}
\keyword{manip}

