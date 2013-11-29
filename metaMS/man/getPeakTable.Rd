\name{getPeakTable}
\alias{getPeakTable}
\title{Extract a peak table from an xcms or CAMERA object}
\description{
  Extracts the peak table (a \code{data.frame}) from an xcms (or CAMERA)
  object (without compound annotation). 
}
\usage{
getPeakTable(xs, intval = "into")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xs}{
    The xcmsSet (or CAMERA) object}
  \item{intval}{
    The intensity measure extracted form \code{xs}.}
}
\details{
  The usual values for \code{intval} are \code{into} (the
  chromatographic peak area for a feature) and \code{maxo}, the maximum
  value for the intensity of the ion over the chromatographic peak. 
}
\value{
  A data frame with the intensity for each feature (rows) in all the
  samples (columns). Each feature is identified by its m/z value and
  retention time (in minutes). If the \code{xs} object is of class
  CMAERA, the results of the camera annotation (isotope, adduct,
  pcgroup) are included in the table.  
}
\author{Pietro Franceschi}
\keyword{manip}

