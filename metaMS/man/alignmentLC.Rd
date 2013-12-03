\name{alignmentLC}
\alias{alignmentLC}
\title{LC alignment}
\description{
Performs grouping and retention time correction via \code{xcms}. The
settings are specified as a list with a form similar to the one
discussed in the help of \code{FEMsettings}. The sequence of actions is
(xcms) grouping, retention time correction, a second run of
(xcms) grouping. Integration of areas of missing peaks is performed
depending on the \code{fillPeaks} setting.}
\usage{alignmentLC(xset, settings)}
\arguments{
  \item{xset}{
    The \code{xcmsSet} object resulting from peak detection
  }
  \item{settings}{
    The subset of settigs contained into the "Alignment" element of the
    XCMSsettings list.
  }
}
\details{
  For across-sample grouping in XCMS, the \code{minsamp} parameter can be
  specified either in the settings as a minimum number of samples
  (\code{min.class.size}) or as the fraction of samples per class
  (\code{min.class.fraction}) - if both are given the smallest number is
  used. For retention time correction, the settings allow to express the
  XCMS parameters \code{missing} and \code{extra} as fractions.  
}
\seealso{\code{\link{FEMsettings}}
\value{A grouped and rt-aligned \code{xcmsSet} object.}
\author{Ron Wehrens and Pietro Franceschi}
\keyword{manip}
