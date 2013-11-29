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
  For across-sample grouping, the \code{minsamp} parameter can be
  specified either as a minimum number of samples (\code{min.class.size})
  or as the fraction of samples per class (\code{min.class.fraction}). In
  the function this parameter is calculated the minimum between the
  \code{min.class.size} and (\code{min.class.fraction})*(\code{Number of
    samples}). For retention time correction, the \code{missing} and
  \code{extra} parameters are aso expressed as a fraction of the number of
  samples.  
}
\value{A grouped and rt-aligned \code{xcmsSet} object.}
\author{Ron Wehrens and Pietro Franceschi}
\keyword{manip}
