\name{alignmentLC}
\alias{alignmentLC}
\title{LC alignment}
\description{
Performs grouping and retention time correction via \code{xcms}. The
settings are specified as a list with a form similar to the one
discussed in the help of \code{FEMsettings}. The sequence of actions depends on the 
type of retention time correction, which is specified inside the \code{Retcor} list inside the 
\code{Alignment} slot (see details). 
Integration of areas of missing peaks is performed depending on the \code{fillPeaks} setting.
}
\usage{alignmentLC(xset, settings)}
\arguments{
  \item{xset}{
    The \code{xcmsSet} object resulting from peak detection
  }
  \item{settings}{
    The subset of settings contained into the "Alignment" element of the
    XCMSsettings list.
  }
}
\details{
  The sequence of actions depends on the algorithm used by \code{xcms} for retention
  time correction. For the density-based approach the sequence is: 1) across sample grouping, 
  2) retention time correction, 3) second across sample grouping, 4) optional fill-peaks. 
  For "obiwarp", instead, the sequence of actions is: 1) retention time correction, 
  2) across sample grouping, 3) optional fill-peaks.
  For across-sample grouping in \code{xcms}, the \code{minsamp} parameter can be
  specified in the settings either as a minimum number of samples
  (\code{min.class.size}) or as the fraction of samples per class
  (\code{min.class.fraction}). If both are given the smallest number is
  used. 
  If the retention time correction is performed by the density-based approach, 
  the settings allow to express the \code{xcms} parameters \code{missing} and \code{extra} as fractions. 
  When "obiwarp" is used for retention time correction, the sample with the bigger
  number of features is automatically selected as the "reference" sample.  
  The \code{xcms} parameters for the \code{retcor} \code{xcms} function can be specified
  in the Retcor list included in the Alignment slot of \code{FEMsettings}. 
}
\seealso{\code{\link{FEMsettings}}}
\value{A grouped and rt-aligned \code{xcmsSet} object.}
\author{Ron Wehrens and Pietro Franceschi}
\examples{
## Example of results
  data(LCresults) ## pre-compiled results
  LCresults$PeakTable
}
\keyword{manip}
