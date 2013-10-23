\name{alignmentLC}
\alias{alignmentLC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LC alignment
}
\description{
Performs the \code{xcms} grouping and retention time alignment by using settings from the XCMSsetting-file. The sequence of actions is xcms-grouping, retention time correction, xcms grouping. Fill misssing peaks is performed depending on the \code{fillPeaks} settings}
\usage{
alignmentLC(xset, settings)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xset}{
The \code{xcmsSet} object resulting from peak detection
}
  \item{settings}{
The subset of settigs contained into the "Alignment" element of the XCMSsettings list.
}
}
\details{
For across sample grouping, the \code{minsamp} parameter is set taking the minimum between the \code{min.class.size} and (\code{min.class.fraction})*(\code{Number of samples}). For retention time correction, the \code{missing} and \code{extra} parameters are calculated as a fraction of the number of samples. 
}
\value{
A grouped,aligned and (filled) \code{xcmsSet} object 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Ron Wehrens and Pietro Franceschi
}

\keyword{ ~manip }