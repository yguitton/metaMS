\name{getAnnotationLC}
\alias{getAnnotationLC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get LC annotation
}
\description{
Main function of LC-MS compound annotation.
}
\usage{
getAnnotationLC(xs, settings, DB, errf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xs}{
The xcmsSet (or CAMERA) object to be annotated
}
  \item{settings}{
The subset of settings contained into the match2DB elements of the settings list. See the help of \code{FEMsettings} for details.
}
  \item{DB}{
The database used within \code{AnnotateTable} for the annotation of the peaklist. See the help of \code{FLCDBtest} for details.
}
  \item{errf}{
  The file containing the error function used to predict the m/z tolerance. See the help of \code{AnnotateTable} for details.
}
}
\details{
The function rely on \code{getPeakTable} to extract the \code{maxo} value (maximum value of the ionic signal over the chromatographic peak)  from \code{xs}. In the case of multiple sample the maximum intensity of a specific feature across all the samples is used for annotation. \code{maxo} is used to calculate the mass tolerance.
}
\value{
A list with two elements. \code{raw} is the complete output of \code{AnnotateTable}. \code{for_table} is a data.frame which summarizes the outputs of the annotation (see \code{AnnotateTable}) and it is included in the output of the \code{runLC} main function.
}
\references{
N Shahaf, P Franceschi, P Arapitsas, I Rogachev, U Vrhovsek, R Wehrens, "Constructing a mass measurement error surface to improve automatic annotations in liquid chromatography/mass spectrometry based metabolomics". Rapid Communications in Mass Spectrometry, 27(21), 2425 (2013).
}
\author{
Pietro Franceschi
}

\seealso{
 \code{\link{AnnotateTable}}, \code{\link{LCDBtest}}, \code{\link{FEMsettings}}
}
\keyword{manip}
