\name{getAnnotationLC}
\alias{getAnnotationLC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get LC annotation
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
The subset of settings contained into the match2DB of the XCMS settings list.
}
  \item{DB}{
The database used within \code{AnnotateTable} for the annotation of the peaklist
}
  \item{errf}{
  The file containing the error function used to predict the m/z tolerance
}
}
\details{
The function rely on \code{getPeakTable} to extract the \code{maxo} value from \code{xs}. In the case of multiple sample the maximum intensity of a specific feature across all the samples is used for annotation.
}
\value{
A list with two elements. \code{raw} is the complete output of \code{AnnotateTable}. \code{for_table} is a data.frame which summarizes the outputs of the annotation and it is attached to the output of the \code{runLC} main function.
}

\author{
Pietro Franceschi
}

\seealso{
 \code{\link{AnnotateTable}}
}
\keyword{manip}
