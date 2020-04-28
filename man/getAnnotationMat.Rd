\name{getAnnotationMat}
\alias{getAnnotationMat}
\alias{relInt}
\title{Obtain relative quantitative annotation results for GC-MS}
\description{
Function \code{getAnnotationMat} obtains relative intensities for features
in individual samples. A robust linear regression is performed
when the number of common features is five or larger; for 2-4 peaks
a weighted linear regression is used, and if only one peak is in
common the intensity ratio for this peak is used. Reference patterns are
the patterns in the database of standards or the patterns in the
\dQuote{unknowns} element of the \code{allMatches} object. Not meant to
be called directly by the user.
}
\usage{
getAnnotationMat(exp.msp, pspectra, allMatches)
relInt(pat, refpat)
}
\arguments{
  \item{exp.msp}{List of experimental pseudospectra.}
  \item{pspectra}{Spectra from the in-house database.}
  \item{allMatches}{Match information in the form of a list.}
  \item{pat, refpat}{Both pseudospectra.}
}
\value{Function \code{getAnnotationMat} returns a matrix containing all
  patterns (standards as well as unknowns) in the rows, and numeric
  values signifying relative intensities in all samples in the
  columns. These relative intensities are the quantities calculated by
  \code{relInt}, which simply returns one number.
}
\author{Ron Wehrens}
\examples{
## Example of results
  data(GCresults) ## pre-compiled results
  GCresults$PeakTable
}
\keyword{manip}
