\name{getFeatureInfo}
\alias{getFeatureInfo}
\title{
  Construct an object containing all meta-information of the annotated
  pseudospectra (GC-MS).
}
\description{
  Function getFeatureInfo yields a data.frame with meta-information
  on all features detected in the samples. Features are rows;
  information is in the columns. Not meant to be called directly by the
  user.
}
\usage{getFeatureInfo(stdDB, allMatches, sampleList)}
\arguments{
  \item{stdDB}{In-house database of standards.}
  \item{allMatches}{Object containing annotation information, generated
    by functions like \code{matchSamples2DB} and
    \code{matchSamples2Samples}.}
  \item{sampleList}{Object containing pseudospectra of samples.}
}
\value{A \code{data.frame} summarizing all meta-information of the
  pseudospectra found in the samples. Exactly what columns are included
  depends on the information contained in the in-house database. The
  last column is always "rt". 
}
\author{Ron Wehrens}
\examples{
## Example of results
  data(GCresults) ## pre-compiled results
  GCresults$PeakTable
}
\keyword{manip}
