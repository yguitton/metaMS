\name{runCAMERA}
\alias{runCAMERA}
\title{
Run CAMERA
}
\description{
Run CAMERA package with settings from the settings list (see \code{FEMsettings}). Works both for LC and GC. 
}
\usage{
runCAMERA(xset, chrom = c("LC", "GC"), settings, polarity, quick = TRUE)
}
\arguments{
  \item{xset}{
The \code{xcmsSet} object.
}
  \item{chrom}{
The type of chromatography. Either "LC" or "GC".
}
  \item{settings}{
The subset of settigs contained into the "CAMERA" element of the settings list.
}
  \item{polarity}{
The polarity of the injection, used by CAMERA to look for common adducts.
}
  \item{quick}{
If \code{TRUE}, CAMERA runs only \code{groupFWHM} and \code{findIsotopes}. 
}
}
\details{
In the case of LC the function is used in data analysis and DB creation. In the first case, it increases the level of the annotation and it it, by default, run with \code{quick} = \code{TRUE}. For DB creation, the grouping of the features into "pcgroups" (features with similar retention time) is used to choose the features to be included into the database. In theis case camera is run with \code{quick} = \code{FALSE}. \code{quick} determines whether or not the correlation among the extracted ion chromatograms should be used to "validate" the pcgroups.
}
\value{
An annotated xcmsSet object (an object of class CAMERA).
}

\author{
Ron Wehrens and Pietro Franceschi
}

\keyword{manip}
