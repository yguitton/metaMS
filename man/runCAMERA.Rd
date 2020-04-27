\name{runCAMERA}
\alias{runCAMERA}
\title{The CAMERA element in the metaMS pipeline}
\description{
  Run CAMERA package with settings from the settings list (see
  \code{FEMsettings}). Works both for LC and GC. Not meant to be called
  directly by the user.
}
\usage{
runCAMERA(xset, chrom = c("LC", "GC"), settings, polarity, quick = TRUE)
}
\arguments{
  \item{xset}{For LC, an \code{xcmsSet} object; for GC, a list of
    \code{xcmsSet} objects.}
  \item{chrom}{
    The type of chromatography. Either "LC" or "GC".}
  \item{settings}{
    The subset of settigs contained into the "CAMERA" element of the
    settings list.}
  \item{polarity}{
    The polarity of the injection, used by CAMERA to look for common
    adducts.}
  \item{quick}{Only relevant for LC data.
    If \code{TRUE}, CAMERA runs only \code{groupFWHM} and
    \code{findIsotopes}. If \code{FALSE}, correlations between EICs are
    calculated and used for grouping (which can take some time).
  }
}
\details{
  In the case of LC the function is used in data analysis and DB
  creation. In the first case, it increases the level of the annotation
  and it it, by default, run with \code{quick} = \code{TRUE}. For DB
  creation, the grouping of the features into "pcgroups" (features with
  similar retention time) is used to choose the features to be included
  into the database. In this case camera is run with \code{quick} =
  \code{FALSE}: \code{quick} determines whether or not the correlation
  among the extracted ion chromatograms should be used to "validate" the
  pcgroups.

  For GC data, only the grouping done by \code{groupFWHM} is performed:
  basically this clusters peaks with a similar retention time.
}
\value{
  An annotated xcmsSet object (an object of class CAMERA).}
\seealso{\code{\link{annotate}}, \code{\link{groupFWHM}}}
\author{Ron Wehrens and Pietro Franceschi}
\examples{
## Example of results
  data(LCresults) ## pre-compiled results
  LCresults$PeakTable
}
\keyword{manip}
