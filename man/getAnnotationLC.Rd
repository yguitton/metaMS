\name{getAnnotationLC}
\alias{getAnnotationLC}
\title{Get LC annotation}
\description{Main function for the annotation of an xcmsSet or CAMERA
  object. This function is not meant to be called directly. Use
  \code{runLC} instead.}
\usage{
getAnnotationLC(xs, settings, DB, errf)
}
\arguments{
  \item{xs}{
    The xcmsSet (or CAMERA) object to be annotated.}
  \item{settings}{
    The subset of settings contained into the match2DB elements of the
    settings list. See the help of \code{FEMsettings} for details.}
  \item{DB}{
    The database used within \code{AnnotateTable} for the annotation of
    the peaklist. See the help of \code{FLCDBtest} for details.}
  \item{errf}{
    The file containing the error function used to predict the m/z
    tolerance. See the help of \code{AnnotateTable} for details.}
}
\details{
  The function extracts from the \code{xs} object a Peaktable with the
  intensities of the features across all the samples. Since this
  Peaktable is meant to be used only for annotation (and not for
  subsequent statistical analysis), the intensities are expressed
  as\code{maxo} - the absolute maximum of the signal over the detected
  chromatographic peak (see the documentation of xcms for more
  details). Within the function the peaktable is converted into a matrix
  in the form \code{(mz,rt,I)} used by \code{AnnotateTable}. If
  \code{xs} contains more than one  sample, the intensity is the maximum
  intensity of each feature across all the samples.
}
\value{
  A list with two elements. \code{raw} is the complete output of
  \code{AnnotateTable}. \code{for_table} is a \code{data.frame} which
  summarizes the outputs of the annotation (see \code{AnnotateTable})
  and it is included in the output of the \code{runLC} main function. 
}
\references{
  N. Shahaf, P. Franceschi, P. Arapitsas, I. Rogachev, U. Vrhovsek and
  R. Wehrens: "Constructing a mass measurement error surface to improve
  automatic annotations in liquid chromatography/mass spectrometry based
  metabolomics". Rapid Communications in Mass Spectrometry, 27(21), 2425
  (2013). 
}
\author{
  Pietro Franceschi
}
\seealso{
  \code{\link{AnnotateTable}}, \code{\link{LCDBtest}}, \code{\link{FEMsettings}}
}
\examples{
## Example of results
  data(LCresults) ## pre-compiled results
  LCresults$PeakTable
}
\keyword{manip}
