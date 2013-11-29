\name{AnnotateFeature}
\alias{AnnotateFeature}
\title{Feature-wise annotation in LC-MS data}
\description{
  Function to match a vector of the form (mz,rt,I) with a specific
  database. The tolerance in retantion time is set in the \code{settings}
  list. The mz tolerance is calculated by a specific function, depending
  ont the mass and the intensity of the feature. If this function is not
  specified, (\code{NULL}) a fixed mass window will be used with the value
  specified in the settings list.}
\usage{
AnnotateFeature(input, DB, settings, errf)
}
\arguments{
  \item{input}{
    A vector with three elements in the form \code{(mz,rt,I)}.}
  \item{DB}{
    A dataframe, used for tha annotation. See the help of
    \code{LCDBtest} for a description of the DB.}
  \item{settings}{
    The subset of settigs contained into the "match2DB" element of the
    XCMSsettings list.}
  \item{errf}{
    The (linear) model used to calculate the \code{m/z} tolerance for
    the matching. If \code{NULL} a fixed mass window will be used, its
    value is specified into the settings.}
}
\details{
  If \code{errf} is provided, it is used to calculate the mass tolerance
  as a function of the m/z value and of the logarithm of the intensity of
  the signal. \code{errf} is used inside a \code{predict(errf,
    data.frame(M = input["mz"], logI = log10(input["I"])))} call. 
  
  The DB is a \code{data.frame} which should contain the following
  columns: \code{"ChemSpiderID"}, \code{"compound"}, \code{"formula"},
  \code{"mz"}, \code{"rt"},  and \code{"maxo"}.
}
\value{
  A vector of indices which associates the feature to the entries  -
  possibly multiple -  in the database.
}
\references{
N. Shahaf, P. Franceschi, P. Arapitsas, I. Rogachev, U. Vrhovsek and
  R. Wehrens: "Constructing a mass measurement error surface to improve
  automatic annotations in liquid chromatography/mass spectrometry based
  metabolomics". Rapid Communications in Mass Spectrometry, 27(21), 2425
  (2013). 
}
\author{Pietro Franceschi}
\examples{
  ## necessary!
  }
\keyword{manip}
