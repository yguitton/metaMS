\name{AnnotateTable}
\alias{AnnotateTable}
\title{AnnotateTable}
\description{Annotate a peak table using a database of standards.}
\usage{
AnnotateTable(peaktable, errf, DB, settings)
}
\arguments{
  \item{peaktable}{A peaktable (matrix) with three column corresponding
    to mz,rt and I values.}
  \item{errf}{The file containing the error function used to predict the \code{m/z}
    tolerance.} 
  \item{DB}{A dataframe used for tha annotation. See the help of \code{LCDBtest} for a description of the DB.}
  \item{settings}{The subset of settigs contained into the "match2DB" element of the XCMSsettings list.}
}
\details{
  If \code{errf} is provided, it is used to calculate the mass tolerance
  as a function of the mass range and of the logarithm of the intensity of
  the signal. \code{errf} is used inside a \code{predict(errf,
    data.frame(M = input["mz"], logI = log10(input["I"])))} call. 
  The DB is a \code{data.frame} which should contain the following
  columns: \code{"ChemSpiderID"}, \code{"compound"}, \code{"formula"},
  \code{"mz"}, \code{"rt"} and \code{"maxo"}. To consider one annotation
  as reliable, more than one feature associated to a specific compound
  should be found in the list of features. This parameter is set inside
  the list of settings (\code{minfeat}). The tolerance among the
  different features assocaited to the same compounds is also set in the
  list of settings \code{rtval}. 
  To see the form of the list of settings, see \code{FEMsettings}.
}
\value{
  A list with the following elements
  \item{annotation.table}{A \code{data.frame} withe the results of the
    annotation and the reference to the DB} 
  \item{compounds}{The names of the annotated compounds}
  \item{IDs}{The IDs of the annotated compounds}
  \item{multiple.annotations}{The features with multiple annotations}
  \item{ann.features}{The features with annotation}
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
\examples{
  }
\keyword{manip}

