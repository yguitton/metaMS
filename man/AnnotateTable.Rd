\name{AnnotateTable}
\alias{AnnotateTable}
\alias{AnnotateFeature}
\title{Annotate a Peaktable}
\description{Functions which annotate a peaktable on the bases af a
  database of standards. Not meant to be called directly by the user.}
\usage{

## Annotate one feature
AnnotateFeature(input, DB, settings, errf)

## Annotate a full table of features
AnnotateTable(peaktable, errf, DB, settings)
}
\arguments{
  \item{input}{A vector with three elements in the form \code{(mz,rt,I)}.}
  \item{peaktable}{A peaktable (matrix) with three column corresponding
    to mz,rt and I values for a series of features.}
  \item{errf}{The file containing the error function used to predict the
    tolerance on the \code{m/z} value used for the matching against the
    DB.}
  \item{DB}{A dataframe used for the annotation. See the help of
    \code{LCDBtest} for a description of the DB.}
  \item{settings}{The subset of settings contained into the "match2DB"
    element of the XCMSsettings list.}
}

\details{The annotation of each feature is performed by comparing its
  m/z value and its retention time to a database provided by the
  user. To account for shifts in retention time and mass occurring
  during data acquisition, the matching of a specific feature against
  the DB is done with a specific tolerance in mass and in retention
  time.
  
  \emph{Retention time tolerance} \cr 
  The retention time tolerance is specified (in minutes) in the settings list
  (field \code{rttol}). This value is instrument- and
  chromatography-dependent. 
  
   \emph{m/z tolerance} \cr 
  The tolerance on the mass scale mainly depends on the characteristics
  of the spectrometer used for the acquisition. For Q-TOF instruments it
  has been recently shown (see references) that the optimal mass
  tolerance can be expressed as a function of the \code{m/z} value and
  of the logarithm of the ion intensity \code{log10(I)}. As a trend, the
  mass drift will be bigger for smaller ions and for low intensity
  signals. \cr 
  In the present implementation the tolerance in mass can be either
  fixed over the complete mass range or calculated as a function of the
  mz and I values of each feature. In the simplest case, the fixed mass
  tolerance is provided in the \code{mzwindow} (in Dalton!) element of
  the list of settings.\cr 
  Alternatively, one can provide (supplying the \code{errf} argument) a
  function used to calculate the mz tolerance (in ppm!) as a function of
  the fields of the input vector (\code{(mz,rt,I))}. \cr 
  As discussed in the publication, for a Waters Synapt Q-TOF the
  function is a linear model taking as inputs \code{M = input["mz"],
  logI = log10(input["I"])}. This error function can be calculated by
  analyzing the results of the injections of the chemical standards. To
  avoid unreasonable small errors where data for mz and I are not
  available, the minimum value for the mass tolerance is explicitly set
  in the settings (\code{ppm}). This value should match the technical
  characteristics of the spectrometer.  
  
  To reduce the number of false positives and make the annotation more
  reliable, a match is retained only if more than one feature
  associated to a specific compound is found in the list of
  features. How many "validation" features are required is defined in the
  list of settings in the \code{minfeat} element. At this validation
  level, another retention time tolerance is introduced:
  two or more features validate one specific annotation if their retention
  time are not very much different. This rt tolerance is also defined in
  the settings (the \code{rtval} field). As a general suggestion,
  \code{rtval} should be kept smaller than \code{rttol}. The latter,
  indeed, refers to the matching of a peaktable with a database which
  has been created from the injections of the chemical standards during
  different instrumental runs (maybe also with different columns). On
  the other hand, \code{rtval} accounts for smaller retention time
  shifts, occurring within the same LC run. 

  For the description of the structure of the DB, refer to the help of the
  \code{LCDBtest} dataset.   
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
(2013).}
\author{Pietro Franceschi}
\examples{
## Example of results
  data(GCresults) ## pre-compiled results
  GCresults$PeakTable
}
\keyword{manip}

