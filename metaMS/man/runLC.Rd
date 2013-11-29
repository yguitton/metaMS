\name{runLC}
\alias{runLC}
\title{Wrapper for processing of LC-MS data files}
\description{
  Main function of the pipeline for LC-MS data processing. It includes
  XCMS peak detection, grouping and alignment, CAMERA and feature
  annotation by comparison to a database of standards. The function also
  calculates the mass tolerance on the bases of the ion intensity and ion
  mass.
}
\usage{
runLC(files, settings, rtrange = NULL, mzrange = NULL, DB = NULL,
      polarity = "positive", errf = NULL, runCAMERA = TRUE,
      returnXset = FALSE, intensity = "into")
}

\arguments{
  \item{files}{input files, given as a vector of strings containing the
    complete paths. All formats supported by XCMS can be used.}
  \item{settings}{nested list of settings, to be used at individual
    steps of the pipeline. See the help of \code{FEMsettings} for details.}
  \item{rtrange}{An optional vector to slice the retention time range
  (in minutes).}
  \item{mzrange}{An optional vector to slice the mass spectrum}
  \item{DB}{database containing the spectra of the pure standards. For
  the description refer to the \code{LCDBtest} help.}
  \item{polarity}{The polarity of the analysis used for CAMERA
  annotation. Either "positive" or "negative".}
  \item{errf}{A linear model used to calculate the mass tolerance in ppm
  from the \code{log10(I)} and \code{m/z}. It should be called  as
  \code{predict(errf, data.frame(M = mz, logI = log10(maxo)))}. For
  further details refer to the help of \code{AnnotateTable}.}
  \item{runCAMERA}{Logical. Should CAMERA annotation be performed?}
  \item{returnXset}{ logical: should the XCMS output be returned? If yes,
    this is a a list of \code{xcmsSet} objects, one element for each
    input file.}
  \item{intensity}{The intensity measure used in the output
  peaktable. The available intensities are the ones provided by
  \code{xcms}. The default one is the total intensity (integral) of the
  feature on the detected chromatographic peak.}
}
\details{The mz range and rtrange parameters are used to subset the mass
  and retantion times considered in the analysis, thus reducing possible
  alignment problems at the boundaries. 
  If the error function is not provided the mass tolerance will be fixed
  to the value defined in the \code{settings} list.}  
\value{
  A list with three elements:
  \item{PeakTable}{data.frame containing annotation information. Every
    line is a feature. The first columns are used
    to give information about these features, annotation, CAMERA,
    Chemspider IDs, etcetera. The last of these meta-information columns
    is always the one giving the retention time: "rt". After that, columns
    correspond to input files, and give measures of intensities for every
    single one of the features.}
  \item{Annoation}{The complete output of the \code{AnnotateTable} function.}
  \item{Settings}{The settings used in the pipeline.}
  \item{xset}{optionally, the xcmsSet (CAMERA) object is returned, which can be
    useful for more detailed inspection of the results.}
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
if (require(metaMSdata)) {
  
  ## get the path 
  cdfpath <- system.file("CDF_LC", package = "metaMSdata")
  
  ## files 
  files <- list.files(cdfpath, "CDF", full.names=TRUE)
  
  
  ## load the settings for the analysis
  data(FEMsettings)
  
  ## load the annotation DB
  data(LCDBtest)
  
  ## Run the analysis with a fixed mass tolernace 
  result <- runLC(files, settings = FEMsettings$Synapt.QTOF.RP, 
                  DB = LCDBtest$DB)
}
}

\keyword{manip}

