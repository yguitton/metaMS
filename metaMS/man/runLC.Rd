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
  \item{errf}{A model used to calculate the mass tolerance in ppm
  for each features on the bases of its mass, retention time and intensity.
  For further details refer to the help of \code{AnnotateTable}.}
  \item{runCAMERA}{Logical. Should CAMERA annotation be performed?}
  \item{returnXset}{ logical: should the XCMS output be returned? If yes,
    this is a a list of \code{xcmsSet} objects, one element for each
    input file.}
  \item{intensity}{The intensity measure used in the output
  peaktable. The available intensities are the ones provided by
  \code{xcms}. The default one is the total intensity (integral) of the
  feature on the detected chromatographic peak.}
}
\details{The \code{mzrange} and \code{rtrange} parameters are used to
  subset the mass and retention times considered in the analysis,
  reducing possible alignment problems at the extremes. \cr
  The error function calculates the expected m/z tolerance for feature
  annotation based on the mz and I values of each feature. To have a
  more complete description of the process refer to the help of
  \code{AnnotateTable} and the literature reference. An example is
  provided as well. Note that the use of "lm" is only one of the
  possible choices, but all kind of functional approximations working
  with the \code{predict} function could be used. If the error function
  is not provided the mass tolerance will be fixed to the value defined
  in the \code{settings} list.}
\value{
  A list with three elements:
  \item{PeakTable}{data.frame containing annotation information. Every
    line is a feature. The first columns are used to give information
    about these features, annotation, CAMERA, Chemspider IDs,
    etcetera. The last of these meta-information columns is always the one
    giving the retention time: "rt". After that, columns correspond to
    input files, and give measures of intensities for every single one of
    the features.}
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
  
  ## <--------------    create a dummy error function     -------------- >
  ## dataset to evaluate it: 
  ## "M" is the mz, 
  ## "logI" is the log of the intensity
  ## "err" is the mass error in ppm. The error is the difference between the 
  ## "real" m/z of a known ion, and the one actually measured with the
  ## spectrometer
  
  MErr.data <- data.frame("M" = seq(1,500,2), 
                          "logI" = rnorm(250, mean = 5, sd = 1), 
                          "err" = rnorm(250, mean = 40, sd = 5))
  
  ## create the linear model
  dummy.model <- lm(err~M+logI, data = MErr.data)


  ## Run the analysis with an adaptive mass tolernace 
  result.adaptive.dummy <- runLC(files, settings = FEMsettings$Synapt.QTOF.RP, 
                                 DB = LCDBtest$DB, errf = dummy.model)
  
  
  
  ## <-------------    Use the Synapt Q-TOF error function     -------------- >
  data(errf)
  
  result.adaptive <- runLC(files, settings = FEMsettings$Synapt.QTOF.RP, 
                           DB = LCDBtest$DB, errf = errf)
  
  
  ## <--------    Run the analysis with a fixed mass tolerance      --------- >
  resultfixed <- runLC(files, settings = FEMsettings$Synapt.QTOF.RP, 
                       DB = LCDBtest$DB)
                  
                 
}
}

\keyword{manip}

