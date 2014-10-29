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
runLC(files, xset, settings, rtrange = NULL, mzrange = NULL, DB = NULL,
      polarity = "positive", errf = NULL, returnXset = FALSE,
      intensity = "into", nSlaves = 0)
}

\arguments{
  \item{files}{input files, given as a vector of strings containing the
    complete paths. All formats supported by XCMS can be used.}
  \item{xset}{alternatively, one can present an object of class
    \code{xsAnnotate}, instead of the input files. In that case all of the
    XCMS and CAMERA tools: peak picking, CAMERA grouping, retention time
    correction and filling of missing peaks will be considered already
    done, and only annotation will be performed. This can be useful if one
    wants to compare different settings for annotation. If both
    \code{files} and \code{xset} are given, the former takes precedence.}
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
  \item{returnXset}{logical: should the XCMS output be returned? If yes,
    this is a a list of \code{xcmsSet} objects, one element for each
    input file.}
  \item{intensity}{The intensity measure used in the output
    peaktable. The available intensities are the ones provided by
    \code{xcms}. The default one is the total intensity (integral) of the
    feature on the detected chromatographic peak.}
  \item{nSlaves}{Number of cores to be used in peak picking.}
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
  \item{xset}{optionally, the xcmsSet/CAMERA object is returned, which can be
    useful for more detailed inspection of the results.}
  \item{sessionInfo}{The output of \code{sessionInfo()} to keep track of the sw
  version used for the processing}
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
data(LCresults)
names(LCresults)

\dontrun{
## load the settings for the analysis
data(FEMsettings)
  
## load the annotation DB
data(LCDBtest)

## load the Synapt Q-TOF error function
data(errf)

results.xset  <- runLC(xset = LCresults$xset, settings = Synapt.RP, 
                       DB = LCDBtest$DB)

## to start directly from the CDF files and include peak picking in the
## pipeline, simply provide the "files" argument rather than the "xset" argument

if (require(metaMSdata)) {
  ## get the path 
  cdfpath <- system.file("extdata", package = "metaMSdata")
  
  ## files 
  files <- list.files(cdfpath, "_RP_", full.names=TRUE)
    
  ## <-------------    Use the Synapt Q-TOF error function     -------------- >
  result.adaptive <- runLC(files, settings = Synapt.RP, 
                           DB = LCDBtest$DB, errf = errf)
  
  ## <--------    Run the analysis with a fixed mass tolerance      --------- >
  result.fixed <- runLC(files, settings = Synapt.RP, 
                        DB = LCDBtest$DB)
}            
}
}
\keyword{manip}

