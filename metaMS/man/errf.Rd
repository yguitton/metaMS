\name{errf}
\alias{errf}
\docType{data}
\title{Mass error surface for Waters Synapt Q-TOF spectrometers}
\description{This function can be used to calculate the optimal mz tolerance for annotation. The surface has been developed for a Waters Synapt QTOF spectrometer }
\usage{data(errf)}
\format{
  \code{errf} is a linear model of the form \code{lm(formula = err ~ M + logI + M * logI, data = linear.approx). \cr
  \describe{
    \item{err}{The mass tolerance}
    \item{M}{The mz of the ion} 
    \item{logI}{Logarithm of the intensity of the \code{M} ion.} 
  }
}
}
\details{
 The function is a linear approaximation of the complete error function (see Reference). The latter has been calculated by comparing the measured and nominal mass of several hundreds of standard. The experiments were performed by using a Waters Synapt Q-TOF spectrometer so this specific surface is valid for this model of spectrometers.
}


\references{
N. Shahaf, P. Franceschi, P. Arapitsas, I. Rogachev, U. Vrhovsek and
R. Wehrens: "Constructing a mass measurement error surface to improve
automatic annotations in liquid chromatography/mass spectrometry based
metabolomics". Rapid Communications in Mass Spectrometry, 27(21), 2425
(2013).}

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
  
  
  ## <--------------    Use the Synapt Q-TOF error function     -------------- >
  data(errf)
  
  result.adaptive  <- runLC(files, settings = FEMsettings$Synapt.QTOF.RP, 
                                 DB = LCDBtest$DB, errf = errf)
  
                  
}
}
\keyword{datasets}
