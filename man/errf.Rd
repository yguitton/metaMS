\name{errf}
\alias{errf}
\docType{data}
\title{Mass error surface for Waters Synapt Q-TOF spectrometers}
\description{This function can be used to calculate the optimal mz
  tolerance (in ppm) for annotation. The surface has been developed for
  a Waters Synapt QTOF spectrometer.} 
\usage{data(errf)}
\format{
  \code{errf} is a linear model of the form \code{M + logI + M * logI}:
  \describe{
    \item{M}{The mz of the ion} 
    \item{logI}{Logarithm of the intensity of the ion.} 
  }
}
\details{
  The function is a linear approaximation of the complete error function
  (see Reference). The latter has been calculated by comparing the
  measured and nominal mass of several hundreds of standards. The
  experiments were performed by using a Waters Synapt Q-TOF spectrometer
  so this specific surface is valid only for this model of spectrometers. 
}


\references{
N. Shahaf, P. Franceschi, P. Arapitsas, I. Rogachev, U. Vrhovsek and
R. Wehrens: "Constructing a mass measurement error surface to improve
automatic annotations in liquid chromatography/mass spectrometry based
metabolomics". Rapid Communications in Mass Spectrometry, 27(21), 2425
(2013).}

\author{Pietro Franceschi}
\examples{
## <--------------    direct use of the error function     -------------- >
## load the Synapt-QTOF error function
data(errf)

## predict the mass error in ppm
newdata <- data.frame(M = c(105, 131, 157), logI = c(1, .5, 1.4))
predict(errf, newdata) ## mass error in ppm


## <--------------    create a dummy error function     -------------- >
## dataset to evaluate it: 
## "M" is the mz, 
## "logI" is the log of the intensity
## "err" is the mass error in ppm. The error is the difference between the 
## actual m/z of a known ion, and the one measured with the spectrometer
  
MErr.data <- data.frame("M" = seq(1,500,2), 
                        "logI" = rnorm(250, mean = 5, sd = 1), 
                        "err" = rnorm(250, mean = 40, sd = 5))
  
## create the linear model
dummy.model <- lm(err~M+logI, data = MErr.data)

\dontrun{
## <--------------    Use this for the annotation     -------------- >
## load the example xcmsSet
data(LCresults)
data(FEMsettings)

## Run the analysis with an adaptive mass tolerance 
result.adaptive.dummy <- runLC(xset = LCresults$xset,
                               settings = Synapt.RP, 
                               DB = LCDBtest$DB,
                               errf = dummy.model)

## <-----------    Use the Synapt Q-TOF error function     ----------- >
## load the Synapt-QTOF error function
data(errf)

result.adaptive  <- runLC(xset = LCresults$xset,
                          settings = Synapt.RP, 
                          DB = LCDBtest$DB,
                          errf = errf)
}
}
\keyword{datasets}
