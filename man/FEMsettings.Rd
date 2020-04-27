\name{FEMsettings}
\alias{FEMsettings}
\alias{Synapt.RP}
\alias{Synapt.NP}
\alias{TSQXLS.GC}
\alias{Orbitrap.RP}
\docType{data}
\title{Example settings for \code{metaMS}}
\description{Examples of settings needed for functions \code{runLC} and
  \code{runGC}: Synapt.RP, Synapt.NP, TSQXLS.GC and Orbitrap.RP. These four
  particular settings are fine-tuned for the analysis of LC-MS runs,
  both normal-phase and reverse-phase chromatography (Waters Synapt
  G1-Thermo Orbitrap)and GC-MS experiments (ThermoXLS TQQ).
}
\usage{data(FEMsettings)}
\format{Four objects of class \code{metaMSsettings}.}
\seealso{\code{\link{findPeaks}}, \code{\link{annotate}}}
\author{Ron Wehrens and Pietro Franceschi}
\examples{
\dontrun{
## The three sets of settings are created as follows:
Synapt.NP <- metaMSsettings(protocolName = "Synapt.QTOF.NP",
                            chrom = "LC",
                            PeakPicking = list(
                              method = "matchedFilter",
                              step = 0.05,
                              fwhm = 20,
                              snthresh = 4,
                              max = 50),
                            Alignment = list(
                              min.class.fraction = .3,
                              min.class.size = 3,
                              mzwid = 0.1,
                              bws = c(130, 10),
                              missingratio = 0.2,
                              extraratio = 0.1,
                              Retcor = list(
                                method = "linear",
                                family = "symmetric"),            
                              fillPeaks = TRUE),
                            CAMERA = list(
                              perfwhm = 0.6,
                              cor_eic_th = 0.7,
                              ppm= 5))
metaSetting(Synapt.NP, "match2DB") <- list(
            rtdiff = 1.5,
            rtval = .1,
            mzdiff = 0.005,
            ppm = 5,
            minfeat = 2)
metaSetting(Synapt.NP, "DBconstruction") <- list(
            minfeat = 3,
            rttol = .3,
            mztol = .01)

## For reverse-phase LC, settings are very similar: the only difference
## is in the alignment settings
Synapt.RP <- Synapt.NP
metaSetting(Synapt.RP, "protocolName") <- "Synapt.QTOF.RP"
metaSetting(Synapt.RP, "Alignment") <- list(
            min.class.fraction = .3,
            min.class.size = 3,
            mzwid = 0.1,
            bws = c(30, 10),
            missingratio = 0.2,
            extraratio = 0.1,
            Retcor = list(
              method = "linear",
              family = "symmetric"),           
            fillPeaks = TRUE)


## For the orbitrap.RP
Orbitrap.RP <- metaMSsettings(protocolName = "Orbitrap",
                              chrom = "LC",
                               PeakPicking = list(
                                 method = "centWave",
                                 ppm = 5,
                                 prefilter = c(3,10000),
                                 peakwidth = c(15,40)),
                             Alignment = list(
                                 bws = 30,
                                 min.class.fraction = 0.3,
                                 min.class.size = 3,
                                 mzwid = 0.01,
                                 Retcor = list(
                                  method = "obiwarp",
                                  profStep = 0.2),
                                 fillPeaks = TRUE),
                             CAMERA = list(
                                 perfwhm = 0.6,
                                 cor_eic_th = 0.7,
                                 ppm = 5))
metaSetting(Orbitrap.RP, "match2DB") <- list(
            rtdiff = 1.5,
            rtval = .1,
            mzdiff = 0.005,
            ppm = 5,
            minfeat = 2)
metaSetting(Orbitrap.RP, "DBconstruction") <- list(
            minfeat = 3,
            rttol = .3,
            mztol = .01)

## For the thermo TQ

TSQXLS.GC <- metaMSsettings("protocolName" = "TSQXLS.QQQ.GC",
                            "chrom" = "GC",
                            PeakPicking = list(
                              method = "matchedFilter",
                              step = 0.5,
                              steps = 2,
                              mzdiff = .5,
                              fwhm = 5,
                              snthresh = 2,
                              max = 500),
                           CAMERA = list(perfwhm = 1))
metaSetting(TSQXLS.GC, "DBconstruction") <- list(
            minintens = 0.0,
            rttol = .1,
            intensityMeasure = "maxo",
            DBthreshold = .80, 
            minfeat = 5)
metaSetting(TSQXLS.GC, "match2DB") <- list(
            simthresh = 0.80,
            timeComparison = "rt",
            rtdiff = .5,
            RIdiff = 5,
            minfeat = 2)
metaSetting(TSQXLS.GC, "matchIrrelevants") <- list(
            irrelevantClasses = c("Bleeding", "Plasticizers"),
            timeComparison = "rt",
            RIdiff = 2,    
            rtdiff = .05,
            simthresh = 0.70)
metaSetting(TSQXLS.GC, "betweenSamples") <- list(
            min.class.fraction = .5,
            min.class.size = 5,
            timeComparison = "rt",
            rtdiff = .05,
            RIdiff = 2,    
            simthresh = .95)              
}
}
\keyword{datasets}
