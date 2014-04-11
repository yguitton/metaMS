\name{metaMSsettings-class}
\Rdversion{1.1}
\docType{class}
\alias{metaMSsettings-class}
\alias{metaMSsettings}
%\alias{metaSetting<-,metaMSsettings}
%\alias{metaSetting,metaMSsettings}
\alias{show,metaMSsettings-method}

\title{Class \code{"metaMSsettings"}}
\description{
  This class contains all settings needed to run the metaMS pipelines
  for LC-MS (\code{runLC}) and GC-MS (\code{runGC}). Slots
  \code{PeakPicking}, \code{Alignment} and \code{CAMERA} are simply
  handed over to the appropriate \pkg{xcms} and \pkg{CAMERA} functions;
  all other slots contain settings for \pkg{metaMS} functions.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{metaMSsettings(...)}. See the example below.
}
\section{Slots}{
  Note: all slots describing retention times or retention time differences use
  minutes and not seconds. If a slot is only relevant for either GCMS or
  LCMS, this is indicated explicitly.
  \describe{
    \item{\code{protocolName}:}{Object of class \code{"character"}: the name
    of the instrumental protocol, a unique identifier.}
    \item{\code{chrom}:}{Object of class \code{"character"}:
      chromatography. Either "LC" or "GC".}
    \item{\code{PeakPicking}:}{Object of class \code{"list"}: The
      parameters used for \pkg{xcms} peakpicking. See the arguments of
      \code{\link{findPeaks}}.}
    \item{\code{Alignment}:}{Object of class \code{"list"}: The
      parameters used for grouping and
      alignment. \code{min.class.fraction} and \code{min.class.size} are
      used to calculate the \code{minsample} xcms parameter. \code{bws}
      is a vector of the two bandwidths used for grouping before and
      after retention time alignent. \code{missingratio} and
      \code{extraratio} are used to set the values for \code{missing}
      and \code{extra} as a function of the number of samples. LC only.}
    \item{\code{CAMERA}:}{Object of class \code{"list"}: The parameters
      for CAMERA.} 
    \item{\code{match2DB.rtdiff}:}{Object of class \code{"numeric"}: the
      maximal difference in retention time to match each feature with the entry
      in the DB.} 
    \item{\code{match2DB.minfeat}:}{Object of class \code{"numeric"}:
      for LC, the minimal number of matching features within a retention time
      interval of width \code{rtval} before we speak of a hit. For GC,
      the minimal number of common masses for calculating a match factor.}
    \item{\code{match2DB.rtval}:}{Object of class \code{"numeric"}: the
      tolerance in retention time for features used in annotation. LC only.}
    \item{\code{match2DB.mzdiff}:}{Object of class \code{"numeric"}:
      the mass accuracy which is used if no error surface is
      provided. LC only.} 
    \item{\code{match2DB.ppm}:}{Object of class \code{"numeric"}: the
      minimum mass tolerance allowed when the error surface is used. LC
      only.}
    \item{\code{match2DB.simthresh}:}{Object of class \code{"numeric"}:
      the minimal match factor to speak of a hit. GC only.}
    \item{\code{match2DB.timeComparison}:}{Object of class
      \code{"character"}: either "rt" or "RI". GC only.}
    \item{\code{match2DB.RIdiff}:}{Object of class \code{"numeric"}:
      maximal retention index difference with DB entry (GC only). }
    \item{\code{DBconstruction.minfeat}:}{Object of class
      \code{"numeric"}: the minimum number of
      features necessary to include a compound in the DB.
    }
    \item{\code{DBconstruction.rttol}:}{Object of class
      \code{"numeric"}: the tolerance in retention
      time to match experimental features with the reference table.}
    \item{\code{DBconstruction.mztol}:}{Object of class
      \code{"numeric"}: the tolerance in m/z (in dalton) to match
      experimental features with the reference table. LC only.}
    \item{\code{DBconstruction.minintens}:}{Object of class
      \code{"numeric"}: the minimum intensity for a feature to
      be included in the list. GC only.}
    \item{\code{DBconstruction.intensityMeasure}:}{Object of class
      \code{"character"}: either \code{"into"} or \code{"maxo"}. GC only.}
    \item{\code{DBconstruction.DBthreshold}:}{Object of class
      \code{"numeric"}: minimal match factor with an external
      DB for a pseudospectrum to be included in the DB of standards. GC
      only.}
    \item{\code{matchIrrelevants.irrelevantClasses}:}{Object of class
      \code{"character"}: classes of compounds are
      considered as irrelevant (a vector of string constants, which
      should exactly match the \code{Class} element in the DB entries). GC
      only.}
    \item{\code{matchIrrelevants.simthresh}:}{Object of class
      \code{"numeric"}: the minimal match factor to speak of a
      hit. GC only.}
    \item{\code{matchIrrelevants.timeComparison}:}{Object of class
      \code{"character"}: either "rt" or "RI". GC only.}
    \item{\code{matchIrrelevants.rtdiff}:}{Object of class
      \code{"numeric"}: maximal retention time difference between two
      unknowns - this can be set to a very high value if a 
      pattern is to be removed whatever the retention time. GC only.}
    \item{\code{matchIrrelevants.RIdiff}:}{Object of class \code{"numeric"}:
      maximal retention index difference between two unknowns - this can
      be set to a very high value if a pattern is to be removed whatever
      the Retention Index. GC only.}
    \item{\code{betweenSamples.min.class.fraction}:}{Object of class
      \code{"numeric"}: fraction of samples in which a pseudospectrum is
      present before it is regarded as an unknown. GC only.}
    \item{\code{betweenSamples.min.class.size}:}{Object of class
      \code{"numeric"}: absolute number of samples in which a pseudospectrum is
      present before it is regarded as an unknown. GC only.}
    \item{\code{betweenSamples.timeComparison}:}{Object of class
      \code{"character"}: either "rt" or "RI". GC only.}
    \item{\code{betweenSamples.rtdiff}:}{Object of class
      \code{"numeric"}: max retention time difference between
      pseudospectra in different samples. GC only.}
    \item{\code{betweenSamples.RIdiff}:}{Object of class
      \code{"numeric"}: max retention index difference between
      pseudospectra in different samples. GC only.}
    \item{\code{betweenSamples.simthresh}:}{Object of class
      \code{"numeric"}: similarity threshold fo comparing pseudospectra
      in different samples. GC only.}
  }
}
\section{Methods}{
  \describe{
    \item{metaSetting<-}{\code{signature(object = "metaMSsettings")}:
      change values in a metaMSsettings object.}
    \item{metaSetting}{\code{signature(object = "metaMSsettings")}: get
      values from a metaMSsettings object. }
    \item{show}{\code{signature(object = "metaMSsettings")}: show a
      metaMSsettings object.}
  }
}
\author{Ron Wehrens}
\seealso{\code{\link{FEMsettings}}}
\examples{
showClass("metaMSsettings")

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
                              retcormethod = "linear",
                              retcorfamily = "symmetric",            
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
}
}
\keyword{classes}
