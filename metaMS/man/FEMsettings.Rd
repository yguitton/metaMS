\name{FEMsettings}
\alias{FEMsettings}
\docType{data}
\title{FEM Settings for \code{metaMS}}
\description{Example of settings needed for functions \code{runLC} and
  \code{runGC}. These particular settings are fine-tuned for the analysis of
  LC-MS runs, both normal-phase and reverse-phase chromatography (Waters
  Synapt G1)and GC-MS experiments (ThermoXLS TQQ). Object
  \code{FEMsettings} contains three elements.
}
\usage{data(FEMsettings)}
\format{\code{FEMsettings} is a list in which each element collects the
  settings used to analyze a specific type of data. Some elements are
  common for LC-MS and GC-MS:
  \describe{
    \item{instName}{One string identifying the instrument.}
    \item{chrom}{The type of chromatography: should be either "GC" or "LC"}
    \item{PeakPicking}{The parameters used for \code{xcms}
      peakpicking. This is a list with elements like \code{method}
      (currently either "matchedFilter" or "centWave", \code{step},
      etcetera. See the arguments of \code{\link{findPeaks}}.}
  }

  Other elements have a common purpose but a different implementation,
  or are completely different.
  
  LC-MS-specific elements:
  \describe{
    \item{Alignment}{The parameters used for grouping and
      alignment. \code{min.class.fraction} and \code{min.class.size} are
      used to calculate the \code{minsample} xcms parameter. \code{bws} is a
      vector of the two bandwidths used for grouping before and after
      retention time alignent. \code{missingratio} and \code{extraratio} are
      used to set the values for \code{missing} and \code{extra} as a
      function of the number of samples.}
    \item{CAMERA}{The parameters for CAMERA.}
    \item{match2DB}{The parameters used for the annotation of the
      features by using a DB. \code{rttol} the tolerance in retention
      time to match each feature with the entry in the DB. \code{rtval}
      the tolerance in retention time used to validate the annotation:
      one annotation for a compond is retained if, in the peaklist, more than
      \code{minfeat} matching  within a \code{rtval} window are
      found. \code{mzwindow}: the m/z fixed mass tolerance which is used
      if no error surface is provided. \code{ppm}: the minimum mass
      tolerance allowed when the error surface is used.} 
    \item{DBconstruction}{the settings used to construct the DB from a
      manually validated table. \code{minfeat} the minimum number of
      features necessary to include a compound in the
      DB. \code{rttol,mztol} the tolerances (in minutes and dalton) used
      to match the data in the reference table and the automatically
      generated feature list.}
  }

  GC-MS-specific elements:
  \describe{
    \item{CAMERA}{Parameters for CAMERA - for GC only \code{perfwhm} is
      needed, indicating the spread in retention time of features that
      will be grouped together into one pseudospectrum.}
    \item{DBconstruction}{Settings for building an in-house library of
      standards. A list containing several slots that
      determines which features will be included in the pseudospectra,
      and which pseudospectra are valid DB entries:
      \code{minintens} contains the minimum intensity for a feature to
      be included in the list. Setting this to zero leads to no
      filtering; \code{intensity.measure}: one of "into" or "maxo";
      \code{minfeat}: the minimal number of features in one
      pseudospectrum; \code{rttol}: maximal difference of pseudospectrum
      to the manually identified retention time of a standard (in
      minutes); \code{DBthreshold}: minimal match factor with an external
      DB for a pseudospectrum to be included in the DB of standards.}
    \item{match2DB}{Settings for comparing pseudospectra with the
      in-house DB of standards. A list with the following elements:
      \code{simthresh}: the minimal match factor to speak of a
      hit; \code{rtdiff}: maximal retention time difference with DB
      entry (in minutes); \code{minfeat}: minimal number of common
      masses for calculating a match factor.}
    \item{matchIrrelevants}{Often it is known that particular patterns
      will be present, but they may still be irrelevant. Examples are
      patterns from plasticizers or bleeding, or in some cases also internal
      standards. This list element contains settings from removing such
      irrelevant patterns. It is a list with several elements:
      \code{irrelevantClasses}: identify which classes of compounds are
      considered as irrelevant (a vector of string constants, which
      should exactly match the \code{Class} element in the DB entries);
      \code{simthresh}: the minimal match factor to speak of a
      hit; \code{rtdiff}: maximal retention time difference with DB
      entry (in minutes) - this can be set to a very high value if a
      pattern is to be removed whatever the retention time.}
    \item{betweenSamples}{Settings for comparing pseudospectra across
      samples. This is relevant when defining "Unknowns". Apart from the
      fields \code{rtdiff} and \code{simthresh} two other fields are
      used to determine in how many of the samples a pseudospectrum
      should be found, before it can be regarded as an "unknown": the
      absolute number is given by \code{min.class.size}, and the minimal
      fraction by \code{min.class.fraction}. The smallest of these will
      be used. Suppose that a pattern is found in three out of ten
      samples, it will be accepted as an unknown if either
      \code{min.class.size} is smaller than or equal to three, or if
      \code{min.class.fraction <= 0.3}.}
  }
}
\seealso{\code{\link{findPeaks}}, \code{\link{annotate}}}
\author{Ron Wehrens and Pietro Franceschi}
\keyword{datasets}
