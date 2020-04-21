\name{matchSamples2Samples}
\alias{matchSamples2Samples}
\alias{match.unannot.patterns}
\title{Compare pseudospectra across samples (GC-MS)}
\description{
  Function \code{matchSamples2Samples} matches pseudospectra across all
  samples - if a pseudospectrum is present at more or less the same
  retention time in several samples, it can get the status of
  \dQuote{unknown}. Exactly how much difference there may be between the
  pseudospectra and retention times, and how often it should be present,
  is determined by the settings. The auxiliary function
  \code{match.unannot.patterns} compares two msp objects, representing
  experimental samples. Both are not meant to be called directly by the
  user.}
\usage{
matchSamples2Samples(xset.msp.scaled, xset.msp, annotations, settings)
match.unannot.patterns(msp1, msp2, settings)
}
\arguments{
  \item{xset.msp.scaled}{Scaled version of all pseudospectra in the
    experimental patterns - a nested list, with one entry for each
    sample, and within every entry an element for each pseudospectrum.}
  \item{xset.msp}{Unscaled version of the first argument: both arguments
    are provided for efficiency reasons.}
  \item{annotations}{Annotations of all pseudospectra: only patterns
    without annotations will be considered.}
  \item{settings}{Settings determining what a valid \dQuote{unknown} is. For an
    example, see the man page of \code{FEMsettings},
    field \code{matchIrrelevants}.}
  \item{msp1, msp2}{lists of pseudospectra}
}
\value{Function \code{matchSamples2Samples} returns an updated
  annotation object such as the one 
  returned by \code{matchSamples2DB}, but now with an additional
  \code{unknowns} element, containing the pseudospectra that are
  recognized as \dQuote{unknowns}.

  Function \code{match.unannot.patterns} returns a list of combinations
  of pseudospectra IDs, retention times (or retention indices) and match
  factors (only for those combinations that satisfy the criteria on
  retention time (index) and match factor).}
\author{Ron Wehrens}
\examples{
## Example of settings
  data(FEMsettings)
  metaSetting(object = TSQXLS.GC, field= "betweenSamples")
}
\keyword{manip}
