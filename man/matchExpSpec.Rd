\name{matchExpSpec}
\alias{matchExpSpec}
\alias{mzmatch}
\title{Match a GC-MS pseudospectrum to a database with a weighted
  crossproduct criterion.}
\description{
  Function \code{matchExpSpec} calculates match factors for a
  pseudospectrum with all entries in the database. A plot of the best
  match can be provided. Function \code{mzmatch} is an auxiliary
  function, not meant to be called directly, that provides the match
  factor, given two appropriately scaled patterns.
}
\usage{
matchExpSpec(pspec, DB, DB.treated, plotIt = FALSE,
             scale.p = c("sqrt", "none"), mass.weight = TRUE, ...)
mzmatch(spec1, spec2)
}
\arguments{
  \item{pspec}{The pseudospectrum, a two- or three-column matrix where
    the third column (the retention time) will be ignored.}
  \item{DB}{Database of standards.}
  \item{DB.treated}{Logical, indicating whether the database has already
    been scaled (TRUE) or not.}
  \item{plotIt}{Logical: show best match?}
  \item{scale.p}{indicates whether \code{"sqrt"} scaling or no scaling
    (\code{"none"}) is to be applied. This should usually be \code{"sqrt"}.}
  \item{mass.weight}{Logical, indicating whether heavier masses receive
    higher weight. Should usually be TRUE.}
  \item{\dots}{Further arguments for the pseudospectrum plot (if
    shown).}
  \item{spec1, spec2}{Preprocessed pseudospectra, here two-column
    matrices where the first column is the (integer) m/z value, and the
    second column the (scaled) intensity.}
}
\value{A vector of match factors.}
\author{Ron Wehrens}
\examples{
data(threeStdsNIST) ## gives smallDB, containing 78 patterns
data(threeStdsDB)   ## gives DB, containing 3 patterns :-D

matchExpSpec(DB[[1]]$pspectrum, smallDB, DB.treated = FALSE, plotIt = TRUE)
}
\keyword{manip}
