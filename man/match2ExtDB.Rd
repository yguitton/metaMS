\name{match2ExtDB}
\alias{match2ExtDB}
\title{Match GC-MS spectra to an external reference DB}
\description{
  When building an in-house database, it is imperative that
  pseudospectra are validated. This function provides the possibility of
  automatic validation by comparing the spectra to an external reference
  database, such as the NIST. Not meant to be called directly by the user.
}
\usage{
match2ExtDB(xsetList, extDB, settings)
}
\arguments{
  \item{xsetList}{A list of xcmsSet objects.}
  \item{extDB}{External database in msp format.}
  \item{settings}{Settings for the comparison, including as the most
    important the minimal number of features required in a valid
    pseudospectrum (\code{minfeat}), and the minimal agreement to speak
    of a match (\code{DBthreshold}).
  }
}
\value{An msp object containing validated pseudospectra.}
\author{Ron Wehrens}
\keyword{manip}
