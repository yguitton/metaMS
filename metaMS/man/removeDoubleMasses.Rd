\name{removeDoubleMasses}
\alias{removeDoubleMasses}
\title{Remove double m/z entries in a pseudospectrum}
\description{
  Since in nominal-mass GC data m/z values are rounded to whole numbers, in
  some cases an m/z value may occur multiple times - in that case the
  mean of the intensities is kept (and the retention times are averaged). 
  removeDoubleMasses takes a list of spectra in the form of a
  three-column matrix, (mz, I, rt), summing the intensities and
  averaging the retention times for multiple identical masses. Not meant
  to be called directly by the user.
}
\usage{removeDoubleMasses(spclist)}
\arguments{
  \item{spclist}{A list of spectra, each one consisting of a
    three-column matrix (mz, I, rt).}
}
\value{The function returns a list of spectra, where all "double" peaks
  have been averaged.}
\author{Ron Wehrens}
\keyword{manip}
