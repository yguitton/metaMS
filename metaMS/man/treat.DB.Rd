\name{treat.DB}
\alias{treat.DB}
\title{Scaling of pseudospectra in an msp object}
\description{
  This function transforms the \dQuote{raw} data in an msp DB object into
  preprocessed data. Even if no scale and no mass.weight is applied,
  the intensities are still changed: scaled to unit length.}
\usage{
treat.DB(DB, scale.p = c("sqrt", "none"), mass.weight = TRUE, isMSP = TRUE)
}
\arguments{
  \item{DB}{A database of spectra in the original intensity units.}
  \item{scale.p}{scale intensities with a square-root function, or leave
    them as they are. Default is to use scaling.}
  \item{mass.weight}{Logical: if TRUE, higher masses are given more
    weight.}
  \item{isMSP}{Logical: if TRUE, then the spectra are stored in slot
    \code{pspectrum}; otherwise the spectra are simply the list
    elements of \code{DB}.}
}
\value{
  The function returns the database, where intensities are scaled.
}
\author{Ron Wehrens}
\examples{
data(threeStdsNIST)  ## provides object smallDB, excerpt from NIST DB
smallDB.scaled <- treat.DB(smallDB)
}
\keyword{manip}
