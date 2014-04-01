\name{addRI}
\alias{addRI}
\title{Add retention index information to an msp object}
\description{
Given an msp object and the retention times and indices of a series of
reference compounds, the function adds an RI field to every entry of the
msp object. This will only be done if there is not already an RI field:
existing information will not be overwritten.
}
\usage{
addRI(mspobj, RIstandards, isMSP = TRUE)
}
\arguments{
  \item{mspobj}{An msp object.}
  \item{RIstandards}{A two-column matrix containing for the standards
    defining the RI scale both retention times and retention indices.}
  \item{isMSP}{Logical: if TRUE, then the spectra are stored in slot
    \code{pspectrum}; otherwise the spectra are simply the list
    elements of \code{DB} - matrices with three columns.}
}
\value{An msp object, now also containing an RI slot.}
\author{Ron Wehrens}
\note{If the retention time of a compound is outside the range of the RI
  standards, NA will be used as RI value.
}
\examples{
if (require(metaMSdata)) {
  manual.fname <- list.files(system.file("extdata", package = "metaMSdata"),
                             pattern = "msp", full.names = TRUE)
  manual <- read.msp(manual.fname)
  RIstandards <- cbind("rt" = c(1.54, 1.68, 1.99, 2.7, 4.36, 6.81, 9.43,
                                11.88, 14.17, 16.34, 18.39, 20.33, 22.18,
                                23.93, 25.5, 27.18, 28.72, 30.26, 31.75,
                                33.19, 34.58, 35.95),
                       "RI" = (6:27)*100)

  manualRI <- addRI(manual, RIstandards)
}
}
\keyword{manip}
