\name{readStdInfo}
\alias{readStdInfo}
\title{Read information of GC injections of standards from a csv file}
\description{
  The csv contains all information necessary to process a series of
  injections of standards (GC-MS). Required fields: \code{Name}, \code{RTman},
  \code{monoMW}, \code{stdFile}, and an identifier such as \code{CAS} or
  \code{ChemspiderID}. At the moment, the system is completely based on
  \code{CAS}, although this may change in the future.}
\usage{
readStdInfo(stdInfo, InputDir, sep = "", dec = ".", ...)
}
\arguments{
  \item{stdInfo}{Input file in csv format, containing information on
    standards.}
  \item{InputDir}{Location of input files.}
  \item{sep, dec, \dots}{optional arguments to \code{read.table}.}
}
\details{In addition to reading all information on the chemical
  standards (whcih will eventually be transferred into an in-house
  database), the function checks whether some input files are
  unavailable, and whether some data files are not used. In the first
  case, an error is returned, in the second case a warning.}
\value{
    A data.frame  
}
\examples{
if (require(metaMSdata)) {
  ## this will lead to the completed version of the R object that is also
  ## available by typing "data(threeStdsInfo)", now containing the
  ## directory information that is not available in the RData object.

  input.file <- list.files(system.file("extdata", package = "metaMSdata"),
                           pattern = "csv", full.names = TRUE)
  threeStdsInfo <- readStdInfo(input.file,
                               system.file("extdata", package = "metaMSdata"),
                               sep = ";", dec = ",")
  ## only one of the files is used to set up the database, the others
  ## are for testing annotation
}
}
\author{Ron Wehrens}
\keyword{manip}
