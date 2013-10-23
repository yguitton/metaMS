\name{threeStdsDB}
\alias{threeStdsInfo}
\alias{stdInfo}
\alias{threeStdsDB}
\alias{DB}
\alias{threeStdsNIST}
\alias{smallDB}
\docType{data}
\title{Information on three chemical standards measured in GC-MS (liquid
  injection)}
\description{User information needed to build up an in-house database,
  the external database for cross-checking mass spectra, and the final
  database obtained with \code{createSTDdbGC} for three chemical standards.}
\usage{
data(threeStdsDB)
data(threeStdsInfo)
data(threeStdsNIST)
}
\details{
  Raw GC-MS data for the three standards, linalool, methyl salicylate
  and ethyl hexanoate, are available in package metaMSdata. Manual
  information required to build up an in-house database should be
  presented in the form of a data.frame, an example of which is
  \code{stdInfo}. Presenting this information to \code{createSTDdbGC}
  leads to processing of the raw data, and cross-checking with an
  external database containing mass spectra. An excerpt of the NIST
  database, containing only spectra for these three compounds, is
  available in \code{smallDB}. The final database that is then obtained
  can be inspected in object \code{DB}, which is a simple list of tags
  and values. For use as a reference database, several of these fields
  are mandatory. Currently these are \code{Name}, \code{monoMW},
  \code{pspectrum} and \code{std.rt}. 
}
\source{Georg Weingart}
\examples{
data(threeStdsNIST)
length(smallDB)

data(threeStdsInfo)
stdInfo

data(threeStdsDB)
par(mfrow = c(3,1))
sapply(DB, function(x) plotPseudoSpectrum(x$pspectrum, main = x$Name))
}
\keyword{datasets}
