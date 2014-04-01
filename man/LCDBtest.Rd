\name{LCDBtest}
\alias{LCDBtest}
\docType{data}
\title{Sample DB for LC-MS annotation}
\description{
  This database has been generated using the \code{exptable} dataset to
  process the data included in the \code{metaMSdata} package.}
\usage{data(LCDBtest)}
\format{
  \describe{
    \code{LCDBtest} is a list with three elements: \cr
    \item{Reftable}{The initial table used to generate the DB.}
    \item{Info}{Some info on the DB: date of creation, and settings used.}
    \item{DB}{A \code{data.frame} containing the actual information. See the
      details section.} 
  }
}
\details{
  The \code{DB} \code{data.frame} contains the following fields:
  \describe{
    \item{ChemSpiderID}{The Chem spider identifier for the compound.}
    \item{compound}{A human-readable name for the compound.}
    \item{adduct}{The output of the CAMERA annotation of the standard file.}
    \item{isotopes}{The output of the CAMERA annotation of the standard file.}
    \item{mz, rt}{the mz and rt values.}
    \item{maxo}{the \code{maxo} intensity value for a given feature in
      the injection.} 
    \item{validated}{a string which defines the origin of a specific feature.}
}
}
\seealso{\code{\link{createSTDdbLC}}, \code{\link{exptable}},
  \code{\link{FEMsettings}}}
\author{Pietro Franceschi}
\keyword{datasets}
