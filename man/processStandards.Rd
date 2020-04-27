\name{processStandards}
\alias{processStandards}
\title{Process input files containing raw data for pure standards.}
\description{Peak picking and further processing for raw data of pure
  standards, including CAMERA processing. This function is not meant to be
  called directly - use \code{createSTDdbLC} or \code{createSTDdbGC}
  instead.}
\usage{processStandards(stdInfo, settings, polarity = NULL, nSlaves)}
\arguments{
  \item{stdInfo}{Object describing the pure standards: a data.frame
    containing, e.g., the name of the file, the name of the standard,
    descriptors like CAS or Chemspider IDs, etcetera.}
  \item{settings}{Settings list, containing sublists for peak picking
    and CAMERA grouping (GC-MS) or annotation (LC-MS).}
  \item{polarity}{Polarity of the analysis (used for CAMERA). Possible
    values are \dQuote{positive} or \dQuote{negative}. Ignored for GC-MS.}
  \item{nSlaves}{Number of cores to be used in peak picking.}
}
\value{
  A list of CAMERA objects resulting from the analysis of the
  standard injections listed in the \code{stdInfo} table.
}
\author{Ron Wehrens and Pietro Franceschi}
\seealso{\code{\link{xcmsSet}}, \code{\link{runCAMERA}}}
\examples{
## Example of results
  data(GCresults) ## pre-compiled results
  GCresults$PeakTable
}
\keyword{manip}
