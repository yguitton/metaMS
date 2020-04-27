\name{constructExpPseudoSpectra}
\alias{constructExpPseudoSpectra}
\title{
  Create a list of all pseudospectra found in a GC-MS experiment of
  several samples.}
\description{
  Function constructExpPseudoSpectra creates an msp object containing all
  the patterns referenced to in the annotation. The first argument is
  the output of function matchSamples2Samples and contains the full
  annotation matrix and the pseudospectra of the known unknowns; the
  second is the msp object containing the standards that are actually found.
  Not meant to be called directly by the user.}
\usage{constructExpPseudoSpectra(allMatches, standardsDB)}
\arguments{
  \item{allMatches}{Result of a call to \code{matchSamples2DB} or
    \code{matchSamples2Samples}: a list containing the element
    \code{annotation}.}
  \item{standardsDB}{Database of standard spectra, in msp format.}
}
\value{A list of spectra. All spectra from the database that are
  referenced to in the annotation slot will be present, and will be
  followed by all unknown spectra. In order to be able to see which
  patterns in the experimental data are pointing to which spectra, an
  extra slot \code{DB.id} is added, containing the position of the
  reference spectrum in the original database (which is what the numbers
  of the annotation object are referring to). Renumbering this is
  impractical since some patterns may be seen as alternative
  annotations, but may have no hits of their own - including them would
  lead to rows containing only zeros.
}
\author{Ron Wehrens}
\seealso{\code{\link{matchSamples2DB}}, \code{\link{matchSamples2Samples}}}
\examples{
## Example of results
  data(GCresults) ## pre-compiled results
  GCresults$PseudoSpectra
}
\keyword{manip}
