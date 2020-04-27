\name{plotPseudoSpectrum}
\alias{plotPseudoSpectrum}
\title{Plot a pseudospectrum.}
\description{
Auxiliary function for plotting a particular pseudospectrum. M/z
values are in the first column of the matrix, and an intensity
measure (either maxo, into or something else) in the second. The third
column is disregarded, usually contains retention time information
}
\usage{
plotPseudoSpectrum(psspc, ...)
}
\arguments{
  \item{psspc}{Pseudospectrum, consisting of a two- or three-column
    matrix. The first column contains the m/z values, the second the
    intensities. A third column containing retention time information
    may be present, but is not used in this function.}
  \item{\dots}{Additional graphical parameters.}
}
\details{A stick spectrum is shown on the graphical device.}
\value{
    A three column matrix m/z and Intensity and Retention time.
}
\author{Ron Wehrens}
\examples{
data("threeStdsDB")
plotPseudoSpectrum(DB[[1]]$pspectrum)
}
\keyword{manip}
