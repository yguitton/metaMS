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
addRI(mspobj, RIstandards)
}
\arguments{
  \item{mspobj}{An msp object.}
  \item{RIstandards}{A two-column matrix containing for the standards
    defining the RI scale both retention times and retention indices.}
}
\value{An msp object, now also containing an RI slot.}
\author{Ron Wehrens}
\note{If the retention time of a compound is outside the range of the RI
  standards, NA will be used as RI value.
}
\examples{
manual.fname <- list.files(system.file("CDF_GC", package = "metaMSdata"),
                           pattern = "msp", full.names = TRUE)
manual <- read.msp(manual.fname)
RIstandards <- cbind("rt" = seq(1.8, 25.0, by = 1.3),
                     "RI" = (1:18)*100 + 200)

manualRI <- addRI(manual, RIstandards)
}
\keyword{manip}
