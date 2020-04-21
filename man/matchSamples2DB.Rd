\name{matchSamples2DB}
\alias{matchSamples2DB}
\title{Match pseudospectra from several samples to an in-house DB (GC-MS)}
\description{Compare experimental results to a database of
  pseudospectra. The result is a nested list, containing for every
  pseudospectrum in every sample either an index of a corresponding
  pattern in the DB (if a hit is found) or nothing (if no hit is
  found). Not meant to be called directly by the user.}
\usage{matchSamples2DB(xset.msp, DB, settings, quick)}
\arguments{
  \item{xset.msp}{An object containing a list of pseudospectra.}
  \item{DB}{The in-house database.}
  \item{settings}{The settings, in the form of a list.}
  \item{quick}{Logical. If TRUE, scaling of the pseudospectra (which is
    necessary for comparing to the database) will be done once using all
    masses in the pseudospectrum. This mode is routinely applied when
    comparing with a database of artefacts such as bleeding patterns or
    plasticizers. When comparing with a database of chemical standards,
    however, only peaks smaller than the molecular weight (+ 4, to allow
    for isotopes) should be taken into account in the scaling, and hence
    the scaling needs to be re-done for every comparison. This is _not_
    so quick...}
}
\value{A list object, with one element for each experimental
  sample. Every element again is a list with an entry for each
  pseudospectrum from that sample: if the element is empty, there is no
  match with the DB - a number points to the DB entry that gives a
  hit. Elements can contain more than one number; these will be split
  into one \dQuote{best} annotation and \dQuote{alternative} annotations
  in function \code{annotations2tab}.
}
\author{Ron Wehrens}
\examples{
## Example of settings
  data(FEMsettings)
  metaSetting(object = TSQXLS.GC, field= "match2DB")
}
\keyword{manip}
