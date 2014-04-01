\name{annotations2tab}
\alias{annotations2tab}
\alias{makeAnnotation}
\title{Conversion of a list of annotation results into a table}
\description{
  Function \code{annotations2tab} converts the output of
  \code{matchSamples2DB} into a table, in the case of multiple DB hits
  for one pseudospectrum distinguishing the \dQuote{best} hit from the
  \dQuote{alternative} hits. Function \code{makeAnnotation} prepares a
  \code{data.frame} object with the correct number of rows. Both functions are
  not meant to be called directly by the user.
}
\usage{
annotations2tab(annlist, matches)
makeAnnotation(n)
}
\arguments{
  \item{annlist}{Annotation list, output of \code{matchSamples2DB}.}
  \item{matches}{Object containing all match factors - needed to
    distinguish the best match from the rest in the case of a double
    annotation.}
  \item{n}{Number of rows in the annotation \code{data.frame} (possibly zero).}
}
\value{A three-column \code{data.frame}, containing the numbers of the
  pseudospectrum in the experimental data, the numbers of the
  best hits in the DB, and finally the alternative hits ain the DB}
\author{Ron Wehrens}
\keyword{manip}
