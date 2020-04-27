\name{printString}
\alias{printString}
\alias{printWarning}
\alias{printInfo}
\title{Functions for metaMS-formatted text output}
\description{
  Functions to present progress output, warnings or information, in a
  consistent way in the console window. Not meant to be called by the user.
}
\usage{
printString(..., screenwidth = 72)
printWarning(...)
printInfo(...)
}
\arguments{
  \item{\dots}{Text strings. These will be concatenated inside the function.}
  \item{screenwidth}{Width of the text field.}
}
\value{
    A string to print.
}
\author{Ron Wehrens and Pietro Franceschi}
\keyword{print}
