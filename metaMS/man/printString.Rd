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
\author{Ron Wehrens and Pietro Franceschi}
\examples{
printString(c("Hello", "world"))
printWarning(c("Hello", "world"))
printInfo(c("Hello", "world"))
}
\keyword{print}
