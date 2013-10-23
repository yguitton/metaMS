\name{AnnotateFeature}
\alias{AnnotateFeature}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Feature Wise Annotation
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function to annotate a vector of the form (mz,rt,I) by using a specific
database and a specific error function to calculate the mass tolerance.
}
\usage{
AnnotateFeature(input, DB, settings, errf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
   A vector with three elements in the form \code{(mz,rt,I)}.
}
  \item{DB}{
  A dataframe used for tha annotation
}
  \item{settings}{
  The subset of settigs contained into the "match2DB" element of the XCMSsettings list.
}
  \item{errf}{
  The (linear) model used to calculate the \code{m/z} tolerance for the matching. If \code{NULL} a fixed mass window will be used, its value is specified into the settings.
}
}
\details{
If \code{errf} is provided, it is used to calculate the mass tolerance as a function of the mass range and of the logarithm of the intensity of the signal. \code{errf} is used inside a \code{predict(errf, data.frame(M = input["mz"], logI = log10(input["I"])))} call.

The DB is a \code{data.frame} which should contain the following columns: \code{"ChemSpiderID"}, \code{"compound"}, \code{"formula"}, \code{"mz"}, \code{"rt"},  \code{"maxo"} %%  ~~ I
}
\value{
A vector of indices which associates the feature to the entries  - possibly multiple -  in the database 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Pietro Franceschi
}

\keyword{ ~manip }
