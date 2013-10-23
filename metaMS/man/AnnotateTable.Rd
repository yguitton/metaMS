\name{AnnotateTable}
\alias{AnnotateTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AnnotateTable
}
\description{
Function to annotate a peak table by using a database. It recalls \code{AnnotateFeature} 
}
\usage{
AnnotateTable(peaktable, errf, DB, settings)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{peaktable}{A peaktable (matrix) with three column corresponding
    to mz,rt and I values.}
  \item{errf}{The file containing the error function used to predict the \code{m/z}
    tolerance.} 
  \item{DB}{A dataframe used for tha annotation.}
  \item{settings}{The subset of settigs contained into the "match2DB" element of the XCMSsettings list.}
}
\details{
If \code{errf} is provided, it is used to calculate the mass tolerance as a function of the mass range and of the logarithm of the intensity of the signal. \code{errf} is used inside a \code{predict(errf, data.frame(M = input["mz"], logI = log10(input["I"])))} call.
The DB is a \code{data.frame} which should contain the following columns: \code{"ChemSpiderID"}, \code{"compound"}, \code{"formula"}, \code{"mz"}, \code{"rt"},  \code{"maxo"}. An annotation is considered reliable if \code{minfeat}
 features associated to the same compound are present. Their retention
 times should differ of less than \code{rtval}.  
}
\value{
  A list with the following elements
  \item{annotation.table}{A \code{data.frame} withe the results of the annotation and the reference to the DB}
  \item{compounds}{The names of the annotated compounds}
  \item{IDs}{The IDs of the annotated compounds}
  \item{multiple.annotations}{The features with multiple annotations}
  \item{ann.features}{The features with annotation}
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~manip }

