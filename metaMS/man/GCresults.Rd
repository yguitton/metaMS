\name{GCresults}
\alias{GCresults}
\alias{GCset}
\alias{allSamples}
\alias{allSamples.msp}
\alias{result.annot}
\docType{data}
\title{Results of metaMS for a small GC-MS data set}
\description{This data file contains final and intermediate results of
  applying the metaMS pipeline to a small GC-MS data set consisting of
  repeated injections of a set of chemical standards. Its aim is to
  demonstrate the structure of the (intermediate) outcomes without
  having to do the slightly time-consuming calculations.
}
\usage{data(GCresults)}
\details{The objects that are loaded from this data object are the
  following:
  \itemize{
  \item{GCset}{an \code{xcmsSet} object containing the results of a call
    to \code{xcmsSet} for the GC files from the \pkg{metaMSdata}
    package.}
  \item{allSamples}{a list of CAMERA objects, one for each injection.}
  \item{allSamples.msp}{results of xcmsSet and CAMERA for each
    injection, in msp format.}
  \item{result.annot}{Result of a complete execution of \code{runGC},
    using the example database for annotation purposes.}
  }
  }
\keyword{datasets}
