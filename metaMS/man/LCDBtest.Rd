\name{LCDBtest}
\alias{LCDBtest}
\docType{data}
\title{Sample DB for LC-MS annotation}
\description{
  This database has been generated using the \code{exptable} dataset to
  process the data included in the \code{metaMSdata} package.}
\usage{data(LCDBtest)}
\format{
  \describe{
    \code{LCDBtest} is a list with three elements: \cr
    \item{Reftable}{The initial table used to generate the DB.}
    \item{Info}{Some info on the DB: date of creation, and settings used.}
    \item{DB}{A \code{data.frame} containing the actual information. See the
      details section.} 
  }
}
\details{
  The \code{DB} \code{data.frame} contains the following fields:
  \describe{
    \item{ChemSpiderID}{The Chem spider identifier for the compound.}
    \item{compound}{A human-readable name for the compound.}
    \item{adduct}{The output of the CAMERA annotation of the standard file.}
    \item{isotopes}{The output of the CAMERA annotation of the standard file.}
    \item{mz, rt}{the mz and rt values.}
    \item{maxo}{the \code{maxo} intensity value for a given feature in
      the injection.} 
    \item{validated}{a string which defines the origin of a specific feature.}
}
}
\seealso{\code{\link{createSTDdbLC}}, \code{\link{exptable}},
  \code{\link{FEMsettings}}}
\author{Pietro Franceschi}
\examples{
if (require(metaMSdata)) {
  ##  load the manually curated table for the standards
  data(exptable)
  ## add location of cdf file from which the standards DB is going to be
  ## built - this depends on your platform and requires the metaMSdata package
  cdfpath <- system.file("CDF_LC", package = "metaMSdata")
  
  ## files 
  files <- list.files(cdfpath, "CDF", full.names=TRUE)
  
  ## get the complete names for the files
  exptable$stdFile <- sapply(exptable$stdFile, function(x){
    out <- files[grep(x,files)]
  })
  
  ## load the settings for the analysis
  data(FEMsettings)
  
  ## set the minimum number of features to 2
  FEMsettings$Synapt.QTOF.RP$DBconstruction$minfeat  <- 2
  
  ## create the DB
  LCDBtest <- createSTDdbLC(stdInfo=exptable, 
                            settings = FEMsettings$Synapt.QTOF.RP,
                            polarity = "positive",
                            Ithr = 20)
                            
  ## saved in "LCDBtest.RData" in the data directory of the metaMS
  ## package
}


}
\keyword{datasets}
