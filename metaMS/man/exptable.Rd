\name{exptable}
\alias{exptable}
\docType{data}
\title{Sample table for DB generation (LC)}
\description{
The table is used to construct the sample db \code{LCDBtest}.
}
\usage{
data(exptable)
}
\format{
\code{exptable} is a dataframe which associates standard compounds, standard injections and manually validated entries for rt and m/z

Columns descripton
  \describe{
    \item{ChemSpiderID}{The Chemspider ID of a specific compound}
    \item{compound}{A string containing the (human) readable name of the compound.}
    \item{formula}{The formula of the compound}
    \item{M.ref}{The theoretical value for the observed m/z reference ion}
    \item{mz.observed}{The manually validated m/z value identifying a specific compound. It can be, for example, the protonated(unprotonated) ion, one known adduct or a characteristic fragment.}
    \item{RTman}{The manually validated retantion time value for the standard.}
    \item{stdFile}{The name (with the path) of the injection of the standard}
  }

}

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
