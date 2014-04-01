\name{createSTDdbLC}
\alias{createSTDdbLC}
\alias{generateStdDBLC}
\title{Create an in-house database for LC-MS annotation}
\description{
For creating an in-house instrument-specific annotation database,
injections of pure standards need to be processed. For each standard the
analyst provides a validated reference value for retention time and m/z,
generally corresponding to the major ionic signal for this compound. On
the bases of this data, the database is constructed by automatically
extracting the features identified in the vicinity of the retention time
of the standard. \cr
The function \code{generateSTDdbLC} is not meant to be
  called directly - use \code{createSTDdbLC} instead.}
\usage{
createSTDdbLC(stdInfo, settings, polarity, Ithr = 10, nSlaves = 0)
generateStdDBLC(stdxsets, settings, Ithr) 
}
\arguments{
  \item{stdInfo}{Information of the standards, given in the form of a
    data.frame. Minimal information: \code{stdFile}, \code{compound},
    \code{ChemSpiderID}, observed m/z (\code{mz.observed}), and
    retention time in miutes (\code{RTman}). The filenames in slot
    \code{stdFile} should include path information.}
  \item{settings}{A list of settings, to be used in peak picking and
    pattern comparison (see details).}
  \item{polarity}{The polarity of the injection: "positive" or "negative"}
  \item{Ithr}{The intensity threshold used to decide weather or not a
    feature should be included in the DB. Typically acting on the
    \code{maxo} value.}
  \item{stdxsets}{A list of CAMERA objects resulting from the analysis
    (performed by \code{processStandards}) of the standard injections
    listed in the \code{stdInfo} table.}
  \item{nSlaves}{Number of cores to be used in peak picking.}
}
\details{
 The DB is created with the following workflow. Peak picking is
 performed on each standard file by using the settings specified in the
 settings list. CAMERA is used to group together the different features
 by considering their retention time and the correlation among the  
 extracted ion traces. The list of features is searched looking for the
 values for mz and Rt included in the \code{stdInfo} table (see the help
 of \code{exptable} for more details), with the mass and retention time
 tolerances specified in the "DBconstruction" element of the settings
 list. In presence of positive match for the feature \code{f}, a
 spectral fingerprint is constructed by using all the features with an
 intensity bigger than \code{Ithr} which are in the same pcgroup of
 \code{f}. A match is retained only if the spectral fingerprint is
 composed of more than \code{minfeat} elements. This parameter is also
 included in the list of settings.
}
\value{A list with three elements. 
  \item{Reftable}{the original table used for the creation of the DB.} 
  \item{Info}{a list with the settings used for the DB generation and
    the date.}
  \item{DB}{the DB which can be used in \code{runLC} for annotation.}
} 
\author{Pietro Franceschi}
\examples{
if (require(metaMSdata)) {
  ##  load the manually curated table for the standards
  data(exptable)
  ## add location of cdf file from which the standards DB is going to be
  ## built - this depends on your platform and requires the metaMSdata package
  cdfpath <- system.file("extdata", package = "metaMSdata")
  
  ## files 
  files <- list.files(cdfpath, "_RP_", full.names=TRUE)
  
  ## get the complete names for the files
  exptable$stdFile <-
    sapply(exptable$stdFile,
           function(x)
           files[grep(x,files)])
  
  \dontrun{
  ## load the settings for the analysis
  data(FEMsettings)
  
  ## set the minimum number of features to 2
  metaSetting(Synapt.RP, "DBconstruction")$minfeat  <- 2
  
  ## create the DB
  LCDBtest <- createSTDdbLC(stdInfo=exptable, 
                            settings = Synapt.RP,
                            polarity = "positive",
                            Ithr = 20)
  }                 
  ## saved in "LCDBtest.RData" in the data directory of the metaMS
  ## package
}

  
}
\keyword{manip}
