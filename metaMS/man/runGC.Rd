\name{runGC}
\alias{runGC}
\title{Wrapper for processing of GC-MS data files}
\description{
  Main function of the pipeline for GC-MS data processing. It includes
  XCMS peak detection, definition of pseudospectra, and compound
  identification by comparison to a database of standards. The function
  also takes care of removal of artefacts like column bleeding and
  plasticizers, and definition of unknowns, consistently present across
  samples.}
\usage{
runGC(files, xset, settings, rtrange = NULL, DB = NULL,
      removeArtefacts = TRUE, findUnknowns = nexp > 1,
      returnXset = FALSE, RIstandards = NULL, nSlaves = 0)
      
}
\arguments{
  \item{files}{input files, given as a vector of strings containing the
    complete paths. All formats supported by XCMS can be used.}
  \item{xset}{alternatively, one can present an xcmsSet object, or a
    list of xcmsSet objects, instead of the input files. In that case no
    peak picking will be performed. If both \code{files} and \code{xset}
    are given, the former takes precedence.}
  \item{settings}{a nested list of settings, to be used at individual
    steps of the pipeline.}
  \item{rtrange}{part of the chromatograms that is to be analysed. If
    given, it should be a vector of two numbers indicating minimal and
    maximal retention time (in minutes).}
  \item{DB}{database containing the spectra of the pure standards. At
    least the following fields should be present: \code{Name},
    \code{std.rt}, \code{pspectrum} and \code{monoMW}.}
  \item{removeArtefacts}{logical, whether or not to remove patterns
    identified as (e.g.) column bleeding. Only performed if a database
    containing such patterns is available.}
  \item{findUnknowns}{logical, whether to find patterns without
    identification that are present consistently in several samples.}
  \item{returnXset}{logical: should the XCMS output be returned? If yes,
    this is a a list of \code{xcmsSet} objects, one element for each
    input file.}
  \item{RIstandards}{A two-column matrix containing for the standards
    defining the RI scale both retention times and retention indices. If
    not given, no RI values will be calculated and retention times will
    be used instead.}
  \item{nSlaves}{Number of cores to be used in peak picking.}
}

\value{
A list with the following elements:
\item{PeakTable}{data.frame containing annotation information. Every
  line is a feature, i.e. a pseudospectrum. The first columns are used
  to give information about these features, a.o. compound name, CAS and
  Chemspider IDs, etcetera. The last of these meta-information columns
  is always the one giving the retention time: \dQuote{rt}. After that,
  columns 
  correspond to input files, and give measures of intensities for every
  single one of the features. If a feature is not detected in a sample,
  this is indicated with \dQuote{0} (zero).} 
\item{PseudoSpecra}{A list of pseudospectra in msp format, in the
  same order as the rows in the PeakTable.}
\item{xset}{optionally, the xcmsSet object is returned, which can be
  useful for more detailed inspection of the results.}
}
\references{
  R. Wehrens, G. Weingart and F. Mattivi: "An open-source pipeline for
  GC-MS-based untargeted metabolomics". Submitted.
}
\author{Ron Wehrens}
\seealso{\code{\link{msp}}, \code{\link{treat.DB}},
  \code{\link{peakDetection}}, \code{\link{matchSamples2DB}},
  \code{\link{matchSamples2Samples}},
  \code{\link{getAnnotationMat}}, \code{\link{addRI}}}
\examples{
## analysis of an xset object
data(threeStdsDB) 
data(FEMsettings)

\dontrun{
result.annot <- runGC(xset = GCset, settings = TSQXLS.GC, DB = DB)
## to start directly from the CDF files and include peak picking in the
## pipeline, simply provide the "files" argument rather than the "xset" argument
if (require(metaMSdata)) {
  cdfdir <- system.file("CDF_GC", package = "metaMSdata")
  cdffiles <- list.files(cdfdir, pattern = "cdf",
                         full.names = TRUE, ignore.case = TRUE)

  ## no annotation database
  result.noannot <- runGC(files = cdffiles, settings = TSQXLS.GC)
  ## only three patterns consistently found, one of which is a standard

  ## more lenient settings
  metaSetting(TSQXLS.GC, "betweenSamples.simthresh") <- .8
  result.noannot2 <- runGC(files = cdffiles, settings = TSQXLS.GC)
  ## now 15 patterns are found consistently across the samples,
  ## including the three standards
}
data(GCresults) ## pre-compiled results
names(result.annot)
}

}
\keyword{manip}
