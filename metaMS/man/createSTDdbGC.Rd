\name{createSTDdbGC}
\alias{createSTDdbGC}
\title{Create an in-house database for GC-MS annotation}
\description{
For creating an in-house instrument-specific annotation database,
injections of pure standards need to be processed. All patterns in the
vicinity of the retention time of the standard (to be provided by the
user) will be compared to an external database - in case of a sufficient
match, they will be retained in the database.}
\usage{
createSTDdbGC(stdInfo, settings, extDB = NULL, manualDB = NULL)
}
\arguments{
  \item{stdInfo}{Information of the standards, given in the form of a
    data.frame. Minimal information: \code{stdFile}, \code{Name},
    \code{CAS}, monoisotopic mass (\code{monoMW}), and retention time
    (\code{rt}). The filenames in slot \code{stdFile} should include
    path information. If this argument is NULL, this function can be
    used to process a manually curated DB. Arguments \code{stdInfo} and
    \code{manualDB} cannot be both NULL.}
  \item{settings}{A list of settings, to be used in peak picking and
    pattern comparison.}
  \item{extDB}{The external database containing spectra, with which to
    compare the patterns found in the standards files.}
  \item{manualDB}{A database of manually curated spectra, that will be
    incorporated in the final DB without any further checks.}
}
\details{Function \code{createSTDdbGC} creates a database object
  containing validated pseudospectra for a number of compounds. The
  injections of the standards, described in the input object
  \code{stdInfo}, are processed using function \code{processStandards};
  comparison with the external database, inclusion of manual compounds
  and final formatting are done in function
  \code{generateStdDBGC}. Several situations can be envisaged:

  A: a series of injections of standards needs to be compared with a
  standard library, such as the NIST. In this case, both \code{stdInfo}
  and \code{extDB} need to be non-null, and the result will be a
  database in which the entries have a sufficient match with the
  external DB. If \code{manualDB} is also non-null, these entries will
  be added too (without checking).

  B: for a series of injections no standard library information is
  available (\code{extDB} is NULL, and \code{stdInfo} is not), and the
  function simply returns all patterns eluting around 
  the indicated retention time. This allows for subsequent manual
  validation and pruning. If \code{manualDB} is non-null, these
  entries will be added, but since this is a somewhat unusual thing to
  do, a warning will be given.

  C: a manual database needs to be processed to be useable as a real
  database. This basically entails renaming the \code{rt} and
  \code{rt.sd} fields into \code{std.rt} and \code{std.rt.sd} (if
  not already present).
}
\value{
  A list, where every entry describes one compound/spectrum
  combination. For use in annotation, the following fields are
  mandatory: \code{Name}, \code{std.rt}, \code{pspectrum} and
  \code{monoMW}.
}
\author{Ron Wehrens}
\seealso{\code{\link{processStandards}}, \code{\link{generateStdDBGC}}}
\examples{
if (require(metaMSdata)) {
  ## first tell the system where to look
  data(threeStdsInfo)
  ## add location of cdf file from which the standards DB is going to be
  ## built - this depends on your platform and requires the metaMSdata package
  stdInfo[,"stdFile"] <-
    rep(list.files(system.file("CDF_GC", package = "metaMSdata"),
                   full.names = TRUE)[3], 3)

  data(FEMsettings)
  data(threeStdsNIST)  ## provides object smallDB, excerpt from NIST DB
  
  DB <- createSTDdbGC(stdInfo, FEMsettings[[3]], extDB = smallDB)
  ## saved in "threeStdsDB.RData" in the data directory of the metaMS
  ## package
}
}
\keyword{manip}
