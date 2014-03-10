\name{metaSetting-methods}
\docType{methods}
%\alias{metaSetting-methods}
\alias{metaSetting}
\alias{metaSetting<-}
\alias{metaSetting,metaMSsettings-method}
\alias{metaSetting<-,metaMSsettings-method}
\title{Get or set values in metaMSsettings objects}
\description{
  Accessor function for \code{metaMSsettings} objects, allowing to get
  or set values from individual slots.
}
\section{Methods}{
  \describe{
\item{\code{signature(object = "metaMSsettings")}}{
  Get or set values from individual slots in a \code{metaMSsettings} objects.
}
}}
\seealso{\code{\link{FEMsettings}}}
\examples{
\dontrun{
## The three sets of settings are created as follows:
Synapt.NP <- metaMSsettings(protocolName = "Synapt.QTOF.NP",
                            chrom = "LC",
                            PeakPicking = list(
                              method = "matchedFilter",
                              step = 0.05,
                              fwhm = 20,
                              snthresh = 4,
                              max = 50),
                            Alignment = list(
                              min.class.fraction = .3,
                              min.class.size = 3,
                              mzwid = 0.1,
                              bws = c(130, 10),
                              missingratio = 0.2,
                              extraratio = 0.1,
                              retcormethod = "linear",
                              retcorfamily = "symmetric",            
                              fillPeaks = TRUE),
                            CAMERA = list(
                              perfwhm = 0.6,
                              cor_eic_th = 0.7,
                              ppm= 5))
metaSetting(Synapt.NP, "match2DB") <- list(
            rtdiff = 1.5,
            rtval = .1,
            mzdiff = 0.005,
            ppm = 5,
            minfeat = 2)
metaSetting(Synapt.NP, "DBconstruction") <- list(
            minfeat = 3,
            rttol = .3,
            mztol = .01)
}
}
\keyword{methods}
