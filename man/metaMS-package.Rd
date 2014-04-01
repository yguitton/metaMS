\name{metaMS-package}
\alias{metaMS-package}
\alias{metaMS}
\docType{package}
\title{Analysis pipeline for MS-based metabolomics data}
\description{
  Analysis pipeline for MS-based metabolomics data: basic peak picking
  and grouping is done using functions from packages xcms and
  CAMERA. The main output is a table of feature intensities in all
  samples, which can immediately be analysed with multivariate methdos.
  The package supports the creation of in-house databases of mass
  spectra (including retention information) of pure chemical
  compounds. Such databases can then be used for annotation purposes.
}
\details{
Index:
\preformatted{
AnnotateFeature         Feature Wise Annotation
AnnotateTable           AnnotateTable
FEMsettings             FEM Settings for 'metaMS'
LCDBtest                Sample DB for LC-MS annotation
alignmentLC             LC alignment
construct.msp           Functions to handle msp-type objects (GC-MS)
constructExpPseudoSpectra
                        Create a list of all pseudospectra found in a
                        GC-MS experiment of several samples.
createSTDdbGC           Create an in-house database for GC-MS
                        annotation
createSTDdbLC           Create an in-house database for LC-MS
                        annotation
exptable                Sample table for DB generation (LC)
generateStdDBGC         Convert an msp object into a GC database object
getAnnotationLC         get LC annotation
getAnnotationMat        Subfunction GC-MS processing
getFeatureInfo          Construct an object containing all
                        meta-information of the annotated pseudospectra
                        (GC-MS).
getPeakTable            get peak table
matchExpSpec            Match a GC-MS pseudospectrum to a database with
                        a weighted crossproduct criterion.
matchSamples2DB         Match pseudospectra from several samples to an
                        in-house DB (GC-MS)
matchSamples2Samples    Compare pseudospectra across samples (GC-MS)
peakDetection           Wrapper for XCMS peak detection, to be used for
                        both GC-MS and LC-MS data.
plotPseudoSpectrum      Plot a pseudospectrum.
processStandards        Process input files containing raw data for
                        pure standards.
readStdInfo             Read information of injections of standards
                        from a csv file.
runCAMERA               Run CAMERA
runGC                   Wrapper for processing of GC-MS data files
runLC                   Wrapper for processing of LC-MS data files
treat.DB                Scaling of pseudospectra in an msp object.
}

The most important functions for running the pipeline are \code{runGC}
and \code{runLC} - in-house databases are created by functions
\code{createSTDdbGC} and \code{createSTDdbLC}.
}
\author{Ron Wehrens [aut, cre] (author of GC-MS part), Pietro Franceschi [aut] (author of LC-MS part), Nir Shahaf [ctb], Matthias Scholz [ctb], Georg Weingart [ctb] (development of GC-MS approach), Elisabete Carvalho [ctb] (testing and feedback of GC-MS pipeline)

Maintainer: Ron Wehrens <ron.wehrens@fmach.it>
}
\keyword{ package }
