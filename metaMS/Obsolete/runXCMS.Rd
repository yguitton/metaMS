\name{runXCMS}
\alias{runXCMS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
run XCMS pipeline for LC-MS and GC-MS data
}
\description{
Main function of the metaMS mass spectrometry processing pipeline. 
It runs XCMS peak detection, compound annotation and gives PCA plots. 
}
\usage{
runXCMS(path = ".", 
        chrom = "LC", 
        instrumentCode = NULL,
        runAnnotation = TRUE,
        processInfo = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
The path of the CDF files to be processed
}
  \item{chrom}{
The type of chromatography (remember tha tannotatin is differnet for LC and GC)
}

  \item{intrumentCode}{
The code to identify the instrument in the configuraton file
}
  \item{runAnnotation}{
Should annotation be performed?
}
  \item{processInfo}{
Should be the details of the analysis saved in a log file?
}
}
\details{
This function run the full analysis. The parameters for the analysis and the annotation are included in the \code{XCMSsettings.RData file}. For LC - MS the annotation is performed by using CAMERA and a specific DB file specified in the settings file. If available, the mass tolerance is caleculated with an error surface.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Pietro Franceschi and Ron Wehrens
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
path <- '/mnt/watersnew/experiments/grape/Synapt-QTOF-RP/cdf/'
runXCMS(path,runAnnotation=TRUE)


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~pipeline}
\keyword{ ~XCMS }% __ONLY ONE__ keyword per line
