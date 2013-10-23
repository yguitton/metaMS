\name{runCAMERA}
\alias{runCAMERA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run CAMERA
}
\description{
Run CAMERA package with settings from XCMSsetting-file. Works both for LC and GC. 
}
\usage{
runCAMERA(xset, chrom = c("LC", "GC"), settings, polarity, quick = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xset}{
The \code{xcmsSet} object.
}
  \item{chrom}{
The type of chromatography. Either "LC" or "GC".
}
  \item{settings}{
The subset of settigs contained into the "CAMERA" element of the XCMSsettings list.
}
  \item{polarity}{
The polarity of the injection, used by CAMERA to look for common adducts.
}
  \item{quick}{
If \code{TRUE}, CAMERA runs only \code{groupFWHM} and \code{findIsotopes}. 
}
}
\details{
In the case of LC the function is used in data analysis and DB creation. In the first case, it increases the level of the annotation and it is, by default, run with \code{quick} = \code{TRUE}. For DB creation, the grouping of the features into "pcgroups" is used to choose the features to be included into the database. In theis case camera is run with \code{quick} = \code{FALSE}
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
Ron Wehrens and Pietro Franceschi
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
