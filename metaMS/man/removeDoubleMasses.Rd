\name{removeDoubleMasses}
\alias{removeDoubleMasses}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subfunction GC-MS processing (matchfuns.R)
}
\description{
Since in our GC data m/z values are rounded to whole numbers, in
some cases an m/z value may occur multiple times - in that case the
mean of the intensities is kept (and the retention times are averaged). 
removeDoubleMasses takes a list of spectra in the form of a
three-column matrix, (mz, I, rt), summing the intensities and
averaging the retention times for multiple identical masses
}
\usage{
removeDoubleMasses(spclist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spclist}{
%%     ~~Describe \code{spclist} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (spclist) 
{
    lapply(spclist, function(pspc) {
        if (max(table(pspc[, 1])) > 1) {
            huhn <- cbind(aggregate(pspc[, 2], list(pspc[, 1]), 
                FUN = sum), aggregate(pspc[, 3], list(pspc[, 
                1]), FUN = mean)$x)
            colnames(huhn) <- colnames(pspc)
            huhn
        }
        else {
            pspc
        }
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
