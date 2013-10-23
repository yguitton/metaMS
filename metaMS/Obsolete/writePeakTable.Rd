\name{writePeakTable}
\alias{writePeakTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write peak table 
}
\description{
Write peak table to text file "xcmsPeakTable.txt" for use in Excel or other programs.
Sample names are shortened and data values are rounded to a practical useful precision.

}
\usage{
writePeakTable(peakTable, fileformat = "default", filename = "xcmsPeakTable.txt")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{peakTable}{
%%     ~~Describe \code{peakTable} here~~
}
  \item{fileformat}{
%%     ~~Describe \code{fileformat} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
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
function (peakTable, fileformat = "default", filename = "xcmsPeakTable.txt") 
{
    if (fileformat == "default") {
        naID = ""
        quoteID = TRUE
        decPoint = ","
        delimiter = "\t"
    }
    if (fileformat == "R") {
        naID = "NA"
        quoteID = TRUE
        decPoint = "."
        delimiter = "\t"
        if (filename == "xcmsPeakTable.txt") 
            filename = "xcmsPeakTable_R.txt"
    }
    if (fileformat == "Matlab") {
        naID = "NaN"
        quoteID = FALSE
        decPoint = "."
        delimiter = "\t"
        if (filename == "xcmsPeakTable.txt") 
            filename = "xcmsPeakTable_Matlab.txt"
    }
    dataPrec = "\%.1f"
    rtPrec = "\%.2f"
    mzPrec = "\%.4f"
    posRT = which(colnames(peakTable) == "rt")
    posMZ = which(colnames(peakTable) == "mz")
    peakTable[, posRT] = as.numeric(sprintf(rtPrec, as.numeric(as.character(peakTable[, 
        posRT]))))
    peakTable[, posMZ] = as.numeric(sprintf(mzPrec, peakTable[, 
        posMZ]))
    b = max(posRT, posMZ) + 1
    e = length(peakTable)
    x = peakTable[, c(b:e)]
    if (length(c(b:e)) == 1) {
        xx = as.numeric(sprintf(dataPrec, x))
    }
    else xx = as.numeric(apply(x, 2, function(x) sprintf(dataPrec, 
        x)))
    peakTable[, c(b:e)] = xx
    write.table(peakTable, file = filename, append = FALSE, quote = quoteID, 
        sep = delimiter, eol = "\n", na = naID, dec = decPoint, 
        row.names = FALSE, col.names = TRUE, qmethod = "double")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
