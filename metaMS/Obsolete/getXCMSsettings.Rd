\name{getXCMSsettings}
\alias{getXCMSsettings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get XCMS settings
}
\description{
read settings from file settingsXCMS.txt 
}
\usage{
getXCMSsettings(settingID = "Synapt-QTOF-RP", filename = "settingsXCMS")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{settingID}{
%%     ~~Describe \code{settingID} here~~
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
function (settingID = "default", filename = "settingsXCMS.txt") 
{
    s = read.table(filename)
    if (is.na(match(settingID, rownames(s)))) {
        textstr = paste(c("'", settingID, "' is not a valid setting name"), 
            collapse = "")
        stop(textstr)
    }
    XCMSsettings = s[settingID, ]
    XCMSsettings$tomcat = FALSE
    return(XCMSsettings)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
