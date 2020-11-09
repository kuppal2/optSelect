\name{makefactors}
\alias{makefactors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makefactors(curdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{curdata}{
%%     ~~Describe \code{curdata} here~~
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
function (curdata) 
{
    alphavec = c("A", "B", "C", "D", "E", "F", "G", "H", "I", 
        "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", 
        "U", "V", "W", "X", "Y", "Z")
    facttest <- apply(curdata, 2, function(x) {
        categ = TRUE
        if (is.na(as.numeric(x[1]))) {
            categ = FALSE
        }
        else {
            for (i in 1:length(x)) {
                mod_val = x[i]\%\%2
                if (mod_val != 0 && mod_val != 1) {
                  categ = FALSE
                }
            }
        }
        if (categ == TRUE) {
            factvec <- as.factor(x)
            getlevels = levels(factvec)
            factvec = as.vector(factvec)
            for (g in 1:length(getlevels)) {
                factvec[which(factvec == getlevels[g])] = alphavec[g]
            }
            x = as.data.frame(factvec)
        }
        return(x)
    })
    facttest = as.data.frame(facttest)
    return(facttest)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
