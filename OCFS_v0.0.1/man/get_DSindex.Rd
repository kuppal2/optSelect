\name{get_DSindex}
\alias{get_DSindex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_DSindex(m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
%%     ~~Describe \code{m} here~~
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
function (m) 
{
    sum1 <- apply(m, 2, sum)
    n <- dim(m)[1]
    DS_res <- {
    }
    for (i in 1:dim(m)[2]) {
        for (j in i:dim(m)[2]) {
            if (i != j) {
                mi <- which(m[, i] == 1)
                mj <- which(m[, j] == 1)
                r <- length(intersect(mi, mj))
                num_mi <- length(mi)
                num_mj <- length(mj)
                dice_index <- (2 * r)/(num_mi + num_mj)
                DS_res <- c(DS_res, dice_index)
            }
        }
    }
    DS_res_mean <- mean(DS_res, na.rm = TRUE)
    return(DS_res_mean)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
