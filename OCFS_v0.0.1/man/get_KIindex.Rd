\name{get_KIindex}
\alias{get_KIindex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_KIindex(m, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
%%     ~~Describe \code{m} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
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
function (m, k) 
{
    sum1 <- apply(m, 2, sum)
    n <- dim(m)[1]
    KI_res <- {
    }
    for (i in 1:dim(m)[2]) {
        for (j in i:dim(m)[2]) {
            if (i != j) {
                mi <- which(m[, i] == 1)
                mj <- which(m[, j] == 1)
                r <- length(intersect(mi, mj))
                k2_n <- (k)^2/n
                KI_ind <- (r - k2_n)/(k - k2_n)
                KI_res <- c(KI_res, KI_ind)
            }
        }
    }
    KI_res_mean <- mean(KI_res, na.rm = TRUE)
    return(KI_res_mean)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
