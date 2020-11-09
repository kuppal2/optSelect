\name{nn_search}
\alias{nn_search}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nn_search(particle_ind, part_group)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{particle_ind}{
%%     ~~Describe \code{particle_ind} here~~
}
  \item{part_group}{
%%     ~~Describe \code{part_group} here~~
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
function (particle_ind, part_group) 
{
    partm <- as.matrix(part_group[particle_ind, ])
    dist_mat <- dist(as.matrix(part_group))
    dist_mat <- as.matrix(dist_mat)
    nn_order <- order(dist_mat[particle_ind, ])
    nn_order <- nn_order[-c(1)]
    if (FALSE) {
        for (i in 1:length(part_group)) {
            groupm <- as.matrix(part_group[i])
            pdist <- dist(rbind(partm, groupm))
            nn_arr <- rbind(nn_arr, as.array(cbind(i, pdist)))
        }
        nn_order <- order(nn_arr[, 2])
    }
    return(nn_order)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
