\name{find_similar_samples}
\alias{find_similar_samples}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
find_similar_samples(part_group, particle_ind = NA, num_neighbors = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{part_group}{
%%     ~~Describe \code{part_group} here~~
}
  \item{particle_ind}{
%%     ~~Describe \code{particle_ind} here~~
}
  \item{num_neighbors}{
%%     ~~Describe \code{num_neighbors} here~~
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
function (part_group, particle_ind = NA, num_neighbors = 3) 
{
    bcdata <- as.matrix(part_group)
    if (is.na(particle_ind) == FALSE) {
        targetdata <- as.matrix(part_group[particle_ind, ])
    }
    else {
        targetdata <- as.matrix(part_group)
    }
    data_dim <- dim(bcdata)
    affy_names <- colnames(bcdata)
    bc.names <- rownames(bcdata)
    i <- 1
    dist.matrix <- {
    }
    dist.matrix <- ann(ref = as.matrix(bcdata), target = as.matrix(targetdata), 
        tree.type = "brute", k = num_neighbors, split.rule = "midpt", 
        shrink.rule = "centroid", verbose = FALSE)
    rm(bcdata)
    if (FALSE) {
        dist.print <- t(dist.matrix$knnIndexDist)
        sample_names <- bc.names[dist.print[1:100, ]]
        sample_list <- {
        }
        exp_list <- {
        }
        tissue_list <- {
        }
        pval_list <- {
        }
        match_bclist <- {
        }
        for (i in 1:100) {
            sample <- sapply(strsplit(as.character(sample_names[i]), 
                "."), head, n = 2)[1]
            sample <- toupper(sample)
            sample_list <- rbind(sample_list, sample)
        }
        tab <- data.frame(Number = (1:100), Sample = as.factor(sample_list), 
            Distance = dist.print[101:200, ], row.names = T)
        return(tab)
    }
    return(dist.matrix$knnIndexDist)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
