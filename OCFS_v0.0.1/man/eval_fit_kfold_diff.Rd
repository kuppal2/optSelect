\name{eval_fit_kfold_diff}
\alias{eval_fit_kfold_diff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eval_fit_kfold_diff(particle, trainm, trainclass, numfolds, errortype = "AUC", accuracyweightA = 5, accuracyweightB = 1, featweight = 0.06, max_num_feats = 10, kname = "radial")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{particle}{
%%     ~~Describe \code{particle} here~~
}
  \item{trainm}{
%%     ~~Describe \code{trainm} here~~
}
  \item{trainclass}{
%%     ~~Describe \code{trainclass} here~~
}
  \item{numfolds}{
%%     ~~Describe \code{numfolds} here~~
}
  \item{errortype}{
%%     ~~Describe \code{errortype} here~~
}
  \item{accuracyweightA}{
%%     ~~Describe \code{accuracyweightA} here~~
}
  \item{accuracyweightB}{
%%     ~~Describe \code{accuracyweightB} here~~
}
  \item{featweight}{
%%     ~~Describe \code{featweight} here~~
}
  \item{max_num_feats}{
%%     ~~Describe \code{max_num_feats} here~~
}
  \item{kname}{
%%     ~~Describe \code{kname} here~~
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
function (particle, trainm, trainclass, numfolds, errortype = "AUC", 
    accuracyweightA = 5, accuracyweightB = 1, featweight = 0.06, 
    max_num_feats = 10, kname = "radial") 
{
    num_feat <- 0
    ind <- which(particle == 1)
    col_sel <- ind
    folderror <- {
    }
    folderror_perm <- {
    }
    num_feat <- length(col_sel)
    if (num_feat > 1) {
        trainset <- trainm[, c(col_sel)]
        trainset <- cbind(trainclass, trainset)
        trainset <- data.frame(trainset)
        folderror <- {
        }
        folderror_perm <- {
        }
        seed_vec <- c(129532, 839147, 407700)
        for (f in 1:3) {
            setseed = seed_vec[f]
            model <- svm_cv(v = numfolds, x = trainset[, -1], 
                y = trainset$trainclass, kname = kname, errortype = errortype, 
                setseed = setseed)
            folderror_cur <- model$confint[1]
            folderror <- c(folderror, folderror_cur)
            rm(model)
            set.seed(seed_vec[f])
            rand_ind <- sample(x = seq(1, length(trainset$trainclass)), 
                size = length(trainset$trainclass))
            model <- svm_cv(v = numfolds, x = trainset[, -1], 
                y = trainset$trainclass[rand_ind], kname = kname, 
                errortype = errortype, setseed = setseed)
            folderror_cur_perm <- model$confint[2]
            folderror_perm <- c(folderror_perm, folderror_cur_perm)
            rm(model)
        }
        folderror <- mean(folderror, na.rm = TRUE) - (1.96 * 
            (sd(folderror, na.rm = TRUE)/sqrt(2)))
        folderror_perm <- mean(folderror_perm, na.rm = TRUE) + 
            (1.96 * (sd(folderror_perm, na.rm = TRUE)/sqrt(2)))
        if (num_feat > max_num_feats) {
            fitfunc <- (accuracyweightA * (folderror - folderror_perm)) + 
                accuracyweightB * (folderror) - (featweight * 
                100 * (num_feat/length(particle)))
        }
        else {
            fitfunc <- (accuracyweightA * (folderror - folderror_perm)) + 
                accuracyweightB * (folderror)
        }
    }
    else {
        folderror <- 1
        folderror_perm <- 100
        fitfunc <- (-100)
    }
    rm(col_sel)
    rm(num_feat)
    fitfunc <- (-1) * fitfunc
    return(list(fitfunc = fitfunc, cverror = folderror, cvpermerror = folderror_perm))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
