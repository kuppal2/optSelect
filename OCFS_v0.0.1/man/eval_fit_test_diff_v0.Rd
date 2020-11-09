\name{eval_fit_test_diff_v0}
\alias{eval_fit_test_diff_v0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eval_fit_test_diff_v0(particle, numfolds, trainm, trainclass, testm, testclass, errortype = "AUC", kname = "radial", featweight = 0.05, accuracyweightA = 5, accuracyweightB = 1, max_num_feats = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{particle}{
%%     ~~Describe \code{particle} here~~
}
  \item{numfolds}{
%%     ~~Describe \code{numfolds} here~~
}
  \item{trainm}{
%%     ~~Describe \code{trainm} here~~
}
  \item{trainclass}{
%%     ~~Describe \code{trainclass} here~~
}
  \item{testm}{
%%     ~~Describe \code{testm} here~~
}
  \item{testclass}{
%%     ~~Describe \code{testclass} here~~
}
  \item{errortype}{
%%     ~~Describe \code{errortype} here~~
}
  \item{kname}{
%%     ~~Describe \code{kname} here~~
}
  \item{featweight}{
%%     ~~Describe \code{featweight} here~~
}
  \item{accuracyweightA}{
%%     ~~Describe \code{accuracyweightA} here~~
}
  \item{accuracyweightB}{
%%     ~~Describe \code{accuracyweightB} here~~
}
  \item{max_num_feats}{
%%     ~~Describe \code{max_num_feats} here~~
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
function (particle, numfolds, trainm, trainclass, testm, testclass, 
    errortype = "AUC", kname = "radial", featweight = 0.05, accuracyweightA = 5, 
    accuracyweightB = 1, max_num_feats = 10) 
{
    num_feat <- 0
    ind <- which(particle == 1)
    folderror_perm <- {
    }
    col_sel <- ind
    num_feat <- length(col_sel)
    if (num_feat > 1) {
        trainset <- trainm[, c(col_sel)]
        trainset <- cbind(trainclass, trainset)
        trainset <- data.frame(trainset)
        folderror <- {
        }
        folderror_vec <- {
        }
        seed_vec <- c(129532, 839147, 407700)
        {
            for (f in 1:3) {
                setseed = seed_vec[f]
                model <- svm_cv(v = numfolds, x = trainset[, 
                  -1], y = trainset$trainclass, kname = kname, 
                  errortype = errortype, setseed = setseed)
                folderror_cur_reg <- model$confint[1]
                set.seed(seed_vec[f])
                rand_ind <- sample(x = seq(1, length(trainset$trainclass)), 
                  size = length(trainset$trainclass))
                model <- svm_cv(v = numfolds, x = trainset[, 
                  -1], y = trainset$trainclass[rand_ind], kname = kname, 
                  errortype = errortype, setseed = setseed)
                folderror_cur_perm <- model$confint[2]
                folderror_perm <- c(folderror_perm, folderror_cur_perm)
                rm(model)
            }
        }
        testset <- testm[, c(col_sel)]
        mod_cv <- svm(x = trainset[, -1], y = trainset$trainclass, 
            type = "C", kernel = kname)
        predfit <- predict(mod_cv, testset)
        svm_table <- table(predfit, testclass)
        class_names <- rownames(svm_table)
        beracc <- {
        }
        i <- 1
        svm_acc <- {
        }
        totacc <- length(which(predfit == testclass))/length(testclass)
        for (c in 1:dim(svm_table)[1]) {
            testclass_ind <- which(testclass == class_names[c])
            beracc <- c(beracc, length(which(predfit[testclass_ind] == 
                testclass[testclass_ind]))/length(testclass_ind))
        }
        beracc <- as.numeric(beracc)
        beracc <- mean(beracc, na.rm = TRUE)
        if (errortype == "CV") {
            svm_acc[i] <- (totacc * 100)
        }
        else {
            if (errortype == "AUC") {
                pred_acc <- multiclass.roc(testclass, as.numeric(predfit))
                pred_acc_orig <- pred_acc$auc[1]
                auc_acc <- pred_acc_orig
                svm_acc[i] <- (auc_acc * 100)
            }
            else {
                svm_acc[i] <- (beracc * 100)
            }
        }
        folderror <- svm_acc[i]
        folderror_perm <- mean(folderror_perm, na.rm = TRUE) + 
            (1.96 * (sd(folderror_perm, na.rm = TRUE)/sqrt(2)))
        if (num_feat > max_num_feats) {
            fitfunc <- (accuracyweightA * (folderror_cur_reg - 
                folderror_perm)) + accuracyweightB * (folderror) - 
                (featweight * 100 * (num_feat/length(particle)))
        }
        else {
            fitfunc <- (accuracyweightA * (folderror_cur_reg - 
                folderror_perm)) + accuracyweightB * (folderror)
        }
        rm(trainset)
    }
    else {
        folderror <- 1
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
