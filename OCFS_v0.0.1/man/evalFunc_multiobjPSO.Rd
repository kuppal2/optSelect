\name{evalFunc_multiobjPSO}
\alias{evalFunc_multiobjPSO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
evalFunc_multiobjPSO(particle, X, Y, numfolds = 10, errortype = "AUC", kname = "radial", featweight = 0.05, accuracyweightA = 5, accuracyweightB = 1, max_num_feats = 10, seednum = NA, evalMode = "CV1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{particle}{
%%     ~~Describe \code{particle} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{numfolds}{
%%     ~~Describe \code{numfolds} here~~
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
  \item{seednum}{
%%     ~~Describe \code{seednum} here~~
}
  \item{evalMode}{
%%     ~~Describe \code{evalMode} here~~
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
function (particle, X, Y, numfolds = 10, errortype = "AUC", kname = "radial", 
    featweight = 0.05, accuracyweightA = 5, accuracyweightB = 1, 
    max_num_feats = 10, seednum = NA, evalMode = "CV1") 
{
    trainm = X
    trainclass = Y
    if (is.na(seednum) == TRUE) {
        seednum = runif(n = 1, min = 1, max = 999999)
    }
    if (evalMode == "CV1" || evalMode == "CV2") {
        numtrain <- (0.8 * nrow(trainm))
        evalmethod = "MCCV"
        set.seed(seednum)
        trainlearningsets <- GenerateLearningsets(y = trainclass, 
            method = evalmethod, fold = numfolds, strat = FALSE, 
            niter = numfolds, ntrain = numtrain)
        trainlearningsets <- trainlearningsets@learnmatrix
        globalpso_maxitr = dim(trainlearningsets)[1]
    }
    else {
        trainlearningsets <- seq(1, nrow(trainm))
        trainlearningsets <- as.matrix(t(trainlearningsets))
    }
    errortype = "AUC"
    kname = "radial"
    featweight = 0.05
    accuracyweightA = 5
    accuracyweightB = 1
    max_num_feats = 10
    num_feat <- 0
    alltrainm <- trainm
    if (is.factor(trainclass)) {
        alltrainclass <- as.numeric(as.factor(trainclass))
    }
    else {
        alltrainclass <- as.numeric(trainclass)
    }
    overall_fitness <- {
    }
    for (pitr in 1:nrow(trainlearningsets)) {
        trainm <- alltrainm
        trainclass <- alltrainclass
        if (evalMode == "CV1") {
            trainm <- alltrainm[-c(trainlearningsets[pitr, ]), 
                ]
            trainclass <- alltrainclass[-c(trainlearningsets[pitr, 
                ])]
            subtest <- alltrainm[c(trainlearningsets[pitr, ]), 
                ]
            subtestclass <- alltrainclass[c(trainlearningsets[pitr, 
                ])]
            set.seed(seednum)
            subtrain_ind <- sample(x = seq(1, dim(trainm)[1]), 
                size = 10 * dim(trainm)[1], replace = TRUE)
            trainm <- trainm[subtrain_ind, ]
            trainclass <- trainclass[subtrain_ind]
        }
        else {
            if (evalMode == "CV2") {
                trainm <- alltrainm[trainlearningsets[pitr, ], 
                  ]
                trainclass <- alltrainclass[trainlearningsets[pitr, 
                  ]]
                subtest <- alltrainm[-c(trainlearningsets[pitr, 
                  ]), ]
                subtestclass <- alltrainclass[-c(trainlearningsets[pitr, 
                  ])]
            }
            else {
                subtest <- alltrainm
                subtestclass <- alltrainclass
            }
        }
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
            folderrorkfold <- {
            }
            folderror_perm <- {
            }
            seed_vec <- c(129532, 839147, 407700)
            {
                for (f in 1:3) {
                  seednum = seed_vec[f]
                  model <- svm_cv(v = numfolds, x = trainset[, 
                    -1], y = trainset$trainclass, kname = kname, 
                    errortype = errortype, seednum)
                  folderror_cur <- model$mean_acc[1]
                  folderrorkfold <- c(folderrorkfold, folderror_cur)
                  set.seed(seed_vec[f])
                  rand_ind <- sample(x = seq(1, length(trainset$trainclass)), 
                    size = length(trainset$trainclass))
                  seednum = seed_vec[f]
                  model <- svm_cv(v = numfolds, x = trainset[, 
                    -1], y = trainset$trainclass[rand_ind], kname = kname, 
                    errortype = errortype, seednum)
                  folderror_cur_perm <- model$mean_acc[1]
                  folderror_perm <- c(folderror_perm, folderror_cur_perm)
                  rm(model)
                }
            }
            testset <- subtest[, c(col_sel)]
            mod_cv <- svm(x = trainset[, -1], y = trainset$trainclass, 
                type = "C", kernel = kname)
            predfit <- predict(mod_cv, testset)
            svm_table <- table(predfit, subtestclass)
            class_names <- rownames(svm_table)
            beracc <- {
            }
            i <- 1
            svm_acc <- {
            }
            totacc <- length(which(predfit == subtestclass))/length(subtestclass)
            for (c in 1:dim(svm_table)[1]) {
                subtestclass_ind <- which(subtestclass == class_names[c])
                beracc <- c(beracc, length(which(predfit[subtestclass_ind] == 
                  subtestclass[subtestclass_ind]))/length(subtestclass_ind))
            }
            beracc <- as.numeric(beracc)
            beracc <- mean(beracc, na.rm = TRUE)
            if (errortype == "CV") {
                svm_acc[i] <- (totacc * 100)
            }
            else {
                if (errortype == "AUC") {
                  pred_acc <- multiclass.roc(subtestclass, as.numeric(predfit))
                  pred_acc_orig <- pred_acc$auc[1]
                  auc_acc <- pred_acc_orig
                  svm_acc[i] <- (auc_acc * 100)
                }
                else {
                  svm_acc[i] <- (beracc * 100)
                }
            }
            folderror <- svm_acc[i]
            folderrorkfold <- mean(folderrorkfold)
            folderror_perm <- mean(folderror_perm, na.rm = TRUE)
            if (num_feat > max_num_feats) {
                fitfunc <- (accuracyweightA * (folderrorkfold - 
                  folderror_perm)) + accuracyweightB * (folderror) - 
                  (featweight * 100 * (num_feat/length(particle)))
            }
            else {
                fitfunc <- (accuracyweightA * (folderrorkfold - 
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
        overall_fitness <- c(overall_fitness, fitfunc)
    }
    overall_fitness <- mean(overall_fitness)
    return(overall_fitness)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
