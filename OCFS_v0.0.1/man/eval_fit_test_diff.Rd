\name{eval_fit_test_diff}
\alias{eval_fit_test_diff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eval_fit_test_diff(particle, numfolds, trainm, trainclass, testm, testclass, errortype = "AUC", kname = "radial", featweight = 0.05, accuracyweightA = 5, accuracyweightB = 1, accuracyweightC = 1, accuracyweightD = 0, max_num_feats = 10)
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
  \item{accuracyweightC}{
%%     ~~Describe \code{accuracyweightC} here~~
}
  \item{accuracyweightD}{
%%     ~~Describe \code{accuracyweightD} here~~
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
    accuracyweightB = 1, accuracyweightC = 1, accuracyweightD = 0, 
    max_num_feats = 10) 
{
    num_feat <- 0
    ind <- which(particle == 1)
    folderror_perm <- {
    }
    num_itrs <- 3
    col_sel <- ind
    num_feat <- length(col_sel)
    testclassorig <- testclass
    testmorig <- testm
    if (num_feat > 2) {
        trainset <- trainm[, c(col_sel)]
        trainset <- cbind(trainclass, trainset)
        trainset <- data.frame(trainset)
        folderror <- {
        }
        folderror_vec <- {
        }
        seed_vec <- runif(num_itrs, 1, 1e+06)
        seed_vec <- c(129532)
        setseed = seed_vec[1]
        model <- svm_cv(v = numfolds, x = trainset[, -1], y = trainset$trainclass, 
            kname = kname, errortype = errortype, setseed = setseed)
        foldacc <- model$confint[1]
        origtenfoldacc <- {
        }
        perm_acc <- {
        }
        seed_vec <- c(129532, 839147, 407700)
        for (r1 in 1:num_itrs) {
            seednum = seed_vec[r1]
            model <- svm_cv(v = numfolds, x = trainset[, -1], 
                y = trainset$trainclass, kname = kname, errortype = errortype, 
                setseed = seednum)
            foldacc <- model$confint[1]
            set.seed(seed_vec[r1])
            rand_ind <- sample(x = seq(1, length(trainset$trainclass)), 
                size = length(trainset$trainclass))
            model <- svm_cv(v = numfolds, x = trainset[, -1], 
                y = trainset$trainclass[rand_ind], kname = kname, 
                errortype = errortype, setseed = seednum)
            permtenfoldacc <- model$confint[2]
            perm_acc <- c(perm_acc, permtenfoldacc)
            origtenfoldacc <- c(origtenfoldacc, foldacc)
        }
        foldacc <- mean(origtenfoldacc, na.rm = TRUE) - 1.96 * 
            sd(origtenfoldacc, na.rm = TRUE)/sqrt(num_itrs)
        folderror_test <- foldacc
        folderror <- (foldacc)
        folderror_perm <- (perm_acc)
        set.seed(seed_vec[r1])
        rand_ind <- sample(x = seq(1, length(trainset$trainclass)), 
            size = length(trainset$trainclass))
        mod_cv <- svm(x = trainset[, -1], y = trainset$trainclass, 
            type = "C", kernel = kname)
        testclass <- trainset$trainclass
        testset <- trainset[, -c(1)]
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
        folderror_train <- svm_acc[i]
        beracc <- {
        }
        svm_acc <- {
        }
        for (i in 1:num_itrs) {
            set.seed(seed_vec[i])
            rand_ind <- sample(x = seq(1, length(trainset$trainclass)), 
                size = length(trainset$trainclass))
            mod_cv <- svm(x = trainset[, -1], y = trainset$trainclass[rand_ind], 
                type = "C", kernel = kname)
            predfit <- predict(mod_cv, trainset[, -1])
            svm_table <- table(predfit, testclass)
            testclass <- trainset$trainclass
            testset <- trainset[, -c(1)]
            class_names <- rownames(svm_table)
            totacc <- length(which(predfit == testclass))/length(testclass)
            for (c in 1:dim(svm_table)[1]) {
                testclass_ind <- which(testclass == class_names[c])
                beracc <- c(beracc, length(which(predfit[testclass_ind] == 
                  testclass[testclass_ind]))/length(testclass_ind))
            }
            beracc <- as.numeric(beracc)
            beracc <- mean(beracc, na.rm = TRUE)
            if (errortype == "CV") {
                svm_acc <- c(svm_acc, (totacc * 100))
            }
            else {
                if (errortype == "AUC") {
                  pred_acc <- multiclass.roc(testclass, as.numeric(predfit))
                  pred_acc_orig <- pred_acc$auc[1]
                  auc_acc <- pred_acc_orig
                  svm_acc <- c(svm_acc, auc_acc * 100)
                }
                else {
                  svm_acc <- c(svm_acc, (beracc * 100))
                }
            }
        }
        folderror_train_perm <- mean(svm_acc, na.rm = TRUE) + 
            1.96 * sd(svm_acc, na.rm = TRUE)/(sqrt(num_itrs))
        {
            testset <- testm[, c(col_sel)]
            trainclass_temp <- trainclass
            trainset_temp <- trainm[, c(col_sel)]
            trainset <- testset
            trainclass <- testclass
            testclass <- trainclass_temp
            testset <- trainset_temp
            max_lim <- max(100, dim(testset)[1])
            set.seed(321)
            subtrain_ind <- sample(x = seq(1, dim(trainset)[1]), 
                size = max_lim, replace = TRUE)
            trainset <- trainset[subtrain_ind, ]
            trainclass <- trainclass[subtrain_ind]
            mod_cv <- svm(x = trainset, y = trainclass, type = "C", 
                kernel = kname)
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
            folderror_test2 <- svm_acc[i]
        }
        {
            folderror_perm <- mean(folderror_perm, na.rm = TRUE) + 
                (1.96 * (sd(folderror_perm, na.rm = TRUE))/sqrt(num_itrs))
            {
                if (FALSE) {
                  weightA <- 0.9
                  weightB <- 0
                  weightC <- 0.1
                  weightD <- 0.001
                }
                if (FALSE) {
                  weightA <- 0.7
                  weightB <- 0
                  weightC <- 0.05
                  weightD <- 0.2
                  weightE <- 0.05
                  featweight <- weightE
                }
                fitfunc <- (accuracyweightA * (folderror - folderror_perm)) + 
                  accuracyweightB * (folderror_train - folderror_train_perm) + 
                  accuracyweightC * (folderror_test2) + accuracyweightD * 
                  (folderror) + ((featweight * (max((100 * (max_num_feats - 
                  num_feat))/max_num_feats, 100))))
            }
        }
        if (FALSE) {
            fitfunc <- accuracyweightA * (foldacc - perm_acc) + 
                accuracyweightB * (folderror_test) - (100 * featweight * 
                ((dim(trainset)[2]/dim(trainm)[2])))
            folderror_perm <- perm_acc
            folderror <- foldacc
        }
        if (FALSE) {
            t1 <- t.test(x = folderror, y = folderror_perm, paired = T, 
                alternative = "greater")
            if (t1$p.value < 0.05) {
                folderror_perm <- mean(folderror_perm, na.rm = TRUE) + 
                  (2 * (sd(folderror_perm, na.rm = TRUE))/sqrt(num_itrs))
                folderror <- mean(folderror, na.rm = TRUE) - 
                  (2 * (sd(folderror, na.rm = TRUE))/sqrt(num_itrs))
                fitfunc <- (accuracyweightA * (folderror - folderror_perm)) + 
                  accuracyweightB * (folderror_test) - (featweight * 
                  100 * (num_feat/length(particle)))
            }
            else {
                fitfunc <- (-100)
            }
        }
        if (FALSE) {
            folderror_perm <- mean(folderror_perm, na.rm = TRUE) + 
                (2 * (sd(folderror_perm, na.rm = TRUE)))
            fitfunc <- (accuracyweightA * (folderror - folderror_perm)) + 
                accuracyweightB * (folderror) + 0 * folderror_test - 
                (featweight * 100 * (num_feat/length(particle)))
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
