\name{performCMA}
\alias{performCMA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
performCMA(trainm, trainclass, testm, testclass, outloc, maxnum, minnum, stepitr, gsmethods, pct_train = 1, featweight, accuracyweight, kname, maxitrs, minpresent, norm_method, tolerance = 1, classindex, numfacts, numfolds = 10, evalmethod = "CV", CVfoldthresh = 0.3, varselmethod = "backward", scheme_val = "one-vs-all", iter_learn = 5, boostweight = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
  \item{outloc}{
%%     ~~Describe \code{outloc} here~~
}
  \item{maxnum}{
%%     ~~Describe \code{maxnum} here~~
}
  \item{minnum}{
%%     ~~Describe \code{minnum} here~~
}
  \item{stepitr}{
%%     ~~Describe \code{stepitr} here~~
}
  \item{gsmethods}{
%%     ~~Describe \code{gsmethods} here~~
}
  \item{pct_train}{
%%     ~~Describe \code{pct_train} here~~
}
  \item{featweight}{
%%     ~~Describe \code{featweight} here~~
}
  \item{accuracyweight}{
%%     ~~Describe \code{accuracyweight} here~~
}
  \item{kname}{
%%     ~~Describe \code{kname} here~~
}
  \item{maxitrs}{
%%     ~~Describe \code{maxitrs} here~~
}
  \item{minpresent}{
%%     ~~Describe \code{minpresent} here~~
}
  \item{norm_method}{
%%     ~~Describe \code{norm_method} here~~
}
  \item{tolerance}{
%%     ~~Describe \code{tolerance} here~~
}
  \item{classindex}{
%%     ~~Describe \code{classindex} here~~
}
  \item{numfacts}{
%%     ~~Describe \code{numfacts} here~~
}
  \item{numfolds}{
%%     ~~Describe \code{numfolds} here~~
}
  \item{evalmethod}{
%%     ~~Describe \code{evalmethod} here~~
}
  \item{CVfoldthresh}{
%%     ~~Describe \code{CVfoldthresh} here~~
}
  \item{varselmethod}{
%%     ~~Describe \code{varselmethod} here~~
}
  \item{scheme_val}{
%%     ~~Describe \code{scheme_val} here~~
}
  \item{iter_learn}{
%%     ~~Describe \code{iter_learn} here~~
}
  \item{boostweight}{
%%     ~~Describe \code{boostweight} here~~
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
function (trainm, trainclass, testm, testclass, outloc, maxnum, 
    minnum, stepitr, gsmethods, pct_train = 1, featweight, accuracyweight, 
    kname, maxitrs, minpresent, norm_method, tolerance = 1, classindex, 
    numfacts, numfolds = 10, evalmethod = "CV", CVfoldthresh = 0.3, 
    varselmethod = "backward", scheme_val = "one-vs-all", iter_learn = 5, 
    boostweight = NA) 
{
    if (is.na(boostweight) == TRUE) {
        boostweight = rep(0, dim(trainm)[2])
    }
    if (is.na(testm) == TRUE) {
        testm <- trainm
    }
    if (is.na(testclass) == TRUE) {
        testclass <- trainclass
    }
    data_dim <- dim(trainm)
    write.csv(testclass, paste(outloc, "orig_40test.csv", sep = ""), 
        row.names = FALSE)
    write.csv(trainclass, paste(outloc, "orig_60train.csv", sep = ""), 
        row.names = FALSE)
    filestr3 <- paste(outloc, "original_traindata.csv", sep = "")
    write.table(trainm, file = filestr3, sep = ",", row.names = FALSE)
    filestr3 <- paste(outloc, "original_testdata.csv", sep = "")
    write.table(testm, file = filestr3, sep = ",", row.names = FALSE)
    data_dim <- dim(trainm)
    nci_x <- trainm
    nci_y <- as.factor(trainclass)
    rm(trainm)
    nci_xm <- as.matrix(nci_x)
    test_xm <- as.matrix(testm)
    test_y <- as.factor(testclass)
    rm(testm)
    factcols <- lapply(1:dim(nci_xm)[2], function(i) {
        factvec <- as.factor(nci_xm[, i])
        getlevels = levels(factvec)
    })
    factcols <- sapply(factcols, head, n = 1e+17)
    factnums <- lapply(1:dim(nci_xm)[2], function(i) {
        factvec <- length(factcols[[i]])
    })
    factcols = which(factnums <= numfacts)
    factnames = colnames(nci_xm)
    trainfact = nci_xm[, c(factcols)]
    testfact = test_xm[, c(factcols)]
    trainfactnames = colnames(trainfact)
    trainfact = makefactors(trainfact)
    testfact = makefactors(testfact)
    trainfact = as.data.frame(trainfact)
    testfact = as.data.frame(testfact)
    if (length(factcols) > 0) {
        nci_xm = nci_xm[, -c(factcols)]
        test_xm = test_xm[, -c(factcols)]
        trainfact_check = apply(trainfact, 2, as.numeric)
        testfact_check = apply(testfact, 2, as.numeric)
        trainfact_num = {
        }
        {
            for (i in 1:dim(trainfact_check)[2]) {
                trainfact_num = c(trainfact_num, is.na(trainfact_check[1, 
                  i]))
            }
            trainfact_num_index = which(trainfact_num == TRUE)
        }
        if (length(trainfact_num_index) > 0) {
            nci_xm = cbind(apply(nci_xm, 2, as.numeric), trainfact[, 
                -trainfact_num_index])
            test_xm = cbind(apply(test_xm, 2, as.numeric), testfact[, 
                -trainfact_num_index])
            trainfactnames = trainfactnames[trainfact_num_index]
            trainfact = trainfact[, trainfact_num_index]
            testfact = testfact[, trainfact_num_index]
        }
    }
    nci_xm = apply(nci_xm, 2, as.numeric)
    test_xm = apply(test_xm, 2, as.numeric)
    if (norm_method == "minmax") {
        nci_xm = apply(nci_xm, 2, function(x) {
            minx = min(x)
            maxx = max(x)
            if (minx != maxx) {
                ((x - min(x))/(max(x) - min(x)))
            }
            else {
                (x - min(x))/(max(x) - min(x) + 1)
            }
        })
        test_xm = apply(test_xm, 2, function(x) {
            minx = min(x)
            maxx = max(x)
            if (minx != maxx) {
                ((x - min(x))/(max(x) - min(x)))
            }
            else {
                (x - min(x))/(max(x) - min(x) + 1)
            }
        })
    }
    else {
        if (norm_method == "znorm") {
            nci_xm = apply(nci_xm, 2, function(x) {
                (x - mean(x))/(sd(x) + 0.001)
            })
            test_xm = apply(test_xm, 2, function(x) {
                (x - mean(x))/(sd(x) + 0.001)
            })
        }
    }
    bestmod <- 0
    if (evalmethod == "MCCV") {
        set.seed(321)
        train <- GenerateLearningsets(y = nci_y, method = evalmethod, 
            fold = numfolds, strat = FALSE, niter = iter_learn, 
            ntrain = (pct_train * nrow(trainm)))
    }
    else {
        set.seed(321)
        train <- GenerateLearningsets(y = nci_y, method = evalmethod, 
            fold = numfolds, strat = FALSE, niter = iter_learn)
    }
    scoringmatrix = matrix(0, data_dim[2] - 1, length(gsmethods))
    if (maxnum > data_dim[2]) {
        maxnum = data_dim[2]
    }
    else {
        if (maxnum < 3) {
            maxnum = 3
        }
    }
    if (FALSE) {
        varsel <- GeneSelection(nci_xm, nci_y, learningsets = train, 
            method = c("t.test"), scheme = scheme_val)
        genelist = {
        }
        for (i in 1:numfolds) {
            genelist <- c(genelist, toplist(varsel, iter = i, 
                maxnum, show = FALSE)$index)
        }
        genelist <- unlist(genelist)
        genelist <- genelist[order(genelist)]
        genelist <- unique(genelist)
        origgenelist <- genelist
        nci_xm <- nci_xm[, genelist]
        test_xm <- test_xm[, genelist]
    }
    mod_dim = dim(nci_xm)[2]
    scoringmatrix = matrix(0, mod_dim, length(gsmethods))
    class_labels <- levels(as.factor(nci_y))
    num_classes <- length(class_labels)
    if (evalmethod == "MCCV" && pct_train == 1) {
        max_i <- 1
    }
    else {
        max_i <- numfolds * iter_learn
    }
    for (m in 1:length(gsmethods)) {
        varsel <- GeneSelection(nci_xm, nci_y, learningsets = train, 
            method = gsmethods[m], scheme = scheme_val)
        genelist = {
        }
        for (i in 1:(max_i)) {
            for (c in 1:num_classes) {
                t1 <- toplist(varsel, iter = i, k = maxnum, show = FALSE)
                if (num_classes > 2) {
                  genelist <- c(genelist, t1[[c]]$index)
                }
                else {
                  genelist <- c(genelist, t1$index)
                }
            }
        }
        genelist_seltable <- sort(table(genelist), dec = TRUE)
        {
            good_genes <- names(genelist_seltable)
            good_genes <- as.numeric(good_genes)
            genelist <- unique(good_genes)
            num_g = min(length(genelist), maxnum)
            bestmod = 0
            prevacc = 0
            noimprovement = 0
            bestgenelist = {
            }
            errortype = "BER"
            if (varselmethod == "backward") {
                while (num_g > minnum) {
                  trainset <- nci_xm[, genelist[1:num_g]]
                  model <- svm_cv(v = numfolds, x = trainset, 
                    y = nci_y, kname = kname, errortype = "BER")
                  tenfoldacc <- model$mean_acc
                  perm_acc <- {
                  }
                  seed_vec <- c(129532, 839147, 407700)
                  for (r1 in 1:3) {
                    seednum = seed_vec[r1]
                    nci_y_perm <- nci_y[sample(1:length(nci_y), 
                      size = length(nci_y))]
                    model <- svm_cv(v = numfolds, x = trainset, 
                      y = nci_y_perm, kname = kname, errortype = errortype, 
                      seednum)
                    permtenfoldacc <- model$mean_acc
                    perm_acc <- c(perm_acc, permtenfoldacc)
                  }
                  perm_acc <- mean(perm_acc, na.rm = TRUE) + 
                    2 * sd(perm_acc, na.rm = TRUE)
                  foldacc <- tenfoldacc
                  fitfunc <- accuracyweight * (foldacc - perm_acc) + 
                    1 * (foldacc) - (100 * featweight * (1 - 
                    num_g/dim(nci_xm)[2]))
                  if (fitfunc > bestmod) {
                    bestmod <- fitfunc
                    bestmethod <- gsmethods[m]
                    bestgenelist <- genelist[1:num_g]
                    noimprovement = 0
                  }
                  else {
                    if (abs(bestmod - fitfunc) <= tolerance) {
                      noimprovement = 0
                      bestgenelist <- genelist[1:num_g]
                    }
                    else {
                      noimprovement = noimprovement + 1
                    }
                  }
                  prevacc = fitfunc
                  if (noimprovement <= maxitrs) {
                    num_g = num_g - stepitr
                  }
                  else {
                    break
                  }
                }
            }
            else {
                if (varselmethod == "forward") {
                  num_g = minnum
                  maxitrs = 5
                  maxnum = min(length(genelist), maxnum)
                  while (num_g <= maxnum) {
                    trainset <- nci_xm[, genelist[1:num_g]]
                    model <- svm(trainset, nci_y, type = "C", 
                      kernel = kname, cross = numfolds)
                    tenfoldacc <- model$tot.accuracy
                    foldacc <- tenfoldacc
                    fitfunc <- accuracyweight * (foldacc)
                    if (fitfunc > bestmod) {
                      bestmod <- fitfunc
                      bestmethod <- gsmethods[m]
                      bestgenelist <- genelist[1:num_g]
                      noimprovement = 0
                    }
                    else {
                      if (abs(bestmod - fitfunc) <= tolerance) {
                        noimprovement = 0
                        bestgenelist <- genelist[1:num_g]
                      }
                      else {
                        noimprovement = noimprovement + 1
                      }
                    }
                    prevacc = fitfunc
                    if (noimprovement <= maxitrs) {
                      num_g = num_g + stepitr
                    }
                    else {
                      break
                    }
                  }
                }
                else {
                  maxnum = min(length(genelist), maxnum)
                  if (varselmethod == "none") {
                    bestgenelist = genelist[1:maxnum]
                  }
                  else {
                    stop("varselmethod should be none,forward, or backward")
                  }
                }
            }
            scoringmatrix[bestgenelist, m] = 1
        }
    }
    summat = apply(scoringmatrix, 1, sum)
    bestgenelist = which(summat >= minpresent)
    if (length(bestgenelist) < 1) {
        minpresent = minpresent - 1
        bestgenelist = which(summat >= minpresent)
        {
            if (length(bestgenelist) < 1) {
                minpresent = minpresent - 1
                bestgenelist = which(summat >= minpresent)
            }
        }
    }
    dicesorenson_res <- get_DSindex(scoringmatrix)
    if (length(bestgenelist) > 0) {
        modtrain <- as.matrix(nci_xm[, c(bestgenelist)])
        modtest <- as.matrix(test_xm[, c(bestgenelist)])
        boostweight = boostweight[c(bestgenelist)]
    }
    else {
        modtrain <- as.matrix(nci_xm)
        modtest <- as.matrix(test_xm)
    }
    traindim = dim(modtrain)
    testdim = dim(modtest)
    model_train_valid <- svm(modtrain, nci_y, kernel = kname, 
        type = "C")
    pred_test <- predict(model_train_valid, modtest)
    test.table <- table(pred_test, test_y)
    testacc <- sum(diag(test.table))/(dim(modtest)[1])
    pred_acc <- multiclass.roc(test_y, as.numeric(pred_test))
    pred_acc_orig <- pred_acc$auc[1]
    auc_acc <- pred_acc_orig
    model_train <- svm(modtrain, nci_y, kernel = kname, type = "C", 
        cross = 10)
    pred10fold <- fitted(model_train)
    tenfold.table <- table(pred10fold, nci_y)
    filestr3 <- paste(outloc, "modified_cmatest.csv", sep = "")
    write.table(modtest, file = filestr3, sep = ",", row.names = FALSE)
    filestr3 <- paste(outloc, "modified_cmatrain.csv", sep = "")
    write.table(modtrain, file = filestr3, sep = ",", row.names = FALSE)
    filestr3 <- paste(outloc, "modified_cmatest_class.csv", sep = "")
    write.table(test_y, file = filestr3, sep = ",", row.names = FALSE)
    filestr3 <- paste(outloc, "modified_cmatrain_class.csv", 
        sep = "")
    write.table(nci_y, file = filestr3, sep = ",", row.names = FALSE)
    filestr3 <- paste(outloc, "modified_cma_testacc.csv", sep = "")
    write.table(testacc, file = filestr3, sep = ",", row.names = FALSE)
    pred_train <- predict(model_train_valid, modtrain)
    train.table <- table(pred_train, nci_y)
    trainacc <- sum(diag(train.table))/(dim(modtrain)[1])
    filestr3 <- paste(outloc, "modified_cma_trainacc.csv", sep = "")
    write.table(trainacc, file = filestr3, sep = ",", row.names = FALSE)
    filestr3 <- paste(outloc, "modified_cma_10foldacc.csv", sep = "")
    write.table(model_train$tot.accuracy, file = filestr3, sep = ",", 
        row.names = FALSE)
    dicesorenson_res <- get_DSindex(scoringmatrix)
    dicesorenson_res <- get_KIindex(scoringmatrix, maxnum)
    if (dim(scoringmatrix)[2] > 1) {
        m1 <- scoringmatrix
        m2 <- new("list")
        for (i in 1:dim(scoringmatrix)[2]) {
            m2[[i]] <- paste("var", which(m1[, i] == 1), sep = "")
        }
        m3 <- as.data.frame(m2)
        m4 <- t(m3)
        r1 <- RankAggreg(m4, k = maxnum)
        bestgenelistRA <- gsub(x = r1$top.list, pattern = "var", 
            replacement = "")
        bestgenelistRA <- as.numeric(as.character(bestgenelistRA))
        modtrainRA <- as.matrix(nci_xm[, c(bestgenelistRA)])
        modtestRA <- as.matrix(test_xm[, c(bestgenelistRA)])
        model_train_valid <- svm(modtrainRA, nci_y, kernel = kname, 
            type = "C")
        pred_test <- predict(model_train_valid, modtestRA)
        test.table <- table(pred_test, test_y)
        testacc <- sum(diag(test.table))/(dim(modtestRA)[1])
        pred_acc <- multiclass.roc(test_y, as.numeric(pred_test))
        pred_acc_orig <- pred_acc$auc[1]
        auc_acc <- pred_acc_orig
        model_trainRA <- svm(modtrainRA, nci_y, kernel = kname, 
            type = "C", cross = 10)
        pred10foldRA <- fitted(model_trainRA)
        tenfold.tableRA <- table(pred10foldRA, nci_y)
        svm_table <- test.table
        class_names <- rownames(svm_table)
        beracc <- {
        }
        i <- 1
        svm_acc <- {
        }
        subtestclass <- test_y
        predfit <- pred_test
        totacc <- length(which(pred_test == subtestclass))/length(subtestclass)
        for (c in 1:dim(svm_table)[1]) {
            subtestclass_ind <- which(subtestclass == class_names[c])
            beracc <- c(beracc, length(which(predfit[subtestclass_ind] == 
                subtestclass[subtestclass_ind]))/length(subtestclass_ind))
        }
        beracc <- as.numeric(beracc)
        beracc <- mean(beracc, na.rm = TRUE)
        r1 <- RankAggreg(m4, k = maxnum, method = "GA")
        bestgenelistRA <- gsub(x = r1$top.list, pattern = "var", 
            replacement = "")
        bestgenelistRA <- as.numeric(as.character(bestgenelistRA))
        modtrainRA <- as.matrix(nci_xm[, c(bestgenelistRA)])
        modtestRA <- as.matrix(test_xm[, c(bestgenelistRA)])
        model_train_valid <- svm(modtrainRA, nci_y, kernel = kname, 
            type = "C")
        pred_test <- predict(model_train_valid, modtestRA)
        test.table <- table(pred_test, test_y)
        testacc <- sum(diag(test.table))/(dim(modtestRA)[1])
        pred_acc <- multiclass.roc(test_y, as.numeric(pred_test))
        pred_acc_orig <- pred_acc$auc[1]
        auc_acc <- pred_acc_orig
        model_trainRA <- svm(modtrainRA, nci_y, kernel = kname, 
            type = "C", cross = 10)
        pred10foldRA <- fitted(model_trainRA)
        tenfold.tableRA <- table(pred10foldRA, nci_y)
        svm_table <- test.table
        class_names <- rownames(svm_table)
        beracc <- {
        }
        i <- 1
        svm_acc <- {
        }
        subtestclass <- test_y
        predfit <- pred_test
        totacc <- length(which(pred_test == subtestclass))/length(subtestclass)
        for (c in 1:dim(svm_table)[1]) {
            subtestclass_ind <- which(subtestclass == class_names[c])
            beracc <- c(beracc, length(which(predfit[subtestclass_ind] == 
                subtestclass[subtestclass_ind]))/length(subtestclass_ind))
        }
        beracc <- as.numeric(beracc)
        beracc <- mean(beracc, na.rm = TRUE)
    }
    if (length(trainfact) > 1) {
        colnames(trainfact) = trainfactnames
        colnames(testfact) = trainfactnames
        modtrain = cbind(apply(modtrain, 2, as.numeric), trainfact)
        modtest = cbind(apply(modtest, 2, as.numeric), testfact)
    }
    modtrain = as.data.frame(modtrain)
    modtest = as.data.frame(modtest)
    return(list(modgenelist = bestgenelist, modtraindata = modtrain, 
        modtestdata = modtest, blindtest = testacc, modtrainclass = nci_y, 
        modtestclass = test_y, numfeat = dim(modtrain)[2], testacc = testacc, 
        tenfoldacc = model_train$tot.accuracy, learningsets = train@learnmatrix, 
        boostweight = boostweight, scoringmatrix = scoringmatrix))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
