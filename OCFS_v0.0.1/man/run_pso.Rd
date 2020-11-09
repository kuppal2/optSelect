\name{run_pso}
\alias{run_pso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
run_pso(trainm, trainclass, testm, testclass, transition_matrix, outloc, c1 = 2.05, c2 = 2.05, itr = 10, globalpso_maxitr = 1, global_max_itr = 3, num_part = 30, kname = "radial", errortype = "BER", weightA = 0.8, weightB = 0, weightC = 0, weightD = 0.2, featweight.max = 0.01, featweight.min = 0.01, numfolds = 10, followerprob = 0.45, confusionprob = 0.25, leaderprob = 0.25, wmax = 1.2, wmin = 0.4, behavior_reset_itr = 5, maxitrreset = 30, num_neighbors = 5, minselect.pct = 0.5, evalMode = "CV2", minfitnessthresh = 50, maxnum = 0.5, minnum = 3, inertia_method = "global", particlebehav_method = "randbased", constriction_factor = 1, select.global.best = TRUE, numnodes = 20, bootstrap.itr = 10, evalFunc, boostweight = NA, train.pct = 0.8, ...)
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
  \item{transition_matrix}{
%%     ~~Describe \code{transition_matrix} here~~
}
  \item{outloc}{
%%     ~~Describe \code{outloc} here~~
}
  \item{c1}{
%%     ~~Describe \code{c1} here~~
}
  \item{c2}{
%%     ~~Describe \code{c2} here~~
}
  \item{itr}{
%%     ~~Describe \code{itr} here~~
}
  \item{globalpso_maxitr}{
%%     ~~Describe \code{globalpso_maxitr} here~~
}
  \item{global_max_itr}{
%%     ~~Describe \code{global_max_itr} here~~
}
  \item{num_part}{
%%     ~~Describe \code{num_part} here~~
}
  \item{kname}{
%%     ~~Describe \code{kname} here~~
}
  \item{errortype}{
%%     ~~Describe \code{errortype} here~~
}
  \item{weightA}{
%%     ~~Describe \code{weightA} here~~
}
  \item{weightB}{
%%     ~~Describe \code{weightB} here~~
}
  \item{weightC}{
%%     ~~Describe \code{weightC} here~~
}
  \item{weightD}{
%%     ~~Describe \code{weightD} here~~
}
  \item{featweight.max}{
%%     ~~Describe \code{featweight.max} here~~
}
  \item{featweight.min}{
%%     ~~Describe \code{featweight.min} here~~
}
  \item{numfolds}{
%%     ~~Describe \code{numfolds} here~~
}
  \item{followerprob}{
%%     ~~Describe \code{followerprob} here~~
}
  \item{confusionprob}{
%%     ~~Describe \code{confusionprob} here~~
}
  \item{leaderprob}{
%%     ~~Describe \code{leaderprob} here~~
}
  \item{wmax}{
%%     ~~Describe \code{wmax} here~~
}
  \item{wmin}{
%%     ~~Describe \code{wmin} here~~
}
  \item{behavior_reset_itr}{
%%     ~~Describe \code{behavior_reset_itr} here~~
}
  \item{maxitrreset}{
%%     ~~Describe \code{maxitrreset} here~~
}
  \item{num_neighbors}{
%%     ~~Describe \code{num_neighbors} here~~
}
  \item{minselect.pct}{
%%     ~~Describe \code{minselect.pct} here~~
}
  \item{evalMode}{
%%     ~~Describe \code{evalMode} here~~
}
  \item{minfitnessthresh}{
%%     ~~Describe \code{minfitnessthresh} here~~
}
  \item{maxnum}{
%%     ~~Describe \code{maxnum} here~~
}
  \item{minnum}{
%%     ~~Describe \code{minnum} here~~
}
  \item{inertia_method}{
%%     ~~Describe \code{inertia_method} here~~
}
  \item{particlebehav_method}{
%%     ~~Describe \code{particlebehav_method} here~~
}
  \item{constriction_factor}{
%%     ~~Describe \code{constriction_factor} here~~
}
  \item{select.global.best}{
%%     ~~Describe \code{select.global.best} here~~
}
  \item{numnodes}{
%%     ~~Describe \code{numnodes} here~~
}
  \item{bootstrap.itr}{
%%     ~~Describe \code{bootstrap.itr} here~~
}
  \item{evalFunc}{
%%     ~~Describe \code{evalFunc} here~~
}
  \item{boostweight}{
%%     ~~Describe \code{boostweight} here~~
}
  \item{train.pct}{
%%     ~~Describe \code{train.pct} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (trainm, trainclass, testm, testclass, transition_matrix, 
    outloc, c1 = 2.05, c2 = 2.05, itr = 10, globalpso_maxitr = 1, 
    global_max_itr = 3, num_part = 30, kname = "radial", errortype = "BER", 
    weightA = 0.8, weightB = 0, weightC = 0, weightD = 0.2, featweight.max = 0.01, 
    featweight.min = 0.01, numfolds = 10, followerprob = 0.45, 
    confusionprob = 0.25, leaderprob = 0.25, wmax = 1.2, wmin = 0.4, 
    behavior_reset_itr = 5, maxitrreset = 30, num_neighbors = 5, 
    minselect.pct = 0.5, evalMode = "CV2", minfitnessthresh = 50, 
    maxnum = 0.5, minnum = 3, inertia_method = "global", particlebehav_method = "randbased", 
    constriction_factor = 1, select.global.best = TRUE, numnodes = 20, 
    bootstrap.itr = 10, evalFunc, boostweight = NA, train.pct = 0.8, 
    ...) 
{
    trainm <- as.data.frame(trainm)
    if (is.na(testm) == TRUE) {
        testm <- trainm
    }
    if (is.na(testclass) == TRUE) {
        testclass <- trainclass
    }
    parentevalMode <- evalMode
    scoringmatrix = matrix(0, dim(trainm)[2], globalpso_maxitr)
    max_num_feats = ceiling(maxnum * dim(trainm)[2])
    overall_gbest = array(0, dim = c(d_dim[2]))
    trainm <- as.data.frame(trainm)
    testm <- as.data.frame(testm)
    mod <- svm(x = trainm, y = trainclass, type = "C", cross = 10)
    testmim = dim(testm)[2]
    alltrainm <- trainm
    alltrainclass <- trainclass
    evalMode <- parentevalMode
    d_dim <- dim(trainm)
    d <- dim(trainm)[2]
    scoringmatrix <- b3pso(outloc = outloc, dimsize = dim(trainm)[2], 
        transition_matrix = transition_matrix, c1 = c1, c2 = c2, 
        globalpso_maxitr = globalpso_maxitr, global_max_itr = global_max_itr, 
        num_part = num_part, kname = kname, errortype = errortype, 
        weightA = weightA, weightB = weightB, weightC = weightC, 
        weightD = weightD, featweight.max = featweight.max, featweight.min = featweight.min, 
        numfolds = numfolds, followerprob = followerprob, confusionprob = confusionprob, 
        leaderprob = leaderprob, wmax = wmax, wmin = wmin, behavior_reset_itr = behavior_reset_itr, 
        maxitrreset = maxitrreset, num_neighbors = num_neighbors, 
        minselect.pct = minselect.pct, evalMode = evalMode, minfitnessthresh = minfitnessthresh, 
        maxnum = maxnum, minnum = minnum, inertia_method = inertia_method, 
        particlebehav_method = particlebehav_method, constriction_factor = constriction_factor, 
        select.global.best = select.global.best, numnodes = numnodes, 
        bootstrap.itr = bootstrap.itr, evalFunc = evalFunc, trainm = trainm, 
        trainclass = trainclass, boostweight = boostweight, train.pct = train.pct, 
        ...)
    testacc <- scoringmatrix$acc
    scoringmatrix <- scoringmatrix$scoringmatrix
    trainm <- alltrainm
    trainclass <- alltrainclass
    summat = apply(scoringmatrix, 1, sum)
    dicesorenson_res <- get_DSindex(scoringmatrix)
    k = dim(scoringmatrix)[1]
    ki_res <- get_KIindex(scoringmatrix, k)
    s1 <- summary(summat)
    num_itr_thresh <- s1[3]
    pct_range <- seq(2, globalpso_maxitr)
    min_fitness_res <- (1e+11)
    for (num_itr_thresh in pct_range) {
        bestgenelist = which(summat >= num_itr_thresh)
        x <- rep(0, length(summat))
        featweightcur <- 0
        x[bestgenelist] <- 1
        fitness_res <- eval_fit_test_diff(particle = x, numfolds = numfolds, 
            trainm = trainm, trainclass = trainclass, testm = trainm, 
            testclass = trainclass, errortype = errortype, kname = kname, 
            accuracyweightA = 0.7, accuracyweightB = 0, accuracyweightC = 0.05, 
            accuracyweightD = 0.25, featweight = featweightcur, 
            max_num_feats = maxnum)
        fitness_res <- fitness_res$fitfunc
        if (fitness_res <= (min_fitness_res - 1)) {
            best_thresh <- num_itr_thresh
            best_results <- bestgenelist
            min_fitness_res <- fitness_res
        }
    }
    summary_testacc <- summary(testacc, na.rm = TRUE)
    bestgenelist <- best_results
    feat_ind <- bestgenelist
    feat_names <- colnames(trainm)
    feat_list <- feat_names[feat_ind]
    feat_col <- 0
    feat_col <- feat_ind
    if (length(feat_ind) < 1) {
    }
    filestr2 <- paste(outloc, "selected_feature_index_final.csv", 
        sep = "")
    write.table(feat_col, file = filestr2, sep = ",", row.names = FALSE)
    trainmata <- trainm
    testmata <- testm
    rm(trainm)
    rm(testm)
    finalset <- trainmata[, c(feat_col)]
    test_mod <- testmata[, c(feat_col)]
    filestr2 <- paste(outloc, "modified_trainpso.csv", sep = "")
    Class = trainclass
    modtrain <- cbind(finalset, Class)
    Class = testclass
    modtest <- cbind(test_mod, Class)
    write.table(modtrain, file = filestr2, sep = ",", row.names = FALSE)
    filestr3 <- paste(outloc, "modified_testpso.csv", sep = "")
    write.table(modtest, file = filestr3, sep = ",", row.names = FALSE)
    modtrain <- data.frame(modtrain)
    modtest <- data.frame(modtest)
    model_train_err <- svm(modtrain$Class ~ ., data = modtrain, 
        type = "C", cross = 10)
    filestr3 <- paste(outloc, "10foldaccuracy.csv", sep = "")
    write.table(model_train_err$tot.accuracy, file = filestr3, 
        sep = ",", row.names = FALSE)
    model_train_valid <- svm(finalset, trainclass, kernel = kname, 
        type = "C")
    pred_train <- predict(model_train_valid, finalset)
    train.table <- table(pred_train, trainclass)
    error <- 1 - sum(diag(train.table))/(dim(finalset)[1])
    mod_dim = dim(modtest)[2]
    test_mod <- as.data.frame(test_mod)
    rm(test_mod)
    rm(finalset)
    trainf = paste(outloc, "modified_trainpso.csv", sep = "")
    testf = paste(outloc, "modified_testpso.csv", sep = "")
    trainm <- read.csv(trainf, header = TRUE)
    testm <- read.csv(testf, header = TRUE)
    trainm <- as.data.frame(trainm)
    testm <- as.data.frame(testm)
    mod <- svm(trainm$Class ~ ., data = trainm, type = "C")
    testmim = dim(testm)[2]
    trainmim = dim(trainm)[2]
    pred <- predict(mod, testm[, -c(testmim)])
    test.table = table(pred, testm$Class)
    testacc <- (sum(diag(test.table))/(dim(testm)[1]))
    filestr3 <- paste(outloc, "testaccuracy.csv", sep = "")
    write.table(testacc, file = filestr3, sep = ",", row.names = FALSE)
    if (dim(trainm)[2] > 3) {
        trainx = subset(trainm, select = -c(Class))
        trainy = trainm$Class
        testx = subset(testm, select = -c(Class))
        testy = testm$Class
        mod <- svm(trainm$Class ~ ., data = trainm, type = "C", 
            cross = 10)
        filestr3 <- paste(outloc, "psotenfoldaccuracy.csv", sep = "")
        write.table(mod$tot.accuracy, file = filestr3, sep = ",", 
            row.names = FALSE)
        trainf = paste(outloc, "modified_trainpso.csv", sep = "")
        testf = paste(outloc, "modified_testpso.csv", sep = "")
        trainm <- read.csv(trainf, header = TRUE)
        testm <- read.csv(testf, header = TRUE)
        trainm <- as.data.frame(trainm)
        testm <- as.data.frame(testm)
        mod <- svm(trainm$Class ~ ., data = trainm, type = "C")
        testmim = dim(testm)[2]
        pred <- predict(mod, testm[, -c(testmim)])
        test.table = table(pred, testm$Class)
        print("Number of features in the final set:")
        print(dim(trainm)[2] - 1)
        testacc <- (sum(diag(test.table))/(dim(testm)[1]))
        filestr3 <- paste(outloc, "testaccuracy.csv", sep = "")
        write.table(testacc, file = filestr3, sep = ",", row.names = FALSE)
        pred_acc <- multiclass.roc(testm$Class, as.numeric(pred))
        pred_acc_orig <- pred_acc$auc[1]
        auc_acc <- pred_acc_orig
        print("Test AUC using the final set of features:")
        print(auc_acc)
        print("Test confusion matrix is ")
        print(test.table)
        print("Classification accuracy in test set:")
        print(testacc)
        filestr3 <- paste(outloc, "testAUC.csv", sep = "")
        write.table(auc_acc, file = filestr3, sep = ",", row.names = FALSE)
        pred <- predict(mod, trainm[, -c(trainmim)])
        trainacc <- (sum(diag(train.table))/(dim(trainm)[1]))
        print("Training confusion matrix is ")
        print(train.table)
        print("Classification accuracy in training set:")
        print(trainacc)
        filestr3 <- paste(outloc, "trainaccuracy.csv", sep = "")
        write.table(trainacc, file = filestr3, sep = ",", row.names = FALSE)
    }
    print("# of features after PSO:")
    print(dim(trainm)[2])
    return(list(scoringmatrix = scoringmatrix, bestfeatlist = bestgenelist, 
        bestfeatnames = feat_names, model = mod, trainm = trainm, 
        testm = testm, outercv = testacc, trainacc = trainacc, 
        testacc = testacc, testauc = auc_acc))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
