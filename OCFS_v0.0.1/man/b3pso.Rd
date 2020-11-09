\name{b3pso}
\alias{b3pso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
b3pso(outloc, dimsize, transition_matrix, c1 = 2.05, c2 = 2.05, itr = 10, globalpso_maxitr = 1, global_max_itr = 3, num_part = 30, kname = "radial", errortype = "BER", weightA = 0.8, weightB = 0, weightC = 0, weightD = 0.2, featweight.max = 0.01, featweight.min = 0.01, numfolds = 10, followerprob = 0.45, confusionprob = 0.25, leaderprob = 0.25, wmax = 1.2, wmin = 0.4, behavior_reset_itr = 5, maxitrreset = 30, num_neighbors = 5, minselect.pct = 0.8, evalMode = "CV1", minfitnessthresh = 50, maxnum = 0.5, minnum = 3, inertia_method = "rankbased", particlebehav_method = "rankbased", constriction_factor = 1, select.global.best = TRUE, numnodes = 20, bootstrap.itr = 10, seednum = NA, varnames = NA, itr.terminate = TRUE, evalFunc, trainm = NA, trainclass = NA, boostweight = NA, train.pct = 0.8, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outloc}{
%%     ~~Describe \code{outloc} here~~
}
  \item{dimsize}{
%%     ~~Describe \code{dimsize} here~~
}
  \item{transition_matrix}{
%%     ~~Describe \code{transition_matrix} here~~
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
  \item{seednum}{
%%     ~~Describe \code{seednum} here~~
}
  \item{varnames}{
%%     ~~Describe \code{varnames} here~~
}
  \item{itr.terminate}{
%%     ~~Describe \code{itr.terminate} here~~
}
  \item{evalFunc}{
%%     ~~Describe \code{evalFunc} here~~
}
  \item{trainm}{
%%     ~~Describe \code{trainm} here~~
}
  \item{trainclass}{
%%     ~~Describe \code{trainclass} here~~
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
function (outloc, dimsize, transition_matrix, c1 = 2.05, c2 = 2.05, 
    itr = 10, globalpso_maxitr = 1, global_max_itr = 3, num_part = 30, 
    kname = "radial", errortype = "BER", weightA = 0.8, weightB = 0, 
    weightC = 0, weightD = 0.2, featweight.max = 0.01, featweight.min = 0.01, 
    numfolds = 10, followerprob = 0.45, confusionprob = 0.25, 
    leaderprob = 0.25, wmax = 1.2, wmin = 0.4, behavior_reset_itr = 5, 
    maxitrreset = 30, num_neighbors = 5, minselect.pct = 0.8, 
    evalMode = "CV1", minfitnessthresh = 50, maxnum = 0.5, minnum = 3, 
    inertia_method = "rankbased", particlebehav_method = "rankbased", 
    constriction_factor = 1, select.global.best = TRUE, numnodes = 20, 
    bootstrap.itr = 10, seednum = NA, varnames = NA, itr.terminate = TRUE, 
    evalFunc, trainm = NA, trainclass = NA, boostweight = NA, 
    train.pct = 0.8, ...) 
{
    if (is.na(boostweight) == TRUE) {
        boostweight = rep(0, dim(trainm)[2])
    }
    parentevalMode <- evalMode
    testacc_all <- {
    }
    if (evalMode == "CV1" || evalMode == "CV2") {
        numtrain <- (train.pct * nrow(trainm))
        evalmethod = "MCCV"
        set.seed(321)
        trainlearningsets <- GenerateLearningsets(y = trainclass, 
            method = evalmethod, fold = globalpso_maxitr, strat = FALSE, 
            niter = globalpso_maxitr, ntrain = numtrain)
        trainlearningsets <- trainlearningsets@learnmatrix
        globalpso_maxitr = dim(trainlearningsets)[1]
        evalFunc = eval_fit_test_diff
    }
    if (evalMode != "custom") {
        alltrainm <- trainm
        alltrainclass <- trainclass
    }
    scoringmatrix = matrix(0, dimsize, globalpso_maxitr)
    for (globalpso_itr in 1:globalpso_maxitr) {
        if (evalMode == "CV1") {
            trainm <- alltrainm[-c(trainlearningsets[globalpso_itr, 
                ]), ]
            trainclass <- alltrainclass[-c(trainlearningsets[globalpso_itr, 
                ])]
            subtest <- alltrainm[c(trainlearningsets[globalpso_itr, 
                ]), ]
            subtestclass <- alltrainclass[c(trainlearningsets[globalpso_itr, 
                ])]
            set.seed(321)
            subtrain_ind <- sample(x = seq(1, dim(trainm)[1]), 
                size = 10 * dim(trainm)[1], replace = TRUE)
            trainm <- trainm[subtrain_ind, ]
            trainclass <- trainclass[subtrain_ind]
        }
        else {
            if (evalMode == "CV2") {
                trainm <- alltrainm[trainlearningsets[globalpso_itr, 
                  ], ]
                trainclass <- alltrainclass[trainlearningsets[globalpso_itr, 
                  ]]
                subtest <- alltrainm[-c(trainlearningsets[globalpso_itr, 
                  ]), ]
                subtestclass <- alltrainclass[-c(trainlearningsets[globalpso_itr, 
                  ])]
            }
            else {
                subtest <- alltrainm
                subtestclass <- alltrainclass
            }
        }
        ll <- 0
        ul <- 1
        bad_pos <- new("list")
        num_obstacles <- 1
        fitness_x <- array(1e+17, dim = c(num_part, 1))
        fitness_gbest <- 1e+14
        cverror_gbest <- (-10000)
        cvpermerror_gbest <- (-10000)
        prev_gbest <- fitness_gbest + 1
        fitness_lbest <- array(1e+17, dim = c(num_part, 1))
        if (is.na(varnames) == TRUE) {
            feat_names <- paste("var", seq(1, dimsize), sep = "")
        }
        else {
            feat_names <- varnames
        }
        feat_list <- feat_names
        global_no_change <- 0
        no_change <- 0
        global_best_index <- 1e+19
        min_fit_x <- 1e+15
        min_fit_index <- 1e+17
        x <- array(0, dim = c(num_part, dimsize))
        v <- array(0, dim = c(num_part, dimsize))
        x_gbest <- array(0, dim = c(dimsize))
        p_gbest <- array(0, dim = c(dimsize))
        x_lbest <- array(0, dim = c(num_part, dimsize))
        count_feat <- array(0, dim = c(num_part))
        a <- c(0.7, 0.05, 0.05, 0.2)
        b <- c(0.2, 0.6, 0.05, 0.15)
        c <- c(0.1, 0.2, 0.6, 0.1)
        d <- c(0.25, 0.1, 0.05, 0.6)
        initial_state_prob_matrix <- rbind(a, b, c, d)
        set.seed(321)
        agent_behavior <- sample(x = 1:dim(transition_matrix)[1], 
            size = num_part, replace = TRUE, prob = c(confusionprob, 
                followerprob, leaderprob, (1 - (confusionprob + 
                  followerprob + leaderprob))))
        prob_behavior_particles <- array(0, dim = c(num_part, 
            dim(transition_matrix)[2]))
        for (row in 1:num_part) {
            num_feat <- 0
            prob_behavior_particles[row, ] <- initial_state_prob_matrix[agent_behavior[row], 
                ]
            ran <- runif(dimsize, 0, 1)
            for (col in 1:dimsize) {
                if (ran[col] < 0) {
                  x[row, col] <- 0
                  x_lbest[row, col] <- 0
                }
                else {
                  x[row, col] <- 1
                  x_lbest[row, col] <- 1
                  num_feat <- num_feat + 1
                }
            }
            count_feat[row] <- num_feat
        }
        num_featl <- dimsize
        num_featg <- dimsize
        overall_x_gbest <- x[1, ]
        cl <- makeCluster(numnodes)
        clusterEvalQ <- function(cl, expr) clusterCall(cl, eval, 
            substitute(expr), env = .GlobalEnv)
        clusterEvalQ(cl, library(e1071))
        clusterEvalQ(cl, library(MASS))
        clusterEvalQ(cl, library(CMA))
        clusterCall(cl, function() library(pROC))
        itr_data = {
        }
        k <- 0
        rank_vec <- seq(1, num_part)
        repeat {
            k <- k + 1
            feat_sel <- 0
            itr_val = {
            }
            min_fit_x <- 1e+15
            min_fit_index <- 1e+17
            part_list = new("list")
            rank_sum_vec <- summary(rank_vec)
            for (i in 1:num_part) {
                part_list[[i]] <- x[i, ]
                if (k\%\%behavior_reset_itr == 0) {
                  agent_behavior <- sample(x = 1:dim(transition_matrix)[1], 
                    size = num_part, replace = TRUE, prob = c(confusionprob, 
                      followerprob, leaderprob, (1 - (confusionprob + 
                        followerprob + leaderprob))))
                  if (particlebehav_method == "rankbased") {
                    prob_behavior_particles[i, ] <- initial_state_prob_matrix[agent_behavior[i], 
                      ]
                    if (rank_vec[i] >= rank_sum_vec[5]) {
                      prob_actions <- prob_behavior_particles[i, 
                        ] \%*\% (transition_matrix \%^\% k)
                      prob_actions <- as.vector(prob_actions)
                      sum_actions <- summary(c(prob_actions))
                      best_actions <- which(prob_actions >= sum_actions[3])
                      best_action <- sample(x = best_actions, 
                        size = 1)
                    }
                    else {
                      if (rank_vec[i] >= rank_sum_vec[3]) {
                        prob_actions <- prob_behavior_particles[i, 
                          ] \%*\% (transition_matrix \%^\% k)
                        prob_actions <- as.vector(prob_actions)
                        sum_actions <- summary(c(prob_actions))
                        best_actions <- which(prob_actions >= 
                          sum_actions[3])
                        best_action <- sample(x = best_actions, 
                          size = 1)
                      }
                      else {
                        best_action <- sample(seq(1, 4), size = 1)
                      }
                    }
                    agent_behavior[i] <- best_action
                  }
                }
            }
            boot_fitness <- new("list")
            boot_cv <- new("list")
            boot_cvperm <- new("list")
            if (featweight.min == featweight.max) {
                featweightcur = featweight.min
            }
            else {
                featweightcur <- featweight.min + (((featweight.max - 
                  featweight.min)/k))
            }
            if (featweightcur < featweight.min) {
                featweightcur <- featweight.min
            }
            fitness_x <- {
            }
            cverror <- {
            }
            cvpermerror <- {
            }
            if (evalMode == "bootstrap") {
                clusterExport(cl, "svm_cv")
                all_ind <- seq(1, dim(trainm)[1])
                for (boot_itr in 1:bootstrap.itr) {
                  subtrain_ind <- sample(x = seq(1, dim(trainm)[1]), 
                    size = dim(alltrainm)[1], replace = TRUE)
                  subtrain <- trainm[subtrain_ind, ]
                  subtrainclass <- trainclass[subtrain_ind]
                  subtrain_ind <- unique(subtrain_ind)
                  subtest_ind <- all_ind[-subtrain_ind]
                  subtest <- trainm[subtest_ind, ]
                  subtestclass <- trainclass[subtest_ind]
                  fitness_x <- {
                  }
                  cverror <- {
                  }
                  cvpermerror <- {
                  }
                  evalFunc = eval_fit_test_diff
                  res1 <- clusterApply(cl, part_list, eval_fit_test_diff, 
                    numfolds = numfolds, trainm = subtrain, trainclass = subtrainclass, 
                    testm = subtest, testclass = subtestclass, 
                    errortype = errortype, kname = kname, accuracyweightA = weightA, 
                    accuracyweightB = weightB, accuracyweightC = weightC, 
                    accuracyweightD = weightD, featweight = featweightcur, 
                    max_num_feats = maxnum)
                  for (np in 1:num_part) {
                    fitness_x <- c(fitness_x, res1[[np]]$fitfunc)
                    cverror <- c(cverror, res1[[np]]$cverror)
                    cvpermerror <- c(cvpermerror, res1[[np]]$cvpermerror)
                  }
                  boot_fitness[[boot_itr]] <- fitness_x
                  boot_cv[[boot_itr]] <- cverror
                  boot_cvperm[[boot_itr]] <- cvpermerror
                }
                boot_fitness <- as.data.frame(boot_fitness)
                boot_fitness <- apply(boot_fitness, 1, function(x) {
                  mean(x, na.rm = TRUE) + 1.96 * (sd(x, na.rm = TRUE)/sqrt(bootstrap.itr))
                })
                fitness_x <- (1) * boot_fitness
                cverror <- as.data.frame(boot_cv)
                cverror <- apply(cverror, 1, function(x) {
                  mean(x, na.rm = TRUE) + 1.96 * (sd(x, na.rm = TRUE)/sqrt(bootstrap.itr))
                })
                cvpermerror <- as.data.frame(boot_cvperm)
                cvpermerror <- apply(cvpermerror, 1, function(x) {
                  mean(x, na.rm = TRUE) + 1.96 * (sd(x, na.rm = TRUE)/sqrt(bootstrap.itr))
                })
            }
            else {
                if (evalMode == "CV1" || evalMode == "CV2") {
                  clusterExport(cl, "svm_cv")
                  fitness_x <- {
                  }
                  cverror <- {
                  }
                  cvpermerror <- {
                  }
                  evalFunc = eval_fit_test_diff
                  res1 <- clusterApply(cl, part_list, eval_fit_test_diff, 
                    numfolds = numfolds, trainm = trainm, trainclass = trainclass, 
                    testm = subtest, testclass = subtestclass, 
                    errortype = errortype, kname = kname, accuracyweightA = weightA, 
                    accuracyweightB = weightB, accuracyweightC = weightC, 
                    accuracyweightD = weightD, featweight = featweightcur, 
                    max_num_feats = maxnum)
                  for (np in 1:num_part) {
                    fitness_x <- (1) * fitness_x
                    fitness_x <- c(fitness_x, res1[[np]]$fitfunc)
                    cverror <- c(cverror, res1[[np]]$cverror)
                    cvpermerror <- c(cvpermerror, res1[[np]]$cvpermerror)
                    if (cverror < cvpermerror) {
                      bad_pos[[num_obstacles]] <- x[np, ]
                      num_obstacles <- num_obstacles + 1
                    }
                  }
                }
                else {
                  if (evalMode == "CV") {
                    clusterExport(cl, "svm_cv")
                    evalFunc = eval_fit_kfold_diff
                    res1 <- clusterApply(cl, part_list, eval_fit_kfold_diff, 
                      trainm = trainm, trainclass = trainclass, 
                      numfolds = numfolds, errortype = errortype, 
                      accuracyweightA = accuracyweightA, accuracyweightB = accuracyweightB, 
                      featweight = featweightcur, max_num_feats = maxnum, 
                      kname = kname)
                    for (np in 1:num_part) {
                      fitness_x <- c(fitness_x, res1[[np]]$fitfunc)
                      cverror <- c(cverror, res1[[np]]$cverror)
                      cvpermerror <- c(cvpermerror, res1[[np]]$cvpermerror)
                      if (cverror < cvpermerror) {
                        bad_pos[[num_obstacles]] <- x[np, ]
                        num_obstacles <- num_obstacles + 1
                      }
                    }
                    fitness_x <- (1) * fitness_x
                  }
                  else {
                    res1 <- clusterApply(cl, part_list, evalFunc, 
                      ...)
                    for (np in 1:num_part) {
                      fitness_x <- c(fitness_x, res1[[np]])
                    }
                    fitness_x <- (1) * fitness_x
                  }
                }
            }
            med_fit_x <- (1) * median(fitness_x)
            min_fit_x <- min(fitness_x)
            min_fit_index <- which(fitness_x == min_fit_x)
            bestind <- runif(1, 1, length(which(fitness_x == 
                min_fit_x)))
            bestind <- min_fit_index[bestind]
            numfeatl <- length(which(x[bestind[1], ] == 1))
            nfeats_perpart <- {
            }
            for (i in 1:num_part) {
                nfeats_perpart <- c(nfeats_perpart, length(which(x[i, 
                  ] == 1)))
                if (fitness_x[i] < fitness_lbest[i]) {
                  fitness_lbest[i] <- fitness_x[i]
                  for (j in 1:dimsize) {
                    x_lbest[i, j] <- x[i, j]
                  }
                  num_featl <- length(which(x[i, ] == 1))
                }
            }
            fitness_var <- as.vector(fitness_x)
            rank_vec <- rank(fitness_var)
            if (min_fit_x < (fitness_gbest)) {
                no_change <- 0
                global_no_change <- 0
                fitness_gbest <- min_fit_x
                cverror_gbest <- max(cverror)[1]
                cvpermerror_gbest <- max(cvpermerror)[1]
                global_best_index <- bestind
                num_featg <- num_featl
                cverror_gbest <- cverror[bestind]
                cvpermerror_gbest <- cvpermerror[bestind]
                for (j in 1:dimsize) {
                  x_gbest[j] <- x[global_best_index, j]
                }
                overall_gbest = x_gbest
                overall_x_gbest <- x_gbest
            }
            else {
                no_change <- no_change + 1
                if (no_change > maxitrreset) {
                  global_no_change <- global_no_change + 1
                  fitness_lbest <- array(1e+17, dim = c(num_part, 
                    1))
                  x_lbest_vec <- apply(x_lbest, 2, mean)
                  if (global_no_change > global_max_itr & length(which(x_lbest_vec > 
                    0)) > minnum) {
                    x_lbest_vec[which(x_lbest_vec >= minselect.pct)] <- 1
                    x_lbest_vec[which(x_lbest_vec < minselect.pct)] <- 0
                    d1 <- dist(as.matrix(rbind(x_lbest_vec, overall_x_gbest)))^2
                    d1pct <- 100 * (d1/length(x_lbest_vec))
                    if (d1pct <= 1 && global_no_change > (global_max_itr)) {
                      pdf_name <- paste("vel_xgbest_itr", globalpso_itr, 
                        ".pdf", sep = "")
                      plot(v[global_best_index, which(overall_x_gbest == 
                        1)], xlab = "velocity")
                      dev.off()
                      break
                    }
                    else {
                      rand_num <- runif(num_part, 0, 1)
                      for (row in 1:num_part) {
                        num_feat <- 0
                        set.seed(321)
                        ran <- runif(1, 0, 1)
                        for (col in 1:dimsize) {
                          if (ran < 0.7) {
                            set.seed(321)
                            ran2 <- runif(1, 0, 1)
                            if (ran2 < 0.9) {
                              x[row, col] <- 0
                            }
                            else {
                              x[row, col] <- 1
                              num_feat <- num_feat + 1
                            }
                          }
                          else {
                            x[row, col] <- x_gbest[col]
                          }
                        }
                        count_feat[row] <- num_feat
                      }
                    }
                  }
                  for (i in 1:num_part) {
                    x[i, ] <- x_lbest_vec
                  }
                  agent_behavior[row] <- sample(x = seq(1, dim(transition_matrix)[1]), 
                    prob = c(0.25, 0.25, 0.25, 0.25), size = num_part, 
                    replace = TRUE)
                  if (global_no_change > (global_max_itr * 1.5)) {
                    break
                  }
                  no_change <- 0
                }
            }
            nn_search_res <- find_similar_samples(x, NA, num_neighbors)
            for (i in 1:num_part) {
                feat_sel <- 0
                w <- 1
                if (inertia_method == "rankbased") {
                  w <- wmin + (wmax - wmin)/((rank_vec[i]))
                }
                else {
                  if (inertia_method == "random") {
                    w <- (wmin) + (runif(1, wmin, wmax)/2)
                  }
                  else {
                    if (inertia_method == "global") {
                      w <- 1
                    }
                    else {
                      if (inertia_method == "dec") {
                        w <- w - (wmax - wmin)/((itr * 0.5))
                      }
                    }
                  }
                }
                x_curbest <- x_gbest
                best_action <- agent_behavior[i]
                social_status <- 1
                if (best_action == 1) {
                  set.seed(321)
                  ran <- runif(dimsize, 0, 1)
                  for (col in 1:dimsize) {
                    if (ran[col] < 0.9) {
                      x_curbest[col] <- 0
                    }
                    else {
                      x_curbest[col] <- 1
                    }
                  }
                }
                else {
                  if (best_action == 2) {
                    x_curbest_ind <- nn_search_res[i, c(1:num_neighbors)]
                    if (FALSE) {
                      best_fitness_neighbor <- which(fitness_x[x_curbest_ind] == 
                        min(fitness_x[x_curbest_ind], na.rm = TRUE)[1])[1]
                      x_curbest <- x[x_curbest_ind[best_fitness_neighbor], 
                        ]
                    }
                    {
                      if (num_neighbors > 1) {
                        x_curbest <- apply(x[x_curbest_ind, ], 
                          2, function(x) {
                            y <- quantile(x, 0.75)
                            return(round(y))
                          })
                      }
                      else {
                        x_curbest <- x[x_curbest_ind[1], ]
                      }
                    }
                  }
                  else {
                    if (best_action == 3) {
                      x_curbest <- x_gbest
                    }
                    else {
                      social_status <- 0
                    }
                  }
                }
                r1 <- runif(dimsize, 0, 1)
                r2 <- runif(dimsize, 0, 1)
                r3 <- runif(dimsize, 0, 1)
                for (j in 1:dimsize) {
                  v[i, j] <- constriction_factor * ((w * v[i, 
                    j]) + (c1 * r1[j] * (x_lbest[i, j] - x[i, 
                    j])) + (c2 * r2[j] * (x_curbest[j] - x[i, 
                    j]) * social_status))
                  if (v[i, j] > 6) {
                    v[i, j] = 6
                  }
                  if (v[i, j] < (-6)) {
                    v[i, j] = -6
                  }
                  S <- 1/(1 + exp(-v[i, j]))
                  S <- S + boostweight[j]
                  if (S >= r3[j]) {
                    x[i, j] <- 1
                    feat_sel <- feat_sel + 1
                    count_feat[i] <- feat_sel
                  }
                  else {
                    x[i, j] <- 0
                  }
                }
                check_badpos <- match(bad_pos, x[i, ])
                num_badpos <- length(which(is.na(check_badpos) == 
                  FALSE))
                if (num_badpos > 0) {
                  x[i, ] <- x_lbest[i, ]
                }
            }
            x_lbest_mean <- apply(x_lbest, 2, mean)
            feat_global_num <- length(which(overall_x_gbest == 
                1))
            feat_ind <- which(x_lbest_mean >= (minselect.pct))
            len_feat_ind <- length(feat_ind)
            if (fitness_gbest < (-90) & feat_global_num <= maxnum & 
                (global_no_change <= global_max_itr)) {
            }
            d1 <- dist(as.matrix(rbind(x_lbest_mean, overall_x_gbest)))^2
            d1pct <- 100 * (d1/length(x_lbest_mean))
            if (k > global_max_itr) {
                if (fitness_gbest < (-minfitnessthresh) & (global_no_change >= 
                  global_max_itr) & d1pct <= 3) {
                }
                else {
                  itr = k + itr - 1
                }
            }
            itr_val <- {
            }
            cnames_sum <- c("Iteration #", "Inertia", "Number of features", 
                "Global best fitness", "Current best fitness", 
                "Number of features in current best agent", "Number of features in global best", 
                "Number of features in local best")
            itr_val <- cbind(itr_val, k)
            itr_val <- cbind(itr_val, w)
            itr_val <- cbind(itr_val, num_featg)
            if (evalMode != "custom") {
                itr_val <- cbind(itr_val, cverror_gbest)
                itr_val <- cbind(itr_val, cvpermerror_gbest)
                cnames_sum <- c("Iteration #", "Inertia", "Number of features", 
                  "CV best", "Permuted CV best", "Global best fitness", 
                  "Current best fitness", "Number of features in current best agent", 
                  "Number of features in global best", "Number of features in local best")
            }
            itr_val <- cbind(itr_val, (-1) * fitness_gbest)
            itr_val <- cbind(itr_val, (-1) * min_fit_x)
            itr_val <- cbind(itr_val, count_feat[min_fit_index])
            itr_val <- cbind(itr_val, length(which(overall_x_gbest == 
                1)))
            itr_val <- cbind(itr_val, length(which(x_lbest[min_fit_index, 
                ] == 1)))
            itr_data <- rbind(itr_data, itr_val)
        }
        filestr <- paste(outloc, "multiobj_itr", globalpso_itr, 
            "_descpso.txt", sep = "")
        bestgenelist <- which(overall_x_gbest == 1)
        scoringmatrix[bestgenelist, globalpso_itr] = 1
        colnames(itr_data) <- cnames_sum
        write.table(itr_data, file = filestr, sep = "\t", row.names = F)
        x_lbest_mean <- apply(x_lbest, 2, mean)
        feat_ind <- which(x_lbest_mean >= minselect.pct)
        if (select.global.best == TRUE) {
            feat_ind <- which(overall_x_gbest == 1)
        }
        if (length(feat_ind) > 0) {
            bestgenelist <- feat_ind
            scoringmatrix[bestgenelist, globalpso_itr] = 1
            modtrain <- as.matrix(trainm[, c(bestgenelist)])
            modtest <- as.matrix(subtest[, c(bestgenelist)])
            model_train_valid <- svm(modtrain, trainclass, kernel = kname, 
                type = "C")
            pred_test <- predict(model_train_valid, modtest)
            test.table <- table(pred_test, subtestclass)
            testacc <- sum(diag(test.table))/(dim(modtest)[1])
        }
        testacc_all <- c(testacc_all, testacc)
        sumr <- paste("global num features ", length(feat_list) - 
            1, "current best set of features ", overall_x_gbest, 
            "global best accuracy ", 1 - fitness_gbest, " test acc ", 
            testacc)
        filestr2 <- paste(outloc, "selected_feature_index_itr", 
            globalpso_itr, ".csv", sep = "")
        write.table(feat_ind, file = filestr2, sep = ",", row.names = FALSE)
        stopCluster(cl)
    }
    return(list(scoringmatrix = scoringmatrix, acc = testacc_all))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
