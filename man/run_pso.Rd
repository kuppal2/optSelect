\name{run_pso}
\alias{run_pso}
\title{run_pso}
\description{
This function uses a multi agent-based stochastic optimization approach for ensemble feature selection. Stage one involves function perturbation, where ranked list of features are generated using different feature selection methods and stage two involves data perturbation, where feature selection is performed within randomly selected learning sets of the training data. The agents are assigned to different behavior states and move according to a binary PSO algorithm. A multi-objective fitness function is used to evaluate the classifica-tion accuracy of the agents. 
}
\usage{
run_pso(trainm, trainclass, testm, testclass, outloc, transition_matrix, c1 = 2.05, c2 = 2.05, itr = 10, globalpso_maxitr = 1, global_max_itr = 3, num_part = 30, kname = "radial", errortype = "BER", weightA = 0.8, weightB = 0, weightC = 0, weightD = 0.2, featweight.max = 0.01, featweight.min = 0.01, numfolds = 10, followerprob = 0.45, confusionprob = 0.25, leaderprob = 0.25, wmax = 1.2, wmin = 0.4, behavior_reset_itr = 5, maxitrreset = 30, num_neighbors = 5, minselect.pct = 0.5, evalMode = "CV2", minfitnessthresh = 50, maxnum = 0.5, minnum = 3, inertia_method = "global", particlebehav_method = "randbased", constriction_factor = 1, select.global.best = TRUE, numnodes = 20, bootstrap.itr = 10, evalFunc, boostweight = NA, train.pct = 0.8, ...)
}
\arguments{
  \item{trainm}{
  A n x p data matrix with training data, where n is the number of samples in the training set and p is the number of variables
}
  \item{trainclass}{
A n x 1 vector with class labels for instances in the training set
}
  \item{testm}{
  A m x p data matrix with training data, where m is the number of samples in the test set and p is the number of variables
}
  \item{testclass}{
 A m x 1 vector with class labels for instances in the test set
}
  \item{outloc}{
 Output folder location
}
 \item{maxnum}{
 Maximum number of features to select (e.g. maxnum=5)
}
 
}
\value{
Returns a list
\item{scoringmatrix }{Binary matrix with 0 (not selected) or 1 (selected) assignment in each iteration}
\item{bestfeatlist }{Indices of selected features}
\item{bestfeatnames }{Names of selected features}
\item{trainm.new }{Updated training set with only selected features}
\item{testm.new }{Updated test set with only selected features}
\item{trainacc}{Classification accuracy using the optimal set of features in the training set}
\item{testacc}{Classification accuracy using the optimal set of features in the test set}
\item{testauc}{AUC using the optimal set of features in the test set}
}
\author{
Karan Uppal
}