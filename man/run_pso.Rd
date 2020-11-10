\name{run_pso}
\alias{run_pso}
\title{run_pso}
\description{
This function uses a multi agent-based stochastic optimization approach for ensemble feature selection. Stage one involves function perturbation, where ranked list of features are generated using different feature selection methods and stage two involves data perturbation, where feature selection is performed within randomly selected learning sets of the training data. The agents are assigned to different behavior states and move according to a binary PSO algorithm. A multi-objective fitness function is used to evaluate the classifica-tion accuracy of the agents. 
}
\usage{
run_pso(trainm, trainclass, testm, testclass, outloc,maxnum)
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
\item{num_part}{
 Number of particles in the PSO (e.g. 5, 10, or 20)
}
\item{stage1.featsel.methods}{
 Feature selection methods to use in stage one. Default: c("limma","lasso","rfe","elasticnet", "f.test")
}
\item{train.pct}{
 In each iteration, what percentage of training data should be used for feature selection & inner cross-validation? A value of
 0.7 would mean that 70 percent of training instances would be used for selecting the features and 30 percent 
 will be used for evaluating the classification accuracy. Default: 0.7
}
\item{c1}{
 Cognitive scaling parameter in PSO. Default: 2.05
}
\item{c2}{
 Social scaling parameter in PSO. Default: 2.05
}
\item{global_max_itr}{
 Number of times to reset the swarm as a result of no change in performance before the PSO is terminated. Default: 3
}
\item{globalpso_maxitr}{
 Number of times the PSO should be run on different subsets of training data. Default: 10
}
\item{numfolds}{
 Number of k-folds for cross-validation. Default: 10
}

}
\value{
\item{scoringmatrix }{Binary matrix with 0 (not selected) or 1 (selected) assignment in each iteration}
\item{bestfeatlist }{Indices of selected features}
\item{bestfeatnames }{Names of selected features}
\item{trainm.new }{Updated training set with only selected features}
\item{testm.new }{Updated test set with only selected features}
\item{trainacc}{Classification accuracy using the optimal set of features in the training set}
\item{testacc}{Classification accuracy using the optimal set of features in the test set}
\item{testauc}{AUC using the optimal set of features in the test set}
}
\author{Karan Uppal}