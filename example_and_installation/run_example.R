library(optSelect)
data(Golub)

# call run_pso function() to perform the two-stage feature selection process. In stage one, features are selected using
# different feature selection methods. In stage two, the results from individual feature selection methods are aggregated
# using the binary behavior-based PSO (B3PSO)
#Description of arguments:
#trainm: A n x p data matrix with training data, where n is the number of samples in the training set and p is the number of variables
#trainclass: A n x 1 vector with class labels for instances in the training set
#testm:  A m x p data matrix with training data, where m is the number of samples in the test set and p is the number of variables
#testclass: A m x 1 vector with class labels for instances in the test set
#outloc: Output folder location
#maxnum: Number of top features to select (e.g. 5)
#num_part: Number of particles in the PSO (e.g. 5, 10, or 20)
#stage1.featsel.methods: Feature selection methods to use in stage one. Default: c("limma","lasso","rfe","elasticnet", "f.test")
#Note: rfe stands for recursive feature elimination with SVM
#Other feature seleciton methods: "kruskal.test", "wilcox.test", "welch.test", "t.test", "rf" (for random forest)
#train.pct: In each iteration, what percentage of training data should be used for feature selection & inner cross-validation? A value of
#           0.7 would mean that 70% of training instances would be used for selecting the features and 30% will be used for evaluating
#           the classification accuracy. Default: 0.7
#c1: Cognitive scaling parameter in B3PSO. Default: 2.05
#c2: Social scaling parameter in B3PSO. Default: 2.05
#global_max_itr: Number of times to reset the swarm as a result of no change in performance before the B3PSO is terminated. Default: 3
#globalpso_maxitr: Number of times the B3PSO should be run on different subsets of training data. Default: 10
#numfolds: Number of k-folds for cross-validation. Default: 10

system.time(optselect_res<-run_pso(trainm=Golub$trainm,trainclass=Golub$trainclass,testm=Golub$testm,testclass=Golub$testclass,
outloc="~/optSelectGolub/",maxnum=5,num_part=5,stage1.featsel.methods=c("limma","lasso","rfe","elasticnet", "f.test"),train.pct=0.7,
c1=2.05,c2=2.05,global_max_itr=3,globalpso_maxitr=10,numfolds = 10))

print("Selected features:")
print(optselect_res$bestfeatnames)
print("Test set AUC:")
print(optselect_res$testauc)

print("Complete")

