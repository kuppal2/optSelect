library(optSelect)
data(Golub)

# call run_pso function()
system.time(optselect_res<-run_pso(trainm=Golub$trainm,trainclass=Golub$trainclass,testm=Golub$testm,testclass=Golub$testclass,
outloc="/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/res/",maxnum=5))


print(optselect_res$bestfeatnames)
print(optselect_res$test_auc)

scoringmatrix<-as.data.frame(optselect_res$scoringmatrix)
print(scoringmatrix)
print(feat_names[feat_ind])
print("Complete")

