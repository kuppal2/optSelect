library(optSelect)
data(Golub)

# call run_pso function()
system.time(optselect_res<-run_pso(trainm=Golub$trainm,trainclass=Golub$trainclass,testm=Golub$testm,testclass=Golub$testclass,
outloc="~/optSelectGolub/",maxnum=5,num_part=5))

print("Selected features:")
print(optselect_res$bestfeatnames)
print("Test set AUC:")
print(optselect_res$testauc)

print("Complete")

