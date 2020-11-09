
#library(CMA)
#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/golub_R/filt_proc_golub2_orig.csv", sep=",", header=T)

#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/nci_R/mean_replace_nci.csv", sep=",", header=T)
#ncid<-ncid[,-c(1)]



performCMA<-function(datafile, outloc, maxnum, minnum, step, methods, percentTest,featweight, accuracyweight, kname)
{
data_dim<-dim(datafile)
classA=levels(datafile[,data_dim[2]])[1]
classB=levels(datafile[,data_dim[2]])[2]

id<-sample(1:data_dim[1],size=percentTest*data_dim[1],replace=F)

testm<-datafile[id,]

ncid<-datafile[-id,]

write.csv(testm[,1], paste(outloc,"orig_40test.csv",sep=""), row.names=FALSE)
write.csv(ncid[,1], paste(outloc,"orig_60train.csv",sep=""), row.names=FALSE)

testm<-testm[,-c(1)]
ncid<-ncid[,-c(1)]


data_dim<-dim(ncid)
print(data_dim)


nci_x<-ncid[,-c(data_dim[2])]
nci_y<-ncid[,data_dim[2]]
nci_xm<-as.matrix(nci_x)

test_y<-testm[,data_dim[2]]

print("Train class")
print(nci_y[1:2])
#kname="radial"
numfolds<-10
bestmod<-0

#methods=c("t.test", "lasso", "f.test", "kruskal.test", "rfe", "rf") # "elasticnet", "boosting")
#, "lasso", "elasticnet", "boosting")
#methods=c("t.test", "rfe", "rf", "kruskal.test", "f.test")

train<-GenerateLearningsets(y=nci_y, method="CV", fold=10, strat=TRUE)


for(m in 1:length(methods))
{
	varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=methods[m])
	genelist={}
	for(i in 1:numfolds)
	{
		genelist<-c(genelist,toplist(varsel,iter=i,maxnum, show=FALSE)$index)

	}
	genelist<-unique(genelist)

	for(num_g in seq(minnum, maxnum, step))
	{
		
		trainset<-nci_xm[,genelist[1:num_g]]

		model<-svm(trainset, nci_y, type="C", kernel=kname, cross=numfolds)

		foldacc<-model$tot.accuracy

		fitfunc<-accuracyweight*(foldacc)+featweight*(1-num_g)

		if(fitfunc>bestmod)
		{
			bestmod<-fitfunc
			bestmethod<-methods[m]
			bestgenelist<-genelist[1:num_g]
		}

		print(paste(methods[m],":",foldacc,sep=""))
	}
}
print("Complete")
print(paste("numgenes selected:",length(bestgenelist),sep=""))
print(paste("best acc:", bestmod, sep=""))
print(paste("best method:", bestmethod, sep=""))

#modtrain<-ncid[,c(bestgenelist,data_dim[2])]
#modtest<-testm[,c(bestgenelist, data_dim[2])]

modtrain<-ncid[,c(bestgenelist)]
modtest<-testm[,c(bestgenelist)]

modtrain<-cbind(modtest,as.matrix(nci_y))
modtest<-cbind(modtest,as.matrix(test_y))

model_train_valid<-svm(nci_xm[,bestgenelist], nci_y,   kernel=kname, type="C")


#pred_train<-predict(model_train_valid, testm[,bestgenelist])
pred_train<-predict(model_train_valid, as.matrix(test_y))
#test.table<-table(pred_train, testm[,data_dim[2]])
test.table<-table(pred_train, test_y)

testacc<-sum(diag(test.table))/(dim(modtest)[1])

return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, method=bestmethod, blindtest=testacc))
}

