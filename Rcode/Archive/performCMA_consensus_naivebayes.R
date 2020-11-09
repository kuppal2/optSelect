
#library(CMA)
#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/golub_R/filt_proc_golub2_orig.csv", sep=",", header=T)

#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/nci_R/mean_replace_nci.csv", sep=",", header=T)
#ncid<-ncid[,-c(1)]



performCMA<-function(datafile, outloc, maxnum, minnum, stepitr, gsmethods, percentTest,featweight, accuracyweight, kname, maxitrs, minpresent)
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
nci_y<-as.factor(ncid[,data_dim[2]])
nci_xm<-as.matrix(nci_x)

test_xm<-as.matrix(testm[,-c(data_dim[2])])

test_y<-as.factor(testm[,data_dim[2]])

print("test class")
print(test_y[1:4])

#kname="radial"
numfolds<-10
bestmod<-0
#methods=c("t.test", "lasso", "f.test", "kruskal.test", "rfe", "rf") # "elasticnet", "boosting")
#, "lasso", "elasticnet", "boosting")
#methods=c("t.test", "rfe", "rf", "kruskal.test", "f.test")

train<-GenerateLearningsets(y=nci_y, method="CV", fold=10, strat=TRUE)

scoringmatrix=matrix(0,data_dim[2]-1,length(gsmethods))


varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=c("t.test"))
	genelist={}
	for(i in 1:numfolds)
	{
		genelist<-c(genelist,toplist(varsel,iter=i,maxnum, show=FALSE)$index)

	}
	genelist<-unique(genelist)
origgenelist<-genelist	

nci_xm<-nci_xm[,genelist]
test_xm<-test_xm[,genelist]

mod_dim=dim(nci_xm)[2]
scoringmatrix=matrix(0,mod_dim,length(gsmethods))


tolerance=1
for(m in 1:length(gsmethods))
{
	varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=gsmethods[m])
	genelist={}
	for(i in 1:numfolds)
	{
		genelist<-c(genelist,toplist(varsel,iter=i,length(origgenelist), show=FALSE)$index)

	}
	genelist<-unique(genelist)

	#for(num_g in seq(minnum, maxnum, step))
	num_g=minnum
	bestmod=0
	prevacc=0
	noimprovement=0
	bestgenelist={}
	#while((noimprovement<=totitrs) || num_g<maxnum)
	while(num_g<maxnum)
	{
		
		trainset<-nci_xm[,genelist[1:num_g]]

		#model<-svm(trainset, nci_y, type="C", kernel=kname, cross=numfolds)
		
		#foldacc<-model$tot.accuracy

		res<-naivebayes_cv(10,trainset,nci_y)
		foldacc<-1-res$avg
		
		fitfunc<-accuracyweight*(foldacc)+featweight*(1-num_g)

		if(fitfunc>bestmod)
		{
			bestmod<-fitfunc
			bestmethod<-gsmethods[m]
			bestgenelist<-genelist[1:num_g]
			noimprovement=0
		}
		else
		{
			if((prevacc-fitfunc)<=tolerance)
			{
				noimprovement=noimprovement+1
			}
		}
		prevacc=fitfunc
		#prevacc=foldacc
		print(paste(gsmethods[m],":",num_g, ":", fitfunc,sep=""))
		if(noimprovement<=maxitrs)
		{
			num_g=num_g+stepitr
		}
		else
		{
			num_g=maxnum
		}
	}
	
	scoringmatrix[bestgenelist,m]=1
}
summat=apply(scoringmatrix,1,sum)

bestgenelist=which(summat>=minpresent)


if(length(bestgenelist)<1)
{
	minpresent=minpresent-1
	bestgenelist=which(summat>=minpresent)
	{
		if(length(bestgenelist)<1)
		{
			minpresent=minpresent-1
			bestgenelist=which(summat>=minpresent)
		}
	#	else
	#	{
	#		bestgenelist=origgenelist
	#	}
	}
	
}

print("bestgenelist")

featnames=colnames(nci_xm)

selectedfeats=featnames[c(bestgenelist)]

print(bestgenelist)
if(length(bestgenelist)>0)
{
	modtrain<-as.matrix(nci_xm[,c(bestgenelist)])
	modtest<-as.matrix(test_xm[,c(bestgenelist)])
}
else
{
	modtrain<-as.matrix(nci_xm)
	modtest<-as.matrix(test_xm)
}
traindim=dim(modtrain)
testdim=dim(modtest)

print(modtest[1:4,])
print(modtrain[1:4,])
print(paste("numgenes selected:",length(bestgenelist),sep=""))
#print(paste("best acc:", bestmod, sep=""))
#print(paste("best method:", bestmethod, sep=""))

#modtrain<-ncid[,c(bestgenelist,data_dim[2])]
#modtest<-testm[,c(bestgenelist, data_dim[2])]

#model_train_valid<-svm(nci_xm[,bestgenelist], nci_y,   kernel=kname, type="C")
#pred_train<-predict(model_train_valid, testm[,bestgenelist])

#model_train_valid<-svm(modtrain,  nci_y,   kernel=kname, type="C")

model_train_valid<-naiveBayes(modtrain,  nci_y,   kernel=kname, type="C")
pred_train<-predict(model_train_valid, modtest)
test.table<-table(pred_train, test_y)

#model_train<-svm(modtrain,  nci_y,   kernel=kname, type="C", cross=10)

testacc<-sum(diag(test.table))/(dim(modtest)[1])
print(paste("test acc:", testacc, sep=""))

model_train<-naivebayes_cv(10,modtrain,nci_y)
#print(paste("10 fold train", model_train$tot.accuracy, sep=""))

print(paste("10 fold train", (1-model_train$avg), sep=""))



print("cma mod train dim is ")
print(dim(modtrain))

print("cma mod test dim is ")
print(dim(modtest))
#modtrain<-cbind(modtrain, as.factor(nci_y))
#modtest<-cbind(modtest, as.factor(test_y))

#modtrain<-nci_xm[,c(bestgenelist,dim(ncid)[2])]
#modtest<-test_xm[,c(bestgenelist, dim(testm)[2])]

return(list(modgenelist=selectedfeats, modtraindata=modtrain, modtestdata=modtest, blindtest=testacc, modtrainclass=nci_y, modtestclass=test_y, numfeat=length(bestgenelist)))
#return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, method=bestmethod, blindtest=testacc))
print("Complete")

}

