
#library(CMA)
#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/golub_R/filt_proc_golub2_orig.csv", sep=",", header=T)

#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/nci_R/mean_replace_nci.csv", sep=",", header=T)
#ncid<-ncid[,-c(1)]



performCMA<-function(datafile, outloc, maxnum, minnum, stepitr, gsmethods, percentTest,featweight, accuracyweight, kname, maxitrs, minpresent, norm_method, tolerance=1)
{
data_dim<-dim(datafile)
classA=levels(datafile[,data_dim[2]])[1]
classB=levels(datafile[,data_dim[2]])[2]

id<-sample(1:data_dim[1],size=percentTest*data_dim[1],replace=F)

testm<-datafile[id,]

ncid<-datafile[-id,]

rm(datafile)
write.csv(testm[,1], paste(outloc,"orig_40test.csv",sep=""), row.names=FALSE)
write.csv(ncid[,1], paste(outloc,"orig_60train.csv",sep=""), row.names=FALSE)

testm<-testm[,-c(1)]
ncid<-ncid[,-c(1)]


data_dim<-dim(ncid)
print(data_dim)


nci_x<-ncid[,-c(data_dim[2])]
nci_y<-as.factor(ncid[,data_dim[2]])
rm(ncid)

nci_xm<-as.matrix(nci_x)
test_xm<-as.matrix(testm[,-c(data_dim[2])])

test_y<-as.factor(testm[,data_dim[2]])

rm(testm)

print("test class")
print(test_y[1:4])

print("orig train matrix")
print(nci_xm[1:5,1:10])
#nci_xm<-as.matrix(nci_xm)
#test_xm<-as.matrix(test_xm)
print("orig train matrix")
print(nci_xm[1:5,1:10])
print(as.numeric(nci_xm[1:3,3]))
if(norm_method=="minmax")
{
	print("minmax normalization")
	nci_xm=apply(nci_xm,2,function(x){ 
	minx=min(x)
	maxx=max(x)
	if(minx!=maxx)
	{
		((x-min(x))/(max(x)-min(x)))
	}
	else
        {       
                (x-min(x))/(max(x)-min(x)+1)
        }
	})
	
	test_xm=apply(test_xm,2,function(x){ 

	minx=min(x)
        maxx=max(x)
        if(minx!=maxx)
        {
                ((x-min(x))/(max(x)-min(x)))
        }
	else
	{
		(x-min(x))/(max(x)-min(x)+1)
	}
        })
        
#((x-min(x))/(max(x)-min(x)))})
}
else
{

	if(norm_method=="znorm")
	{
		print("znorm")
        	nci_xm=apply(nci_xm,2,function(x){ (x-mean(x))/(sd(x)+0.001)})
        	test_xm=apply(test_xm,2,function(x){(x-mean(x))/(sd(x)+0.001)})
	}
}

print("norm train matrix")
print(nci_xm[1:5,1:10])
print("mean of feat 2")
print(mean(nci_xm[,2]))
print("sd of feat 2")
print(sd(nci_xm[,2]))
#kname="radial"
numfolds<-dim(nci_xm)[1]-1
bestmod<-0
#methods=c("t.test", "lasso", "f.test", "kruskal.test", "rfe", "rf") # "elasticnet", "boosting")
#, "lasso", "elasticnet", "boosting")
#methods=c("t.test", "rfe", "rf", "kruskal.test", "f.test")

train<-GenerateLearningsets(y=nci_y, method="LOOCV") #, fold=10, strat=TRUE)

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


#tolerance=0.5
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
	#while((noimprovement<=maxitrs) || num_g<maxnum)
	while(num_g<maxnum)
	{
		
		trainset<-nci_xm[,genelist[1:num_g]]

		model<-svm(trainset, nci_y, type="C", kernel=kname, cross=numfolds)

		fitval<-fitted(model)

                tab1=table(fitval, nci_y)
                foldacc=(tab1[1,1]/sum(tab1[,1]))+(tab1[2,2]/sum(tab1[,2]))
                foldacc=100*(0.5*foldacc)
		
		#foldacc<-model$tot.accuracy

		fitfunc<-accuracyweight*(foldacc)+featweight*(1-num_g)

		#if(abs(prevacc-fitfunc)<=tolerance) #>bestmod)
		if(fitfunc<bestmod)
		{
			noimprovement=noimprovement+1
		}
		else
		{
			if(fitfunc>=bestmod)
			{
			bestmod<-fitfunc
			bestmethod<-gsmethods[m]
			bestgenelist<-genelist[1:num_g]
			noimprovement=0
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
print("dim of scoring matrix is ")
print(dim(scoringmatrix))

bestgenelist=which(summat>=minpresent)

print(length(summat))
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
print(modtrain[1:3,])
print(modtest[1:3,])



print(paste("numgenes selected:",length(bestgenelist),sep=""))
#print(paste("best acc:", bestmod, sep=""))
#print(paste("best method:", bestmethod, sep=""))

#modtrain<-ncid[,c(bestgenelist,data_dim[2])]
#modtest<-testm[,c(bestgenelist, data_dim[2])]

#model_train_valid<-svm(nci_xm[,bestgenelist], nci_y,   kernel=kname, type="C")
#pred_train<-predict(model_train_valid, testm[,bestgenelist])

model_train_valid<-svm(modtrain,  nci_y,   kernel=kname, type="C")
pred_train<-predict(model_train_valid, modtest)
test.table<-table(pred_train, test_y)

model_train<-svm(modtrain,  nci_y,   kernel=kname, type="C", cross=10)
testacc<-sum(diag(test.table))/(dim(modtest)[1])
print(paste("test acc:", testacc, sep=""))

print(paste("10 fold train", model_train$tot.accuracy, sep=""))

 filestr3<-paste(outloc, "modified_cmatest.csv", sep="")
        write.table(modtest, file=filestr3, sep=",", row.names=FALSE)
 filestr3<-paste(outloc, "modified_cmatrain.csv", sep="")
        write.table(modtrain, file=filestr3, sep=",", row.names=FALSE)
 filestr3<-paste(outloc, "modified_cmatest_class.csv", sep="")
        write.table(test_y, file=filestr3, sep=",", row.names=FALSE)
 
 filestr3<-paste(outloc, "modified_cmatrain_class.csv", sep="")
        write.table(nci_y, file=filestr3, sep=",", row.names=FALSE)


 filestr3<-paste(outloc, "modified_cma_testacc.csv", sep="")
        write.table(testacc, file=filestr3, sep=",", row.names=FALSE)

#modtrain<-cbind(modtrain, as.factor(nci_y))
#modtest<-cbind(modtest, as.factor(test_y))

#modtrain<-nci_xm[,c(bestgenelist,dim(ncid)[2])]
#modtest<-test_xm[,c(bestgenelist, dim(testm)[2])]

return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, blindtest=testacc, modtrainclass=nci_y, modtestclass=test_y, numfeat=length(bestgenelist), testacc=testacc))
#return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, method=bestmethod, blindtest=testacc))
print("Complete")

}

