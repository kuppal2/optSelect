
#library(CMA)
#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/golub_R/filt_proc_golub2_orig.csv", sep=",", header=T)

#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/nci_R/mean_replace_nci.csv", sep=",", header=T)
#ncid<-ncid[,-c(1)]

meanreplace<-function(curdata)
{
	
lucamod<-apply(curdata,2, function(x){
checkmd=which(x=="MD")
lucadtemp=x
if(length(checkmd)>=1)
{
lucadtemp<-gsub("MD",-9999,x)
lucadtemp<-as.numeric(lucadtemp)
#print(lucadtemp[1:10])
badind=which(lucadtemp==-9999)
lucadtemp1<-lucadtemp[-badind]
lucadtemp2<-as.numeric(lucadtemp1)
meanval=mean(lucadtemp2)
#print(meanval)
lucadtemp[badind]=meanval
}
return(lucadtemp)})

return(lucamod)
}


makefactors<-function(curdata)
{
alphavec=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

########FACTOR TEST#########
facttest<-apply(curdata,2, function(x){
factvec<-as.factor(x)
getlevels=levels(factvec)
factvec=as.vector(factvec)
if(length(getlevels)<10)
{
   for(g in 1:length(getlevels))
   {
       #print(length(which(factvec==getlevels[g])))
       #flevelind=which(factvec==getlevels[g])
       
       #print(factvec[flevelind[1:3]])
       #factvec[flevelind]=alphavec[g]
       factvec[which(factvec==getlevels[g])]=alphavec[g]
       
   }
#x=as.factor(x)
#x=as.character(x)
x=factvec
}
return(x)})

return(facttest)
}
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

nci_xm<-meanreplace(nci_xm)
test_xm<-meanreplace(test_xm)

nci_xm<-makefactors(nci_xm)
test_xm<-makefactors(test_xm)

nci_xm<-apply(nci_xm,2,as.numeric)
test_xm<-apply(test_xm,2,as.numeric)

md=which(nci_xm=="")
print(length(md))
print("test class")
print(test_y[1:4])

print(nci_xm[1:10,748:753])
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
test_xm<-testm[,genelist]

mod_dim=dim(nci_xm)[2]
scoringmatrix=matrix(0,mod_dim,length(gsmethods))



tolerance=0.5
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

		model<-svm(trainset, nci_y, type="C", kernel=kname, cross=numfolds)

		foldacc<-model$tot.accuracy

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
			#if(abs(prevacc-fitfunc)>tolerance)
			if((bestmod-fitfunc)>tolerance)
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

print(paste("numgenes selected:",length(bestgenelist),sep=""))
#print(paste("best acc:", bestmod, sep=""))
#print(paste("best method:", bestmethod, sep=""))

#modtrain<-ncid[,c(bestgenelist,data_dim[2])]
#modtest<-testm[,c(bestgenelist, data_dim[2])]

#model_train_valid<-svm(nci_xm[,bestgenelist], nci_y,   kernel=kname, type="C")
#pred_train<-predict(model_train_valid, testm[,bestgenelist])
modtrain<-as.data.frame(modtrain)
modtest<-as.data.frame(modtest)

modtrain<-apply(modtrain,2,as.numeric)
modtest<-apply(modtest,2,as.numeric)
print(dim(modtrain))
print(is.matrix(modtrain))

print(nci_y[1:10])

print(length(nci_y))
nci_y=as.matrix(nci_y)
#test_y<-as.matrix(test_y)

print("modtest dim")
print(dim(modtest))
#print(dim(test_y))
print(test_y[1:10])
print(length(test_y))
test_y<-as.matrix(test_y)
model_train_valid<-svm(modtrain,  nci_y[,1],   kernel=kname, type="C")
pred_train<-predict(model_train_valid, modtest)


test.table<-table(pred_train, test_y)

model_train<-svm(modtrain,  nci_y[,1],   kernel=kname, type="C", cross=10)
testacc<-sum(diag(test.table))/(dim(modtest)[1])
print(paste("test acc:", testacc, sep=""))

print(paste("10 fold train", model_train$tot.accuracy, sep=""))

#modtrain<-cbind(modtrain, as.factor(nci_y))
#modtest<-cbind(modtest, as.factor(test_y))

#modtrain<-nci_xm[,c(bestgenelist,dim(ncid)[2])]
#modtest<-test_xm[,c(bestgenelist, dim(testm)[2])]

return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, blindtest=testacc, modtrainclass=nci_y, modtestclass=test_y, numfeat=length(bestgenelist)))
#return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, method=bestmethod, blindtest=testacc))
print("Complete")

}

