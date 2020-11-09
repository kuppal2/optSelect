
#library(CMA)
#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/golub_R/filt_proc_golub2_orig.csv", sep=",", header=T)

#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/nci_R/mean_replace_nci.csv", sep=",", header=T)
#ncid<-ncid[,-c(1)]

znorm<-function(x){ (x-mean(x))/(sd(x)+0.001)}
minmax<-function(x){
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
        }


makefactors<-function(curdata)
{
alphavec=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

	########FACTOR TEST#########
	facttest<-apply(curdata,2, function(x){

		categ=TRUE
		if(is.na(as.numeric(x[1])))
		{
			categ=FALSE;
		}
		else
		{
			for(i in 1:length(x))
			{

				mod_val=x[i]%%2

				if(mod_val!=0 && mod_val!=1)
				{
					categ=FALSE;
				}


			}	
		}
		if(categ==TRUE)
		{
		factvec<-as.factor(x)
		getlevels=levels(factvec)
		factvec=as.vector(factvec)

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

		x=as.data.frame(factvec)
		}
		
		return(x)
	
})
facttest=as.data.frame(facttest)
return(facttest)
}



preProcess<-function(datafile, outloc,percentTest, norm_method,classindex, upclasssep, dclasssep, removeindex, numfacts)
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

filestr3<-paste(outloc, "original_traindata.csv", sep="")
write.table(ncid, file=filestr3, sep=",", row.names=FALSE)

filestr3<-paste(outloc, "original_testdata.csv", sep="")
write.table(testm, file=filestr3, sep=",", row.names=FALSE)

testm<-testm[,-removeindex]
ncid<-ncid[,-removeindex]
classindex=classindex-1

print("classindex is ")
print(classindex)

meantrainimt=mean(ncid[,classindex])
print("mean is ")
print(meantrainimt)
write.csv(meantrainimt, paste(outloc,"mean_classtrain.csv",sep=""), row.names=FALSE)

print(ncid[1:2,c(classindex-1,classindex)])

#upsep<-meantrainimt+upclasssep
#dsep<-meantrainimt-dclasssep

#ncid[which(ncid[,classindex]>=upsep),classindex]="A"
#ncid[which(ncid[,classindex]<=dsep),classindex]="B"
#ncidA<-ncid[which(ncid[,classindex]>=upsep),]
#ncidB<-ncid[which(ncid[,classindex]<=dsep),]

#ncid<-rbind(ncidA,ncidB)
print("dim of ncid is ")
print(dim(ncid))
#print(levels(ncid[,classindex]))
#print(ncidB[1:4,1:10])
#testm[which(testm[,classindex]>=upsep),classindex]="A"
#testm[which(testm[,classindex]<=dsep),classindex]="B"
#testmA<-testm[which(testm[,classindex]>=upsep),]
#testmB<-testm[which(testm[,classindex]<=dsep),]
#testm<-rbind(testmA,testmB)

data_dim<-dim(ncid)
print(data_dim)


nci_x<-ncid[,-c(classindex)]
nci_y<-ncid[,classindex]
rm(ncid)

nci_xm<-as.matrix(nci_x)
test_xm<-as.matrix(testm[,-c(classindex)])

test_y<-testm[,classindex]
rm(testm)

datafile=datafile[,-c(classindex)]


factcols<-lapply(1:dim(nci_xm)[2],function(i){
 factvec<-as.factor(nci_xm[,i])
 getlevels=levels(factvec)
 }
 )

factcols<-sapply(factcols,head,n=100000000000000000)

factnums<-lapply(1:dim(nci_xm)[2],function(i){
 factvec<-length(factcols[[i]])

 }
 )

factcols=which(factnums<=numfacts)

print("length of factcols")
print(length(factcols))

print(dim(nci_xm))
print(dim(test_xm))
print(factcols)

trainfact=nci_xm[,c(factcols)]
testfact=test_xm[,c(factcols)]

print("ok")
trainfact=makefactors(trainfact)
testfact=makefactors(testfact)
trainfact=as.data.frame(trainfact)
testfact=as.data.frame(testfact)


nci_xm=nci_xm[,-c(factcols)]
test_xm=test_xm[,-c(factcols)]

trainfact_check=apply(trainfact,2,as.numeric)
testfact_check=apply(testfact,2,as.numeric)

#trainfact_num=apply(trainfact_check[1,],2,is.na)
#testfact_num=which(testfact_check,2,is.NA)
trainfact_num={}
for(i in 1:dim(trainfact_check)[2])
{
	trainfact_num=c(trainfact_num,is.na(trainfact_check[1,i]))
}

#print(trainfact_num[1:10])

trainfact_num_index=which(trainfact_num==TRUE)
print(trainfact_num_index)

#trainfact_num=which(is.na(trainfact_check))
if(length(trainfact_num_index)>0)
{
nci_xm=cbind(apply(nci_xm,2,as.numeric),trainfact[,-trainfact_num_index])
test_xm=cbind(apply(test_xm,2,as.numeric),testfact[,-trainfact_num_index])

trainfact=trainfact[,trainfact_num_index]
testfact=testfact[,trainfact_num_index]
print("trainfact samp ")
print(trainfact[1:10,1:3])
}

nci_xm=apply(nci_xm,2,as.numeric)
test_xm=apply(test_xm,2,as.numeric)
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
 #      test_y=minmax(test_y)
#	nci_y=minmax(nci_y) 
#((x-min(x))/(max(x)-min(x)))})
}
else
{

	if(norm_method=="znorm")
	{
		print("znorm")
        	nci_xm=apply(nci_xm,2,function(x){ (x-mean(x))/(sd(x)+0.001)})
        	test_xm=apply(test_xm,2,function(x){(x-mean(x))/(sd(x)+0.001)})
		#nci_y=apply(nci_y,2,function(x){ (x-mean(x))/(sd(x)+0.001)})
		#test_y=apply(test_y,2,function(x){(x-mean(x))/(sd(x)+0.001)})
		nci_y=znorm(nci_y)
		test_y=znorm(test_y)
	}
}

print("norm train matrix")
print(nci_xm[1:5,1:10])
print("mean of feat 2")
print(mean(nci_xm[,2]))
print("sd of feat 2")
print(sd(nci_xm[,2]))
#kname="radial"
numfolds<-10
bestmod<-0
#methods=c("t.test", "lasso", "f.test", "kruskal.test", "rfe", "rf") # "elasticnet", "boosting")
#, "lasso", "elasticnet", "boosting")
#methods=c("t.test", "rfe", "rf", "kruskal.test", "f.test")

	modtrain<-as.matrix(nci_xm)
	modtest<-as.matrix(test_xm)

traindim=dim(modtrain)
testdim=dim(modtest)
print(modtrain[1:3,])
print(modtest[1:3,])

#modtrain=cbind(apply(modtrain,2,as.numeric),trainfact)
#modtest=cbind(apply(modtest,2,as.numeric),testfact)

#print(paste("best acc:", bestmod, sep=""))
#print(paste("best method:", bestmethod, sep=""))

#modtrain<-ncid[,c(bestgenelist,data_dim[2])]
#modtest<-testm[,c(bestgenelist, data_dim[2])]

#model_train_valid<-svm(nci_xm[,bestgenelist], nci_y,   kernel=kname, type="C")
#pred_train<-predict(model_train_valid, testm[,bestgenelist])

modtrain=cbind(apply(modtrain,2,as.numeric),trainfact)
modtest=cbind(apply(modtest,2,as.numeric),testfact)

modtrain=as.data.frame(modtrain)
modtest=as.data.frame(modtest)

if(FALSE)
{
model_train_valid<-svm(modtrain,  nci_y,   kernel=kname, type="eps")
pred_train<-predict(model_train_valid, modtest)
testacc<-mean((pred_train-nci_y)^2)

model_train<-svm(modtrain,  nci_y,   kernel=kname, type="eps", cross=10)

print(paste("test acc:", testacc, sep=""))

print(paste("10 fold train", model_train$tot.MSE, sep=""))
                    

 filestr3<-paste(outloc, "modified_test.csv", sep="")
        write.table(modtest, file=filestr3, sep=",", row.names=FALSE)
 filestr3<-paste(outloc, "modified_train.csv", sep="")
        write.table(modtrain, file=filestr3, sep=",", row.names=FALSE)
 filestr3<-paste(outloc, "modified_test_class.csv", sep="")
        write.table(test_y, file=filestr3, sep=",", row.names=FALSE)
 
 filestr3<-paste(outloc, "modified_train_class.csv", sep="")
        write.table(nci_y, file=filestr3, sep=",", row.names=FALSE)


 filestr3<-paste(outloc, "original_testacc.csv", sep="")
        write.table(testacc, file=filestr3, sep=",", row.names=FALSE)


pred_train<-predict(model_train_valid, modtrain)
train.table<-table(pred_train, nci_y)
trainacc<-sum(diag(train.table))/(dim(modtrain)[1])
print(paste("train acc:", trainacc, sep=""))
print("confusion matrix train")
print(train.table)
 filestr3<-paste(outloc, "modified_cma_trainacc.csv", sep="")
        write.table(trainacc, file=filestr3, sep=",", row.names=FALSE)

filestr3<-paste(outloc, "modified_cma_10foldacc.csv", sep="")
        write.table(model_train$tot.accuracy, file=filestr3, sep=",", row.names=FALSE)
}
#modtrain<-cbind(modtrain, as.factor(nci_y))
#modtest<-cbind(modtest, as.factor(test_y))

#estgenelist,dim(ncid)[2])]
#modtest<-test_xm[,c(bestgenelist, dim(testm)[2])]

return(list(modtraindata=modtrain, modtestdata=modtest, modtrainclass=nci_y, modtestclass=test_y, numfeat=dim(modtrain)[2]))
#return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, method=bestmethod, blindtest=testacc))
print("Complete")

}

