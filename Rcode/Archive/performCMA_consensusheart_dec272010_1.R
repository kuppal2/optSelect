
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
if(is.na(meanval))
{
stop("too many iterations")
}


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

meantrainimt=mean(ncid$Carotid_IMT)
#meantrainimt=0.68

ncid$Carotid_IMT[which(ncid$Carotid_IMT>meantrainimt)]="A"
ncid$Carotid_IMT[which(ncid$Carotid_IMT<=meantrainimt)]="B"

testm$Carotid_IMT[which(testm$Carotid_IMT>meantrainimt)]="A"
testm$Carotid_IMT[which(testm$Carotid_IMT<=meantrainimt)]="B"

nci_x<-ncid[,-c(1)]
nci_y<-ncid$Carotid_IMT
nci_xm<-as.matrix(nci_x)
test_xm<-as.matrix(testm[,-c(1)])
test_y<-testm$Carotid_IMT

#nci_y<-as.matrix(nci_y)
#test_y<-as.matrix(test_y)

#nci_xm<-meanreplace(nci_xm)
#test_xm<-meanreplace(test_xm)

#nci_xm<-apply(nci_xm,2,as.numeric)
#test_xm<-apply(test_xm,2,as.numeric)

#nci_xm<-makefactors(nci_xm)
#test_xm<-makefactors(test_xm)


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

factcols=which(factnums<=3)

print("length of factcols")
print(length(factcols))

nci_factm<-nci_xm[,c(factcols)]
test_factm<-test_xm[,c(factcols)]

nci_xm<-nci_xm[,-c(factcols)]
test_xm<-test_xm[,-c(factcols)]

nci_xm<-as.data.frame(nci_xm)
test_xm<-as.data.frame(test_xm)

nci_xm<-apply(nci_xm,2,as.numeric)
test_xm<-apply(test_xm,2,as.numeric)

md=which(nci_xm=="")
print(length(md))

#kname="radial"
numfolds<-10
bestmod<-0

print("okay now")
nci_y<-as.factor(nci_y)
test_y<-as.factor(test_y)

print(test_y)
train<-GenerateLearningsets(y=nci_y, method="CV", fold=10, strat=TRUE)

maxnum=dim(nci_xm)[2]-1

print(dim(nci_xm))


varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=c("t.test"))
	genelist={}
	for(i in 1:numfolds)
	{
		genelist<-c(genelist,toplist(varsel,iter=i,maxnum, show=FALSE)$index)

	}
	genelist<-unique(genelist)
origgenelist<-genelist	
scoringmatrix=matrix(0,length(origgenelist),length(gsmethods))
nci_xm<-nci_xm[,genelist]
test_xm<-test_xm[,genelist]

mod_dim=dim(nci_xm)[2]

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
	while((noimprovement<=maxitrs) && num_g<maxnum)
	#while(num_g<maxnum)
	{
		
		trainset<-nci_xm[,genelist[1:num_g]]

		model<-svm(trainset, nci_y, type="C", kernel=kname, cross=numfolds)

		#fitval<-fitted(model)
		
		#tab1=table(fitval, nci_y)
		#foldacc=(tab1[1,1]/sum(tab1[,1]))+(tab1[2,2]/sum(tab1[,2]))
		#foldacc=0.5*foldacc
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
			if(abs(prevacc-fitfunc)<=tolerance)
			{
				noimprovement=noimprovement+1
			}
			#else
				
			
			
		
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
modtraindim=dim(modtrain)[2]

selectnames<-colnames(modtrain)

write.csv(selectnames, "cmaselectedfeats.csv", row.names=FALSE)

modtest<-apply(modtest,2,as.numeric)
print("mod train dim")
print(dim(modtrain))

print("mod test dim")
print(dim(modtest))

print(is.matrix(modtrain))

#print(modtrain[1:10,1:4])

#print(modtest[1:10,1:4])

model_train_valid<-svm(modtrain,  nci_y,   kernel=kname, type="C")

pred_train<-predict(model_train_valid, modtest)

print("done")
print(length(pred_train))
print(length(test_y))

test.table<-table(pred_train, test_y)

print("classified")

model_train<-svm(modtrain,  nci_y,   kernel=kname, type="C", cross=10)
testacc<-sum(diag(test.table))/(dim(modtest)[1])
print(paste("test acc:", testacc, sep=""))
modtrain=cbind(modtrain, nci_factm)
modtest=cbind(modtest,test_factm)
print(paste("10 fold train", model_train$tot.accuracy, sep=""))


num_feats<-dim(modtrain)[2]

return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, blindtest=testacc, modtrainclass=nci_y, modtestclass=test_y, numfeat=num_feats))
#return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, method=bestmethod, blindtest=testacc))
print("Complete")

}

