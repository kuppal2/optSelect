
#library(CMA)
#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/golub_R/filt_proc_golub2_orig.csv", sep=",", header=T)

#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/nci_R/mean_replace_nci.csv", sep=",", header=T)
#ncid<-ncid[,-c(1)]
mdist<-function(x)
{
	
	t<-as.matrix(x)
	p<-dim(t)[2]
	#m<-apply(t,2,mean)
	m<-colMeans(t)
	s<-cov(t)
	mahalanobis(t,m,s, inverted=TRUE)
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



performCMA<-function(ncid, testm, outloc, maxnum, minnum, stepitr, gsmethods, percentTest,featweight, accuracyweight, kname, maxitrs, minpresent, norm_method, tolerance=1, classindex, upclasssep, dclasssep, removeindex, numfacts,numfolds=10,evalmethod="CV",CVfoldthresh=0.3,backward.sel=FALSE, scheme_val="one-vs-all",iter_learn=5)
{

data_dim<-dim(ncid)
write.csv(testm[,1], paste(outloc,"orig_40test.csv",sep=""), row.names=FALSE)
write.csv(ncid[,1], paste(outloc,"orig_60train.csv",sep=""), row.names=FALSE)

filestr3<-paste(outloc, "original_traindata.csv", sep="")
write.table(ncid, file=filestr3, sep=",", row.names=FALSE)

filestr3<-paste(outloc, "original_testdata.csv", sep="")
write.table(testm, file=filestr3, sep=",", row.names=FALSE)


print("classindex is ")
print(classindex)

#print(ncid[1:5,c(classindex-1,classindex)])
#meantrainimt=mean(ncid[,classindex])
#print("mean is ")
#print(meantrainimt)
#write.csv(meantrainimt, paste(outloc,"mean_classtrain.csv",sep=""), row.names=FALSE)

upsep<-upclasssep
dsep<-dclasssep

#ncid[which(ncid[,classindex]>=upsep),classindex]="A"
#ncid[which(ncid[,classindex]<=dsep),classindex]="B"
#ncidA<-ncid[which(ncid[,classindex]=="A"),]
#ncidB<-ncid[which(ncid[,classindex]=="B"),]

#ncid<-rbind(ncidA,ncidB)
print("dim of ncid is ")
print(dim(ncid))
print(levels(ncid[,classindex]))
#print(ncidB[1:4,1:10])
#testm[which(testm[,classindex]>=upsep),classindex]="A"
#testm[which(testm[,classindex]<=dsep),classindex]="B"
#testmA<-testm[which(testm[,classindex]=="A"),]
#testmB<-testm[which(testm[,classindex]=="B"),]
#testm<-rbind(testmA,testmB)

data_dim<-dim(ncid)
print(data_dim)


nci_x<-ncid[,-c(classindex)]
nci_y<-as.factor(ncid[,classindex])
rm(ncid)

nci_xm<-as.matrix(nci_x)
test_xm<-as.matrix(testm[,-c(classindex)])

test_y<-as.factor(testm[,classindex])
rm(testm)

#datafile=datafile[,-c(classindex)]


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
factnames=colnames(nci_xm)

print("length of factcols")
print(length(factcols))

print(dim(nci_xm))
print(dim(test_xm))
print(factcols)
print(factnames[factcols])
#trainfactnames=colnames(nci_xm)
trainfact=nci_xm[,c(factcols)]
testfact=test_xm[,c(factcols)]

trainfactnames=colnames(trainfact)
print(trainfactnames)

print("ok")
trainfact=makefactors(trainfact)
testfact=makefactors(testfact)
trainfact=as.data.frame(trainfact)
testfact=as.data.frame(testfact)

if(length(factcols)>0)
{
nci_xm=nci_xm[,-c(factcols)]
test_xm=test_xm[,-c(factcols)]

trainfact_check=apply(trainfact,2,as.numeric)
testfact_check=apply(testfact,2,as.numeric)

#trainfact_num=apply(trainfact_check[1,],2,is.na)
#testfact_num=which(testfact_check,2,is.NA)
trainfact_num={}
#print(trainfact_check[1:10,1:5])
#if(length(trainfact_check)>0)
{
for(i in 1:dim(trainfact_check)[2])
{
	trainfact_num=c(trainfact_num,is.na(trainfact_check[1,i]))
}

#print(trainfact_num[1:10])

trainfact_num_index=which(trainfact_num==TRUE)
print(trainfact_num_index)
}
#trainfactnames=colnames(trainfact[,trainfact_num_index])



#trainfact_num=which(is.na(trainfact_check))
if(length(trainfact_num_index)>0)
{
nci_xm=cbind(apply(nci_xm,2,as.numeric),trainfact[,-trainfact_num_index])
test_xm=cbind(apply(test_xm,2,as.numeric),testfact[,-trainfact_num_index])

trainfactnames=trainfactnames[trainfact_num_index]
trainfact=trainfact[,trainfact_num_index]
testfact=testfact[,trainfact_num_index]
print("trainfact samp ")
print(trainfact[1:10,1:3])
}
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


###outlier chek########
#d1=nci_xm[which(nci_y=="A"),]
#d2=nci_xm[which(nci_y=="B"),]
#print("dim of train class A")
#print(dim(d1))
#print("dim of train class B")
#print(dim(d2))

#print(d1[1:10,])
#print(d2[1:10,1:10])
#m1<-mdist(d1)
#m2<-mdist(d2)
#c<-qchisq(0.99,df=10)
#print(c)
#x1<-d1[m1<c,]
#x2<-d2[m2<c,]
#print("dim of train class A after mahalanobis test")
#print(dim(x1))
#print("dim of train class B after mahalanobis test")
#print(dim(x2))

#nci_xm<-rbind(x1,x2)

#numfolds<-10
bestmod<-0
#methods=c("t.test", "lasso", "f.test", "kruskal.test", "rfe", "rf") # "elasticnet", "boosting")
#, "lasso", "elasticnet", "boosting")
#methods=c("t.test", "rfe", "rf", "kruskal.test", "f.test")

train<-GenerateLearningsets(y=nci_y, method=evalmethod, fold=numfolds, strat=TRUE, niter=iter_learn)

scoringmatrix=matrix(0,data_dim[2]-1,length(gsmethods))

maxnum=round(maxnum*dim(nci_xm)[2])
print("maxnum is ")
print(maxnum)

if(FALSE)
{
varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=c("t.test"),scheme=scheme_val)
	genelist={}
	for(i in 1:numfolds)
	{
		genelist<-c(genelist,toplist(varsel,iter=i,maxnum, show=FALSE)$index)

	}
	print(head(genelist))

	print(length(genelist))
	genelist<-unlist(genelist)
	genelist<-genelist[order(genelist)]
	genelist<-unique(genelist)
origgenelist<-genelist	

nci_xm<-nci_xm[,genelist]
test_xm<-test_xm[,genelist]
}
print("# of genes left after filtering:")
print(dim(nci_xm))
mod_dim=dim(nci_xm)[2]
scoringmatrix=matrix(0,mod_dim,length(gsmethods))

class_labels<-levels(as.factor(nci_y))
num_classes<-length(class_labels)

#tolerance=0.5
for(m in 1:length(gsmethods))
{
	#varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=gsmethods[m],scheme="pairwise")
	varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=gsmethods[m],scheme=scheme_val)
	genelist={}
	
	
	for(i in 1:(numfolds*iter_learn))
	{
		#for(c in 1:num_classes){
		for(c in 1:1){
		t1<-toplist(varsel,iter=i,k=maxnum, show=FALSE)

		#genelist<-c(genelist,toplist(varsel,iter=i,dim(nci_xm)[2], show=FALSE)$index)

		genelist<-c(genelist,t1[[c]]$index)
		}

		#r1<-varsel@rankings[[1]][1,]
		#imp1<-varsel@importance[[1]][1,]
	}

	genelist_seltable<-sort(table(genelist), dec = TRUE)

	genelist_seltable<-genelist_seltable[which(genelist_seltable>=(CVfoldthresh*numfolds*iter_learn))]
	
	#if(length(genelist_good)>0)
	{
	good_genes<-names(genelist_seltable)
	
	good_genes<-as.numeric(good_genes)
	genelist<-unique(good_genes)

	#for(num_g in seq(minnum, maxnum, step))
	num_g=maxnum
	bestmod=0
	prevacc=0
	noimprovement=0
	bestgenelist={}
	#while((noimprovement<=totitrs) || num_g<maxnum)
	
	if(backward.sel==TRUE){
	
	while(num_g>minnum)
	{
		trainset<-nci_xm[,genelist[1:num_g]]
		model<-svm(trainset, nci_y, type="C", kernel=kname, cross=numfolds)

		#fitval<-fitted(model)
		#fitval<-predict(model,trainset)
         #       tab1=table(fitval, nci_y)
          #      beracc=(tab1[1,1]/sum(tab1[,1]))+(tab1[2,2]/sum(tab1[,2]))
           #     beracc=0.5*beracc*100
	
		tenfoldacc<-model$tot.accuracy
		foldacc<-tenfoldacc	
		#foldacc<-0.7*(model$tot.accuracy)+0.3*(beracc)

		fitfunc<-accuracyweight*(foldacc) #+featweight*(1-num_g/dim(nci_xm)[2])
		
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
                        if(abs(bestmod-fitfunc)<=tolerance)
                        {
                                noimprovement=0
				#noimprovement+1
                        	bestgenelist<-genelist[1:num_g]
			}
			else{
                        noimprovement=noimprovement+1
			}	

		}

		prevacc=fitfunc
		#prevacc=foldacc
		print(paste(gsmethods[m],":",num_g, ":", fitfunc,sep=""))
		if(noimprovement<=maxitrs)
		{
			num_g=num_g-stepitr
		}
		else
		{
			num_g=minnum
		}
		
	print(paste(gsmethods[m],":",length(bestgenelist), ":", bestmod,sep=""))	
	}
	}else{
		bestgenelist=genelist
		}
	
	scoringmatrix[bestgenelist,m]=1
	}
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

#modtrain=cbind(apply(modtrain,2,as.numeric),trainfact)
#modtest=cbind(apply(modtest,2,as.numeric),testfact)

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
pred10fold<-fitted(model_train)
tenfold.table<-table(pred10fold,nci_y)
print("confusion matrix train 10 fold")
print(tenfold.table)
                    
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

print("confusion matrix test")
print(test.table)

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

#modtrain<-cbind(modtrain, as.factor(nci_y))
#modtest<-cbind(modtest, as.factor(test_y))

#estgenelist,dim(ncid)[2])]
#modtest<-test_xm[,c(bestgenelist, dim(testm)[2])]

colnames(trainfact)=trainfactnames
colnames(testfact)=trainfactnames
modtrain=cbind(apply(modtrain,2,as.numeric),trainfact)
modtest=cbind(apply(modtest,2,as.numeric),testfact)

modtrain=as.data.frame(modtrain)
modtest=as.data.frame(modtest)
return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, blindtest=testacc, modtrainclass=nci_y, modtestclass=test_y, numfeat=dim(modtrain)[2], testacc=testacc))
#return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, method=bestmethod, blindtest=testacc))
print("Complete")

}

