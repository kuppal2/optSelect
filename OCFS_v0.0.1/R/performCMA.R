performCMA <-
function(trainm, trainclass, testm, testclass, outloc, maxnum, minnum, stepitr, gsmethods, pct_train=1,featweight, accuracyweight, kname, maxitrs, minpresent, norm_method, tolerance=1, classindex, numfacts,numfolds=10,evalmethod="CV",CVfoldthresh=0.3,varselmethod="backward", scheme_val="one-vs-all",iter_learn=5,boostweight=NA)
{

if(is.na(boostweight)==TRUE){

boostweight=rep(0,dim(trainm)[2])
}

if(is.na(testm)==TRUE){
	testm<-trainm
}
if(is.na(testclass)==TRUE){
	testclass<-trainclass
}

data_dim<-dim(trainm)
write.csv(testclass, paste(outloc,"orig_40test.csv",sep=""), row.names=FALSE)
write.csv(trainclass, paste(outloc,"orig_60train.csv",sep=""), row.names=FALSE)

filestr3<-paste(outloc, "original_traindata.csv", sep="")
write.table(trainm, file=filestr3, sep=",", row.names=FALSE)

filestr3<-paste(outloc, "original_testdata.csv", sep="")
write.table(testm, file=filestr3, sep=",", row.names=FALSE)



##print("dim of trainm is ")
##print(dim(trainm))

data_dim<-dim(trainm)
##print(data_dim)


nci_x<-trainm #[,-c(classindex)]
nci_y<-as.factor(trainclass)
rm(trainm)

nci_xm<-as.matrix(nci_x)
test_xm<-as.matrix(testm)

test_y<-as.factor(testclass)
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

trainfact=nci_xm[,c(factcols)]
testfact=test_xm[,c(factcols)]

trainfactnames=colnames(trainfact)

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
##print(trainfact_check[1:10,1:5])
#if(length(trainfact_check)>0)
{
for(i in 1:dim(trainfact_check)[2])
{
	trainfact_num=c(trainfact_num,is.na(trainfact_check[1,i]))
}

##print(trainfact_num[1:10])

trainfact_num_index=which(trainfact_num==TRUE)
##print(trainfact_num_index)
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
##print("trainfact samp ")
##print(trainfact[1:10,1:3])
}
}
nci_xm=apply(nci_xm,2,as.numeric)
test_xm=apply(test_xm,2,as.numeric)
##print("test class")
##print(test_y[1:4])

#print("orig train matrix")
#print(nci_xm[1:5,1:10])
#nci_xm<-as.matrix(nci_xm)
#test_xm<-as.matrix(test_xm)
#print("orig train matrix")
#print(nci_xm[1:5,1:10])
#print(as.numeric(nci_xm[1:3,3]))





if(norm_method=="minmax")
{
	#print("minmax normalization")
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
		#print("znorm")
        	nci_xm=apply(nci_xm,2,function(x){ (x-mean(x))/(sd(x)+0.001)})
        	test_xm=apply(test_xm,2,function(x){(x-mean(x))/(sd(x)+0.001)})
	}
}

#print("norm train matrix")
#print(nci_xm[1:5,1:10])
#print("mean of feat 2")
#print(mean(nci_xm[,2]))
#print("sd of feat 2")
#print(sd(nci_xm[,2]))
#kname="radial"


###outlier chek########
#d1=nci_xm[which(nci_y=="A"),]
#d2=nci_xm[which(nci_y=="B"),]
##print("dim of train class A")
##print(dim(d1))
##print("dim of train class B")
##print(dim(d2))

##print(d1[1:10,])
##print(d2[1:10,1:10])
#m1<-mdist(d1)
#m2<-mdist(d2)
#c<-qchisq(0.99,df=10)
##print(c)
#x1<-d1[m1<c,]
#x2<-d2[m2<c,]
##print("dim of train class A after mahalanobis test")
##print(dim(x1))
##print("dim of train class B after mahalanobis test")
##print(dim(x2))

#nci_xm<-rbind(x1,x2)

#numfolds<-10
bestmod<-0
#methods=c("t.test", "lasso", "f.test", "kruskal.test", "rfe", "rf") # "elasticnet", "boosting")
#, "lasso", "elasticnet", "boosting")
#methods=c("t.test", "rfe", "rf", "kruskal.test", "f.test")

#train<-GenerateLearningsets(y=nci_y, method=evalmethod, fold=numfolds, strat=FALSE, niter=iter_learn)
if(evalmethod=="MCCV"){

set.seed(321)
train<-GenerateLearningsets(y=nci_y, method=evalmethod, fold=numfolds, strat=FALSE, niter=iter_learn,ntrain=(pct_train*nrow(trainm)))

}else{

set.seed(321)
train<-GenerateLearningsets(y=nci_y, method=evalmethod, fold=numfolds, strat=FALSE, niter=iter_learn)
}
#set.seed(321)
#trainlearningsets<-GenerateLearningsets(y=trainclass, method="MCCV", fold=numfolds, strat=FALSE, niter=iter_learn,ntrain=(0.8*nrow(trainm)))

scoringmatrix=matrix(0,data_dim[2]-1,length(gsmethods))

#maxnum=round(maxnum*dim(nci_xm)[2])

if(maxnum>data_dim[2]){
        maxnum=data_dim[2]
}else{

	if(maxnum<3){

		maxnum=3
	}
}
#print("maxnum is ")
#print(maxnum)

if(FALSE)
{
varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=c("t.test"),scheme=scheme_val)
	genelist={}
	for(i in 1:numfolds)
	{
		genelist<-c(genelist,toplist(varsel,iter=i,maxnum, show=FALSE)$index)

	}
	#print(head(genelist))

	#print(length(genelist))
	genelist<-unlist(genelist)
	genelist<-genelist[order(genelist)]
	genelist<-unique(genelist)
origgenelist<-genelist	

nci_xm<-nci_xm[,genelist]
test_xm<-test_xm[,genelist]
}
#print("# of genes left after filtering:")
#print(dim(nci_xm))
mod_dim=dim(nci_xm)[2]
scoringmatrix=matrix(0,mod_dim,length(gsmethods))

class_labels<-levels(as.factor(nci_y))
num_classes<-length(class_labels)

if(evalmethod=="MCCV" && pct_train==1){

	max_i<-1
}else{
	max_i<-numfolds*iter_learn

}
#tolerance=0.5
for(m in 1:length(gsmethods))
{
	#varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=gsmethods[m],scheme="pairwise")
	varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=gsmethods[m],scheme=scheme_val)
	genelist={}
	
	
	for(i in 1:(max_i))
	{
		#for(c in 1:num_classes){
		for(c in 1:num_classes){
		t1<-toplist(varsel,iter=i,k=maxnum, show=FALSE)

		if(num_classes>2){
		#genelist<-c(genelist,toplist(varsel,iter=i,dim(nci_xm)[2], show=FALSE)$index)

		genelist<-c(genelist,t1[[c]]$index)
		}else{
			genelist<-c(genelist,t1$index)
		}
		}

		#r1<-varsel@rankings[[1]][1,]
		#imp1<-varsel@importance[[1]][1,]
	}

	genelist_seltable<-sort(table(genelist), dec = TRUE)


	#select genes that were selected in at least CVfoldthresh iterations
	
	#genelist_seltable<-genelist_seltable[which(genelist_seltable>=(CVfoldthresh*numfolds*iter_learn))]
	
	#if(length(genelist_good)>0)
	{
	good_genes<-names(genelist_seltable)
	
	good_genes<-as.numeric(good_genes)
	genelist<-unique(good_genes)

	#for(num_g in seq(minnum, maxnum, step))
	num_g=min(length(genelist),maxnum)
	bestmod=0
	prevacc=0
	noimprovement=0
	bestgenelist={}
	errortype="BER"
	#while((noimprovement<=totitrs) || num_g<maxnum)
	#print("varselmethod")
	#print(varselmethod)	
	if(varselmethod=="backward"){

	#maxitrs=100
	
	while(num_g>minnum)
	{
		trainset<-nci_xm[,genelist[1:num_g]]
		##print(nci_y)
		##print(genelist[1:num_g])
		#model<-svm(trainset, nci_y, type="C", kernel=kname, cross=numfolds)
		#tenfoldacc<-model$tot.accuracy
		

		model<-svm_cv(v=numfolds,x=trainset,y=nci_y,kname=kname,errortype="BER")
		tenfoldacc<-model$mean_acc

		perm_acc<-{}
		seed_vec<-c(129532,839147,407700)

		for(r1 in 1:3)
		{
			seednum=seed_vec[r1]
			nci_y_perm<-nci_y[sample(1:length(nci_y),size=length(nci_y))]
			#model<-svm_cv(trainset, nci_y_perm, type="C", kernel=kname, cross=numfolds)
                	model<-svm_cv(v=numfolds,x=trainset,y=nci_y_perm,kname=kname,errortype=errortype,seednum)
                        #folderror_cur<-model$confint[1] #mean_acc
                        #folderror<-c(folderror,folderror_cur)
			permtenfoldacc<-model$mean_acc #tot.accuracy
			perm_acc<-c(perm_acc,permtenfoldacc)
		}
		
		
		perm_acc<-mean(perm_acc,na.rm=TRUE)+2*sd(perm_acc,na.rm=TRUE)
		foldacc<-tenfoldacc	
		#fitfunc<-accuracyweight*(foldacc) #+featweight*(1-num_g/dim(nci_xm)[2])
		
		fitfunc<-accuracyweight*(foldacc-perm_acc)+1*(foldacc)-(100*featweight*(1-num_g/dim(nci_xm)[2]))

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
		#print(paste(gsmethods[m],":",num_g, ":", fitfunc,sep=""))
		if(noimprovement<=maxitrs)
		{
			num_g=num_g-stepitr
		}
		else
		{
			#num_g=minnum
			break
		}
		
	#print(paste(gsmethods[m],":",length(bestgenelist), ":", bestmod," : ",num_g,sep=""))	
	}
	}else{
		if(varselmethod=="forward"){
			num_g=minnum
			maxitrs=5	
			#maxnum=min(maxnum,dim(nci_xm)[2])

			maxnum=min(length(genelist),maxnum)	
	while(num_g<=maxnum)
        {
                trainset<-nci_xm[,genelist[1:num_g]]
                model<-svm(trainset, nci_y, type="C", kernel=kname, cross=numfolds)
                tenfoldacc<-model$tot.accuracy
                foldacc<-tenfoldacc
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
                #print(paste(gsmethods[m],":",num_g, ":", fitfunc,sep=""))
                if(noimprovement<=maxitrs)
                {
                        num_g=num_g+stepitr
                }
                else
                {
			break
                }

        #print(paste(gsmethods[m],":",length(bestgenelist), ":", bestmod,sep=""))
        }

			
		}else{
	
		maxnum=min(length(genelist),maxnum)
			if(varselmethod=="none"){
			bestgenelist=genelist[1:maxnum]
			}else{

				stop("varselmethod should be none,forward, or backward")
			}
		}
		
	}
	
	scoringmatrix[bestgenelist,m]=1
	}
}
summat=apply(scoringmatrix,1,sum)
#print("dim of scoring matrix is ")
#print(dim(scoringmatrix))

bestgenelist=which(summat>=minpresent)

#print(length(summat))
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

dicesorenson_res<-get_DSindex(scoringmatrix)

#print("DS index stage 1")
#print(dicesorenson_res)

#print("bestgenelist")

#print(bestgenelist)
if(length(bestgenelist)>0)
{
	modtrain<-as.matrix(nci_xm[,c(bestgenelist)])
	modtest<-as.matrix(test_xm[,c(bestgenelist)])

	boostweight=boostweight[c(bestgenelist)]
}
else
{
	modtrain<-as.matrix(nci_xm)
	modtest<-as.matrix(test_xm)
}
traindim=dim(modtrain)
testdim=dim(modtest)
#print(modtrain[1:3,])
#print(modtest[1:3,])


#modtrain=cbind(apply(modtrain,2,as.numeric),trainfact)
#modtest=cbind(apply(modtest,2,as.numeric),testfact)

#print(paste("numgenes selected:",length(bestgenelist),sep=""))

##print(paste("best acc:", bestmod, sep=""))
##print(paste("best method:", bestmethod, sep=""))
#modtrain<-trainm[,c(bestgenelist,data_dim[2])]
#modtest<-testm[,c(bestgenelist, data_dim[2])]
#model_train_valid<-svm(nci_xm[,bestgenelist], nci_y,   kernel=kname, type="C")
#pred_train<-predict(model_train_valid, testm[,bestgenelist])

model_train_valid<-svm(modtrain,  nci_y,   kernel=kname, type="C")
pred_test<-predict(model_train_valid, modtest)
test.table<-table(pred_test, test_y)

testacc<-sum(diag(test.table))/(dim(modtest)[1])
#print(paste("test acc:", testacc, sep=""))

pred_acc<-multiclass.roc(test_y,as.numeric(pred_test))
        pred_acc_orig<-pred_acc$auc[1]
        auc_acc<-pred_acc_orig
#print(paste("test AUC acc:", auc_acc, sep=""))

model_train<-svm(modtrain,  nci_y,   kernel=kname, type="C", cross=10)
#print(paste("10 fold train", model_train$tot.accuracy, sep=""))
pred10fold<-fitted(model_train)
tenfold.table<-table(pred10fold,nci_y)
#print("confusion matrix train 10 fold")
#print(tenfold.table)



                    
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

#print("confusion matrix test")
#print(test.table)

pred_train<-predict(model_train_valid, modtrain)
train.table<-table(pred_train, nci_y)
trainacc<-sum(diag(train.table))/(dim(modtrain)[1])
#print(paste("train acc:", trainacc, sep=""))
#print("confusion matrix train")
#print(train.table)
 filestr3<-paste(outloc, "modified_cma_trainacc.csv", sep="")
        write.table(trainacc, file=filestr3, sep=",", row.names=FALSE)

filestr3<-paste(outloc, "modified_cma_10foldacc.csv", sep="")
        write.table(model_train$tot.accuracy, file=filestr3, sep=",", row.names=FALSE)
dicesorenson_res<-get_DSindex(scoringmatrix)
#print("DS index stage 1")
#print(dicesorenson_res)
dicesorenson_res<-get_KIindex(scoringmatrix,maxnum)
#print("KI index stage 1")
#print(dicesorenson_res)
if(dim(scoringmatrix)[2]>1){
m1<-scoringmatrix
m2<-new("list")
for(i in 1:dim(scoringmatrix)[2]){m2[[i]]<-paste("var",which(m1[,i]==1),sep="")}
#print(m2)
m3<-as.data.frame(m2)
m4<-t(m3)
r1<-RankAggreg(m4,k=maxnum)
bestgenelistRA<-gsub(x=r1$top.list,pattern="var",replacement="")
bestgenelistRA<-as.numeric(as.character(bestgenelistRA))
modtrainRA<-as.matrix(nci_xm[,c(bestgenelistRA)])
modtestRA<-as.matrix(test_xm[,c(bestgenelistRA)])
model_train_valid<-svm(modtrainRA,  nci_y,   kernel=kname, type="C")
pred_test<-predict(model_train_valid, modtestRA)
test.table<-table(pred_test, test_y)
testacc<-sum(diag(test.table))/(dim(modtestRA)[1])
#print(paste("test acc rank aggreg CE:", testacc, sep=""))
pred_acc<-multiclass.roc(test_y,as.numeric(pred_test))
        pred_acc_orig<-pred_acc$auc[1]
        auc_acc<-pred_acc_orig
#print(paste("test AUC acc rank aggreg CE:", auc_acc, sep=""))
model_trainRA<-svm(modtrainRA,  nci_y,   kernel=kname, type="C", cross=10)
#print(paste("10 fold train rank aggreg res CE", model_trainRA$tot.accuracy, sep=""))
pred10foldRA<-fitted(model_trainRA)
tenfold.tableRA<-table(pred10foldRA,nci_y)
#print("confusion matrix train 10 fold rank aggreg CE")
#print(tenfold.tableRA)
#print("Num itr RA CE")
#print(r1$num.iter)
svm_table<-test.table
class_names<-rownames(svm_table)
beracc<-{}
i<-1
svm_acc<-{}
subtestclass<-test_y
predfit<-pred_test
totacc<-length(which(pred_test==subtestclass))/length(subtestclass)
for(c in 1:dim(svm_table)[1]){
subtestclass_ind<-which(subtestclass==class_names[c])
beracc<-c(beracc,length(which(predfit[subtestclass_ind]==subtestclass[subtestclass_ind]))/length(subtestclass_ind))
}

beracc<-as.numeric(beracc)

beracc<-mean(beracc,na.rm=TRUE)

#print("Test BER aggreg CE is")
#print(beracc)


r1<-RankAggreg(m4,k=maxnum,method="GA")
bestgenelistRA<-gsub(x=r1$top.list,pattern="var",replacement="")
bestgenelistRA<-as.numeric(as.character(bestgenelistRA))
modtrainRA<-as.matrix(nci_xm[,c(bestgenelistRA)])
modtestRA<-as.matrix(test_xm[,c(bestgenelistRA)])
model_train_valid<-svm(modtrainRA,  nci_y,   kernel=kname, type="C")
pred_test<-predict(model_train_valid, modtestRA)
test.table<-table(pred_test, test_y)
testacc<-sum(diag(test.table))/(dim(modtestRA)[1])
#print(paste("test acc rank aggreg GA:", testacc, sep=""))
pred_acc<-multiclass.roc(test_y,as.numeric(pred_test))
        pred_acc_orig<-pred_acc$auc[1]
        auc_acc<-pred_acc_orig
#print(paste("test AUC acc rank aggreg GA:", auc_acc, sep=""))
model_trainRA<-svm(modtrainRA,  nci_y,   kernel=kname, type="C", cross=10)
#print(paste("10 fold train rank aggreg res GA", model_trainRA$tot.accuracy, sep=""))
pred10foldRA<-fitted(model_trainRA)
tenfold.tableRA<-table(pred10foldRA,nci_y)
#print("confusion matrix train 10 fold rank aggreg GA")
#print(tenfold.tableRA)

#print("Num itr RA GA")
#print(r1$num.iter)

svm_table<-test.table
class_names<-rownames(svm_table)
beracc<-{}
i<-1
svm_acc<-{}
subtestclass<-test_y
predfit<-pred_test
totacc<-length(which(pred_test==subtestclass))/length(subtestclass)
for(c in 1:dim(svm_table)[1]){
subtestclass_ind<-which(subtestclass==class_names[c])
beracc<-c(beracc,length(which(predfit[subtestclass_ind]==subtestclass[subtestclass_ind]))/length(subtestclass_ind))
}

beracc<-as.numeric(beracc)

beracc<-mean(beracc,na.rm=TRUE)

#print("Test BER aggreg GA is")
#print(beracc)




}

#modtrain<-cbind(modtrain, as.factor(nci_y))
#modtest<-cbind(modtest, as.factor(test_y))

#estgenelist,dim(trainm)[2])]
#modtest<-test_xm[,c(bestgenelist, dim(testm)[2])]

if(length(trainfact)>1){
colnames(trainfact)=trainfactnames
colnames(testfact)=trainfactnames
modtrain=cbind(apply(modtrain,2,as.numeric),trainfact)
modtest=cbind(apply(modtest,2,as.numeric),testfact)
}
modtrain=as.data.frame(modtrain)
modtest=as.data.frame(modtest)

return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, blindtest=testacc, modtrainclass=nci_y, modtestclass=test_y, numfeat=dim(modtrain)[2], testacc=testacc, tenfoldacc=model_train$tot.accuracy,learningsets=train@learnmatrix,boostweight=boostweight,scoringmatrix=scoringmatrix))
#return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, method=bestmethod, blindtest=testacc))
##print("Complete")

}
