run_pso <-
function(trainm,trainclass,testm,testclass,outloc,maxnum=5,
                  transition_matrix=rbind(c(0,0.4,0.1,0.5),c(0.2,0.3,0.4,0.1),c(0,0.4,0.4,0.2),c(0.9,0.1,0,0)),
                  c1=2.05,
                  c2=2.05,
                  itr=10,
                  globalpso_maxitr=10,
                  global_max_itr=3,
                  num_part=20,
                  kname="radial",
                  errortype="BER",
                  weightA=0.7,
                  weightB=0.0,
                  weightC=0.05,
                  weightD=0.25,
                  featweight.max=0.01,
                  featweight.min=0.01,
                  numfolds=10,
                  followerprob=0.45,
                  confusionprob=0.25,
                  leaderprob=0.25,
                  wmax=1,
                  wmin=1,
                  behavior_reset_itr=5,
                  maxitrreset=10,
                  num_neighbors=3,
                  minselect.pct=0.5,
                  evalMode="CV2",
                  minfitnessthresh=50,
                  minnum=3,inertia_method="global",particlebehav_method="randbased",constriction_factor=1,
                  select.global.best=TRUE,numnodes=4,itr.terminate=FALSE,train.pct=0.7,min.iter.select=1,
                  bootstrap.itr=10,evalFunc=NA,boostweight=NA,stage1.featsel.methods=c("limma","lasso","rfe","elasticnet", "f.test"),
                  RankAggreg.run=FALSE,...)
{
	
suppressWarnings(dir.create(outloc))
  suppressWarnings(setwd(outloc))
  suppressWarnings(sink(file="Log.txt"))
  
  print("########Input parameters#######")
print(paste("Stage 1 feature selection methods: ",paste(stage1.featsel.methods,collapse=";"),sep=""))
print(paste("c1: ",c1,sep=""))
print(paste("c2: ",c2,sep=""))
print(paste("itr: ",itr,sep=""))
print(paste("globalpso_maxitr: ",globalpso_maxitr,sep=""))
print(paste("global_max_itr: ",global_max_itr,sep=""))
print(paste("num_part: ",num_part,sep=""))
print(paste("kname: ",kname,sep=""))
print(paste("errortype: ",errortype,sep=""))
print(paste("weightA: ",weightA,sep=""))
print(paste("weightB: ",weightB,sep=""))
print(paste("weightC: ",weightC,sep=""))
print(paste("weightD: ",weightD,sep=""))
print(paste("featweight.max: ",featweight.max,sep=""))
print(paste("featweight.min: ",featweight.min,sep=""))
print(paste("numfolds: ",numfolds,sep=""))
print(paste("followerprob: ",followerprob,sep=""))
print(paste("confusionprob: ",confusionprob,sep=""))
print(paste("leaderprob: ",leaderprob,sep=""))
print(paste("wmax: ",wmax,sep=""))
print(paste("wmin: ",wmin,sep=""))
print(paste("behavior_reset_itr: ",behavior_reset_itr,sep=""))
print(paste("maxitrreset: ",maxitrreset,sep=""))
print(paste("num_neighbors: ",num_neighbors,sep=""))
print(paste("minselect.pct: ",minselect.pct,sep=""))
print(paste("minfitnessthresh: ",minfitnessthresh,sep=""))
print(paste("maxnum: ",maxnum,sep=""))
print(paste("minnum: ",minnum,sep=""))
print(paste("inertia_method: ",inertia_method,sep=""))
print(paste("particlebehav_method: ",particlebehav_method,sep=""))
print(paste("constriction_factor: ",constriction_factor,sep=""))
print(paste("select.global.best: ",select.global.best,sep=""))
print(paste("RankAggreg.run:",RankAggreg.run,sep=""))
print("#################")

print("#####R Session Info")
print(sessionInfo())

trainm<-as.data.frame(trainm)

if(is.na(evalFunc)==TRUE){
evalFunc=eval_fit_kfold_diff
}


if(is.na(testm)==TRUE){
	testm<-trainm
}
if(is.na(testclass)==TRUE){
	testclass<-trainclass
}

parentevalMode<-evalMode


CMAres<-performCMA(trainm=trainm, trainclass=trainclass, testm=testm, testclass=testclass,outloc=outloc,
                   maxnum=maxnum,
                   minnum=3,
                   stepitr=1,
                   gsmethods=stage1.featsel.methods, #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
                   accuracyweight=1,
                   featweight=0.06,
                   minpresent=1,
                   kname="radial",
                   norm_method="none",
                   tolerance=0.1,
                   maxitrs=5,
                   classindex=1,
                   numfacts=0,
                   evalmethod="CV",
                   numfolds=10,
                   CVfoldthresh=0.7,
                   varselmethod="none",
                   scheme_val="one-vs-all",
                   iter_learn=1,boostweight=rep(0,dim(trainm)[2]),RankAggreg.run=RankAggreg.run)

cma_feat_list<-colnames(trainm)

trainm<-CMAres$modtraindata
testm<-CMAres$modtestdata
trainclass<-CMAres$modtrainclass
testclass<-CMAres$modtestclass
learningsets<-CMAres$learningsets
bestgenelistGA=CMAres$bestgenelistGA
bestgenelistRA=CMAres$bestgenelistRA

print("Stage 1 complete. Starting B3PSO now...")

d_dim<-dim(trainm)

print("Original train data dimension")
print(d_dim)

print("Original test data dimension")
print(dim(testm))

scoringmatrix=matrix(0,dim(trainm)[2],globalpso_maxitr)

max_num_feats=ceiling(maxnum*dim(trainm)[2])

overall_gbest=array(0, dim=c(d_dim[2]))


trainm<-as.data.frame(trainm)
testm<-as.data.frame(testm)

mod<-svm(x=trainm,y=trainclass,type="C", cross=10)

#print("train 10 fold")
#print(mod$tot.accuracy)

testmim=dim(testm)[2]


alltrainm<-trainm
alltrainclass<-trainclass

evalMode<-parentevalMode 
	 d_dim<-dim(trainm)
	 d<-dim(trainm)[2]
	

	scoringmatrix<-b3pso(outloc=outloc,dimsize=dim(trainm)[2],
transition_matrix=transition_matrix,c1=c1,
c2=c2,
globalpso_maxitr=globalpso_maxitr,
global_max_itr=global_max_itr,
num_part=num_part,
kname=kname,
errortype=errortype,
weightA=weightA,
weightB=weightB,
weightC=weightC,
weightD=weightD,
featweight.max=featweight.max,
featweight.min=featweight.min,
numfolds=numfolds,
followerprob=followerprob,
confusionprob=confusionprob,
leaderprob=leaderprob,
wmax=wmax,
wmin=wmin,
behavior_reset_itr=behavior_reset_itr,
maxitrreset=maxitrreset,
num_neighbors=num_neighbors,
minselect.pct=minselect.pct,
evalMode=evalMode,
minfitnessthresh=minfitnessthresh,
maxnum=maxnum,minnum=minnum,inertia_method=inertia_method,particlebehav_method=particlebehav_method,
constriction_factor=constriction_factor,select.global.best=select.global.best,numnodes=numnodes,bootstrap.itr=bootstrap.itr,evalFunc=evalFunc,trainm=trainm,trainclass=trainclass,
boostweight=boostweight,train.pct=train.pct,...)


		testacc<-scoringmatrix$acc
		#print("testacc")
		#print(testacc)
		#print(mean(testacc))
		#print(summary(testacc))

		#print(sd(testacc))
		scoringmatrix<-scoringmatrix$scoringmatrix

		trainm<-alltrainm	
		trainclass<-alltrainclass
		
		#print(scoringmatrix)
		summat=apply(scoringmatrix,1,sum)
		#print("dim of scoring matrix is ")
		#print(dim(scoringmatrix))
	
		dicesorenson_res<-get_DSindex(scoringmatrix)

#print("DS index stage 2")
#print(dicesorenson_res)


k=dim(scoringmatrix)[1]
ki_res<-get_KIindex(scoringmatrix,k)

#print("KI index stage 2")
#print(ki_res)
		#print(length(summary))

		#print(summary(summat))
		               s1<-summary(summat)
                num_itr_thresh<-s1[3] #ceiling(minselect.pct*globalpso_maxitr)

                pct_range<-seq(2,globalpso_maxitr)
                min_fitness_res<-(100000000000)
                for(num_itr_thresh in pct_range){

                bestgenelist=which(summat>=num_itr_thresh)  #ceiling(minselect.pct*globalpso_maxitr))

                #print(paste("Number of features selected in ",num_itr_thresh," iterations:", sep=""))
                #print(length(bestgenelist))

                x<-rep(0,length(summat))

                featweightcur<-0.0
                x[bestgenelist]<-1
                fitness_res<- eval_fit_test_diff(particle=x,numfolds=numfolds,trainm=trainm,trainclass=trainclass,
                                                testm=trainm,testclass=trainclass,errortype=errortype,kname=kname,accuracyweightA=0.7,
                                                accuracyweightB=0,accuracyweightC=0.05,accuracyweightD=0.25,featweight=featweightcur,max_num_feats=maxnum)
                fitness_res<-fitness_res$fitfunc
                #print(fitness_res)
                        if(fitness_res<=(min_fitness_res-1)){

                                best_thresh<-num_itr_thresh
                                best_results<-bestgenelist
                                min_fitness_res<-fitness_res
                        }

                }

                summary_testacc<-summary(testacc,na.rm=TRUE)
                #print(summary_testacc)


                bestgenelist<-best_results
                feat_ind<-bestgenelist
                feat_names<-colnames(trainm)
                 feat_list<-feat_names[feat_ind]
                #print(paste("Number of features selected in ",best_thresh," iterations:", sep=""))
                #print(length(bestgenelist))

                                feat_col<-0
 

        feat_col<-feat_ind
               
		if(length(feat_ind)<1){

			#print(stop("No features selected!"))
		}
		
	filestr2<-paste(outloc,  "selected_feature_index_final.csv", sep="")
	write.table(feat_col, file=filestr2, sep=",", row.names=FALSE)

		trainmata<-trainm
		testmata<-testm
		rm(trainm)
		rm(testm)
	 
		##print(feat_col)
		##print(trainmata[1:2,])
		
		
		finalset<-trainmata[,c(feat_col)]
			

		test_mod<-testmata[,c(feat_col)]


	filestr2<-paste(outloc, "modified_trainpso.csv", sep="")

	Class=trainclass

	modtrain<-cbind(finalset, Class)
	
	Class=testclass
	modtest<-cbind(test_mod, Class)

	write.table(modtrain, file=filestr2, sep=",", row.names=FALSE)

	filestr3<-paste(outloc, "modified_testpso.csv", sep="")
	write.table(modtest, file=filestr3, sep=",", row.names=FALSE)


	modtrain<-data.frame(modtrain)
	modtest<-data.frame(modtest)
	#model_train_err<-svm(finalset, trainclass,  kernel=kname, type="C", cross=10)

	#model_train_valid<-svm(finalset, trainclass,   kernel=kname, type="C")

	model_train_err<-svm(modtrain$Class~., data=modtrain, type="C", cross=10)

	#print("Modified train 10 fold accuracy using train data is ")
        #print(model_train_err$tot.accuracy)
train10foldacc<-model_train_err$tot.accuracy

filestr3<-paste(outloc, "10foldaccuracy.csv", sep="")
     write.table(model_train_err$tot.accuracy, file=filestr3, sep=",", row.names=FALSE)



#	model_train_valid<-svm(modtrain$Class~., data=modtrain, type="C")

	model_train_valid<-svm(finalset, trainclass,   kernel=kname, type="C")	
		pred_train<-predict(model_train_valid, finalset)
		train.table<-table(pred_train, trainclass)
		
	



	error<-1-sum(diag(train.table))/(dim(finalset)[1])
	#print("Modified train accuracy is ")
	#print(1-error)

	#print("train confusion matrix is ")
	#print(train.table)
	#print("Train dimension is ")
	#print(dim(finalset))
	
	##print(modtrain[1:4,])
	##print(modtest[1:4,])
	
	 mod_dim=dim(modtest)[2]
	 
	 test_mod<-as.data.frame(test_mod)

	#print("Test dimension is ")
	#print(dim(test_mod))

	rm(test_mod)
	rm(finalset)
	
		
		#run_pso(data_dim)		

trainf=paste(outloc, "modified_trainpso.csv", sep="")
testf=paste(outloc, "modified_testpso.csv", sep="")
trainm<-read.csv(trainf, header=TRUE)

testm<-read.csv(testf, header=TRUE)
trainm<-as.data.frame(trainm)
testm<-as.data.frame(testm)
mod<-svm(trainm$Class~., data=trainm,type="C")
testmim=dim(testm)[2]

trainmim=dim(trainm)[2]

pred<-predict(mod,testm[,-c(testmim)])
test.table=table(pred,testm$Class)
#print("Test confusion matrix is ")
#print(test.table)
testacc<-(sum(diag(test.table))/(dim(testm)[1]))
#print("Test acc is ")
#print(testacc)

filestr3<-paste(outloc, "testaccuracy.csv", sep="")
write.table(testacc, file=filestr3, sep=",", row.names=FALSE)


#testy<-testy[order(testy)]
#pred<-pred[order(testy)]


if(dim(trainm)[2]>3)
{
trainx=subset(trainm, select=-c(Class))
trainy=trainm$Class

testx=subset(testm, select=-c(Class))
testy=testm$Class

mod<-svm(trainm$Class~., data=trainm,type="C", cross=10)
#fitfunc<-testacc #-(0.04*(1-testmim-1))

#print("train 10 fold")
#print(mod$tot.accuracy)


filestr3<-paste(outloc, "psotenfoldaccuracy.csv", sep="")
write.table(mod$tot.accuracy, file=filestr3, sep=",", row.names=FALSE)

trainf=paste(outloc, "modified_trainpso.csv", sep="")
testf=paste(outloc, "modified_testpso.csv", sep="")
trainm<-read.csv(trainf, header=TRUE)
testm<-read.csv(testf, header=TRUE)
trainm<-as.data.frame(trainm)
testm<-as.data.frame(testm)

mod<-svm(trainm$Class~., data=trainm,type="C")
testmim=dim(testm)[2]


pred<-predict(mod,testm[,-c(testmim)])
test.table=table(pred,testm$Class)

print("Number of features in the final set:")
print(dim(trainm)[2]-1)


testacc<-(sum(diag(test.table))/(dim(testm)[1]))
#print("Test acc is ")
#print(testacc)
filestr3<-paste(outloc, "testaccuracy.csv", sep="")
        write.table(testacc, file=filestr3, sep=",", row.names=FALSE)
pred_acc<-multiclass.roc(testm$Class,as.numeric(pred))
        pred_acc_orig<-pred_acc$auc[1]
        auc_acc<-pred_acc_orig

print("Test AUC using the final set of features:")
print(auc_acc)

print("Test confusion matrix is ")
print(test.table)

print("Classification accuracy in test set:")
print(testacc)


 filestr3<-paste(outloc, "testAUC.csv", sep="")
 write.table(auc_acc, file=filestr3, sep=",", row.names=FALSE)

pred<-predict(mod,trainm[,-c(trainmim)])
#train.table=table(pred,trainm$Class)

trainacc<-(sum(diag(train.table))/(dim(trainm)[1]))
#print("Train acc is ")
#print(trainacc)

print("Training confusion matrix is ")
print(train.table)

print("Classification accuracy in training set:")
print(trainacc)

filestr3<-paste(outloc, "trainaccuracy.csv", sep="")
write.table(trainacc, file=filestr3, sep=",", row.names=FALSE)





}


#print("# of features after CMA:")
#print(dim(CMAres$modtrainmata))
print("# of features after PSO:")
print(dim(trainm)[2])


sink(file=NULL)
suppressWarnings(sink(file=NULL))

return(list(scoringmatrix=scoringmatrix,bestfeatlist=bestgenelist,bestfeatnames=feat_names,
            train.svm.model=mod,trainm.new=trainm,testm.new=testm,trainacc=trainacc,train10foldacc=train10foldacc,testacc=testacc,testauc=auc_acc,
            RankAggregCE.optimallist=bestgenelistRA,RankAggregGA.optimallist=bestgenelistGA))

}
