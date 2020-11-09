evalFunc_multiobj <-
function(particle)
	{
		
		trainm=X
		trainclass=Y
		if(is.na(seednum)==TRUE){
			seednum=runif(n=1,min=1,max=999999)
		}
		if(evalMode=="CV1" || evalMode=="CV2")
		{

numtrain<-(0.8*nrow(trainm))
evalmethod='MCCV'
set.seed(seednum)
trainlearningsets<-GenerateLearningsets(y=trainclass, method=evalmethod, fold=numfolds, strat=FALSE, niter=numfolds,ntrain=numtrain)
trainlearningsets<-trainlearningsets@learnmatrix
globalpso_maxitr=dim(trainlearningsets)[1]


}else{
	
		trainlearningsets<-seq(1,nrow(trainm))
		
		trainlearningsets<-as.matrix(t(trainlearningsets))
}
		errortype="AUC"
		kname="radial"
		featweight=0.05
		accuracyweightA=5
		accuracyweightB=1
		max_num_feats=10
		num_feat<-0
		alltrainm<-trainm
		alltrainclass<-as.numeric(as.factor(trainclass))
		
		overall_fitness<-{}
		for(pitr in 1:nrow(trainlearningsets)){
		
		trainm<-alltrainm
		trainclass<-alltrainclass
				 if(evalMode=="CV1")
	{
	
 
	trainm<-alltrainm[-c(trainlearningsets[pitr,]),]
	 trainclass<-alltrainclass[-c(trainlearningsets[pitr,])]
	subtest<-alltrainm[c(trainlearningsets[pitr,]),]
	subtestclass<-alltrainclass[c(trainlearningsets[pitr,])]

	
	
	#evalMode<-"bootstrap"
		set.seed(seednum)
		subtrain_ind<-sample(x=seq(1,dim(trainm)[1]),size=10*dim(trainm)[1],replace=TRUE)
                                trainm<-trainm[subtrain_ind,]
                               trainclass<-trainclass[subtrain_ind]	
	}else{

		if(evalMode=="CV2"){
       
			
     
	 trainm<-alltrainm[trainlearningsets[pitr,],]
         trainclass<-alltrainclass[trainlearningsets[pitr,]]
        subtest<-alltrainm[-c(trainlearningsets[pitr,]),]
        subtestclass<-alltrainclass[-c(trainlearningsets[pitr,])]
        }else{
		
			 subtest<-alltrainm
			subtestclass<-alltrainclass
		}

	}
		#select the columns with selected features 
		ind<-which(particle==1)

		folderror_perm<-{}

		#need to add 1 to take into account the index of the feature in the original dataset
		col_sel<-ind
		num_feat<-length(col_sel)
		##print("itr num feat:")
		##print(length(ind))	
		if(num_feat>1)
		{
			trainset<-trainm[,c(col_sel)]
			trainset<-cbind(trainclass,trainset)
			
			trainset<-data.frame(trainset)
			
			##print(trainset[1:4,])
			#trainset<-traindata[,c(col_sel)]
			
			#model<-svm(trainset, trainclass, type="C", kernel=kname, cross=kcross)

			#model<-svm(trainset, trainclass, type="C", kernel=kname, degree=tune_degree, gamma=tune_gamma, cost=tune_cost, cross=kcross)
			folderror<-{}
			#if(FALSE)
			
			folderrorkfold<-{}
			folderror_perm<-{}
			seed_vec<-c(129532,839147,407700)
			{
			 for(f in 1:3)
                        {
			 #model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype="AUC")
	
			seednum=seed_vec[f]	
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype=errortype,seednum)
			
			
			# folderror_cur<-model$confint[1]
			
			folderror_cur<-model$mean_acc[1]
			
			
			folderrorkfold<-c(folderrorkfold,folderror_cur) #model$tot.accuracy)
			set.seed(seed_vec[f])
			rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass[rand_ind],kname=kname,errortype=errortype,seednum)
			#if(numfolds>5){
                        #folderror_cur_perm<-model$confint[2]
			#}else{
			 folderror_cur_perm<-model$mean_acc[1]
			#}
                        folderror_perm<-c(folderror_perm,folderror_cur_perm)
                                                rm(model)
                        }
			
                       } 

		testset<-subtest[,c(col_sel)]	
		mod_cv <- svm(x=trainset[,-1],y=trainset$trainclass, type="C",kernel=kname)
predfit<-predict(mod_cv,testset)
svm_table<-table(predfit,subtestclass)

class_names<-rownames(svm_table)
beracc<-{}
i<-1
svm_acc<-{}
totacc<-length(which(predfit==subtestclass))/length(subtestclass)
for(c in 1:dim(svm_table)[1]){
subtestclass_ind<-which(subtestclass==class_names[c])
beracc<-c(beracc,length(which(predfit[subtestclass_ind]==subtestclass[subtestclass_ind]))/length(subtestclass_ind))
}

beracc<-as.numeric(beracc)

beracc<-mean(beracc,na.rm=TRUE)

if(errortype=="CV"){
        svm_acc[i]<-(totacc*100)
}else{
if(errortype=="AUC"){
        pred_acc<-multiclass.roc(subtestclass,as.numeric(predfit))
        pred_acc_orig<-pred_acc$auc[1]
        auc_acc<-pred_acc_orig


        svm_acc[i]<-(auc_acc*100)
}else{
svm_acc[i]<-(beracc*100)
}
}

		#	folderror<-mean(folderror)-(1.96*(sd(folderror)/sqrt(3)))
		folderror<-svm_acc[i]
		
	#	#print(folderrorkfold)
	#	#print(folderror_perm)
	#	#print(num_feat)
	#	#print(folderror)

				folderrorkfold<-mean(folderrorkfold) #-(1.96*(sd(folderrorkfold,na.rm=TRUE)/sqrt(2)))
                                folderror_perm<-mean(folderror_perm,na.rm=TRUE) #+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(2)))
                                if(num_feat>max_num_feats){
                                fitfunc<-(accuracyweightA*(folderrorkfold-folderror_perm))+accuracyweightB*(folderror)-(featweight*100*(num_feat/length(particle))) #(featweight*(num_feat))
                                }else{
                                        fitfunc<-(accuracyweightA*(folderrorkfold-folderror_perm))+accuracyweightB*(folderror)
                                }
					
	
			#folderror<-model$tot.accuracy
			#folderror<-(1-ber)*100
			rm(trainset)

		}
		else
		{
		folderror<-1
		 folderror<-1
                folderror_perm<-100
                                fitfunc<-(-100)
		}
		#fitfunc<-(accuracyweight*(folderror))+(featweight*(1-num_feat))  #-(featweight*100*(num_feat/length(particle)))  #+(featweight*(1-num_feat))
		
		rm(col_sel)
		rm(num_feat)
		#return(fitfunc)
		
		fitfunc<-(-1)*fitfunc
		#return(list("fitfunc"=fitfunc,"cverror"=folderror,"cvpermerror"=folderror_perm))
		overall_fitness<-c(overall_fitness,fitfunc)
	}
	overall_fitness<-mean(overall_fitness)
	#print(paste("Fitness: ",overall_fitness,sep=""))
	return(overall_fitness)
}
