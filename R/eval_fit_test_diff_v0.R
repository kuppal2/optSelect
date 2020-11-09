eval_fit_test_diff_v0 <-
function(particle, numfolds,trainm,trainclass,testm,testclass,errortype="AUC",kname="radial",featweight=0.05,accuracyweightA=5,
	accuracyweightB=1,max_num_feats=10)
	{
		
		num_feat<-0
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
			folderror_vec<-{}

			#if(FALSE)
			seed_vec<-c(129532,839147,407700)
			{
			 for(f in 1:3)
                        {
			 #model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype="AUC")
	
			setseed=seed_vec[f]
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype=errortype,setseed=setseed)
                        folderror_cur_reg<-model$confint[1]
	
			#folderror_cur<-model$mean_acc
			##print(model)
			##print(folderror_cur)	
			#folderror<-c(folderror,folderror_cur) #model$tot.accuracy)
			set.seed(seed_vec[f])
			rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))
                        model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass[rand_ind],kname=kname,errortype=errortype,setseed=setseed)
                        folderror_cur_perm<-model$confint[2]
                        folderror_perm<-c(folderror_perm,folderror_cur_perm)
                                                rm(model)
                        }
                       } 

		testset<-testm[,c(col_sel)]	
		mod_cv <- svm(x=trainset[,-1],y=trainset$trainclass, type="C",kernel=kname)
predfit<-predict(mod_cv,testset)
svm_table<-table(predfit,testclass)

class_names<-rownames(svm_table)
beracc<-{}
i<-1
svm_acc<-{}
totacc<-length(which(predfit==testclass))/length(testclass)
for(c in 1:dim(svm_table)[1]){
testclass_ind<-which(testclass==class_names[c])
beracc<-c(beracc,length(which(predfit[testclass_ind]==testclass[testclass_ind]))/length(testclass_ind))
}
##print(beracc)
beracc<-as.numeric(beracc)
##print(beracc)
beracc<-mean(beracc,na.rm=TRUE)

if(errortype=="CV"){
        svm_acc[i]<-(totacc*100)
}else{
if(errortype=="AUC"){
        pred_acc<-multiclass.roc(testclass,as.numeric(predfit))
        pred_acc_orig<-pred_acc$auc[1]
        auc_acc<-pred_acc_orig


        svm_acc[i]<-(auc_acc*100)
}else{
svm_acc[i]<-(beracc*100)
}
}

		#	folderror<-mean(folderror)-(1.96*(sd(folderror)/sqrt(3)))
		folderror<-svm_acc[i]

                                folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(2)))
                                if(num_feat>max_num_feats){
                                fitfunc<-(accuracyweightA*(folderror_cur_reg-folderror_perm))+accuracyweightB*(folderror)-(featweight*100*(num_feat/length(particle))) #(featweight*(num_feat))
                                }else{
                                        fitfunc<-(accuracyweightA*(folderror_cur_reg-folderror_perm))+accuracyweightB*(folderror)
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
		##print(paste("accuracy: ", folderror," num_feat:",num_feat," fitness:", fitfunc,sep=""))
		rm(col_sel)
		rm(num_feat)
		#return(fitfunc)
		
		fitfunc<-(-1)*fitfunc
		return(list("fitfunc"=fitfunc,"cverror"=folderror,"cvpermerror"=folderror_perm))
	}
