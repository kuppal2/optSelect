eval_fit_test_diff <-
function(particle, numfolds,trainm,trainclass,testm,testclass,errortype="AUC",kname="radial",featweight=0.05,accuracyweightA=5,
	accuracyweightB=1,accuracyweightC=1,accuracyweightD=0,max_num_feats=10)
	{
		
		num_feat<-0
		#select the columns with selected features 
		ind<-which(particle==1)

		folderror_perm<-{}

		num_itrs<-3
		#need to add 1 to take into account the index of the feature in the original dataset
		col_sel<-ind
		num_feat<-length(col_sel)
		##print("itr num feat:")
		##print(length(ind))	

		testclassorig<-testclass
	testmorig<-testm		

		
		if(num_feat>2)
		{
			trainset<-trainm[,c(col_sel)]
			trainset<-cbind(trainclass,trainset)
			trainset<-data.frame(trainset)
			
			#model<-svm(trainset, trainclass, type="C", kernel=kname, degree=tune_degree, gamma=tune_gamma, cost=tune_cost, cross=kcross)
			folderror<-{}
			folderror_vec<-{}

			#if(FALSE)
			seed_vec<-runif(num_itrs,1,1000000) #c(129532,839147,407700,100,1000,555,9999,123456,83414,1242)
			seed_vec<-c(129532)
			setseed=seed_vec[1] #129532 #seed_vec[3]
                        model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype=errortype,setseed=setseed)
                        foldacc<-model$confint[1]
		
		origtenfoldacc<-{}
                perm_acc<-{}
                seed_vec<-c(129532,839147,407700)

                for(r1 in 1:num_itrs)
		#perm_acc<-lapply(1:num_itrs,function(r1)
                {
                        seednum=seed_vec[r1]
        
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype=errortype,setseed=seednum)
                        foldacc<-model$confint[1] #$mean_acc #$confint[1]nohupOCFSvmay2415_v11_arcene_CV2_top5itr2.out 

			set.seed(seed_vec[r1])
                        rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))
                        model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass[rand_ind],kname=kname,errortype=errortype,setseed=seednum)

	                permtenfoldacc<-model$confint[2] #$mean_acc #$confint[2] #tot.accuracy
                        perm_acc<-c(perm_acc,permtenfoldacc)
             		#return(list("perm"=permtenfoldacc,"orig"=foldacc))
			origtenfoldacc<-c(origtenfoldacc,foldacc)
		   }#)

	#	perm_acc1<-as.data.frame(perm_acc)
		
		#perm_acc<-as.numeric(perm_acc1[,seq(1,dim(perm_acc1)[2],2)])
		#origtenfoldacc<-as.numeric(perm_acc1[,seq(2,dim(perm_acc1)[2],2)])
		#perm_acc<-unlist(perm_acc$perm)
		#foldacc<-unlist(perm_acc$orig)

	#write.table(perm_acc,file="perm_acc.txt",sep="\t",row.names=FALSE)	
	#perm_acc<-as.numeric(perm_acc)
		#fold_acc<-as.numeric(foldacc)
		
		#pdf("perm_acc_hist.pdf")
		#hist(perm_acc)
		#dev.off()
                
		#perm_acc<-mean(perm_acc,na.rm=TRUE)+2*sd(perm_acc,na.rm=TRUE)
                #fitfunc<-accuracyweight*(foldacc) #+featweight*(1-num_g/dim(nci_xm)[2])

                #fitfunc<-accuracyweightA*(foldacc-perm_acc)+accuracyweightB*(foldacc)-(100*featweight*(1-(dim(trainset)[2]/dim(trainm)[2])))

		foldacc<-mean(origtenfoldacc,na.rm=TRUE)-1.96*sd(origtenfoldacc,na.rm=TRUE)/sqrt(num_itrs)
		folderror_test<-foldacc #mean(origtenfoldacc,na.rm=TRUE)
		folderror<-(foldacc)

			
			folderror_perm<-(perm_acc)
		#folderror<-mean(folderror,na.rm=TRUE)-(1.96*(sd(folderror,na.rm=TRUE)/sqrt(2)))
                #                folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(2)))
                                #folderror<-mean(folderror,na.rm=TRUE)
                                #folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(3)))

#if(FALSE){

 set.seed(seed_vec[r1])
 rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))

 mod_cv <- svm(x=trainset[,-1],y=trainset$trainclass, type="C",kernel=kname)

testclass<-trainset$trainclass
                testset<-trainset[,-c(1)]

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

folderror_train<-svm_acc[i]

#}


beracc<-{}
svm_acc<-{}
for(i in 1:num_itrs){
#perm train
set.seed(seed_vec[i])
 rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))

 mod_cv <- svm(x=trainset[,-1],y=trainset$trainclass[rand_ind], type="C",kernel=kname)
 predfit<-predict(mod_cv,trainset[,-1])
 svm_table<-table(predfit,testclass)

testclass<-trainset$trainclass
                testset<-trainset[,-c(1)]

 class_names<-rownames(svm_table)
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
        svm_acc<-c(svm_acc,(totacc*100))
}else{
if(errortype=="AUC"){
        pred_acc<-multiclass.roc(testclass,as.numeric(predfit))
        pred_acc_orig<-pred_acc$auc[1]
        auc_acc<-pred_acc_orig


        svm_acc<-c(svm_acc,auc_acc*100)
}else{
svm_acc<-c(svm_acc,(beracc*100))
}
}

}
folderror_train_perm<-mean(svm_acc,na.rm=TRUE)+1.96*sd(svm_acc,na.rm=TRUE)/(sqrt(num_itrs))





#REVERSE evaluation
#if(FALSE)
{

		#testm<-testmorig
		#testclass<-testclassorig

		testset<-testm[,c(col_sel)]

		trainclass_temp<-trainclass
		trainset_temp<-trainm[,c(col_sel)]
	
		trainset<-testset
		trainclass<-testclass
		
		testclass<-trainclass_temp
		testset<-trainset_temp

		max_lim<-max(100,dim(testset)[1])	
		set.seed(321)
                subtrain_ind<-sample(x=seq(1,dim(trainset)[1]),size=max_lim,replace=TRUE)
                trainset<-trainset[subtrain_ind,]	
		trainclass<-trainclass[subtrain_ind]

#testclass<-trainclass_temp
 #               testset<-trainset_temp

		mod_cv <- svm(x=trainset,y=trainclass, type="C",kernel=kname)
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
beracc<-as.numeric(beracc)
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
		folderror_test2<-svm_acc[i]
}

#if(FALSE)
{
		#folderror_test<-folderror+runif(1,-5,5)
		 #folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(2*(sd(folderror_perm,na.rm=TRUE))) #/sqrt(num_itrs))

		folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(1.96*(sd(folderror_perm,na.rm=TRUE))/sqrt(num_itrs))

		#if(num_feat>max_num_feats)
		{
                
#		 fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror_test-folderror_test2)-(featweight*100*(num_feat/length(particle)))
 
#	             fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(mean(c(folderror_test,folderror_test2),na.rm=TRUE))-(featweight*100*(num_feat/length(particle))) #(featweight*(num_feat)) 
       

#Serialization technique: all functions range between 0-100
 #fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror_train-folderror_train_perm)+accuracyweightB*(folderror_test2)+((featweight*100*((max_num_feats-num_feat)/length(particle))))

if(FALSE){
weightA<-0.9
weightB<-0.0
weightC<-0.1
weightD<-0.001
}

#gives golub 0.96 AUC
if(FALSE){
weightA<-0.7
weightB<-0
weightC<-0.05
weightD<-0.2
weightE<-0.05
featweight<-weightE
}




fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror_train-folderror_train_perm)+accuracyweightC*(folderror_test2)+accuracyweightD*(folderror)+((featweight*(max((100*(max_num_feats-num_feat))/max_num_feats,100))))


	                        #}else{
                                 #       fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror_test)
                                }
}

 
		if(FALSE){
		fitfunc<-accuracyweightA*(foldacc-perm_acc)+accuracyweightB*(folderror_test)-(100*featweight*((dim(trainset)[2]/dim(trainm)[2])))
folderror_perm<-perm_acc
folderror<-foldacc
		}	
                       
		if(FALSE)
		{
				t1<-t.test(x=folderror,y=folderror_perm,paired=T,alternative="greater")
				
				if(t1$p.value<0.05){
					#folderror_cur_reg<-min(folderror)
					folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(2*(sd(folderror_perm,na.rm=TRUE))/sqrt(num_itrs)) #/sqrt(4))) #max(folderror_perm)
					folderror<-mean(folderror,na.rm=TRUE)-(2*(sd(folderror,na.rm=TRUE))/sqrt(num_itrs))
					fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror_test)-(featweight*100*(num_feat/length(particle)))
			
					# fitfunc<-accuracyweight*(foldacc-perm_acc)+1*(foldacc)-(100*featweight*(1-num_g/dim(nci_xm)[2]))

				}else{
					fitfunc<-(-100)
				}	
		}


if(FALSE)
{
			#folderror<-mean(folderror,na.rm=TRUE)-(2*(sd(folderror_perm,na.rm=TRUE)))
			folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(2*(sd(folderror_perm,na.rm=TRUE)))
                #fitfunc<-accuracyweight*(foldacc) #+featweight*(1-num_g/dim(nci_xm)[2])
			fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror)+0*folderror_test-(featweight*100*(num_feat/length(particle)))
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
		#folderror_perm<-mean(folderror_perm,na.rm=TRUE)
		#folderror<-mean(folderror,na.rm=TRUE)	
		fitfunc<-(-1)*fitfunc
		return(list("fitfunc"=fitfunc,"cverror"=folderror,"cvpermerror"=folderror_perm))
	}
