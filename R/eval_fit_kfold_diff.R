eval_fit_kfold_diff <-
function(particle, trainm,trainclass,numfolds,errortype="AUC",accuracyweightA=5,accuracyweightB=1,featweight=0.06,max_num_feats=10,kname="radial")
        {

                num_feat<-0
                #select the columns with selected features 
                ind<-which(particle==1)

                #need to add 1 to take into account the index of the feature in the original dataset
                col_sel<-ind
		folderror<-{}
                        folderror_perm<-{}
                num_feat<-length(col_sel)
                ##print("itr num feat:")
                if(num_feat>1)
                {
                        trainset<-trainm[,c(col_sel)]
                        trainset<-cbind(trainclass,trainset)
                        trainset<-data.frame(trainset)
                        folderror<-{}
			folderror_perm<-{}
                        seed_vec<-c(129532,839147,407700)
			for(f in 1:3)
                        {
                      	#setseed=runif(1,1000000,n=1)
			#setseed=round(setseed)  
			#setseed=NA #321 
			setseed=seed_vec[f]
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype=errortype,setseed=setseed)
			folderror_cur<-model$confint[1] #-(model$confint[2]-model$confint[1]) #mean_acc
                        folderror<-c(folderror,folderror_cur) #model$tot.accuracy)
			rm(model)
			
			#for(f in 1:3)
                        #{
			set.seed(seed_vec[f])
			rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass[rand_ind],kname=kname,errortype=errortype,setseed=setseed)
                        folderror_cur_perm<-model$confint[2]
                        folderror_perm<-c(folderror_perm,folderror_cur_perm)
						rm(model)	
                        }
				###############******************************
				#Note v40: changed confint to only mean 
				folderror<-mean(folderror,na.rm=TRUE)-(1.96*(sd(folderror,na.rm=TRUE)/sqrt(2)))
				folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(2)))
				#folderror<-mean(folderror,na.rm=TRUE)
                                #folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(3)))

				if(num_feat>max_num_feats){
				fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror)-(featweight*100*(num_feat/length(particle))) #(featweight*(num_feat))	
				}else{
					fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror)	
				}
                ##print(paste("accuracy: ", folderror," num_feat:",num_feat," fitness:", fitfunc,sep=""))
	}
                else
                {
                folderror<-1
                folderror_perm<-100
				fitfunc<-(-100)
		}
                rm(col_sel)
                rm(num_feat)
		fitfunc<-(-1)*fitfunc
                return(list("fitfunc"=fitfunc,"cverror"=folderror,"cvpermerror"=folderror_perm))

        }
