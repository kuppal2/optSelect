eval_fit_kfold_diff_vold <-
function(particle, trainm,trainclass,numfolds,errortype="AUC",accuracyweightA=5,featweight=0.06,max_num_feats=10,kname="radial")
        {
                num_feat<-0
                #select the columns with selected features
                ind<-which(particle==1)
                #need to add 1 to take into account the index of the feature in the original dataset
                col_sel<-ind
                folderror<-{}
                folderror_perm<-{}
                num_feat<-length(col_sel)
                if(num_feat>1)
                {
                        trainset<-trainm[,c(col_sel)]
                        trainset<-cbind(trainclass,trainset)
                        trainset<-data.frame(trainset)
                        folderror<-{}
                        folderror_perm<-{}
                        {
                         for(f in 1:3)
                        {
                        model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype=errortype)
                        folderror_cur<-model$confint[1] #mean_acc
                        folderror<-c(folderror,folderror_cur) #model$tot.accuracy)
                        rm(model)
                        {
                        rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))
                        model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass[rand_ind],kname=kname,errortype=errortype)
                        folderror_cur_perm<-model$confint[2]
                        folderror_perm<-c(folderror_perm,folderror_cur_perm)
                                                rm(model)
                        }
                        }
                                folderror<-mean(folderror,na.rm=TRUE)
                                folderror_perm<-mean(folderror_perm,na.rm=TRUE) #+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(4)))
                        }
                                if(num_feat>max_num_feats){
                                        fitfunc<-((accuracyweightA)*(folderror-folderror_perm))-(featweight*100*(num_feat/length(particle)))
                                }else{
                                        fitfunc<-(accuracyweightA*(folderror-folderror_perm))
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
