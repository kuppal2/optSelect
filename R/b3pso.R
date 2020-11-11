b3pso <-
function(outloc,dimsize,
                transition_matrix,c1=2.05,
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
                wmax=1.2,
                wmin=0.4,
                behavior_reset_itr=5,
                maxitrreset=10,
                num_neighbors=3,
                minselect.pct=0.5,
                evalMode="CV2",
                minfitnessthresh=50,
                maxnum=5,minnum=3,inertia_method="global",particlebehav_method="randbased",
                constriction_factor=1,select.global.best=TRUE,numnodes=4,bootstrap.itr=10,seednum=NA,varnames=NA, 
                itr.terminate=FALSE,evalFunc,trainm=NA,trainclass=NA,boostweight=NA,train.pct=0.7,...)
{

if(is.na(boostweight)==TRUE){

boostweight=rep(0,dim(trainm)[2])
}

parentevalMode<-evalMode
testacc_all<-{}
#print("here")

		if(evalMode=="CV1" || evalMode=="CV2")
{

numtrain<-(train.pct*nrow(trainm))
evalmethod='MCCV'
set.seed(321)
#print("s")
#print(numtrain)
#print(dim(trainm))
#print(numfolds)


trainlearningsets<-GenerateLearningsets(y=trainclass, method=evalmethod, fold=globalpso_maxitr, strat=FALSE, niter=globalpso_maxitr,ntrain=numtrain)
trainlearningsets<-trainlearningsets@learnmatrix
globalpso_maxitr=dim(trainlearningsets)[1]

evalFunc=eval_fit_test_diff
}
if(evalMode!="custom"){
alltrainm<-trainm
alltrainclass<-trainclass
}

#print("here")
  scoringmatrix=matrix(0,dimsize,globalpso_maxitr)

	for (globalpso_itr in 1:globalpso_maxitr)
	{		
	 if(evalMode=="CV1")
	{
	#print(paste("learning sets: ",globalpso_itr,sep=""))
	#print(trainlearningsets[globalpso_itr,])
 
	trainm<-alltrainm[-c(trainlearningsets[globalpso_itr,]),]
	 trainclass<-alltrainclass[-c(trainlearningsets[globalpso_itr,])]
	subtest<-alltrainm[c(trainlearningsets[globalpso_itr,]),]
	subtestclass<-alltrainclass[c(trainlearningsets[globalpso_itr,])]

	#print(dim(trainm))
	#print(dim(subtest))
	
	#evalMode<-"bootstrap"
		set.seed(321)
		subtrain_ind<-sample(x=seq(1,dim(trainm)[1]),size=10*dim(trainm)[1],replace=TRUE)
                                trainm<-trainm[subtrain_ind,]
                               trainclass<-trainclass[subtrain_ind]	
	}else{

		if(evalMode=="CV2"){
       
			#print(paste("learning sets: ",globalpso_itr,sep=""))
        #print(trainlearningsets[globalpso_itr,])
	 trainm<-alltrainm[trainlearningsets[globalpso_itr,],]
         trainclass<-alltrainclass[trainlearningsets[globalpso_itr,]]
        subtest<-alltrainm[-c(trainlearningsets[globalpso_itr,]),]
        subtestclass<-alltrainclass[-c(trainlearningsets[globalpso_itr,])]
        }else{

			subtest<-alltrainm
                        subtestclass<-alltrainclass
	}

	}
	
	 #print(paste("Starting global iteration number : ",globalpso_itr,sep=""))
	 ll<-0

	 ul<-1

	bad_pos<-new("list")
        num_obstacles<-1

	fitness_x<-array(100000000000000000, dim=c(num_part, 1)) 
	 fitness_gbest<-100000000000000 
	cverror_gbest<-(-10000)
	cvpermerror_gbest<-(-10000)
	prev_gbest<-fitness_gbest+1 
	 fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))
	 if(is.na(varnames)==TRUE){
		feat_names<-paste("var",seq(1,dimsize),sep="")
	 }else{	
	 feat_names<-varnames #colnames(trainm)
		}
		feat_list<-feat_names

		global_no_change<-0
		no_change<-0
		
		global_best_index<-10000000000000000000
		
		min_fit_x<-1000000000000000

		min_fit_index<-100000000000000000
	
	#position
	 x<-array(0, dim=c(num_part,dimsize))

	#velocity
	v<-array(0, dim=c(num_part,dimsize))
	
	x_gbest<-array(0, dim=c(dimsize))

	p_gbest<-array(0, dim=c(dimsize))
	   x_lbest<-array(0, dim=c(num_part, dimsize))
	   count_feat<-array(0,dim=c(num_part))

#confused
a<-c(0.7,0.05,0.05,0.2)

#neighbor
b<-c(0.2,0.6,0.05,0.15)

#leader
c<-c(0.1,0.2,0.6,0.1)

#self
d<-c(0.25,0.1,0.05,0.6)

initial_state_prob_matrix<-rbind(a,b,c,d)

set.seed(321)
#randomly assign behavior
agent_behavior <- sample(x=1:dim(transition_matrix)[1], size = num_part, replace = TRUE, prob = c(confusionprob, followerprob,leaderprob,(1-(confusionprob+followerprob+leaderprob))))


prob_behavior_particles<-array(0, dim=c(num_part, dim(transition_matrix)[2]))
	for (row in 1:num_part)
	 {
		num_feat<-0
		
		#prob_behavior_particles[row,]<-c(0.4,0.5,0.1,0)	
	
		prob_behavior_particles[row,]<-initial_state_prob_matrix[agent_behavior[row],]		
		
		ran<-runif(dimsize,0,1)	
		for (col in 1:dimsize)		
		{
			if (ran[col]<0)
			{
				x[row,col]<-0
				x_lbest[row,col]<-0
				#num_feat<-num_feat+1

			}
			else	
			{
				x[row,col]<-1
				x_lbest[row,col]<-1
				num_feat<-num_feat+1
				
				#col_sel[row,num_feat]<-col
				
			}
			
		}
		 count_feat[row]<-num_feat
	 
	 }
	 
	 num_featl<-dimsize
	 num_featg<-dimsize

	overall_x_gbest<-x[1,]
	
	cl<-parallel::makeCluster(numnodes)


	#clusterExport(cl, "evalFunc")
	
	#clusterExport(cl, library(pROC))
	clusterEvalQ<-function(cl, expr)
	clusterCall(cl, eval, substitute(expr), env=.GlobalEnv)

	#load a library on all clusters
	clusterEvalQ(cl, library(e1071))
	
	clusterEvalQ(cl, library(MASS))
	clusterEvalQ(cl, library(CMA))
	clusterCall(cl, function() library(pROC))
 	
	
	itr_data={}
        k<-0



       	rank_vec<-seq(1,num_part)


		repeat
		{
			k<-k+1
			feat_sel<-0
		
			itr_val={}	
			#print("iteration number: ") 
			#print(k)

			min_fit_x<-1000000000000000
			 
			min_fit_index<-100000000000000000 
			part_list=new("list") #{}
			rank_sum_vec<-summary(rank_vec)


			

			
			for (i in 1:num_part)
			{
				#each element of the list represents a particle
				#part_list=c(part_list, c=list(sapply(x[i,],head)))
				part_list[[i]]<-x[i,]				

				if(k%%behavior_reset_itr==0)
				{
					#agent_behavior=runif(num_part,0,1)
					#agent_behavior<-sample(x=seq(1,dim(transition_matrix)[1]),size=num_part,replace=TRUE)
		#set.seed(321)          
    agent_behavior <- sample(x=1:dim(transition_matrix)[1], size = num_part, replace = TRUE, prob = c(confusionprob, followerprob,leaderprob,(1-(confusionprob+followerprob+leaderprob))))
 
			
				#if(FALSE)
				if(particlebehav_method=="rankbased")
				{	
					prob_behavior_particles[i,]<-initial_state_prob_matrix[agent_behavior[i],]
					if(rank_vec[i]>=rank_sum_vec[5]){

						#prob_behavior_particles[i,]<-c(0.1,0.4,0.5,0)
					
                                        prob_actions<-prob_behavior_particles[i,] %*% (transition_matrix %^% k)

                                        prob_actions<-as.vector(prob_actions)
                                        sum_actions<-summary(c(prob_actions))

                                        #best_action<-which(prob_actions==max(prob_actions)[1])

                                        best_actions<-which(prob_actions>=sum_actions[3])

                                                                             best_action<-sample(x=best_actions,size=1)
					}else
					{

						if(rank_vec[i]>=rank_sum_vec[3]){

                                                	#prob_behavior_particles[i,]<-c(0.333,0.333,0.333,0)
                                       
                                        prob_actions<-prob_behavior_particles[i,] %*% (transition_matrix %^% k)

                                        prob_actions<-as.vector(prob_actions)
                                        sum_actions<-summary(c(prob_actions))

                                        #best_action<-which(prob_actions==max(prob_actions)[1])

                                        best_actions<-which(prob_actions>=sum_actions[3])

                                                                             best_action<-sample(x=best_actions,size=1)

					 		}else{

								#prob_behavior_particles[i,]<-c(0.4,0.1,0,0.5)
								
								best_action<-sample(seq(1,4),size=1)
							}

					}
					 #prob_behavior_particles[i,]<-runif(4,0,1)
					
										agent_behavior[i]<-best_action
	
				}
		

				}
						
				
				}
		 	
	
				#res1=clusterCall(cl, (-1)*eval_fit_kfold(x[i,], 10,1,0.01))
				
				#res1=clusterCall(cl, function(y) eval_fit_kfold(x[y,],10), 1)
				#res1=parLapply(cl,list(a=as.data.frame(x[1,]),b=as.data.frame(x[5,1:5])),eval_fit_kfold_diff)
				
				#correct method
				#res1<-clusterApply(cl,list(a=as.data.frame(x[1,]),b=as.data.frame(x[5,1:5])),eval_fit_kfold_diff))
			
				
				boot_fitness<-new("list") #{}	
				boot_cv<-new("list")
				boot_cvperm<-new("list")
				#featweightcur<-featweight.max-(((featweight.max-featweight.min)/itr)*k)
			
	if(featweight.min==featweight.max){
		featweightcur=featweight.min
	}else{	
	featweightcur<-featweight.min+(((featweight.max-featweight.min)/k))				
	}
				if(featweightcur<featweight.min){featweightcur<-featweight.min}
				#featweightcur<-(1/k)*(featweight.max-featweight.min)

		fitness_x<-{}
		cverror<-{}
		cvpermerror<-{}
		
		
		if(evalMode=="bootstrap")
			{
			
				
					clusterExport(cl, "svm_cv")
				all_ind<-seq(1,dim(trainm)[1])
				for(boot_itr in 1:bootstrap.itr)
				{
				
				subtrain_ind<-sample(x=seq(1,dim(trainm)[1]),size=dim(alltrainm)[1],replace=TRUE)
				subtrain<-trainm[subtrain_ind,]
				subtrainclass<-trainclass[subtrain_ind]

				subtrain_ind<-unique(subtrain_ind)
				subtest_ind<-all_ind[-subtrain_ind]
				subtest<-trainm[subtest_ind,]
				subtestclass<-trainclass[subtest_ind]

				
				fitness_x<-{}
				cverror<-{}
				cvpermerror<-{}
				
				evalFunc=eval_fit_test_diff
		
#				res1<-clusterApply(cl,part_list,evalFunc,numfolds=numfolds,trainm=subtrain,trainclass=subtrainclass,
#						testm=subtest,testclass=subtestclass,errortype=errortype,kname=kname,accuracyweightA=accuracyweightA,
#						accuracyweightB=accuracyweightB,featweight=featweightcur,maxnum=maxnum)

			res1<-clusterApply(cl,part_list,eval_fit_test_diff,numfolds=numfolds,trainm=subtrain,trainclass=subtrainclass,
                                                testm=subtest,testclass=subtestclass,errortype=errortype,kname=kname,accuracyweightA=weightA,
                                                accuracyweightB=weightB,accuracyweightC=weightC,accuracyweightD=weightD,featweight=featweightcur,max_num_feats=maxnum)


				 for(np in 1:num_part)
				 {
                                       

                                        fitness_x<-c(fitness_x,res1[[np]]$fitfunc)
                                        cverror<-c(cverror,res1[[np]]$cverror)
                                        cvpermerror<-c(cvpermerror,res1[[np]]$cvpermerror)

                                        }

				#res1<-clusterApply(cl,part_list,evalFunc,numfolds=numfolds,errortype="BER")
				#fitness_x<-sapply(res1,head)
				boot_fitness[[boot_itr]]<-fitness_x #c(boot_fitness[[bool_itr]],fitness_x)
				boot_cv[[boot_itr]]<-cverror
				boot_cvperm[[boot_itr]]<-cvpermerror

				}
				

	
				boot_fitness<-as.data.frame(boot_fitness)
			
				
				boot_fitness<-apply(boot_fitness,1,function(x){mean(x,na.rm=TRUE)+1.96*(sd(x,na.rm=TRUE)/sqrt(bootstrap.itr))}) #mean(boot_fitness,na.rm=TRUE)
				fitness_x<-(1)*boot_fitness #fitness_x
			
				cverror<-as.data.frame(boot_cv)

                                cverror<-apply(cverror,1,function(x){mean(x,na.rm=TRUE)+1.96*(sd(x,na.rm=TRUE)/sqrt(bootstrap.itr))}) #mean(boot_fitness,na.rm=TRUE)

				 cvpermerror<-as.data.frame(boot_cvperm)

                                cvpermerror<-apply(cvpermerror,1,function(x){mean(x,na.rm=TRUE)+1.96*(sd(x,na.rm=TRUE)/sqrt(bootstrap.itr))}) #mean(boot_fitness,na.rm=TRUE)


					
			}	
				else
				{
					if(evalMode=="CV1" || evalMode=="CV2")
					{
							clusterExport(cl, "svm_cv")
						fitness_x<-{}
                                cverror<-{}
                                cvpermerror<-{}
                               
			 
					evalFunc=eval_fit_test_diff
				
					#validation set
                                res1<-clusterApply(cl,part_list,eval_fit_test_diff,numfolds=numfolds,trainm=trainm,trainclass=trainclass,
                                                testm=subtest,testclass=subtestclass,errortype=errortype,kname=kname,accuracyweightA=weightA,
						accuracyweightB=weightB,accuracyweightC=weightC,accuracyweightD=weightD,featweight=featweightcur,max_num_feats=maxnum)
						
				 #  res1<-clusterApply(cl,part_list,eval_fit_test_diff,numfolds=numfolds,errortype=errortype,kname=kname,accuracyweightA=accuracyweightA,
				#		accuracyweightB=accuracyweightB,featweight=featweightcur,max_num_feats=maxnum)			
					
				#res1<-clusterApply(cl,part_list,evalFunc_multiobjPSO, X=trainm, Y=trainclass, numfolds=numfolds,errortype=errortype,kname=kname,accuracyweightA=accuracyweightA,
				#		accuracyweightB=accuracyweightB,featweight=featweightcur,max_num_feats=maxnum, seednum = seednum,evalMode=evalMode)



                                 for(np in 1:num_part){
                                        #fitness_x<-sapply(res1,head)
                                        
					
					fitness_x<-(1)*fitness_x

                                        fitness_x<-c(fitness_x,res1[[np]]$fitfunc)
                                        cverror<-c(cverror,res1[[np]]$cverror)
                                        cvpermerror<-c(cvpermerror,res1[[np]]$cvpermerror)

					if(cverror<cvpermerror){

                                                bad_pos[[num_obstacles]]<-x[np,]
                                                num_obstacles<-num_obstacles+1

                                        }
	
                                        }

                                #res1<-clusterApply(cl,part_list,evalFunc,numfolds=numfolds,errortype="BER")
                                #fitness_x<-sapply(res1,head)

                                }else{

					if(evalMode=="CV"){
					#kfold 	
					clusterExport(cl, "svm_cv")
					evalFunc=eval_fit_kfold_diff
					##print(dim(trainm))
				res1<-clusterApply(cl,part_list,eval_fit_kfold_diff,trainm=trainm,trainclass=trainclass,numfolds=numfolds,
				errortype=errortype,accuracyweightA=accuracyweightA,accuracyweightB=accuracyweightB,featweight=featweightcur,max_num_feats=maxnum,kname=kname)
				#res1<-clusterApply(cl,part_list,evalFunc_multiobjPSO, X=trainm, Y=trainclass, numfolds=numfolds,errortype=errortype,kname=kname,accuracyweightA=accuracyweightA,
				#		accuracyweightB=accuracyweightB,featweight=featweightcur,max_num_feats=maxnum, seednum = seednum,evalMode=evalMode)

					for(np in 1:num_part)
					{
			        	
					
					fitness_x<-c(fitness_x,res1[[np]]$fitfunc)
					cverror<-c(cverror,res1[[np]]$cverror)
					cvpermerror<-c(cvpermerror,res1[[np]]$cvpermerror)
					
					if(cverror<cvpermerror){

						bad_pos[[num_obstacles]]<-x[np,]
						num_obstacles<-num_obstacles+1
						
					}
						
					}
					
					fitness_x<-(1)*fitness_x
					}else{
					
					
						
						res1<-clusterApply(cl,part_list,evalFunc,...)
						for(np in 1:num_part)
						{
						
						
						fitness_x<-c(fitness_x,res1[[np]])
						
						
					
							
						}
						fitness_x<-(1)*fitness_x
					}
					
					
					
					}
					
					
				}


				
			
				med_fit_x<-(1)*median(fitness_x)	
				min_fit_x<-min(fitness_x)
				min_fit_index<-which(fitness_x==min_fit_x)
				
				bestind<-runif(1,1,length(which(fitness_x==min_fit_x)))
				bestind<-min_fit_index[bestind]
				##print(bestind)
				numfeatl<-length(which(x[bestind[1],]==1))
			
				nfeats_perpart<-{}
				for (i in 1:num_part)
				{

					nfeats_perpart<-c(nfeats_perpart,length(which(x[i,]==1)))
					if(fitness_x[i]<fitness_lbest[i])
					{
					
						fitness_lbest[i]<-fitness_x[i]
						for (j in 1:dimsize)
						{
							
							x_lbest[i,j] <- x[i,j]
							
						}
				
						num_featl<-length(which(x[i,]==1))	
					}
			
				}

					
			fitness_var<-as.vector(fitness_x)
						
			
			 rank_vec<-rank(fitness_var)
						
			 #update the global best and its fitness
			if(min_fit_x < (fitness_gbest))
			{
				no_change<-0

				#print("Best fitness updated to:")
				#print(min_fit_x)
				#print("Best solution:")
				#print(x[global_best_index,])
				#print(length(which(x[global_best_index,]==1)))
				global_no_change<-0
						fitness_gbest<-min_fit_x
				
						cverror_gbest<-max(cverror)[1]
						cvpermerror_gbest<-max(cvpermerror)[1]
	
						global_best_index<-bestind
						num_featg<-num_featl

						cverror_gbest<-cverror[bestind]
                                                cvpermerror_gbest<-cvpermerror[bestind]

					
	
						#global_best_index<-round(runif(1,1,length(global_best_index)))
						
						for (j in 1:dimsize)
						{
							
							x_gbest[j]<-x[global_best_index,j]
						
								
						}
						overall_gbest=x_gbest
						
						 overall_x_gbest<-x_gbest
			}
			else
			{
				no_change<-no_change+1

				
				#maxitrreset<-5
				
				
				
				#global no change is incremented if multiple reinitializations don't improve results
				if(no_change>maxitrreset)
				{
					global_no_change<-global_no_change+1
					
					#print("RE-INITIALIZING...")
				 	#stop(paste("No change for ",maxitr," iterations",sep=""))	
					 fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))
					##print(paste("No change for ",maxitr," iterations. Exiting PSO.",sep=""))

					x_lbest_vec<-apply(x_lbest,2,mean)
				
					#print(global_no_change)
					#print(global_max_itr)
					#print(length(which(x_lbest_vec>0)))

					if(global_no_change>global_max_itr & length(which(x_lbest_vec>0))>minnum)
					{
		
					x_lbest_vec[which(x_lbest_vec>=minselect.pct)]<-1
					x_lbest_vec[which(x_lbest_vec<minselect.pct)]<-0	
					d1<-dist(as.matrix(rbind(x_lbest_vec,overall_x_gbest)))^2
		                        d1pct<-100*(d1/length(x_lbest_vec))
						if(d1pct<=1 && global_no_change>(global_max_itr))
						{	
					#	#print(dim(x_lbest))	
					#	x_lbest<-apply(x_lbest,2,median)
						pdf_name<-paste("vel_xgbest_itr",globalpso_itr,".pdf",sep="")

						plot(v[global_best_index,which(overall_x_gbest==1)],xlab="velocity")
						dev.off()
						
						#print(paste("No change for ",global_no_change," iterations. Exiting PSO.",sep=""))
							break;
						}else{
							rand_num<-runif(num_part,0,1)
							for (row in 1:num_part)
                                        	{
                                                num_feat<-0

						 set.seed(321)
                                                 ran<-runif(1,0,1)

                                                for (col in 1:dimsize)

                                                {
                                                        #ran<-runif(1,0,1)
                                                        if (ran<0.7)
                                                        {
								set.seed(321)
                                                                ran2<-runif(1,0,1)

                                                                if(ran2<0.9)
                                                                {
                                                                        x[row,col]<-0
                                                                }
                                                                else
                                                                {
                                                                        x[row,col]<-1
                                                                        num_feat<-num_feat+1
                                                                }

                                                                #x_lbest[row,col]<-0
                                                        #       if(ran==1)
                                                        #       {
                                                        #               num_feat<-num_feat+1
                                                        #       }
                                                        }else{
                                                                x[row,col]<-x_gbest[col]
                                                                #x_lbest[row,col]<-1

                                                                #col_sel[row,num_feat]<-col

                                                        }

                                                }
                                                count_feat[row]<-num_feat

                                        	}

						}
					}
							#	#print("exited")
					
		#			min_fit_x<-1000000000000000000000
		#			v1<-sample(size=num_part*dimsize,x=c(0,1),replace=TRUE,prob=c(0.4,0.6))
		#			dim(v1)<-c(num_part, dimsize)	
						
			#agent_behavior[row]<-sample(x=seq(1,dim(transition_matrix)[1]),size=num_part,replace=TRUE)
						
			#agent_behavior<-sample(x=c(1,4,3),prob=c(0.4,0.4,0.2), size=num_part,replace=TRUE)
	
			#agent_behavior<-sample(x=c(1,4),prob=c(0.3,0.7), size=num_part,replace=TRUE)
			#agent_behavior<-sample(x=c(1,4,3),prob=c(0.3,0.4,0.3), size=num_part,replace=TRUE)	
			#agent_behavior <- sample(x=1:dim(transition_matrix)[1], size = num_part, replace = TRUE, prob = c(confusionprob, followerprob,(1-(confusionprob+followerprob)),0))

	 #agent_behavior <- sample(x=1:dim(transition_matrix)[1], size = num_part, replace = TRUE, prob = c(confusionprob, followerprob,leaderprob,(1-(confusionprob+followerprob+leaderprob))))

		#print(agent_behavior)

					#new addition in v19; revert to personal best
					#x<-x_lbest	
					#agent_behavior<-rep(4,num_part)

					#new addition in v20;
					for (i in 1:num_part)
                        		{
						x[i,]<-x_lbest_vec
					}
					
					agent_behavior[row]<-sample(x=seq(1,dim(transition_matrix)[1]),prob=c(0.25,0.25,0.25,0.25),size=num_part,replace=TRUE)
					
					#new addition in v7; removed in v13
					#x_gbest<-runif(dimsize,0,1)

					if(global_no_change>(global_max_itr*1.5))
					{
						#print(paste("No change for ",global_no_change," iterations. Exiting PSO.",sep=""))
                                                        break;
						
					}

				no_change<-0
				}
			}
			
			
				##print("x_lbest")
                                ##print(x_lbest[i,])
                                ##print(x[i,])
				##print(x_gbest)

			 ###update the particle velocity and position

				nn_search_res<-find_similar_samples(x,NA,num_neighbors)
				
				#w_vec<-(runif(num_part,wmin,wmax))

			for (i in 1:num_part)
			{
					feat_sel<-0
			     
			 
				#w<-w_vec[i]

				#w<-1/(3-exp(-num_part/200)+((rank_vec[i])/(8*dimsize))^2)
				#w<-(exp(rank_vec[i]/num_part))-(exp(1/num_part))	
				#w<-((exp(rank_vec[i]/num_part))/1.7)-((exp(1/num_part))/1.705)
				#w<-((exp(rank_vec[i]/num_part))/1.359)
				#constant; global search
				w<-1				
				
				#random inertia;
				#w<-(-1)+(runif(1,0,1)/2)
				

				if(inertia_method=="rankbased"){

				
					#linearly increasing with rank
					#w<-wmin+(((wmax-wmin)/num_part)*rank_vec[i])
					w<-wmin+(wmax-wmin)/((rank_vec[i]))
				}else{

					if(inertia_method=="random"){
				
						#random inertia;
                                		w<-(wmin)+(runif(1,wmin,wmax)/2)
					}else{

						if(inertia_method=="global"){
							w<-1
						}else{
							if(inertia_method=="dec"){

								w<-w-(wmax-wmin)/((itr*0.5))
							}
						}
					}
				}
				
				##print("x_lbest")
				##print(x_lbest[i,])
				##print(x[i,])		



				x_curbest<-x_gbest

				best_action<-agent_behavior[i]

				social_status<-1
	
					#confusion
					if(best_action==1){
									set.seed(321)
									ran<-runif(dimsize,0,1)

					                for (col in 1:dimsize)
					                {
					
					                        if (ran[col]<0.9)
					                        {
					                                x_curbest[col]<-0
					
					                        }else{
					
					                                x_curbest[col]<-1
					
					                        }
					
					                }




						}else
						{
							if(best_action==2){
							x_curbest_ind<- nn_search_res[i,c(1:num_neighbors)]  #nn_search(i,x) #getnearestneighbor
                                                       
							#using nbest in v9 
							if(FALSE)
							{
							#print("nearest neighbors are ")
                                                        #print(x_curbest_ind)
							#print(fitness_x[x_curbest_ind])
							best_fitness_neighbor<-which(fitness_x[x_curbest_ind]==min(fitness_x[x_curbest_ind],na.rm=TRUE)[1])[1]
                                                        x_curbest<-x[x_curbest_ind[best_fitness_neighbor],]
							#print(x_curbest_ind[best_fitness_neighbor])
							}

							#using summary of neighbors in v10
							#if(FALSE)
							{
							
                                                        if(num_neighbors>1){
                                                        x_curbest<-apply(x[x_curbest_ind,],2,function(x){y<-quantile(x,0.75);return(round(y));})
                                                        }else{
                                                        x_curbest<-x[x_curbest_ind[1],]
                                                        }

							}

								}else{
						
								if(best_action==3){
									x_curbest<-x_gbest	
								}else{

											#self status: select
						
										social_status<-0	
							



									
									}
									
							}	
						}
					
			#set.seed(321)	
			r1<-runif(dimsize,0,1)

			#set.seed(100045)
                                        r2<-runif(dimsize,0,1)			
			
			r3<-runif(dimsize,0,1)
	
	
			       for (j in 1:dimsize)
				{
					
					
					
					#r1<-runif(1,0,1)

					#r2<-runif(1,0,1)

					#r3<-runif(1,0,1)
							
					v[i,j]<-constriction_factor*((w*v[i,j])+(c1*r1[j]*(x_lbest[i,j]-x[i,j]))+(c2*r2[j]*(x_curbest[j]-x[i,j])*social_status))
					
					if(v[i,j]>6)
					{
						v[i,j]=6
					}	
					if(v[i,j]< (-6))
					{
						v[i,j]=-6
					}
					
					S<-1/(1+exp(-v[i,j]))
					S<-S+boostweight[j]
					
					if(S>=r3[j])
					{
							x[i,j]<-1

							feat_sel<-feat_sel+1
							
							count_feat[i]<-feat_sel						

					}			
					else
					{
						x[i,j]<-0
						
					}
				
					
				}
				check_badpos<-match(bad_pos,x[i,])
				num_badpos<-length(which(is.na(check_badpos)==FALSE))
				if(num_badpos>0){

					x[i,]<-x_lbest[i,]
				}

					
			}
			
	
			
			 x_lbest_mean<-apply(x_lbest,2,mean)
	

			feat_global_num<-length(which(overall_x_gbest==1))

	                feat_ind<-which(x_lbest_mean>=(minselect.pct))
			
			len_feat_ind<-length(feat_ind)

				 if(fitness_gbest<(-90) & feat_global_num<=maxnum & (global_no_change<=global_max_itr))
				 {
				
				
                                      ##print(paste("Population consensus reached after ",k," iterations! Exiting PSO.",sep=""))
                                        #break;

		                 }

			d1<-dist(as.matrix(rbind(x_lbest_mean,overall_x_gbest)))^2
			d1pct<-100*(d1/length(x_lbest_mean))
				
								
				if(k>global_max_itr)
				{
					
					#	if(itr.terminate==TRUE){
					#		break;
					#	}
						if(fitness_gbest<(-minfitnessthresh) & (global_no_change>=global_max_itr) & d1pct<=3){

						
						#break;

						}else{

							
							#print(paste("Minimum fitness threshold not reached after ",k," iterations! Continuing PSO.",sep=""))
							itr=k+itr-1

						}

	
				}
				
			
			
			
			
			itr_val<-{}
				cnames_sum<-c("Iteration #", "Inertia", "Number of features", "Global best fitness","Current best fitness","Number of features in current best agent","Number of features in global best", "Number of features in local best")

			itr_val<-cbind(itr_val, k)
			itr_val<-cbind(itr_val, w)
			itr_val<-cbind(itr_val, num_featg)
			if(evalMode!="custom"){
			itr_val<-cbind(itr_val, cverror_gbest)
			itr_val<-cbind(itr_val,cvpermerror_gbest)	
			cnames_sum<-c("Iteration #", "Inertia", "Number of features","CV best", "Permuted CV best", "Global best fitness","Current best fitness","Number of features in current best agent","Number of features in global best", "Number of features in local best")

			}
			itr_val<-cbind(itr_val, (-1)*fitness_gbest)
			itr_val<-cbind(itr_val, (-1)*min_fit_x)
			itr_val<-cbind(itr_val, count_feat[min_fit_index])
			itr_val<-cbind(itr_val,length(which(overall_x_gbest==1)))
			itr_val<-cbind(itr_val,length(which(x_lbest[min_fit_index,]==1)))
	
			itr_data<-rbind(itr_data, itr_val)
		}
	
		
		filestr<-paste(outloc,  "multiobj_itr",globalpso_itr,"_descpso.txt", sep="")

	
		

		bestgenelist<-which(overall_x_gbest==1)
		
		#print(bestgenelist)
		#print(globalpso_itr)
		
		scoringmatrix[bestgenelist,globalpso_itr]=1
	colnames(itr_data)<-cnames_sum
#	write.table(itr_data, file=filestr, sep="\t", row.names=F)


#print("##################################")
			#print(paste("Results summary for itr:",globalpso_itr,sep=""))
			 x_lbest_mean<-apply(x_lbest,2,mean)


                feat_ind<-which(x_lbest_mean>=minselect.pct)

                #print("number of features selected using population mean")
                #print(length(feat_ind))
		
			if(select.global.best==TRUE){	
				feat_ind<-which(overall_x_gbest==1)
			}
		#print("number of features selected using current global best")
                #print(length(which(overall_x_gbest==1)))

		#print("feat ind length")
		#print(length(feat_ind))

	if(length(feat_ind)>0){
		bestgenelist<-feat_ind
                scoringmatrix[bestgenelist,globalpso_itr]=1

		#print("best accuracy")
		#print((-1)*fitness_gbest)


		modtrain<-as.matrix(trainm[,c(bestgenelist)])
        modtest<-as.matrix(subtest[,c(bestgenelist)])
model_train_valid<-svm(modtrain,  trainclass,   kernel=kname, type="C")
pred_test<-predict(model_train_valid, modtest)
test.table<-table(pred_test, subtestclass)

testacc<-sum(diag(test.table))/(dim(modtest)[1])
}

#print(paste("test acc:", testacc, sep=""))
testacc_all<-c(testacc_all,testacc)
		sumr<-paste("global num features ", length(feat_list)-1, "current best set of features ", overall_x_gbest, "global best accuracy ", 1-fitness_gbest," test acc ", testacc)

		filestr2<-paste(outloc,  "selected_feature_index_itr",globalpso_itr,".csv", sep="")
	#	write.table(feat_ind, file=filestr2, sep=",", row.names=FALSE)
		#print("##################################")
		
	stopCluster(cl)	
	
	}

return(list("scoringmatrix"=scoringmatrix,"acc"=testacc_all))
}
