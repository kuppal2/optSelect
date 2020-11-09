


run_pso<-function(trainm,trainclass,transition_matrix,c1=2.05,
c2=2.05,
itr=10,
globalpso_maxitr=1,
global_max_itr=3,
num_part=30,
kname="radial",
errortype="BER",
accuracyweight=5,
featweight.max=10,
featweight.min=0.08,
k=10,
followerprob=0.35,
confusionprob=0.15,
leaderprob=0.35,
wmax=1,
wmin=(-1),
behavior_reset_itr=1,
maxitrreset=5,
num_neighbors=5,
max_num_feats=5,
minselect.pct=0.8,
bootstrap_val=FALSE,
minfitnessthresh=50,
max_num_feats=44)
{
	

  scoringmatrix=matrix(0,dim(trainm)[2],globalpso_maxitr)


for (globalpso_itr in 1:globalpso_maxitr)
        {	 #iitrt:<-numitrs
	 #maxitr<-50 #reinititrs
	 
	 d_dim<-dim(trainm)
	 d<-dim(trainm)[2]
	 
	c1<-2.05

	 c2<-2.05
	 
	 ll<-0

	 ul<-1
	fitness_x<-array(100000000000000000, dim=c(num_part, 1)) 
	 fitness_gbest<-100000000000000 #(-1)*CMAres$tenfoldacc #10000000000000000
cverror_gbest<-(-10000)
cvpermerror_gbest<-(-10000)
	prev_gbest<-fitness_gbest+1 
	 fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))
	 feat_names<-colnames(trainm)
		feat_list<-feat_names

		global_no_change<-0
		no_change<-0
		
		global_best_index<-10000000000000000000
		
		min_fit_x<-1000000000000000

		min_fit_index<-100000000000000000
	
	#position
	 x<-array(0, dim=c(num_part,d_dim[2]))

	#velocity
	v<-array(0, dim=c(num_part,d_dim[2]))
	
	x_gbest<-array(0, dim=c(d_dim[2]))

	p_gbest<-array(0, dim=c(d_dim[2]))
	   x_lbest<-array(0, dim=c(num_part, d_dim[2]))
	   count_feat<-array(0,dim=c(num_part))
	
prob_behavior_particles<-array(0, dim=c(num_part, dim(transition_matrix)[2]))
	for (row in 1:num_part)
	 {
		num_feat<-0
		
		prob_behavior_particles[row,]<-c(0.4,0.5,0.1,0)	
		ran<-runif(d_dim[2],0,1)	
		for (col in 1:d_dim[2])		
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
	 
	 num_featl<-d 
	 num_featg<-d

	
	
	clusterExport(cl, "x")
	clusterExport(cl, "eval_fit_test_diff")
	clusterExport(cl, "eval_fit_kfold_diff")
	clusterExport(cl, "trainm")
	clusterExport(cl, "trainclass")
	clusterExport(cl, "accuracyweight")
	clusterExport(cl, "featweight.max")
	clusterExport(cl, "featweight.min")
	clusterExport(cl, "k")
        clusterExport(cl, "kname")	
	clusterExport(cl, "svm_cv")
	#clusterExport(cl, library(pROC))
	clusterEvalQ<-function(cl, expr)
	clusterCall(cl, eval, substitute(expr), env=.GlobalEnv)

	#load a library on all clusters
	clusterEvalQ(cl, library(e1071))

	clusterCall(cl, function() library(pROC))
 	
	


	itr_data={}
        k<-0

	agent_behavior <- sample(x=1:dim(transition_matrix)[1], size = num_part, replace = TRUE, prob = c(confusionprob, followerprob,leaderprob,(1-(confusionprob+followerprob+leaderprob))))


       	rank_vec<-seq(1,num_part)


		repeat
		{
			k<-k+1
			feat_sel<-0
		
			itr_val={}	
			print("iteration number: ") 
			print(k)

			min_fit_x<-1000000000000000
			 
			min_fit_index<-100000000000000000 
			part_list={}
			rank_sum_vec<-summary(rank_vec)
			for (i in 1:num_part)
			{
				#each element of the list represents a particle
				part_list=c(part_list, c=list(sapply(x[i,],head)))
				
				if(k%%behavior_reset_itr==0)
				{
					#agent_behavior=runif(num_part,0,1)
					#agent_behavior<-sample(x=seq(1,dim(transition_matrix)[1]),size=num_part,replace=TRUE)
		#agent_behavior <- sample(x=1:dim(transition_matrix)[1], size = num_part, replace = TRUE, prob = c(confusionprob, followerprob,leaderprob,(1-(confusionprob+followerprob+leaderprob))))
               	
				#if(FALSE)
				{	
					if(rank_vec[i]>=rank_sum_vec[5]){

						prob_behavior_particles[i,]<-c(0.1,0.4,0.5,0)
					
                                        prob_actions<-prob_behavior_particles[i,] %*% (transition_matrix %^% k)

                                        prob_actions<-as.vector(prob_actions)
                                        sum_actions<-summary(c(prob_actions))

                                        #best_action<-which(prob_actions==max(prob_actions)[1])

                                        best_actions<-which(prob_actions>=sum_actions[3])

                                                                             best_action<-sample(x=best_actions,size=1)
					}else
					{

						if(rank_vec[i]>=rank_sum_vec[3]){

                                                	prob_behavior_particles[i,]<-c(0.333,0.333,0.333,0)
                                       
                                        prob_actions<-prob_behavior_particles[i,] %*% (transition_matrix %^% k)

                                        prob_actions<-as.vector(prob_actions)
                                        sum_actions<-summary(c(prob_actions))

                                        #best_action<-which(prob_actions==max(prob_actions)[1])

                                        best_actions<-which(prob_actions>=sum_actions[3])

                                                                             best_action<-sample(x=best_actions,size=1)


					 		}else{

								prob_behavior_particles[i,]<-c(0.4,0.1,0,0.5)
								
								best_action<-sample(seq(1,4),size=1)
							}


					}
				
					 #prob_behavior_particles[i,]<-runif(4,0,1)
					
					
										agent_behavior[i]<-best_action
	
				}
		

				}
						
				
			}
		 
		print("agent_behavior")
		print(agent_behavior)
				#res1=clusterCall(cl, (-1)*eval_fit_kfold(x[i,], 10,1,0.01))
				
				#res1=clusterCall(cl, function(y) eval_fit_kfold(x[y,],10), 1)
				#res1=parLapply(cl,list(a=as.data.frame(x[1,]),b=as.data.frame(x[5,1:5])),eval_fit_kfold_diff)
				
				#correct method
				#res1<-clusterApply(cl,list(a=as.data.frame(x[1,]),b=as.data.frame(x[5,1:5])),eval_fit_kfold_diff))
			
				all_ind<-seq(1,dim(trainm)[1])
				boot_fitness<-new("list") #{}	

				featweightcur<-1 #featweight.max-(((featweight.max-featweight.min)/maxitr)*k)
				
				if(featweightcur<featweight.min){featweightcur<-featweight.min}
				#featweightcur<-(1/k)*(featweight.max-featweight.min)

		fitness_x<-{}
		cverror<-{}
		cvpermerror<-{}
		if(bootstrap_val==TRUE)
			{
				for(boot_itr in 1:30){
				subtrain_ind<-sample(x=seq(1,dim(trainm)[1]),size=dim(trainm)[1],replace=TRUE)
				subtrain<-trainm[subtrain_ind,]
				subtrainclass<-trainclass[subtrain_ind]

				subtest_ind<-all_ind[-subtrain_ind]
				subtest<-trainm[subtest_ind,]
				subtestclass<-testclass[subtest_ind]

			
				res1<-clusterApply(cl,part_list,eval_fit_test_diff,numfolds=numfolds,trainm=subtrain,trainclass=subtrainclass,
						testm=subtest,testclass=subtestclass,errortype=errortype)
				
				#res1<-clusterApply(cl,part_list,eval_fit_kfold_diff,numfolds=numfolds,errortype="BER")
				fitness_x<-sapply(res1,head)
				boot_fitness[[boot_itr]]<-fitness_x #c(boot_fitness[[bool_itr]],fitness_x)
				
				}
				
				boot_fitness<-as.data.frame(boot_fitness)

				boot_fitness<-apply(boot_fitness,1,function(x){mean(x,na.rm=TRUE)+1.96*(sd(x,na.rm=TRUE)/sqrt(boot_itr))}) #mean(boot_fitness,na.rm=TRUE)
				fitness_x<-(-1)*boot_fitness #fitness_x
			
			}	
				else
				{
				res1<-clusterApply(cl,part_list,eval_fit_kfold_diff,numfolds=numfolds,errortype=errortype,featweight=featweightcur,max_num_feats=max_num_feats)
                        
				for(np in 1:num_part){
			        	#fitness_x<-sapply(res1,head)	
					#fitness_x<-(-1)*fitness_x
					
					fitness_x<-c(fitness_x,res1[[np]]$fitfunc)
					cverror<-c(cverror,res1[[np]]$cverror)
					cvpermerror<-c(cvpermerror,res1[[np]]$cvpermerror)
			
					}
					
					fitness_x<-(-1)*fitness_x

				}

					print(fitness_x)
					print(cverror)		
					print(cvpermerror)	
				med_fit_x<-(-1)*median(fitness_x)	
				min_fit_x<-min(fitness_x)
				min_fit_index<-which(fitness_x==min_fit_x)
				
				bestind<-runif(1,1,length(which(fitness_x==min_fit_x)))
				bestind<-min_fit_index[bestind]
				#print(bestind)
				numfeatl<-length(which(x[bestind[1],]==1))
			
				nfeats_perpart<-{}
				for (i in 1:num_part)
				{

					nfeats_perpart<-c(nfeats_perpart,length(which(x[i,]==1)))
					if(fitness_x[i]<fitness_lbest[i])
					{
					
						fitness_lbest[i]<-fitness_x[i]
						for (j in 1:d_dim[2])
						{
							
							x_lbest[i,j] <- x[i,j]
							
						}
				
						num_featl<-length(which(x[i,]==1))	
					}
			
				}

					print(nfeats_perpart)
			fitness_var<-as.vector(fitness_x)
						
			
			 rank_vec<-rank(fitness_var)
						
			 #update the global best and its fitness
			if(min_fit_x < fitness_gbest)
			{
				no_change<-0

				global_no_change<-0
						fitness_gbest<-min_fit_x
				
						cverror_gbest<-max(cverror)[1]
						cvpermerror_gbest<-max(cvpermerror)[1]
	
						global_best_index<-bestind
						num_featg<-num_featl

						cverror_gbest<-cverror[bestind]
                                                cvpermerror_gbest<-cvpermerror[bestind]

						print ("global fitness updated to :")
						print(fitness_gbest)
						print(min(fitness_x))
						print("min fit index")
						print(min_fit_index)
						print(global_best_index)
						print ("global best num features updated to")
						
						print(length(which(x[global_best_index,]==1)))
					
						print("CV error")
						print(cverror_gbest)
	
						print("CVperm error")
						print(cvpermerror_gbest)
	
						#global_best_index<-round(runif(1,1,length(global_best_index)))
						
						for (j in 1:d_dim[2])
						{
							
							x_gbest[j]<-x[global_best_index,j]
							
						}
						print(length(which(x_gbest==1)))
			}
			else
			{
				no_change<-no_change+1

				print("No change")
				print(no_change)
				
				print("Max itr reset")
				print(maxitrreset)
				#maxitrreset<-5
				
				
				
				#global no change is incremented if multiple reinitializations don't improve results
				if(no_change>maxitrreset)
				{
					global_no_change<-global_no_change+1
					
					print("RE-INITIALIZING...")
				 	#stop(paste("No change for ",maxitr," iterations",sep=""))	
					 fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))
					#print(paste("No change for ",maxitr," iterations. Exiting PSO.",sep=""))
					if(global_no_change>maxitr){
				
					#	print(dim(x_lbest))	
					#	x_lbest<-apply(x_lbest,2,median)
						print(paste("No change for ",global_no_change," iterations. Exiting PSO.",sep=""))
					break;
					}
							#	print("exited")
					min_fit_x<-1000000000000000000000
					
					
											
						#agent_behavior[row]<-sample(x=seq(1,dim(transition_matrix)[1]),size=num_part,replace=TRUE)
						
						#agent_behavior<-sample(x=c(1,4),size=num_part,replace=TRUE)
			#	agent_behavior <- sample(x=1:dim(transition_matrix)[1], size = num_part, replace = TRUE, prob = c(confusionprob, followerprob,(1-(confusionprob+followerprob)),0))

			agent_behavior <- sample(x=1:dim(transition_matrix)[1], size = num_part, replace = TRUE, prob = c(confusionprob, followerprob,leaderprob,(1-(confusionprob+followerprob+leaderprob))))


									


					no_change<-0
				}
			}
			
			
				#print("x_lbest")
                                #print(x_lbest[i,])
                                #print(x[i,])
				#print(x_gbest)

			 ###update the particle velocity and position

				nn_search_res<-find_similar_samples(x,NA,num_neighbors)
				print(dim(nn_search_res))
				print(head(nn_search_res))

				#w_vec<-(runif(num_part,wmin,wmax))

			for (i in 1:num_part)
			{
					feat_sel<-0
			     
			 
				#w<-w_vec[i]

				#w<-1/(3-exp(-num_part/200)+((rank_vec[i])/(8*d_dim[2]))^2)
				#w<-(exp(rank_vec[i]/num_part))-(exp(1/num_part))	
				#w<-((exp(rank_vec[i]/num_part))/1.7)-((exp(1/num_part))/1.705)
				#w<-((exp(rank_vec[i]/num_part))/1.359)
				#constant; global search
				w<-1				
				
				#random inertia;
				#w<-(-1)+(runif(1,0,1)/2)
				

				#linearly increasing with rank
				#w<-wmin+(((wmax-wmin)/num_part)*rank_vec[i])

				#w<-(0.5 - (1/(k+1)))

				#w<-wmax-(((wmax-wmin)/num_part)*rank_vec[i])
				
				w<-(wmax-wmin)*((itr-k)/itr)+wmin

				#print("x_lbest")
				#print(x_lbest[i,])
				#print(x[i,])		



				x_curbest<-x_gbest

				best_action<-agent_behavior[i]
	
					#confusion
					if(best_action==1){

									ran<-runif(d_dim[2],0,1)

					                for (col in 1:d_dim[2])
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
                                                        print("nearest neighbors are ")
                                                        print(x_curbest_ind)
                                                        if(num_neighbors>1){
                                                        x_curbest<-apply(x[x_curbest_ind,],2,function(x){y<-quantile(x,0.75);return(round(y));})
                                                        }else{
                                                        x_curbest<-x[x_curbest_ind[1],]
                                                        }

								}else{
						
								if(best_action==3){
									x_curbest<-x_gbest	
								}else{

											#DEath status: select all
											ran<-runif(d_dim[2],0,1)

							                for (col in 1:d_dim[2])
							                {
							
							                        if (ran[col]<0)
							                        {
							                                x_curbest[col]<-0
							
							                        }else{
							
							                                x_curbest[col]<-1
							
							                        }
							
							                }
							



									
									}
									
							}	
						}
						
			r1<-runif(d_dim[2],0,1)

                                        r2<-runif(d_dim[2],0,1)			
			
			r3<-runif(d_dim[2],0,1)
	
	
			       for (j in 1:d_dim[2])
				{
					
					
					
					#r1<-runif(1,0,1)

					#r2<-runif(1,0,1)

					#r3<-runif(1,0,1)
							
					v[i,j]<-((w*v[i,j])+(c1*r1[j]*(x_lbest[i,j]-x[i,j]))+(c2*r2[j]*(x_curbest[j]-x[i,j])))
					
					if(v[i,j]>6)
					{
						v[i,j]=6
					}	
					if(v[i,j]< (-6))
					{
						v[i,j]=-6
					}
					
					S<-1/(1+exp(-v[i,j]))
					
					if(S>r3[j])
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
					
			}
			
	
			
			 x_lbest_mean<-apply(x_lbest,2,mean)
	
		#	print(head(x_lbest))
			feat_global_num<-length(which(x_gbest==1))

	                feat_ind<-which(x_lbest_mean>=(minselect.pct))
			
			len_feat_ind<-length(feat_ind)

			#	if(len_feat_ind<=max_num_feats*2 & len_feat_ind>2 & med_fit_x>90 & len_feat_ind<=feat_global_num & no_change>50){

			#	      print(paste("Population consensus reached after ",k," iterations! Exiting PSO.",sep=""))
			#		break;
				
			#	}

				 if(fitness_gbest<(-90) & feat_global_num<=max_num_feats & (global_no_change<=maxitr))
				 {
				
				
                                      #print(paste("Population consensus reached after ",k," iterations! Exiting PSO.",sep=""))
                                        #break;

                 }

				
				print(paste("global no change :",global_no_change,sep=""))
				print(paste("fitness :",fitness_gbest,sep=""))
								
				if(k>itr)
				{
					
						
						if(fitness_gbest<(-minfitnessthresh) & (global_no_change>=global_max_itr)){

						
						break;

						}else{

							if(global_no_change<=global_max_itr)
							{
							
							
							
							print(paste("Minimum fitness threshold not reached after ",k," iterations! Continuing PSO.",sep=""))
							itr=k+itr-1
							
						
							
							
							}else{
								print(paste("Max init count reachced. Minimum fitness threshold not reached after ",global_max_itr," global iterations! Exiting PSO.",sep=""))
								break;
								
							}
						}

	
				}
				
			
			
			
			
			itr_val<-{}
			
			itr_val<-cbind(itr_val, k)
			itr_val<-cbind(itr_val, w)
			itr_val<-cbind(itr_val, num_featg)
			itr_val<-cbind(itr_val, cverror_gbest)
			itr_val<-cbind(itr_val,cvpermerror_gbest)	
			itr_val<-cbind(itr_val, (-1)*fitness_gbest)
			itr_val<-cbind(itr_val, (-1)*min_fit_x)
			itr_val<-cbind(itr_val, count_feat[min_fit_index])
			itr_val<-cbind(itr_val,length(which(x_gbest==1)))
			itr_val<-cbind(itr_val,length(which(x_lbest[min_fit_index,]==1)))
	
			itr_data<-rbind(itr_data, itr_val)
		}
	
		
		filestr<-paste(outloc,  "multiobj_itr",globalpso_itr,"_descpso.txt", sep="")

	cnames_sum<-c("Iteration #", "Inertia", "Number of features","CV best", "Permuted CV best", "Global best fitness","Current best fitness","Number of features in current best agent","Number of features in global best", "Number of features in local best")

		bestgenelist<-which(x_gbest==1)
		scoringmatrix[bestgenelist,globalpso_itr]=1
	colnames(itr_data)<-cnames_sum
	write.table(itr_data, file=filestr, sep="\t", row.names=F)


print("##################################")
			print(paste("Results summary for itr:",globalpso_itr,sep=""))
			 x_lbest_mean<-apply(x_lbest,2,mean)


                feat_ind<-which(x_lbest_mean>=minselect.pct)

                print("number of features selected using population mean")
                print(length(feat_ind))
			
				feat_ind<-which(x_gbest==1)
				
				 print("number of features selected using current global best")
                print(length(which(x_gbest==1)))

		print("best accuracy")
		print((-1)*fitness_gbest)

		if(length(feat_ind)<1){
		print(stop("No features selected!"))
		}
		sumr<-paste("global num features ", length(feat_list)-1, "current best set of features ", x_gbest, "global best accuracy ", 1-fitness_gbest)

		filestr2<-paste(outloc,  "selected_feature_index_itr",globalpso_itr,".csv", sep="")
		write.table(feat_ind, file=filestr2, sep=",", row.names=FALSE)
		print("##################################")
		
		
}

	return(list(scoringmatrix=scoringmatrix))

}