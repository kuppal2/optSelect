
#1)
###################################################################
ocfs.run<-function(train)
{

print("Stage 1: Consensus based")
print("Stage 1: Consensus based")
#CMAres<-preProcess(datafile, outloc, percentTest,norm_method,classindex, upclasssep, dclasssep, removeindex, maxfacts)
CMAres<-performCMA(traind,testd, outloc, maxfeatspercent, minfeats, stepitr, methods, percentTest, featWeight,accWeight, kname, max.nochange.itrs, minpresent, norm_method, tolerance, classindex, upclasssep, dclasssep, removeindex, maxfacts,numfolds=k,evalmethod,CVfoldthresh,backward.sel,scheme_val,iter_learn)

#if(FALSE)
{
print("Stage 2: Selecting optimal subset using PSO")
#system.time(

#psores<-performPSO(trainx, testx, trainy, testy, outloc, numitrs, reinititrs, numpart, c1, c2, kname, percentTest, 0.04, fitfunc)

psores<-performPSO(CMAres$modtraindata, CMAres$modtestdata, CMAres$modtrainclass, CMAres$modtestclass,outloc, numitrs, reinititrs, numpart, c1, c2, kname, percentTest, 0.0, 1, fitfunc)

trainm<-CMAres$modtraindata
testm<-CMAres$modtestdata
trainclass<-CMAres$modtrainclass
testclass<-CMAres$modtestclass


d_dim<-dim(trainm)
}
}
###################################################################
	

#2)
###################################################################
#function to find nearest neighbor of a particle
nn_search<-function(particle_ind, part_group)
{
	partm<-as.matrix(part_group[particle_ind,])
	
	#nn_arr<-array(0,2)
	dist_mat<-dist(as.matrix(part_group))
	dist_mat<-as.matrix(dist_mat)
	nn_order<-order(dist_mat[particle_ind,])
	nn_order<-nn_order[-c(1)]
	return(nn_order)
}
###################################################################


#3)
	#function to evaluate k-fold CV
	###################################################################
	eval_fit_kfold_diff<-function(particle,trainm,trainclass,accuracyweight=1,featweight=0.06)
	{
		num_feat<-0
		#select the columns with selected features 
		ind<-which(particle==1)
		#need to add 1 to take into account the index of the feature in the original dataset
		col_sel<-ind
		num_feat<-length(col_sel)
		if(num_feat>1)
		{
			trainset<-trainm[,c(col_sel)]
			trainset<-cbind(trainclass,trainset)
			
			trainset<-data.frame(trainset)
			folderror<-{}
			 for(f in 1:3)
                        {
	
									folderror_cur<-svm_cv(x=trainset[,-c(1)],y=trainset[,1],v=10,kname="radial")
									print(folderror_cur)	
									folderror_cur<-folderror_cur$error #-(1.96*folderror_cur$sderror)
									folderror<-c(folderror,folderror_cur) #model$tot.accuracy)
			
                        }
                        folderror<-mean(folderror)-(1.96*sd(folderror)/(sqrt(length(folderror))))
						#folderror<-model$tot.accuracy
						#folderror<-(1-ber)*100
						rm(trainset)
		}
		else
		{
		folderror<-1
		}
		fitfunc<-(accuracyweight*(folderror))-(featweight*(num_feat/length(particle)))  #+(featweight*(1-num_feat))
		print(paste("accuracy: ", folderror," num_feat:",num_feat," fitness:", fitfunc,sep=""))
		rm(col_sel)
		rm(num_feat)
		return(fitfunc)

	}
	###################################################################
	
	#4)
	###################################################################
	#Function to run binary PSO:
	#stochastic behavioral optimization
	#PSBBO: particle swarm behavior based optimization
	#final output is a vector that includes the optimum set of features
	
	performPSOsvm<-function(trainm,trainclass,num_part=30,c1=2.05,c2=2.05,wmax=1,wmin=0.4,itr=5000,max.nochange.itr=50,svm.kernel="radial",accuracy.weight=1,feat.weight=0.06,
	num.folds=10,followerprob=0.1,confusionprob=0.5,behavior.reset.itr=50,scale=TRUE, degree=NA, gamma=NA, coef0=0, cost=1, nu=NA,tolerance=NA,epsilon=NA,
shrinking=TRUE, fitted=TRUE, probability=TRUE,outloc,numnodes=4)
	{
	 #itr<-numitrs
	 #max.nochange.itr<-50 #reinititrs
	 d<-dim(trainm)[2]
	 d_dim<-dim(trainm)

	c1<-2.05

	 c2<-2.05
	 
	 ll<-0
	 
	 ul<-1
	 fitness_x<-array(100000000000000000, dim=c(num_part, 1)) 
	 fitness_gbest<-10000000000000000
	 prev_gbest<-fitness_gbest+1 
	 fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))
	 feat_names<-colnames(trainm)
	 feat_list<-feat_names

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
	 
	for (row in 1:num_part)
	 {
		num_feat<-0
		ran<-runif(d_dim[2])
		
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

	cl<-makeCluster(numnodes)
	#clusterExport(cl, "x")

	clusterExport(cl, "eval_fit_kfold_diff")
	#clusterExport(cl, "trainm")
	#clusterExport(cl, "trainclass")
	#clusterExport(cl, "accuracyweight")
	#clusterExport(cl, "featweight")
	#clusterExport(cl, "k")
        #clusterExport(cl, "kname")	
	clusterExport(cl, "svm_cv")
	clusterEvalQ<-function(cl, expr)
	clusterCall(cl, eval, substitute(expr), env=.GlobalEnv)

	#load a library on all clusters
	clusterEvalQ(cl, library(e1071))

 	agent_behavior<-runif(num_part,0,1)	
	itr_data={}
	for (k in 1:itr)
		{
			feat_sel<-0
		
			itr_val={}	
			print("iteration number: ") 
			print(k)

			min_fit_x<-1000000000000000
			 
			min_fit_index<-100000000000000000 
			part_list={}
			for (i in 1:num_part)
			{
				#each element of the list represents a particle
				part_list=c(part_list, c=list(sapply(x[i,],head)))
				
				if(k%%behavior.reset.itr==0){
					agent_behavior=runif(num_part,0,1)
				}	
			}
				#res1=clusterCall(cl, (-1)*eval_fit_kfold(x[i,], 10,1,0.01))
				
				#res1=clusterCall(cl, function(y) eval_fit_kfold(x[y,],10), 1)
				#res1=parLapply(cl,list(a=as.data.frame(x[1,]),b=as.data.frame(x[5,1:5])),eval_fit_kfold_diff)
				
				#correct method
				#res1<-clusterApply(cl,list(a=as.data.frame(x[1,]),b=as.data.frame(x[5,1:5])),eval_fit_kfold_diff))
				
				res1<-clusterApply(cl,part_list,eval_fit_kfold_diff,trainm,trainclass)
				fitness_x<-sapply(res1,head)
				fitness_x<-(-1)*fitness_x
				
				if(FALSE){	
				for(i in 1:num_part){
				fitness_curx<-eval_fit_kfold_diff(part_list[[i]])
				fitness_x<-c(fitness_x,(-1)*fitness_curx)
				print("fitness is ")
				print(fitness_curx)
				print(part_list[i])
				}
				}
				
				min_fit_x<-min(fitness_x)
				min_fit_index<-which(fitness_x==min_fit_x)
				
				bestind<-runif(1,1,length(which(fitness_x==min_fit_x)))
				bestind<-min_fit_index[bestind]
				#print(bestind)
				numfeatl<-length(which(x[bestind[1],]==1))
				
				for (i in 1:num_part)
				{
					if(fitness_x[i]<fitness_lbest[i])
					{
					
						fitness_lbest[i]<-fitness_x[i]
						for (j in 1:d_dim[2])
						{
							
							x_lbest[i,j] <- x[i,j]
						
					}
					
				}
				
			
			 rank_vec<-rank(fitness_x)
			 
			 #update the global best and its fitness
			if(min_fit_x < fitness_gbest)
			{
				no_change<-0

				
						fitness_gbest<-min_fit_x
					
						global_best_index<-bestind
						num_featg<-num_featl
						print ("global fitness updated to :")
						print(fitness_gbest)
						print(min(fitness_x))
						print("min fit index")
						print(min_fit_index)
						print(global_best_index)
						print ("golbal best num features updated to")
						
						print(length(which(x[global_best_index,]==1)))
						
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

				if(no_change>max.nochange.itr)
				{
					#print("RE-INITIALIZING...")
				 	#stop(paste("No change for ",max.nochange.itr," iterations",sep=""))	
					 fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))
					print(paste("No change for ",max.nochange.itr," iterations. Exiting PSO.",sep=""))
					break;
					print("exited")
					min_fit_x<-1000000000000000000000
					 for (row in 1:num_part)
					{
						num_feat<-0
					
						 #ran<-runif(1,0,1)

						for (col in 1:d_dim[2])

						{
							x[row,col]<-x_gbest[col]
							num_feat<-num_feat+1
						}

								#col_sel[row,num_feat]<-col

							#}

						}
						count_feat[row]<-num_feat

					}				


					no_change<-0
				}
			}
			 ###update the particle velocity and position
			for (i in 1:num_part)
			{
					feat_sel<-0
			      
				
				#w<-1/(3-exp(-num_part/200)+((rank_vec[i])/(8*d_dim[2]))^2)
				#w<-(exp(rank_vec[i]/num_part))-(exp(1/num_part))	
				#w<-((exp(rank_vec[i]/num_part))/1.7)-((exp(1/num_part))/1.705)
				#w<-((exp(rank_vec[i]/num_part))/1.359)
				#constant; global search
				#w<-1				
				
				#random inertia;
				#w<-0.5+(runif(1,0,1)/2)
				
				#linearly increasing with rank
				w<-wmin+(((wmax-wmin)/num_part)*rank_vec[i])

				#w<-wmax-(((wmax-wmin)/num_part)*rank_vec[i])
				
				#w<-(wmax-wmin)*((itr-k)/itr)+wmin
				#print("x_lbest")
				#print(x_lbest[i,])
				#print(x[i,])		
			       x_curbest<-x_gbest
                                        if(agent_behavior[i]<confusionprob)
                                        {
                                             	ran<-runif(d_dim[2])
                                             
                				for (col in 1:d_dim[2]) 
                				{                               
                                                        
                        if (ran[col]<0)         
                        {
                                x_curbest[col]<-0

                        }else{

                                x_curbest[col]<-1

                        }

                }
                
                                        }else{
                                                if(agent_behavior[i]<(confusionprob+followerprob)){
                                                        x_curbest_ind<- nn_search(i,x) #getnearestneighbor
                                			#print("current best is ")
							#print(x_curbest_ind)
				                	x_curbest<-x[x_curbest_ind[1],]
						}else{
							x_curbest<-x_gbest
						}

                                        }

	
			       for (j in 1:d_dim[2])
				{
					r1<-runif(1,0,1)

					r2<-runif(1,0,1)

					r3<-runif(1,0,1)
							
					v[i,j]<-((w*v[i,j])+(c1*r1*(x_lbest[i,j]-x[i,j]))+(c2*r2*(x_curbest[j]-x[i,j])))
					
					if(v[i,j]>6)
					{
						v[i,j]=6
					}	
					if(v[i,j]< (-6))
					{
						v[i,j]=-6
					}
					
					S<-1/(1+exp(-v[i,j]))
					
					if(S>r3)
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
			itr_val<-cbind(itr_val, k)
			itr_val<-cbind(itr_val, w)
			itr_val<-cbind(itr_val, num_featg)
			itr_val<-cbind(itr_val, fitness_gbest)
			
			itr_val<-cbind(itr_val, min_fit_x)
			itr_val<-cbind(itr_val, count_feat[min_fit_index])
			itr_data<-rbind(itr_data, itr_val)
			
		}
		filestr<-paste(outloc,  "multiobj_itr_descpso.txt", sep="")

	write.table(itr_data, file=filestr, sep="\t", row.names=F)
	print("numfeatg is ")
		print(num_featg)

		feat_col<-0
		feat_list<-array("",dim(1))

		feat_ind<-which(x_gbest==1)
		feat_list<-feat_names[feat_ind]
	       
		feat_col<-feat_ind
		print("number of features selected")
		print(length(feat_list))
		print("best accuracy")
		print((-1)*fitness_gbest)
		sumr<-paste("global num features ", length(feat_list)-1, "global best set of features ", x_gbest, "global best accuracy ", 1-fitness_gbest)

	filestr2<-paste(outloc,  "selected_feature_index.csv", sep="")
	write.table(feat_col, file=filestr2, sep=",", row.names=FALSE)

		

	return(x_gbest)
}

	
	###################################################################
	
	#5)	
	###################################################################
	#Function to evaluate results using 10-fold and/or test -dataset
	#1) permutation tests; bootstrap; k-fold, AUC
	#2) options for classifiers: svm, random forest, naive bayes
	#3) 
	eval.results<-function(outloc)
	{

				traindata<-trainm
		testdata<-testm
		rm(trainm)
		rm(testm)
	 
		#print(feat_col)
		#print(traindata[1:2,])
		finalset<-traindata[,c(feat_col)]
			

		test_mod<-testdata[,c(feat_col)]


	filestr2<-paste(outloc, "modified_train.csv", sep="")

	Class=trainclass

	modtrain<-cbind(finalset, Class)
	
	Class=testclass
	modtest<-cbind(test_mod, Class)

	write.table(modtrain, file=filestr2, sep=",", row.names=FALSE)

	filestr3<-paste(outloc, "modified_test.csv", sep="")
	write.table(modtest, file=filestr3, sep=",", row.names=FALSE)


	modtrain<-data.frame(modtrain)
	modtest<-data.frame(modtest)
	#model_train_err<-svm(finalset, trainclass,  kernel=kname, type="C", cross=10)

	#model_train_valid<-svm(finalset, trainclass,   kernel=kname, type="C")

	model_train_err<-svm(modtrain$Class~., data=modtrain, type="C", cross=10)

	print("Modified train 10 fold accuracy using train data is ")
        print(model_train_err$tot.accuracy)

          filestr3<-paste(outloc, "10foldaccuracy.csv", sep="")
        write.table(model_train_err$tot.accuracy, file=filestr3, sep=",", row.names=FALSE)



#	model_train_valid<-svm(modtrain$Class~., data=modtrain, type="C")

	model_train_valid<-svm(finalset, trainclass,   kernel=kname, type="C")	
		pred_train<-predict(model_train_valid, finalset)
		train.table<-table(pred_train, trainclass)
		
	



	error<-1-sum(diag(train.table))/(dim(finalset)[1])
	print("Modified train accuracy is ")
	print(1-error)

	print("train confusion matrix is ")
	print(train.table)
	print("Train dimension is ")
	print(dim(finalset))
	
	#print(modtrain[1:4,])
	#print(modtest[1:4,])
	
	 mod_dim=dim(modtest)[2]
	 
	 test_mod<-as.data.frame(test_mod)

	#	pred_test<-predict(model_train_valid, test_mod)
		
	#	test.table<-table(pred_test, testclass)

	#knn.table<-table(test_mod[,1],knn(finalset[,-c(1)],test_mod[,-c(1)],finalset[,1],k=best_k[1],l=0,prob=FALSE, use.all=TRUE))
	  
	#print("test predicted table")
	#print(test.table)

	#testacc<-sum(diag(test.table))/(dim(test_mod)[1])
	#print("Test accuracy is ")
	#print(testacc)

	#finalsetx<-finalset
	#finalsety<-trainclass

	#kname="radial"

	# filestr3<-paste(outloc, "testaccuracy.csv", sep="")
        #write.table(testacc, file=filestr3, sep=",", row.names=FALSE)

	#res<-svm_10fold_blind(10,finalsetx,finalsety,test_mod, testclass, kname, classA, classB)
	#folderror<-(res$avg)

	#print("Modified 10fold blind test accuracy is ")
	#print(1-folderror)

	print("Test dimension is ")
	print(dim(test_mod))

	rm(test_mod)
	rm(finalset)
	
		
		#run_pso(data_dim)		

trainf=paste(outloc, "modified_train.csv", sep="")
testf=paste(outloc, "modified_test.csv", sep="")
traind<-read.csv(trainf, header=TRUE)

testd<-read.csv(testf, header=TRUE)
traind<-as.data.frame(traind)
testd<-as.data.frame(testd)
mod<-svm(traind$Class~., data=traind,type="C")
testdim=dim(testd)[2]

traindim=dim(traind)[2]

pred<-predict(mod,testd[,-c(testdim)])
test.table=table(pred,testd$Class)
print("Test confusion matrix is ")
print(test.table)
testacc<-(sum(diag(test.table))/(dim(testd)[1]))
print("Test acc is ")
print(testacc)

filestr3<-paste(outloc, "testaccuracy.csv", sep="")
write.table(testacc, file=filestr3, sep=",", row.names=FALSE)


#testy<-testy[order(testy)]
#pred<-pred[order(testy)]


if(dim(traind)[2]>15)
{

#trainx=as.matrix(traind[,-c(dim(traind)[2])])
#trainy=traind[,dim(traind)[2]]

#testx=as.matrix(testd[,-c(Class)])
#testy=testd[,dim(testd)[2]]

trainx=subset(traind, select=-c(Class))
trainy=traind$Class

testx=subset(testd, select=-c(Class))
testy=testd$Class

mod<-svm(traind$Class~., data=traind,type="C", cross=10)
#fitfunc<-testacc #-(0.04*(1-testdim-1))

print("train 10 fold")
print(mod$tot.accuracy)

filestr3<-paste(outloc, "psotenfoldaccuracy.csv", sep="")
        write.table(mod$tot.accuracy, file=filestr3, sep=",", row.names=FALSE)

trainf=paste(outloc, "modified_train.csv", sep="")
testf=paste(outloc, "modified_test.csv", sep="")
traind<-read.csv(trainf, header=TRUE)
testd<-read.csv(testf, header=TRUE)
traind<-as.data.frame(traind)
testd<-as.data.frame(testd)

mod<-svm(traind$Class~., data=traind,type="C")
testdim=dim(testd)[2]

pred<-predict(mod,testd[,-c(testdim)])
test.table=table(pred,testd$Class)
print("Test confusion matrix is ")
print(test.table)

testacc<-(sum(diag(test.table))/(dim(testd)[1]))
print("Test acc is ")
print(testacc)
filestr3<-paste(outloc, "testaccuracy.csv", sep="")
        write.table(testacc, file=filestr3, sep=",", row.names=FALSE)


pred<-predict(mod,traind[,-c(traindim)])
train.table=table(pred,traind$Class)

trainacc<-(sum(diag(train.table))/(dim(traind)[1]))
print("Train acc is ")
print(trainacc)
filestr3<-paste(outloc, "trainaccuracy.csv", sep="")
write.table(trainacc, file=filestr3, sep=",", row.names=FALSE)

#)

print("# of features after CMA:")
print(dim(CMAres$modtraindata))
print("# of features after PSO:")
print("Complete")
}
#stopCluster(cl)
}	
###################################################################


#6)
###################################################################

library(class)
svm_cv<-function(v,x,y,kname="radial"){

num_samp=dim(x)[1]

num_datasets= floor(num_samp)
n1<-floor(num_samp/v)
n2<-num_samp-n1*v
n3<-v-n2

ind<-rep(c(n1,n1+1),c(n3,n2))
ind<-diffinv(ind)
min_err=1
best_k=1

group<-sample(1:num_samp,num_samp, replace=FALSE)


itr=0
#svm_error <- matrix(0,v)  # we set K=30 before, it can be changed to any number<100.
svm_error<-rep(0,v)
for ( i in 1:v)
{
g<-group[(ind[i]+1):ind[i+1]]
temptest<-x[g,]
temptrain <-x[-g,]
tempclass <-y[-g]
testclass<-y[g]

svm_cv <- svm(x=temptrain,y=tempclass, type="C",kernel=kname) 
predfit<-predict(svm_cv,temptest)
svm_table<-table(predfit,testclass)


svm_toterror<-(1-sum(diag(svm_table))/length(testclass))
svm_bererror<-apply(svm_table,2,function(x){x/sum(x)})
svm_bererror<-replace(svm_bererror,which(is.na(svm_bererror)==TRUE),1)
svm_bererror<-1-mean(diag(svm_bererror))
svm_error[i]<-100-(svm_bererror*100)

}
avg.error <-mean(svm_error)
sd.error<-sd(svm_error)

#limit<-avg.error-(sd.error*(avg.error) # 1 sd criterion
print(avg.error)
print(sd.error)

return(list(error=avg.error,sderror=sd.error))

#return(list(num=best_k,error=min_err, avg=avg.error))
}
###################################################################


