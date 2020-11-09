.libPaths("~/karan_libs/Rlibs")
library(snow)
library(e1071)

cl<-makeCluster(2)

traind<-read.csv("/home/stu/kuppal3/Research/Heart_data/Output/CMA_PSO_results/itr13msimt/modified_train.csv")
trainm<-subset(traind, select=-c(Class))
trainclass<-traind$Class

testd<-read.csv("/home/stu/kuppal3/Research/Heart_data/Output/CMA_PSO_results/itr13msimt/modified_test.csv")
testm<-subset(testd, select=-c(Class))
testclass<-testd$Class

data_dim<-dim(trainm)
d_dim<-data_dim
feat_names<-colnames(trainm)
#d<-d_dim[2]
num_part<-10
#x<-1
outloc<-"./"
#res1=clusterCall(cl, function(y) x + y, 2)
#sapply(res1,head)

update_fitness<-function(fitvec)
{
if(fitness_x[i]<fitness_lbest[i])
				 {
				
					fitness_lbest[i]<-fitness_x[i]
					
					#for (j in 1:d_dim[2])
					{
					

						x_lbest[i,j] <- x[i,j]
					}
					
				}
}

#function to evaluate k-fold CV
	eval_fit_kfold<-function(particle, k, accuracyweight, featweight)
	{
		
		num_feat<-0
		#select the columns with selected features 
		ind<-which(particle==1)

		#need to add 1 to take into account the index of the feature in the original dataset
		col_sel<-ind
		num_feat<-length(col_sel)
		#print("itr num feat:")
		#print(length(ind))	
		if(num_feat>1)
		{
			trainset<-trainm[,c(col_sel)]
			trainset<-cbind(trainclass,trainset)
			
			trainset<-data.frame(trainset)
			
			#print(trainset[1:4,])
			#trainset<-traindata[,c(col_sel)]
			
			#model<-svm(trainset, trainclass, type="C", kernel=kname, cross=kcross)

			#model<-svm(trainset, trainclass, type="C", kernel=kname, degree=tune_degree, gamma=tune_gamma, cost=tune_cost, cross=kcross)
			
			 model<-svm(trainset$trainclass~., data=trainset, type="C", cross=10)
			 #svm.table<-table(model$fitted, trainclass)
			#fitval<-predict(model, trainset[,-c(1)])
                        #svm.table<-table(fitval, trainclass)
                        #trainacc<-sum(diag(svm.table))/(sum(svm.table))
                        #beracc<-(svm.table[1,1]/(sum(svm.table[,1])))+(svm.table[2,2]/(sum(svm.table[,2])))
                        #folderror<-0.5*(model$tot.accuracy+(0.5*(beracc)*100))

			folderror<-model$tot.accuracy
			#folderror<-(1-ber)*100
			rm(trainset)

		}
		else
		{
		folderror<-1
		}

		fitfunc<-(accuracyweight*(folderror))+(featweight*(1-num_feat))
		rm(col_sel)
		rm(num_feat)
		return(fitfunc)

	}
	
	eval_fit_kfold_diff<-function(particle)
	{
		
		num_feat<-0
		#select the columns with selected features 
		ind<-which(particle==1)

		#need to add 1 to take into account the index of the feature in the original dataset
		col_sel<-ind
		num_feat<-length(col_sel)
		#print("itr num feat:")
		#print(length(ind))	
		if(num_feat>1)
		{
			trainset<-trainm[,c(col_sel)]
			trainset<-cbind(trainclass,trainset)
			
			trainset<-data.frame(trainset)
			
			#print(trainset[1:4,])
			#trainset<-traindata[,c(col_sel)]
			
			#model<-svm(trainset, trainclass, type="C", kernel=kname, cross=kcross)

			#model<-svm(trainset, trainclass, type="C", kernel=kname, degree=tune_degree, gamma=tune_gamma, cost=tune_cost, cross=kcross)
			
			 model<-svm(trainset$trainclass~., data=trainset, type="C", cross=10)
			 #svm.table<-table(model$fitted, trainclass)
			#fitval<-predict(model, trainset[,-c(1)])
                        #svm.table<-table(fitval, trainclass)
                        #trainacc<-sum(diag(svm.table))/(sum(svm.table))
                        #beracc<-(svm.table[1,1]/(sum(svm.table[,1])))+(svm.table[2,2]/(sum(svm.table[,2])))
                        #folderror<-0.5*(model$tot.accuracy+(0.5*(beracc)*100))

			folderror<-model$tot.accuracy
			#folderror<-(1-ber)*100
			rm(trainset)

		}
		else
		{
		folderror<-1
		}

		fitfunc<-(accuracyweight*(folderror))+(featweight*(1-num_feat))
		rm(col_sel)
		rm(num_feat)
		return(fitfunc)

	}
	
	#run_pso<-function(d_dim)
	{
	 #itr<-numitrs
	 maxitr<-50 #reinititrs
	 d<-data_dim[2]
	 
	c1<-2.05

	 c2<-2.05
	 
	 ll<-0

	 ul<-1
	fitness_x<-array(100000000000000000, dim=c(num_part, 1)) 
	 fitness_gbest<-10000000000000000
	prev_gbest<-fitness_gbest+1 
	 fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))
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
	 itr<-5
	 accuracyweight=1
	featweight=0.01
	k<-10
	clusterExport(cl, "x")
	clusterExport(cl, "eval_fit_kfold")
	clusterExport(cl, "eval_fit_kfold_diff")
	clusterExport(cl, "trainm")
	clusterExport(cl, "trainclass")
	clusterExport(cl, "accuracyweight")
	clusterExport(cl, "featweight")
	clusterExport(cl, "k")
	
	clusterEvalQ<-function(cl, expr)
	clusterCall(cl, eval, substitute(expr), env=.GlobalEnv)

	#load a library on all clusters
	clusterEvalQ(cl, library(e1071))

 	
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
				
			}
				#res1=clusterCall(cl, (-1)*eval_fit_kfold(x[i,], 10,1,0.01))
				
				#res1=clusterCall(cl, function(y) eval_fit_kfold(x[y,],10), 1)
				#res1=parLapply(cl,list(a=as.data.frame(x[1,]),b=as.data.frame(x[5,1:5])),eval_fit_kfold_diff)
				
				#correct method
				#res1<-clusterApply(cl,list(a=as.data.frame(x[1,]),b=as.data.frame(x[5,1:5])),eval_fit_kfold_diff))
				
				res1<-clusterApply(cl,part_list,eval_fit_kfold_diff)
				fitness_x<-sapply(res1,head)
				fitness_x<-(-1)*fitness_x
				
				min_fit_x<-min(fitness_x)
				min_fit_index<-which(fitness_x==min_fit_x)
				bestind<-runif(1,1,length(which(fitness_x==min_fit_x)))
				numfeatl<-length(which(x[bestind,]==1))
				
				for (i in 1:num_part)
				{
					if(fitness_x[i]<fitness_lbest[i])
					{
					
						fitness_lbest[i]<-fitness_x[i]
						
						x_lbest[i,] <- x[i,]
						
						
					}
					
				}
				
			
			 rank_vec<-rank(fitness_x)
			 
			 #update the global best and its fitness
			if(min_fit_x < fitness_gbest)
			{
				no_change<-0

				
						fitness_gbest<-min_fit_x
					
						global_best_index<-min_fit_index
						num_featg<-num_featl
						print ("global fitness updated to :")
						print(fitness_gbest)
						print ("golbal best num features updated to")

						print (num_featg)
						
						global_best_index<-round(runif(1,1,length(global_best_index)))
						
						for (j in 1:d_dim[2])
						{
							
							x_gbest[j]<-x[global_best_index,j]
							
						}

			}
			else
			{
				no_change<-no_change+1

				if(no_change>maxitr)
				{
					print("RE-INITIALIZING...")
					
					# fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))
					min_fit_x<-1000000000000000000000
					 for (row in 1:num_part)
					{
						num_feat<-0
					
						 #ran<-runif(1,0,1)

						for (col in 1:d_dim[2])

						{
							
							#if (ran<0.9)
							{
								ran2<-runif(1,0,1)
								
								if(ran2<0.5)
								{	
									x[row,col]<-0
								}
								else
								{
									x[row,col]<-1
									num_feat<-num_feat+1	
								}
								#x_lbest[row,col]<-0
							#	if(ran==1)
							#	{
							#		num_feat<-num_feat+1
							#	}
							}
							#else

							#{
							#	x[row,col]<-x_gbest[col]
								#x_lbest[row,col]<-1
							#	if(x_gbest[col]==1){
							#	num_feat<-num_feat+1}

								#col_sel[row,num_feat]<-col

							#}

						}
						count_feat[row]<-num_feat

					}				


					no_change<-0
				}
			}
			
			
 print("x_lbest")
                                print(x_lbest[i,])
                                print(x[i,])
				print(x_gbest)

			 ###update the particle velocity and position

			for (i in 1:num_part)
			{
					feat_sel<-0
			      
				
				#w<-1/(3-exp(-num_part/200)+((rank_vec[i])/(8*d_dim[2]))^2)
				#w<-(exp(rank_vec[i]/num_part))-(exp(1/num_part))	
				#w<-((exp(rank_vec[i]/num_part))/1.7)-((exp(1/num_part))/1.705)
				w<-1				
				#w<-(wmax-wmin)*((itr-k)/itr)+wmin
				#print("x_lbest")
				#print(x_lbest[i,])
				#print(x[i,])		
	
			       for (j in 1:d_dim[2])
				{
					r1<-runif(1,0,1)

					r2<-runif(1,0,1)

					r3<-runif(1,0,1)
							
			
			
					v[i,j]<-((w*v[i,j])+(c1*r1*(x_lbest[i,j]-x[i,j]))+(c2*r2*(x_gbest[j]-x[i,j])))
					
					if(v[i,j]>6)
					{
						v[i,j]=6
					}	
					if(v[i,j]< -6)
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

		traindata<-trainm
		testdata<-testm
		rm(trainm)
		rm(testm)
	 
		print(feat_col)
		print(traindata[1:2,])
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

	model_train_valid<-svm(modtrain$Class~., data=modtrain, type="C")
	
		pred_train<-predict(model_train_valid, finalset)
		
		train.table<-table(pred_train, trainclass)
		
	print("Modified train 10 fold accuracy using train data is ")
	print(model_train_err$tot.accuracy)
	
	  filestr3<-paste(outloc, "10foldaccuracy.csv", sep="")
        write.table(model_train_err$tot.accuracy, file=filestr3, sep=",", row.names=FALSE)



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
	}
		
		#run_pso(data_dim)		
