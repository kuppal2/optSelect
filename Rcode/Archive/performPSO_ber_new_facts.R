###################################################################

performPSO<-function(trainm, testm, trainclass, testclass, outloc, numitrs, reinititrs, numpart, c1, c2, kname, percentTest, featweight, accuracyweight, cmaacc)
{
	
	data_dim<-dim(trainm)[2]

	#data_dim<-data_dim-1

	traindata<-trainm[,1:data_dim]
	#trainclass<-trainm[,(data_dim+1)]

	test_dim<-dim(testm)[2]

	#test_dim<-test_dim-1

	testdata<-testm[,1:test_dim]
	#testclass<-testm[,(test_dim+1)]

	print(dim(trainm))
	print("PSO test dim")
	print(dim(testm))
	print("Train class")
	print(trainclass[1:2])
	print("Test class")
	print(testclass[1:5])
	all_names<-names(traindata)
	feat_names<-names(traindata)
	data_dim<-dim(traindata)
	 
	print("train dim is")
	print(data_dim)
	kcross<-10

	#function to evaluate k-fold CV
	eval_fit_kfold<-function(particle, k)
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
			#trainset<-trainm[,c(1,col_sel)]
			
			trainset<-traindata[,c(col_sel)]
			
			#model<-svm(trainset, trainclass, type="C", kernel=kname, cross=kcross)

			#model<-svm(trainset, trainclass, type="C", kernel=kname, degree=tune_degree, gamma=tune_gamma, cost=tune_cost, cross=kcross)
		 	
			#model<-svm(as.factor(trainclass)~., data=trainset, type="C", cross=10)

			model<-svm(as.factor(trainclass)~trainset, type="C", cross=10)


			 #tab1<-table(model$fitted, trainclass)

			 #foldacc=(tab1[1,1]/sum(tab1[,1]))+(tab1[2,2]/sum(tab1[,2]))
                	 #folderror=0.5*foldacc*100

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


	data_cols<-data_dim[2]
	class_factor<-factor(as.matrix(trainm[,1]))

	class_labels <-levels(class_factor)

	run_pso<-function(d_dim)
	{
	 itr<-numitrs
	 maxitr<-reinititrs
	 d<-d_dim[2]
	 
	# c1<-2.05

	 #c2<-2.05
	 
	 ll<-0

	 ul<-1
	 
	 num_part<-numpart
	count_feat<-array(0,dim=c(num_part))
	 

	#position
	 x<-array(0, dim=c(num_part,d_dim[2]))

	#velocity
	v<-array(0, dim=c(num_part,d_dim[2]))
	 
	#group<-sample(1:1709,50, replace=FALSE)

	 x_gbest<-array(0, dim=c(d_dim[2]))

	p_gbest<-array(0, dim=c(d_dim[2]))
	   x_lbest<-array(0, dim=c(num_part, d_dim[2]))
	   
	   r_arr1<-array(0, dim=c(d_dim[1]))
	    r_arr2<-array(0, dim=c(d_dim[1]))

	 #random initialization of the particle 2D array
	#where 0: feature not selected, 1 otherwise
	 for (row in 1:num_part)
	 {
		num_feat<-0
		r_arr1[row]<-runif(1,0,1)
		r_arr2[row]<-runif(1,0,1)
		
		while(num_feat<2)
		{	
		
		for (col in 1:d_dim[2])		
		{
			ran<-runif(1,ll,ul)
			if (ran<0.5)
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
		}
		 count_feat[row]<-num_feat
	 
	 }
	 
	 num_featl<-d 
	 num_featg<-d

	print(count_feat)

	wmax<-0.9
	wmin<-0.4

	itr_data<-""

	
	 #fitness_gbest<-10000000000000000
	fitness_gbest<-cmaacc
	prev_gbest<-fitness_gbest
	 fitness_lbest<-array(cmaacc, dim=c(num_part, 1))

	best_kfold<-50
		fitness_x<-array(100000000000000000, dim=c(num_part, 1)) 

		feat_list<-feat_names

		no_change<-0
		
		global_best_index<-10000000000000000000
		
		min_fit_x<-1000000000000000

		min_fit_index<-100000000000000000
		k<-0

		for (k in 1:itr)
		{
			feat_sel<-0
		
			itr_val={}	
			print("iteration number: ") 
			print(k)

			min_fit_x<-1000000000000000
			 
			min_fit_index<-100000000000000000 
			
			for (i in 1:num_part)
			{
				
				fitness_x[i]<-(-1)*eval_fit_kfold(x[i,], 10)
				
				if(fitness_x[i]<min_fit_x)
				{
					
					min_fit_x<-fitness_x[i]

					min_fit_index<-i
					num_featl<-count_feat[i]
					

				}
				
				 if(fitness_x[i]<fitness_lbest[i])
				 {
				
					fitness_lbest[i]<-fitness_x[i]
					
					for (j in 1:d_dim[2])
					{
					

						x_lbest[i,j] <- x[i,j]
					}
					
				}
				
				
			
			}
			

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
					
						 ran<-runif(1,0,1)

						for (col in 1:d_dim[2])

						{
							
							if (ran<0.9)
							{
								ran2<-runif(1,0,1)
								if(ran2<0.9){	
								x[row,col]<-0}
								else
								{x[row,col]<-1
								num_feat<-num_feat+1	
								}
								#x_lbest[row,col]<-0
							#	if(ran==1)
							#	{
							#		num_feat<-num_feat+1
							#	}
							}
							else

							{
								x[row,col]<-x_gbest[col]
								#x_lbest[row,col]<-1
								if(x_gbest[col]==1){
								num_feat<-num_feat+1}

								#col_sel[row,num_feat]<-col

							}

						}
						count_feat[row]<-num_feat

					}				


					no_change<-0
				}
			}
			rank_vec<-rank(fitness_x)

			 ###update the particle velocity and position

			for (i in 1:num_part)
			{
					feat_sel<-0
			      
				
				#w<-1/(3-exp(-num_part/200)+((rank_vec[i])/(8*d_dim[2]))^2)
				#w<-(exp(rank_vec[i]/num_part))-(exp(1/num_part))	
				#w<-((exp(rank_vec[i]/num_part))/1.7)-((exp(1/num_part))/1.705)
				w<-1				
				#w<-(wmax-wmin)*((itr-k)/itr)+wmin
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
		print(length(feat_ind))
		print("best accuracy")
		print((-1)*fitness_gbest)
		sumr<-paste("global num features ", length(feat_list)-1, "global best set of features ", x_gbest, "global best accuracy ", 1-fitness_gbest)

	filestr2<-paste(outloc,  "selected_feature_index.csv", sep="")
	write.table(feat_col, file=filestr2, sep=",", row.names=FALSE)

	 
		finalset<-traindata[,c(feat_col)]
			

		test_mod<-testdata[,c(feat_col)]


	filestr2<-paste(outloc, "modified_psotrain.csv", sep="")


	modtrain<-cbind(finalset, trainclass)
	modtest<-cbind(test_mod, testclass)

	write.table(modtrain, file=filestr2, sep=",", row.names=FALSE)

	filestr3<-paste(outloc, "modified_psotest.csv", sep="")
	write.table(modtest, file=filestr3, sep=",", row.names=FALSE)


	model_train_err<-svm(trainclass~finalset, kernel=kname, type="C", cross=10)

	#model_train_valid<-svm(finalset, trainclass,   kernel=kname, type="C")
	model_train_valid<-svm(trainclass~finalset,kernel=kname, type="C")


		
		pred_train<-predict(model_train_valid, finalset)
		
		train.table<-table(pred_train, trainclass)
		
	print("Modified train 10 fold accuracy using train data is ")
	print(model_train_err$tot.accuracy)
	
	  filestr3<-paste(outloc, "pso10foldaccuracy.csv", sep="")
        write.table(model_train_err$tot.accuracy, file=filestr3, sep=",", row.names=FALSE)


	error<-1-sum(diag(train.table))/(dim(finalset)[1])
	print("Modified train accuracy is ")
	print(1-error)

	print("Train dimension is ")
	print(dim(finalset))
	 

		pred_test<-predict(model_train_valid, test_mod)
		
		test.table<-table(pred_test, testclass)

	#knn.table<-table(test_mod[,1],knn(finalset[,-c(1)],test_mod[,-c(1)],finalset[,1],k=best_k[1],l=0,prob=FALSE, use.all=TRUE))
	  
	print("test predicted table")
	print(test.table)

	testacc<-sum(diag(test.table))/(dim(test_mod)[1])
	print("Test accuracy is ")
	print(testacc)

	finalsetx<-finalset
	finalsety<-trainclass

	kname="radial"

	 filestr3<-paste(outloc, "psotestaccuracy.csv", sep="")
        write.table(testacc, file=filestr3, sep=",", row.names=FALSE)

	#res<-svm_10fold_blind(10,finalsetx,finalsety,test_mod, testclass, kname, classA, classB)
	#folderror<-(res$avg)

	#print("Modified 10fold blind test accuracy is ")
	#print(1-folderror)

	print("Test dimension is ")
	print(dim(test_mod))

	rm(test_mod)
	rm(finalset)
	}
#if(data_dim[2]>1)
#{
	run_pso(data_dim)
#}
}
