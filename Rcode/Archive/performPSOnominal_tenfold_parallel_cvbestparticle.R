.libPaths("~/karan_libs/Rlibs")
library(snow)
library(e1071)

cl<-makeCluster(2)

traind<-read.csv("/home/stu/kuppal3/Research/Heart_data/Output/CMA_PSO_results/itr13msimt/modified_train.csv")
trainm<-subset(traind, select=-c(Class))
trainclass<-traind$Class
d_dim<-dim(trainm)
d<-d_dim[2]
num_part<-10
#x<-1

#res1=clusterCall(cl, function(y) x + y, 2)
#sapply(res1,head)
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
	 itr<-1
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
				part_list=c(part_list, c=list(sapply(x[i,],head)))
				
			}
				#res1=clusterCall(cl, (-1)*eval_fit_kfold(x[i,], 10,1,0.01))
				
				#res1=clusterCall(cl, function(y) eval_fit_kfold(x[y,],10), 1)
				#res1=parLapply(cl,list(a=as.data.frame(x[1,]),b=as.data.frame(x[5,1:5])),eval_fit_kfold_diff)
				
				#correct method
				#res1<-clusterApply(cl,list(a=as.data.frame(x[1,]),b=as.data.frame(x[5,1:5])),eval_fit_kfold_diff))
				
				res1<-clusterApply(cl,part_list,eval_fit_kfold_diff)
				res1<-sapply(res1,head)
			
		}
				
