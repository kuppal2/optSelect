#library(snow)
#library(e1071)
#source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/svm_cv_func.R")

#cl<-makeCluster(8)
#cl<-makeSOCKcluster(c(rep("godel2",2),rep("godel3",3),rep("lebniz2",3),rep("leibniz3",2)))
#cl<-makeSOCKcluster("godel2",2)

.libPaths("/home/stu/kuppal3/karan_libs/Rlibs")
library(snow)
library(e1071)
library(yaImpute)
cl<-makeCluster(20)
#cl<-makeSOCKcluster(c(rep("godel2",20),rep("godel3",20))) #,rep("lebniz2",10),rep("leibniz3",10)))



#options(echo=FALSE)
library(e1071, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
library(CMA, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
library(bioDist, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
#source("/home/stu/kuppal3/Research/Genomic_imprinting/Huira/Source/PSO/svm_cv_func_10foldblindtest.R")

#source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/svm_cv_func.R")

source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/svm_cv_v1.0.R")

#options(echo=FALSE)
#library(e1071, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
#library(CMA) #, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
#library(bioDist) #, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
#source("/home/stu/kuppal3/Research/Genomic_imprinting/Huira/Source/PSO/svm_cv_func_10foldblindtest.R")

#source("/home/stu/kuppal3/Research/Feature_selection/performPSOnominalheart_regression.R")
#source("/home/stu/kuppal3/Research/Feature_selection/performCMA_consensusheart_regression.R")

#source("/home/stu/kuppal3/Research/Heart_data/regression/performPSOnominalheart_regression.R")
#source("performPSOnominallung_regression_factors_oct242010.R")
#source("/home/stu/kuppal3/Research/Heart_data/regression/performCMA_consensusheart_regression.R")

#source("/home/stu/kuppal3/Research/Feature_selection/regression/perform_PSO_oct242010.R")
#source("/home/stu/kuppal3/Research/Feature_selection/regression/perform_PSO_nov192010.R")
#source("/home/stu/kuppal3/Research/Feature_selection/regression/perform_PSO_nov192010.R")

#source("/home/stu/kuppal3/Research/Feature_selection/regression/perform_PSO_nov192010.R")
#source("/home/stu/kuppal3/Research/Feature_selection/performPSOnominal_tenfold_regression.R")
#source("/home/stu/kuppal3/Research/Feature_selection/preprocessdata_regression.R")


#source("/home/stu/kuppal3/Research/Feature_selection/performCMA_consensus_tenfold_ber_nominal_lungheart_feb242011.R")
#source("/home/stu/kuppal3/Research/Feature_selection/performCMA_consensus_tenfold_ber_nominal_lungheart_train_test_feb282011.R")

#source("/home/stu/kuppal3/Research/Feature_selection/

source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/performCMA_consensus_tenfold_ber_nominal_lungheart_train_test_sep52013_backward.R")

#source("performCMA_consensus_tenfold_ber_nominal_lungheart_train_test_sep192012_backward.R")
#datafile<-read.csv("/home/stu/kuppal3/Research/Heart_data/data_nominal.csv", na.strings="", sep=",", header=TRUE)

#datafile<-read.csv("/home/stu/kuppal3/Research/Lung_cancer_sequences/Data_may172009/orderednominal/luca_no_missing_1056feats_nominal_ordered_events.csv", na.strings="", sep=",", header=TRUE)
#datafile<-read.csv("/home/stu/kuppal3/Research/Lung_cancer_sequences/Data_may172009/count_lung_cancer_len1to8_patterns_normbylengthmult1000_withevents.csv", na.strings="", sep=",", header=TRUE)

#outloc<-"/home/stu/kuppal3/Research/Lung_cancer_sequences/Data_may172009/regression/PSO_reg_results/itr1/"

#datafile=read.csv("/home/stu/kuppal3/Research/Lung_cancer_sequences/Data_may172009/LuCa_RLGS_Features_modified_051409_update_withevents.csv", sep=",", header=TRUE)

#outloc<-"/home/stu/kuppal3/Research/Heart_data/Output/CMA_PSO_results/itr4_ms_parallel_IMT/"

#traind<-read.csv("/home/stu/kuppal3/Research/Feature_selection/Datasets/ARCENE/arcene_train.data")
#testd<-read.csv("/home/stu/kuppal3/Research/Feature_selection/Datasets/ARCENE/arcene_valid.data")
if(FALSE){
trainy<-read.csv("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_train.labels", header=FALSE)
testy<-read.csv("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_valid.labels", header=FALSE)

traind<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_train.data", sep=" ",header=FALSE)
testd<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_valid.data", sep=" ",header=FALSE)
}

#/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtest
trainy<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.ytrain", header=FALSE)
testy<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.ytest", header=FALSE)

trainy<-t(trainy)
testy<-t(testy)

#traind<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtrain", sep=" ",header=FALSE)
#testd<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtest", sep=" ",header=FALSE)


traind<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtrain")

testd<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtest")

traind<-t(traind) #[,-c(10001)]
testd<-t(testd) #[,-c(10001)]
traind<-cbind(trainy,traind)
testd<-cbind(testy,testd)

traind<-na.omit(traind)
testd<-na.omit(testd)

#outloc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/minpres2union_pso1000f0.25c0.25_backwardfalse_cvfoldthres0.7itr4w1/"
outloc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/itr1pso1000f0.6c0.1_behav10itrbackwardtrue_cvfoldthres0.7_w1test0601/"
#traind<-read.csv("/home/stu/kuppal3/Research/SpectF_data/SPECTF.train", header=FALSE)
#outloc<-"/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/itr6_allmethods_minmethods1_ttestfilt_backwardsel_pso10/"
#traind<-read.csv("/home/stu/kuppal3/Research/SpectF_data/SPECTF.train", header=FALSE)
#testd<-read.csv("/home/stu/kuppal3/Research/SpectF_data/SPECTF.test", header=FALSE)
#outloc<-"/home/stu/kuppal3/Research/SpectF_data/Results/itr2_ms_parallel/"
dir.create(outloc)
setwd(outloc)
temp2=t(traind)
temp2=apply(temp2, 2, function(x){which(x=="MD")})
temp2=unlist(temp2)
temp2=unique(temp2)
if(length(temp2)>1)
{
	traind=traind[,-c(temp2)]

	rm(temp2)
}

minfeats=2
maxgenes=2000
mingenes=1
stepitr=1
#methods=c("rf","rfe","limma","lasso","elasticnet","kruskal.test","f.test") #, "f.test", "elasticnet", "wilcox.test", "welch.test")
#methods=c("t.test")
#methods=methods[2]
methods=c("rf","rfe","elasticnet","lasso")
methods=methods[3]
#methods=c("elasticnet")
percentTest=0.40
kname="radial"
maxitrs=100
removeindex=c(0)
norm_method="znorm"
evalmethod="10 fold"
classindex=1
maxfacts=0
percentTest=0.40
kname="radial"
upclasssep=1
dclasssep=-1
tolerance=0.1
accWeight=1
featWeight=0.06
minpresent=round(1*length(methods))
maxfeats=1000 #dim(datafile)[2]-length(removeindex)-1
maxfeatspercent=0.1
evalmethod="CV"
numfolds=5
CVfoldthresh=0.7
backward.sel=TRUE
scheme_val="one-vs-all"
iter_learn=5
######PSO parameters#########
c1=2.05
c2=2.05
itr=1000
maxitr=(0.02)*itr
num_part=30
kname="radial"
accuracyweight=1
featweight=0.08
k<-numfolds
followerprob=0.6
confusionprob=0.1
wmax<-1
wmin<-0.4
behavior_reset_itr<-(0.1)*itr
maxitrreset<-0.3*itr
num_neighbors<-5
##################

#id<-sample(1:data_dim[1],size=percentTest*data_dim[1],replace=F)

#testm<-datafile[id,]

#ncid<-datafile[-id,]





print("Stage 1: Consensus based")
print("Stage 1: Consensus based")
#CMAres<-preProcess(datafile, outloc, percentTest,norm_method,classindex, upclasssep, dclasssep, removeindex, maxfacts)
#CMAres<-performCMA(traind, testd, outloc, maxfeatspercent, minfeats, stepitr, methods, percentTest, featWeight,accWeight, kname, maxitrs, minpresent, norm_method, tolerance, classindex, upclasssep, dclasssep, removeindex, maxfacts,numfolds=k,evalmethod,CVfoldthresh,backward.sel,scheme_val,iter_learn)


#if(FALSE)
print("Stage 2: Selecting optimal subset using PSO")
#system.time(

#psores<-performPSO(trainx, testx, trainy, testy, outloc, numitrs, reinititrs, numpart, c1, c2, kname, percentTest, 0.04, fitfunc)

#psores<-performPSO(CMAres$modtraindata, CMAres$modtestdata, CMAres$modtrainclass, CMAres$modtestclass,outloc, numitrs, reinititrs, numpart, c1, c2, kname, percentTest, 0.0, 1, fitfunc)

if(FALSE)
{
trainm<-CMAres$modtraindata
testm<-CMAres$modtestdata
trainclass<-CMAres$modtrainclass
testclass<-CMAres$modtestclass
}

#if(FALSE)
{
trainm<-traind[,-c(1)] #CMAres$modtraindata
testm<-testd[,-c(1)] #CMAres$modtestdata
trainclass<-traind[,1] #CMAres$modtrainclass
testclass<-testd[,1] #CMAres$modtestclass
}

d_dim<-dim(trainm)

print("Original dimension")
print(d_dim)

if(d_dim[2]>5){
#Function to find top 100 similar samples for every platform
find_similar_samples<-function(part_group, particle_ind=NA,num_neighbors=3){

	bcdata<-as.matrix(part_group)

	if(is.na(particle_ind)==FALSE){
	targetdata<-as.matrix(part_group[particle_ind,])
	}else{

	targetdata<-as.matrix(part_group)
	}

	data_dim<-dim(bcdata)

	#num_neighb<-100
	affy_names<-colnames(bcdata)

	bc.names<-rownames(bcdata)

	i<-1
	
	dist.matrix<-{}
	
	#k=100, for top 100 matches
	dist.matrix<-ann(ref=as.matrix(bcdata), target=as.matrix(targetdata), tree.type="brute", k=num_neighbors, split.rule="midpt", shrink.rule="centroid", verbose=TRUE)

	rm(bcdata)
	
	if(FALSE){
	dist.print<-t(dist.matrix$knnIndexDist)

	sample_names<-bc.names[dist.print[1:100,]]

	sample_list<-{}
	exp_list<-{}
	tissue_list<-{}
	pval_list<-{}
	match_bclist<-{}


	for (i in 1:100)
	{
		#format sample names##

		
		sample<-sapply(strsplit(as.character(sample_names[i]), "\\."), head, n=2)[1]

		sample<-toupper(sample)

		sample_list<-rbind(sample_list, sample)

		
	}

	tab<-data.frame(Number=(1:100), Sample=as.factor(sample_list), Distance=dist.print[101:200,], row.names=T)
	
	return(tab)
	}
	return(dist.matrix$knnIndexDist)
}


#function to find nearest neighbor of a particle
nn_search<-function(particle_ind, part_group)
{
	partm<-as.matrix(part_group[particle_ind,])
	
	#nn_arr<-array(0,2)
	dist_mat<-dist(as.matrix(part_group))
	dist_mat<-as.matrix(dist_mat)
	nn_order<-order(dist_mat[particle_ind,])
	nn_order<-nn_order[-c(1)]
	if(FALSE){
	for(i in 1:length(part_group))
	{
		groupm<-as.matrix(part_group[i])
		
		pdist<-dist(rbind(partm, groupm))
		
		nn_arr<-rbind(nn_arr, as.array(cbind(i,pdist)))
	}
	
	#ascending order with respect to distance
	nn_order<-order(nn_arr[,2])
	}

	return(nn_order)
}
	#function to evaluate k-fold CV
	eval_fit_kfold_diff<-function(particle, numfolds)
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
			folderror<-{}
			 for(f in 1:3)
                        {
			 #model<-svm(trainset$trainclass~., data=trainset, type="C", cross=numfolds, kernel=kname)
			 model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype="BER")
			 #svm.table<-table(model$fitted, trainclass)
			#fitval<-predict(model, trainset[,-c(1)])
                        #svm.table<-table(fitval, trainclass)
                        #trainacc<-sum(diag(svm.table))/(sum(svm.table))
                        #beracc<-(svm.table[1,1]/(sum(svm.table[,1])))+(svm.table[2,2]/(sum(svm.table[,2])))
                        #folderror_cur<-1*(model$tot.accuracy) #+(0*(beracc)*100)
		
			folderror_cur<-model$mean_acc
			print(model)
			#folderror_cur<-model$confint[1]
			#folderror_cur<-svm_cv(x=trainset[,-c(1)],y=trainset[,1],v=10,kname="radial")
			print(folderror_cur)	
			#folderror_cur<-folderror_cur$tot.accuracy #-(1.96*folderror_cur$sderror)
			folderror<-c(folderror,folderror_cur) #model$tot.accuracy)
			
                        }
                        folderror<-mean(folderror) #-(1.96*sd(folderror))

					
	
			#folderror<-model$tot.accuracy
			#folderror<-(1-ber)*100
			rm(trainset)

		}
		else
		{
		folderror<-1
		}
		fitfunc<-(accuracyweight*(folderror))-(featweight*1000*(num_feat/length(particle)))  #+(featweight*(1-num_feat))
		print(paste("accuracy: ", folderror," num_feat:",num_feat," fitness:", fitfunc,sep=""))
		rm(col_sel)
		rm(num_feat)
		return(fitfunc)

	}
	
	#run_pso<-function(d_dim)
	{
	 #itr<-numitrs
	 #maxitr<-50 #reinititrs
	 d<-dim(trainm)[2]
	 
	c1<-2.05

	 c2<-2.05
	 
	 ll<-0

	 ul<-1
	fitness_x<-array(100000000000000000, dim=c(num_part, 1)) 
	 fitness_gbest<-100000000000000 #(-1)*CMAres$tenfoldacc #10000000000000000
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

	clusterExport(cl, "x")

	clusterExport(cl, "eval_fit_kfold_diff")
	clusterExport(cl, "trainm")
	clusterExport(cl, "trainclass")
	clusterExport(cl, "accuracyweight")
	clusterExport(cl, "featweight")
	clusterExport(cl, "k")
        clusterExport(cl, "kname")	
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
				
				if(k%%behavior_reset_itr==0){
					agent_behavior=runif(num_part,0,1)
				}	
			}
				#res1=clusterCall(cl, (-1)*eval_fit_kfold(x[i,], 10,1,0.01))
				
				#res1=clusterCall(cl, function(y) eval_fit_kfold(x[y,],10), 1)
				#res1=parLapply(cl,list(a=as.data.frame(x[1,]),b=as.data.frame(x[5,1:5])),eval_fit_kfold_diff)
				
				#correct method
				#res1<-clusterApply(cl,list(a=as.data.frame(x[1,]),b=as.data.frame(x[5,1:5])),eval_fit_kfold_diff))
				
				res1<-clusterApply(cl,part_list,eval_fit_kfold_diff,numfolds=numfolds)
				fitness_x<-sapply(res1,head)
				fitness_x<-(-1)*fitness_x
			
				print("fitness is: ")
				print(fitness_x)	
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

				if(no_change>maxitr)
				{
				
					global_no_change<-global_no_change+1
					print("RE-INITIALIZING...")
				 	#stop(paste("No change for ",maxitr," iterations",sep=""))	
					 fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))
					#print(paste("No change for ",maxitr," iterations. Exiting PSO.",sep=""))
					if(global_no_change>maxitrreset){
				
					#	print(dim(x_lbest))	
					#	x_lbest<-apply(x_lbest,2,median)
						print(paste("No change for ",global_no_change," iterations. Exiting PSO.",sep=""))
					break;
					}
				#	print("exited")
					min_fit_x<-1000000000000000000000
					 for (row in 1:num_part)
					{
						num_feat<-0
					
						 ran<-runif(1,0,1)

						for (col in 1:d_dim[2])

						{
							
							if (ran<0.5)
							{
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
							#	if(ran==1)
							#	{
							#		num_feat<-num_feat+1
							#	}
							}else{
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
			
			
				#print("x_lbest")
                                #print(x_lbest[i,])
                                #print(x[i,])
				#print(x_gbest)

			 ###update the particle velocity and position

				nn_search_res<-find_similar_samples(x,NA,num_neighbors)
				print(dim(nn_search_res))
				print(head(nn_search_res))
			for (i in 1:num_part)
			{
					feat_sel<-0
			      
				
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
                                                        
                        if (ran[col]<0.9)         
                        {
                                x_curbest[col]<-0

                        }else{

                                x_curbest[col]<-1

                        }

                }
                

                                                #x_gbest[j]<- #generaterandom
                                        }else{
                                                if(agent_behavior[i]<(confusionprob+followerprob)){
                                                        x_curbest_ind<- nn_search_res[i,c(1:num_neighbors)]  #nn_search(i,x) #getnearestneighbor
                                			print("nearest neighbors are ")
							print(x_curbest_ind)
				                	if(num_neighbors>1){
							x_curbest<-apply(x[x_curbest_ind,],2,median)
							}else{
							x_curbest<-x[x_curbest_ind[1],]
							}	
							
						}else{
							x_curbest<-x_gbest
						}

                                        }
				
				r3<-runif(1,0,1)
	
			       for (j in 1:d_dim[2])
				{
					r1<-runif(1,0,1)

					r2<-runif(1,0,1)

					#r3<-runif(1,0,1)
							
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
			#itr_data<-rbind(itr_data, itr_val)
			
		}
		filestr<-paste(outloc,  "multiobj_itr_descpso.txt", sep="")

	write.table(itr_data, file=filestr, sep="\t", row.names=F)

		print("numfeatg is ")
		print(num_featg)

		feat_col<-0
		#feat_list<-array("",dim(1))

		#feat_ind<-which(x_gbest==1)

		print(dim(x_lbest))
                x_lbest<-apply(x_lbest,2,median)
		
		feat_ind<-which(x_lbest==1)

		feat_list<-feat_names[feat_ind]
	       
		feat_col<-feat_ind
		print("number of features selected")
		print(length(feat_ind))
		print("best accuracy")
		print((-1)*fitness_gbest)
		sumr<-paste("global num features ", length(feat_list)-1, "global best set of features ", x_gbest, "global best accuracy ", 1-fitness_gbest)

	filestr2<-paste(outloc,  "selected_feature_index.csv", sep="")
	write.table(feat_col, file=filestr2, sep=",", row.names=FALSE)

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
	}
		
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


if(dim(traind)[2]>3)
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
}
#)
#print("# of features after CMA:")
#print(dim(CMAres$modtraindata))
print("# of features after PSO:")
print(dim(traind))
}else{

#print("# of features after CMA:")
#print(dim(CMAres$modtraindata))
}
print("Complete")
stopCluster(cl)
