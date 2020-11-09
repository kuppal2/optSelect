#library(snow)
#library(e1071)
#source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/svm_cv_func.R")

#cl<-makeCluster(8)
#cl<-makeSOCKcluster(c(rep("godel2",2),rep("godel3",3),rep("lebniz2",3),rep("leibniz3",2)))
#cl<-makeSOCKcluster("godel2",2)

#.libPaths("/home/stu/kuppal3/karan_libs/Rlibs")
library(snow)
library(e1071)
library(yaImpute)
library(pROC)
library(bioDist)
#library(CMA, lib="/home/stu/kuppal3/karan_libs/Rlibs/")

library(CMA)
library(expm)

cl<-makeCluster(20)
#cl<-makeSOCKcluster(c(rep("godel2",20),rep("godel3",20))) #,rep("lebniz2",10),rep("leibniz3",10)))



#options(echo=FALSE)
#library(e1071, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
#library(CMA, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
#library(bioDist, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
#source("/home/stu/kuppal3/Research/Genomic_imprinting/Huira/Source/PSO/svm_cv_func_10foldblindtest.R")

#source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/svm_cv_func.R")

#source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/svm_cv_v1.1.R")


source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/svm_cv_v1.1.R")

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

#source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/performCMA_consensus_tenfold_ber_nominal_lungheart_train_test_sep52013_backward.R")

source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/performCMA_consensus_tenfold_ber_nominal_lungheart_train_test_nov172014_backward.R")


#source("performCMA_consensus_tenfold_ber_nominal_lungheart_train_test_sep192012_backward.R")
#datafile<-read.csv("/home/stu/kuppal3/Research/Heart_data/data_nominal.csv", na.strings="", sep=",", header=TRUE)

#datafile<-read.csv("/home/stu/kuppal3/Research/Lung_cancer_sequences/Data_may172009/orderednominal/luca_no_missing_1056feats_nominal_ordered_events.csv", na.strings="", sep=",", header=TRUE)
#datafile<-read.csv("/home/stu/kuppal3/Research/Lung_cancer_sequences/Data_may172009/count_lung_cancer_len1to8_patterns_normbylengthmult1000_withevents.csv", na.strings="", sep=",", header=TRUE)

#outloc<-"/home/stu/kuppal3/Research/Lung_cancer_sequences/Data_may172009/regression/PSO_reg_results/itr2/"

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

#/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtest
trainy<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014//khan.ytrain", header=FALSE)
testy<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014//khan.ytest", header=FALSE)



trainy<-t(trainy)
testy<-t(testy)

#traind<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtrain", sep=" ",header=FALSE)
#testd<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtest", sep=" ",header=FALSE)


traind<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014//khan.xtrain")

testd<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014//khan.xtest")

traind<-t(traind) #[,-c(10001)]
testd<-t(testd) #[,-c(10001)]
traind<-cbind(trainy,traind)
testd<-cbind(testy,testd)

traind<-na.omit(traind)
testd<-na.omit(testd)

#outloc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/minpres2union_pso1000f0.25c0.25_backwardfalse_cvfoldthres0.7itr4w1/"
#outloc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/itr1cmapso100_best90pctf0.3c0.1behav0.2w1_0920eval10foldVIP_minpres_all2/"
#traind<-read.csv("/home/stu/kuppal3/Research/SpectF_data/SPECTF.train", header=FALSE)
#outloc<-"/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/itr6_allmethods_minmethods1_ttestfilt_backwardsel_pso10/"


traind<-read.csv("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/SPECTF.train", header=FALSE)
testd<-read.csv("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/SPECTF.test", header=FALSE)


outloc<-"/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014//CMAPSO100itrgmax1maxitr30fit70featw100to0.08_rankbehavf0.35l0.35c0.151itr_randtopol/"



#trainy<-traind[,c(1)]
#testy<-testd[,c(1)]
#a: Confusions
#b: Neighbors
#c: Global
#d: Death

a<-c(0.25,0.25,0.25,0.25)
b<-c(0.3,0.1,0.4,0.1)
c<-c(0.25,0.25,0.5,0)
d<-c(0.9,0.1,0,0.1)

a<-c(0,0.4,0.1,0.5)
b<-c(0.3,0.1,0.4,0.1)
c<-c(0,0.5,0.5,0)
d<-c(0.9,0.1,0,0)

a<-c(0,0.4,0.1,0.5)
b<-c(0.2,0.3,0.4,0.1)
c<-c(0,0.4,0.4,0.2)
d<-c(0.9,0.1,0,0)

transition_matrix<-rbind(a,b,c,d)


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
methods=c("rf","rfe","t.test","elasticnet","lasso")
methods=methods[3]
#methods=methods[5]
#methods=c("elasticnet")
percentTest=0.40
kname="radial"
maxitrs=100
removeindex=c(0)
norm_method="none" #"znorm"
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
minpresent=1 #round(0.6*length(methods))
maxfeats=1000 #dim(datafile)[2]-length(removeindex)-1
maxfeatspercent=0.5
evalmethod="CV"
numfolds=10 #dim(traind)[1]
CVfoldthresh=0.7
backward.sel=TRUE
scheme_val="one-vs-all"
iter_learn=100
######PSO parameters#########
c1=2.05
c2=2.05
itr=100
globalpso_maxitr<-10
global_max_itr=(0.5)*itr
num_part=30
kname="radial"
errortype="BER"
accuracyweight=5
featweight.max=10
featweight.min=0.08
k<-numfolds
followerprob=0.35
confusionprob=0.15
leaderprob=0.35
wmax<-1
wmin<-(-1)
behavior_reset_itr<-1 #(0.3)*itr
maxitrreset<-5 #0.2*itr
num_neighbors<-5
max_num_feats<-5 #0.1*dim(traind[,-c(1)])[1]
minselect.pct<-0.8
bootstrap_val=FALSE
minfitnessthresh<-50
max_num_feats<-44
##################

#id<-sample(1:data_dim[1],size=percentTest*data_dim[1],replace=F)

#testm<-datafile[id,]

#ncid<-datafile[-id,]



maxitr<-global_max_itr

print("Stage 1: Consensus based")
print("Stage 1: Consensus based")
#CMAres<-preProcess(datafile, outloc, percentTest,norm_method,classindex, upclasssep, dclasssep, removeindex, maxfacts)
CMAres<-performCMA(traind, testd, outloc, maxfeatspercent, minfeats, stepitr, methods, percentTest, featWeight,accWeight, kname, maxitrs, minpresent, norm_method, tolerance, classindex, upclasssep, dclasssep, removeindex, maxfacts,numfolds=k,evalmethod,CVfoldthresh,backward.sel,scheme_val,iter_learn)


#if(FALSE)
print("Stage 2: Selecting optimal subset using PSO")
#system.time(

#psores<-performPSO(trainx, testx, trainy, testy, outloc, numitrs, reinititrs, numpart, c1, c2, kname, percentTest, 0.04, fitfunc)

#psores<-performPSO(CMAres$modtraindata, CMAres$modtestdata, CMAres$modtrainclass, CMAres$modtestclass,outloc, numitrs, reinititrs, numpart, c1, c2, kname, percentTest, 0.0, 1, fitfunc)

#if(FALSE)
{
trainm<-CMAres$modtraindata
testm<-CMAres$modtestdata
trainclass<-CMAres$modtrainclass
testclass<-CMAres$modtestclass
}

cma_feat_list<-colnames(trainm)

write.table(cma_feat_list,file="selected_cma_feat_list.txt",sep="\t",row.names=FALSE)

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


if(d_dim[2]>3)
{
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
        eval_fit_kfold_diff<-function(particle, numfolds,errortype="AUC",featweight=0.06,max_num_feats=10)
        {

                num_feat<-0
                #select the columns with selected features 
                ind<-which(particle==1)

                #need to add 1 to take into account the index of the feature in the original dataset
                col_sel<-ind
		folderror<-{}
                        folderror_perm<-{}
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
			folderror_perm<-{}
                        #if(FALSE)
			{
                         for(f in 1:1)
                        {
                         
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype=errortype)

			#model<-svm(trainset, trainclass, type="C", kernel=kname, cross=numfolds)
        
			#cur_acc_vec<-model$accuracies

			#cur_acc_sderror<-sd(cur_acc_vec,na.rm=TRUE)/sqrt(length(cur_acc_vec))
			            
			#folderror_cur<-mean(cur_acc_vec,na.rm=TRUE)-(1.96*cur_acc_sderror)
	
			folderror_cur<-model$confint[1] #mean_acc
                        print(model)
                        print(folderror_cur)
                        folderror<-c(folderror,folderror_cur) #model$tot.accuracy)
			
			rm(model)

			
			#if(FALSE)
			{
			set.seed(321)
			rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))
			
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass[rand_ind],kname=kname,errortype=errortype)

			 #model<-svm(trainset, trainclass[rand_ind], type="C", kernel=kname, cross=numfolds)
                        
                        #cur_acc_vec<-model$accuracies

                        #cur_acc_sderror<-sd(cur_acc_vec,na.rm=TRUE)/sqrt(length(cur_acc_vec))

                        #folderror_cur_perm<-mean(cur_acc_vec,na.rm=TRUE)+(1.96*cur_acc_sderror)

                        #folderror_cur_perm<-model$confint[2]
                
			folderror_cur_perm<-model$mean_acc
        
						print(model)
                        print(folderror_cur_perm)
                        folderror_perm<-c(folderror_perm,folderror_cur_perm)
						rm(model)	
				}


                        }
			#	folderror<-mean(folderror,na.rm=TRUE)  #-(1.96*(sd(folderror,na.rm=TRUE)/sqrt(4)))
				#folderror<-mean(folderror,na.rm=TRUE)  
			#	folderror_perm<-mean(folderror_perm,na.rm=TRUE) #+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(4)))
                       		#folderror_perm<-mean(folderror_perm,na.rm=TRUE) #+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(3)))
			}

			
				#featweight=0.06
			
				

				if(num_feat>max_num_feats){
				fitfunc<-(accuracyweight*(folderror)*(folderror-folderror_perm))+(featweight*(1-num_feat))    #+(accuracyweight*(folderror))
				}else{

					fitfunc<-(accuracyweight*(folderror)*(folderror-folderror_perm))
				
				
				}
			fitfunc<-(accuracyweight*(folderror-folderror_perm))-(featweight*100*(num_feat/length(particle)))  #+(featweight*(1-num_feat))	
                print(paste("accuracy: ", folderror," num_feat:",num_feat," fitness:", fitfunc,sep=""))
	}
                else
                {
                folderror<-1
                folderror_perm<-100
				fitfunc<-(-100)
		}
                rm(col_sel)
                rm(num_feat)
                return(list("fitfunc"=fitfunc,"cverror"=folderror,"cvpermerror"=folderror_perm))

        }






	#function to evaluate k-fold CV
	eval_fit_test_diff<-function(particle, numfolds,trainm,trainclass,testm,testclass,errortype="AUC")
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
			if(FALSE){
			 for(f in 1:3)
                        {
			 model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype="AUC")
		
			folderror_cur<-model$mean_acc
			print(model)
			print(folderror_cur)	
			folderror<-c(folderror,folderror_cur) #model$tot.accuracy)
			
                        }
                       } 
			mod_cv <- svm(x=trainm,y=trainclass, type="C",kernel=kname)
predfit<-predict(mod_cv,testm)
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
		folderror<-svm_acc[i]


					
	
			#folderror<-model$tot.accuracy
			#folderror<-(1-ber)*100
			rm(trainset)

		}
		else
		{
		folderror<-1
		}
		fitfunc<-(accuracyweight*(folderror))+(featweight*(1-num_feat))  #-(featweight*100*(num_feat/length(particle)))  #+(featweight*(1-num_feat))
		print(paste("accuracy: ", folderror," num_feat:",num_feat," fitness:", fitfunc,sep=""))
		rm(col_sel)
		rm(num_feat)
		return(fitfunc)

	}
	


	#run_pso<-function(d_dim)
	{
	

  scoringmatrix=matrix(0,dim(trainm)[2],globalpso_maxitr)


for (globalpso_itr in 1:globalpso_maxitr)
        {	 #iitrt:<-numitrs
	 #maxitr<-50 #reinititrs
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
 	
	#prob_behavior<-rep(c(0.2,0.4,0.4,0),num_part) #runif(num_part,0,1)
	
	
	#agent_behavior<-sample(x=c(1,2,3),size=num_part,replace=TRUE)	

	#agent_behavior<-c(rep(1,round(confusionprob*num_part)),rep(2,round(followerprob*num_part)),rep(3,round((1-(confusionprob+followerprob)*num_part))))

	#agent_behavior<-c(rep(1,round(confusionprob*num_part)),rep(2,round(followerprob*num_part)),rep(3,round((1-(confusionprob+followerprob))*num_part)))
	
	#agent_behavior <- sample(x=1:dim(transition_matrix)[1], size = num_part, replace = TRUE, prob = c(confusionprob, followerprob,leaderprob,(1-(confusionprob+followerprob+leaderprob))))


		itr_data={}
        k<-0

	agent_behavior <- sample(x=1:dim(transition_matrix)[1], size = num_part, replace = TRUE, prob = c(confusionprob, followerprob,leaderprob,(1-(confusionprob+followerprob+leaderprob))))


        print(dim(scoringmatrix))
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
	
		scoringmatrix<-as.data.frame(scoringmatrix)	
		summat=apply(scoringmatrix,1,sum)
		print("dim of scoring matrix is ")
		print(dim(scoringmatrix))

		print(length(summary))

		print(summary(summat))

		bestgenelist=which(summat>=round(minselect.pct*globalpso_maxitr))

		print("numfeatg is ")
		print(length(bestgenelist))

		feat_col<-0
		#feat_list<-array("",dim(1))

		#feat_ind<-which(x_gbest==1)

			
		feat_ind<-bestgenelist

                feat_list<-feat_names[feat_ind]

                feat_col<-feat_ind
               
		if(length(feat_ind)<1){

			print(stop("No features selected!"))
		}
		
	filestr2<-paste(outloc,  "selected_feature_index_final.csv", sep="")
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
pred_acc<-multiclass.roc(testd$Class,as.numeric(pred))
        pred_acc_orig<-pred_acc$auc[1]
        auc_acc<-pred_acc_orig

print("Test AUC:")
print(auc_acc)
 filestr3<-paste(outloc, "testAUC.csv", sep="")
 write.table(auc_acc, file=filestr3, sep=",", row.names=FALSE)

pred<-predict(mod,traind[,-c(traindim)])
train.table=table(pred,traind$Class)

trainacc<-(sum(diag(train.table))/(dim(traind)[1]))
print("Train acc is ")
print(trainacc)
filestr3<-paste(outloc, "trainaccuracy.csv", sep="")
write.table(trainacc, file=filestr3, sep=",", row.names=FALSE)





}
#)
}
print("# of features after CMA:")
print(dim(CMAres$modtraindata))
print("# of features after PSO:")
print(dim(traind))
#}else{

#print("# of features after CMA:")
#print(dim(CMAres$modtraindata))
#}
print("Complete")
stopCluster(cl)
