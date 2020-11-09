
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

#source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/svm_cv_v1.1.R")
#source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/OCFS_vnov252014.R")

#same results
#source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_vnov302014_v11.R")

source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_vnov302014_v26.R")
#source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/svm_cv_v1.1.R")

#source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/performCMA_consensus_tenfold_ber_nominal_lungheart_train_test_nov172014_backward.R")

if(FALSE){
trainy<-read.csv("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_train.labels", header=FALSE)
testy<-read.csv("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_valid.labels", header=FALSE)
trainm<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_train.data", sep=" ",header=FALSE)
testm<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_valid.data", sep=" ",header=FALSE)
}


trainm<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/IRIS_Probe/trainx.txt",header=TRUE)
testm<-NA
trainclass<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/IRIS_Probe/trainy.txt", header=TRUE)
testclass<-NA


trainclass<-as.numeric(as.factor(trainclass[,1]))
trainm<-cbind(trainclass,trainm)

#trainm<-na.omit(trainm)
#testm<-na.omit(testm)

outloc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/IRIS_Probe/OCFSv26itr1_CMAallpres1novarsel_l0.45f0.45c0.05minsel0.1randbehavfeatw0.01_local_CV/"

dir.create(outloc)
setwd(outloc)

trainm<-as.matrix(trainm)

trainclass<-trainm[,1] #CMAres$modtrainclass
trainm<-trainm[,-c(1)] #CMAres$modtrainmata


a<-c(0,0.4,0.1,0.5)
b<-c(0.2,0.3,0.4,0.1)
c<-c(0,0.4,0.4,0.2)
d<-c(0.9,0.1,0,0)

transition_matrix<-rbind(a,b,c,d)


dir.create(outloc)
setwd(outloc)
temp2=t(trainm)
temp2=apply(temp2, 2, function(x){which(x=="MD")})
temp2=unlist(temp2)
temp2=unique(temp2)
if(length(temp2)>1)
{
	trainm=trainm[,-c(temp2)]

	rm(temp2)
}

if(FALSE)
{
#1
CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=0.1,
minnum=3,
stepitr=1,
#gsmethods=c("limma","lasso","rfe"), #"rfe"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
gsmethods=c("kruskal.test", "f.test", "rfe", "rf","lasso", "elasticnet", "wilcox.test", "welch.test"),
percentTest=0.40,
accuracyweight=1,
featweight=0.06,
minpresent=1,
kname="radial",
norm_method="none",
tolerance=0.1,
maxitrs=5,
classindex=1,
numfacts=0,
evalmethod="CV",
numfolds=10,
CVfoldthresh=0.7,
varselmethod="none",
scheme_val="one-vs-all",
iter_learn=10)

cma_feat_list<-colnames(trainm)

write.table(cma_feat_list,file="selected_cma_feat_list.txt",sep="t",row.names=FALSE)

# modtraindata=modtrain, modtestdata=modtest, blindtest=testacc, modtrainclass=nci_y, modtestclass=test_y

trainm<-CMAres$modtraindata
testm<-CMAres$modtestdata
trainclass<-CMAres$modtrainclass
testclass<-CMAres$modtestclass
learningsets<-CMAres$learningsets
}

if(FALSE)
{
trainclass<-trainm[,1] #CMAres$modtrainclass
testclass<-testm[,1] #CMAres$modtestclass
trainm<-trainm[,-c(1)] #CMAres$modtrainmata
testm<-testm[,-c(1)] #CMAres$modtestmata

}

d_dim<-dim(trainm)

print("Original dimension")
print(d_dim)

#2 call run_pso function()

outloc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/IRIS_Probe/OCFSv26itr1_CMAallpres1novarsel_l0.45f0.45c0.05minsel0.1randbehavfeatw0.01_local_CV1/"

dir.create(outloc)
setwd(outloc)

source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_vnov302014_v26.R")
system.time(psores<-run_pso(outloc=outloc,trainm,trainclass,testm,testclass,transition_matrix,c1=2.05,
c2=2.05,
itr=10,
globalpso_maxitr=3,
global_max_itr=10,
num_part=20,
kname="radial",
errortype="BER",
accuracyweightA=5,
accuracyweightB=1,
featweight.max=0.01,
featweight.min=0.01,
numfolds=10,
followerprob=0.1,
confusionprob=0.05,
leaderprob=0.8,
wmax=1,
wmin=1,
behavior_reset_itr=5,
maxitrreset=3,
num_neighbors=5,
minselect.pct=0.1,
evalMode="CV1",
minfitnessthresh=50,
maxnum=3,minnum=3,inertia_method="global",particlebehav_method="randbased",constriction_factor=1,
select.global.best=TRUE,numnodes=30,evalFunc=eval_fit_kfold_diff,itr.terminate=NA))



scoringmatrix<-as.data.frame(psores$scoringmatrix)	
feat_ind<-psores$bestfeatlist
feat_names<-psores$bestfeatnames

print(scoringmatrix)
print(feat_names[feat_ind])

print("Complete")
