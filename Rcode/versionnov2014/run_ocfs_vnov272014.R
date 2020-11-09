
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


source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/OCFS_vnov272014_v2.R")



if(FALSE){
trainy<-read.csv("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_train.labels", header=FALSE)
testy<-read.csv("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_valid.labels", header=FALSE)
trainm<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_train.data", sep=" ",header=FALSE)
testm<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_valid.data", sep=" ",header=FALSE)
}

#/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtest

#/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtest
trainy<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014//khan.ytrain", header=FALSE)
testy<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014//khan.ytest", header=FALSE)

trainy<-t(trainy)
testy<-t(testy)

#trainm<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtrain", sep=" ",header=FALSE)
#testm<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtest", sep=" ",header=FALSE)


trainm<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014//khan.xtrain")

testm<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014//khan.xtest")

trainm<-t(trainm) #[,-c(10001)]
testm<-t(testm) #[,-c(10001)]
trainm<-cbind(trainy,trainm)
testm<-cbind(testy,testm)

trainm<-na.omit(trainm)
testm<-na.omit(testm)

#outloc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/minpres2union_pso1000f0.25c0.25_backwardfalse_cvfoldthres0.7itr4w1/"
#outloc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/itr1cmapso100_best90pctf0.3c0.1behav0.2w1_0920eval10foldVIP_minpres_all2/"
#trainm<-read.csv("/home/stu/kuppal3/Research/SpectF_data/SPECTF.train", header=FALSE)
#outloc<-"/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/itr6_allmethods_minmethods1_ttestfilt_backwardsel_pso10/"


trainm<-read.csv("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/SPECTF.train", header=FALSE)
testm<-read.csv("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/SPECTF.test", header=FALSE)


outloc<-"/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014//CMAPSO100itrgmax1maxitr30fit70featw100to0.08_rankbehavf0.6l0.25c0.15100itr_randtopol/"



#trainy<-trainm[,c(1)]
#testy<-testm[,c(1)]
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
temp2=t(trainm)
temp2=apply(temp2, 2, function(x){which(x=="MD")})
temp2=unlist(temp2)
temp2=unique(temp2)
if(length(temp2)>1)
{
	trainm=trainm[,-c(temp2)]

	rm(temp2)
}


#1
CMAres<-performCMA(trainm, testm, outloc,
maxnum=0.5,
minnum=1,
stepitr=1,
gsmethods=c("rf","rfe"), #,"limma","lasso","elasticnet","kruskal.test","f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
percentTest=0.40,
accuracyweight=1,
featweight=0.06,
minpresent=1,
kname="radial",
norm_method="none", 
tolerance=0.1,
maxitrs=100,
removeindex=c(0),
classindex=1,
numfacts=0,
upclasssep=1,
dclasssep=-1,
evalmethod="CV",
numfolds=10,
CVfoldthresh=0.7,
backward.sel=TRUE,
scheme_val="one-vs-all",
iter_learn=5)




cma_feat_list<-colnames(trainm)

write.table(cma_feat_list,file="selected_cma_feat_list.txt",sep="t",row.names=FALSE)

if(FALSE)
{
trainm<-CMAres$modtrainmata
testm<-CMAres$modtestmata
trainclass<-CMAres$modtrainclass
testclass<-CMAres$modtestclass
}

#if(FALSE)
{
trainclass<-trainm[,1] #CMAres$modtrainclass
testclass<-testm[,1] #CMAres$modtestclass
trainm<-trainm[,-c(1)] #CMAres$modtrainmata
testm<-testm[,-c(1)] #CMAres$modtestmata

}

d_dim<-dim(trainm)

print("Original dimension")
print(d_dim)

#2
psores<-run_pso(trainm,trainclass,testm,testclass,transition_matrix,c1=2.05,
c2=2.05,
itr=10,
globalpso_maxitr=1,
global_max_itr=3,
num_part=10,
kname="radial",
errortype="BER",
accuracyweight=5,
featweight.max=2,
featweight.min=0.06,
numfolds=10,
followerprob=0.6,
confusionprob=0.1,
leaderprob=0.25,
wmax=1,
wmin=0.4,
behavior_reset_itr=1,
maxitrreset=3,
num_neighbors=3,
minselect.pct=1,
bootstrap_val=FALSE,
minfitnessthresh=50,
maxnum=0.5,minnum=5,inertia_method="global",particlebehav_method="randbased",constriction_factor=0.73,select.global.best=FALSE,numnodes=20)

scoringmatrix<-as.data.frame(psores$scoringmatrix)	
feat_ind<-psores$bestfeatlist
feat_names<-psores$bestfeatnames

kname<-"radial"


print("Complete")
