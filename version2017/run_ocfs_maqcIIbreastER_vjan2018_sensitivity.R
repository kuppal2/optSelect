
#.libPaths("/home/kuppal3/karan_libs/Rlibs")
library(snow)
library(e1071)
library(yaImpute)
library(pROC)
library(bioDist)
#library(CMA, lib="/home/kuppal3/karan_libs/Rlibs/")
library(RankAggreg)
library(CMA)
library(expm)

cl<-makeCluster(1)


args<-commandArgs(trailingOnly=TRUE)

dirloc<-"/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/"
#sname<-paste("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_",args[9],".R",sep="")

sname<-paste(dirloc,"version2017/OCFS_",args[9],".R",sep="")
source(sname)

outloc<-paste(dirloc,"/Datasets/MAQCII_BreastCancer/OCFSvmay2415_MAQCER",args[9],"/",sep="")


sname<-paste(dirloc,"Datasets/MAQCII_BreastCancer/MaqcIIbr.Rda",sep="")
load(sname)

trainm<-MaqcIIbr$trainx
testm<-MaqcIIbr$testx
trainclass<-MaqcIIbr$trainER #PCRvsRD
testclass<-MaqcIIbr$testER #PCRvsRD

trainm<-trainm[,-c(22284)]
testm<-testm[,-c(22284)]
trainm<-apply(trainm,2,as.numeric)
testm<-apply(testm,2,as.numeric)

trainm<-cbind(trainclass,trainm)
testm<-cbind(testclass,testm)

trainm<-na.omit(trainm)
testm<-na.omit(testm)

dir.create(outloc)
setwd(outloc)


trainm<-as.matrix(trainm)
testm<-as.matrix(testm)
trainclass<-trainm[,1] #CMAres$modtrainclass
testclass<-testm[,1] #CMAres$modtestclass
trainm<-trainm[,-c(1)] #CMAres$modtrainmata
testm<-testm[,-c(1)] #CMAres$modtestmata

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

boostweight=rep(0,dim(trainm)[2])


d_dim<-dim(trainm)

print("Original dimension")
print(d_dim)


system.time(psores<-run_pso(outloc=outloc,trainm,trainclass,testm,testclass,transition_matrix,c1=2.05,
c2=2.05,
itr=10,
globalpso_maxitr=1,
global_max_itr=5,
num_part=20,
kname="radial",
errortype="BER",
weightA<-as.numeric(args[1]),
weightB<-as.numeric(args[2]),
weightC<-as.numeric(args[3]),
weightD<-as.numeric(args[4]),
featweight.max=0.01,
featweight.min=0.01,
numfolds=10,
followerprob=as.numeric(args[6]),
confusionprob=as.numeric(args[7]),
leaderprob=as.numeric(args[8]),
wmax=1,
wmin=1,
behavior_reset_itr=50,
maxitrreset=100,
num_neighbors=3,
minselect.pct=0.5,
evalMode="CV2",
minfitnessthresh=50,
maxnum=as.numeric(args[10]),minnum=3,inertia_method=args[5],particlebehav_method="rankbased",constriction_factor=1,
select.global.best=TRUE,numnodes=4,evalFunc=eval_fit_kfold_diff,itr.terminate=FALSE,train.pct=0.8,evalmethod="MCCV",featselmethods=c("limma","lasso","rfe","elasticnet", "f.test")))



feat_ind<-psores$bestfeatlist
feat_names<-psores$bestfeatnames

scoringmatrix<-as.data.frame(psores$scoringmatrix)
print(scoringmatrix)
print(feat_names[feat_ind])

save(psores,file="psores.Rda")
print("Complete")
